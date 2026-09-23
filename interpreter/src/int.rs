#![allow(clippy::missing_errors_doc)]

use std::{cell::RefCell, cmp::Ordering, collections::HashMap, ops::Range, rc::Rc};

use miette::{LabeledSpan, miette};

use crate::{
    LoxError,
    ast::{Expr, ExprKind, ExprVisitor, FunctionKind, LoxValue, Stmt, StmtKind, StmtVisitor},
    call::{Class, Function, Instance, NATIVES},
    env::Environment,
};

use scanner::{INIT, SUPER, THIS, Token};

/// Maximum nesting of Lox calls. Each call recurses through the evaluator on
/// the native stack, so the limit turns runaway recursion into a Lox runtime
/// error instead of overflowing the process stack.
const MAX_CALL_DEPTH: usize = 1024;

pub struct Interpreter<'a, W: std::io::Write> {
    /// Current environment that keeps current scope vars. Global by default
    environment: Rc<RefCell<Environment<'a>>>,
    globals: Rc<RefCell<Environment<'a>>>,
    writer: W,
    locals: HashMap<u64, usize>,
    /// Value of the `return` statement currently unwinding via [`LoxError::Return`].
    returned: Option<LoxValue<'a>>,
    call_depth: usize,
}

impl<'a, W: std::io::Write> Interpreter<'a, W> {
    #[must_use]
    pub fn new(writer: W) -> Self {
        let globals = Rc::new(RefCell::new(Environment::new()));
        for native in NATIVES {
            globals
                .borrow_mut()
                .define(native.name(), LoxValue::Native(native));
        }
        Self {
            environment: globals.clone(),
            globals,
            writer,
            locals: HashMap::new(),
            returned: None,
            call_depth: 0,
        }
    }

    /// Evaluates an expression. Errors raised without a source location get
    /// the location of the innermost expression that failed.
    pub fn evaluate(&mut self, expr: &Expr<'a>) -> crate::Result<LoxValue<'a>> {
        expr.accept(self).map_err(|e| match e {
            LoxError::Error(report) if report.labels().is_none() => {
                runtime_error(expr.location.clone(), &report.to_string())
            }
            other => other,
        })
    }

    pub fn resolve(&mut self, obj: &Expr<'a>, depth: usize) {
        self.locals.insert(obj.get_hash_code(), depth);
    }

    /// Executes statements until the first runtime error, which is returned.
    pub fn interpret(&mut self, statements: &'a [crate::Result<Stmt<'a>>]) -> crate::Result<()> {
        for statement in statements {
            self.execute(statement)?;
        }
        Ok(())
    }

    fn execute(&mut self, statement: &'a crate::Result<Stmt<'a>>) -> crate::Result<()> {
        match statement {
            Ok(s) => s.accept(self),
            Err(err) => Err(err.duplicate()),
        }
    }

    fn execute_block(
        &mut self,
        statements: &'a [crate::Result<Stmt<'a>>],
        environment: Rc<RefCell<Environment<'a>>>,
    ) -> crate::Result<()> {
        let previous = std::mem::replace(&mut self.environment, environment);
        let result = self.interpret(statements);
        self.environment = previous;
        result
    }

    fn lookup_variable(&self, obj: &Expr<'a>, name: &str) -> crate::Result<LoxValue<'a>> {
        if let Some(distance) = self.locals.get(&obj.get_hash_code()) {
            self.environment.borrow().get_at(*distance, name)
        } else {
            self.globals.borrow().get(name)
        }
    }

    fn call_value(
        &mut self,
        callee: &LoxValue<'a>,
        arguments: &[LoxValue<'a>],
    ) -> crate::Result<LoxValue<'a>> {
        match callee {
            LoxValue::Native(native) => {
                check_arity(native.arity(), arguments.len())?;
                native.call(arguments)
            }
            LoxValue::Function(function) => {
                check_arity(function.arity(), arguments.len())?;
                self.call_function(function, arguments)
            }
            LoxValue::Class(class) => {
                check_arity(class.arity(), arguments.len())?;
                let instance = Rc::new(RefCell::new(Instance::new(class.clone())));
                if let Some(init) = class.find_method(INIT) {
                    self.call_function(&init.bind(instance.clone()), arguments)?;
                }
                Ok(LoxValue::Instance(instance))
            }
            _ => Err(LoxError::Error(miette!(
                "Can only call functions and classes."
            ))),
        }
    }

    fn call_function(
        &mut self,
        function: &Function<'a>,
        arguments: &[LoxValue<'a>],
    ) -> crate::Result<LoxValue<'a>> {
        if self.call_depth >= MAX_CALL_DEPTH {
            return Err(LoxError::Error(miette!("Stack overflow.")));
        }
        let mut environment = Environment::child(function.closure());
        for (parameter, argument) in function.parameters().iter().zip(arguments) {
            environment.define(parameter, argument.clone());
        }

        self.call_depth += 1;
        let result = self.execute_block(function.body(), Rc::new(RefCell::new(environment)));
        self.call_depth -= 1;

        let value = match result {
            Ok(()) => LoxValue::Nil,
            Err(LoxError::Return) => self.returned.take().unwrap_or(LoxValue::Nil),
            Err(e) => return Err(e),
        };
        if function.is_initializer() {
            // `init` always returns `this`, which `bind` put in its closure.
            return function.closure().borrow().get_at(0, THIS);
        }
        Ok(value)
    }
}

fn runtime_error(location: Range<usize>, message: &str) -> LoxError {
    LoxError::Error(miette!(
        labels = vec![LabeledSpan::at(location, message)],
        "Runtime error"
    ))
}

fn check_arity(arity: usize, args_count: usize) -> crate::Result<()> {
    if arity == args_count {
        Ok(())
    } else {
        Err(LoxError::Error(miette!(
            "Expected {arity} arguments but got {args_count}."
        )))
    }
}

fn identifier<'a>(token: &Token<'a>) -> crate::Result<&'a str> {
    if let Token::Identifier(id) = token {
        Ok(id)
    } else {
        Err(LoxError::Error(miette!(
            "Expected identifier but was '{token}'"
        )))
    }
}

fn parameter_names<'a>(params: &[Box<Expr<'a>>]) -> Vec<&'a str> {
    params
        .iter()
        .filter_map(|param| match param.kind {
            ExprKind::Variable(Token::Identifier(name)) => Some(name),
            _ => None,
        })
        .collect()
}

/// Statements of a function body. The parser always produces a block here.
pub(crate) fn function_body<'a>(
    body: &'a crate::Result<Stmt<'a>>,
) -> crate::Result<&'a [crate::Result<Stmt<'a>>]> {
    match body {
        Ok(Stmt {
            kind: StmtKind::Block(statements),
            ..
        }) => Ok(statements),
        Ok(stmt) => Err(runtime_error(
            stmt.location.clone(),
            "Function body must be a block",
        )),
        Err(e) => Err(e.duplicate()),
    }
}

fn numbers(lhs: &LoxValue, rhs: &LoxValue) -> crate::Result<(f64, f64)> {
    if let (LoxValue::Number(l), LoxValue::Number(r)) = (lhs, rhs) {
        Ok((*l, *r))
    } else {
        Err(LoxError::Error(miette!("Operands must be numbers.")))
    }
}

/// Orders numbers or strings. `None` means unordered (a NaN operand).
fn compare(lhs: &LoxValue, rhs: &LoxValue) -> crate::Result<Option<Ordering>> {
    match (lhs, rhs) {
        (LoxValue::Number(l), LoxValue::Number(r)) => Ok(l.partial_cmp(r)),
        (LoxValue::String(l), LoxValue::String(r)) => Ok(Some(l.cmp(r))),
        _ => Err(LoxError::Error(miette!("Operands must be numbers."))),
    }
}

impl<'a, W: std::io::Write> ExprVisitor<'a, crate::Result<LoxValue<'a>>> for Interpreter<'a, W> {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> crate::Result<LoxValue<'a>> {
        match token {
            Some(Token::String(s)) => Ok(LoxValue::String((*s).to_string())),
            Some(Token::Number(n)) => Ok(LoxValue::Number(*n)),
            Some(Token::False) => Ok(LoxValue::Bool(false)),
            Some(Token::True) => Ok(LoxValue::Bool(true)),
            Some(Token::Nil) | None => Ok(LoxValue::Nil),
            Some(_) => Err(LoxError::Error(miette!("Invalid literal"))),
        }
    }

    fn visit_binary_expr(
        &mut self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> crate::Result<LoxValue<'a>> {
        let lhs = self.evaluate(left)?;
        let rhs = self.evaluate(right)?;

        let value = match operator {
            Token::Minus => {
                let (l, r) = numbers(&lhs, &rhs)?;
                LoxValue::Number(l - r)
            }
            Token::Slash => {
                let (l, r) = numbers(&lhs, &rhs)?;
                LoxValue::Number(l / r)
            }
            Token::Star => {
                let (l, r) = numbers(&lhs, &rhs)?;
                LoxValue::Number(l * r)
            }
            Token::Plus => match (lhs, rhs) {
                (LoxValue::Number(l), LoxValue::Number(r)) => LoxValue::Number(l + r),
                (LoxValue::String(l), LoxValue::String(r)) => LoxValue::String(l + &r),
                _ => {
                    return Err(LoxError::Error(miette!(
                        "Operands must be two numbers or two strings."
                    )));
                }
            },
            Token::BangEqual => LoxValue::Bool(!lhs.equal(&rhs)),
            Token::EqualEqual => LoxValue::Bool(lhs.equal(&rhs)),
            Token::Greater => LoxValue::Bool(compare(&lhs, &rhs)? == Some(Ordering::Greater)),
            Token::GreaterEqual => LoxValue::Bool(matches!(
                compare(&lhs, &rhs)?,
                Some(Ordering::Greater | Ordering::Equal)
            )),
            Token::Less => LoxValue::Bool(compare(&lhs, &rhs)? == Some(Ordering::Less)),
            Token::LessEqual => LoxValue::Bool(matches!(
                compare(&lhs, &rhs)?,
                Some(Ordering::Less | Ordering::Equal)
            )),
            _ => return Err(LoxError::Error(miette!("Invalid binary operator"))),
        };
        Ok(value)
    }

    fn visit_unary_expr(
        &mut self,
        operator: &Token<'a>,
        expr: &Expr<'a>,
    ) -> crate::Result<LoxValue<'a>> {
        let val = self.evaluate(expr)?;
        match (operator, val) {
            (Token::Minus, LoxValue::Number(n)) => Ok(LoxValue::Number(-n)),
            (Token::Minus, _) => Err(LoxError::Error(miette!("Operand must be a number."))),
            (Token::Bang, val) => Ok(LoxValue::Bool(!val.is_truthy())),
            _ => Err(LoxError::Error(miette!("Invalid unary operator"))),
        }
    }

    fn visit_assign_expr(&mut self, lhs: &Expr<'a>, rhs: &Expr<'a>) -> crate::Result<LoxValue<'a>> {
        let ExprKind::Variable(name) = &lhs.kind else {
            return Err(runtime_error(
                lhs.location.clone(),
                "Invalid assignment target.",
            ));
        };
        let id = identifier(name)?;
        let value = self.evaluate(rhs)?;

        if let Some(distance) = self.locals.get(&lhs.get_hash_code()) {
            self.environment
                .borrow_mut()
                .assign_at(*distance, id, value.clone())?;
        } else {
            self.globals.borrow_mut().assign(id, value.clone())?;
        }
        Ok(value)
    }

    fn visit_call_expr(
        &mut self,
        _paren: &Token<'a>,
        callee: &Expr<'a>,
        args: &[Box<Expr<'a>>],
    ) -> crate::Result<LoxValue<'a>> {
        let callee = self.evaluate(callee)?;
        let mut arguments = Vec::with_capacity(args.len());
        for arg in args {
            arguments.push(self.evaluate(arg)?);
        }
        self.call_value(&callee, &arguments)
    }

    fn visit_get_expr(
        &mut self,
        name: &Token<'a>,
        object: &Expr<'a>,
    ) -> crate::Result<LoxValue<'a>> {
        let object = self.evaluate(object)?;
        let name = identifier(name)?;
        let LoxValue::Instance(instance) = object else {
            return Err(LoxError::Error(miette!("Only instances have properties.")));
        };
        // Fields shadow methods.
        let field = instance.borrow().field(name);
        if let Some(value) = field {
            return Ok(value);
        }
        let method = instance.borrow().class().find_method(name);
        match method {
            Some(method) => Ok(LoxValue::Function(Rc::new(method.bind(instance)))),
            None => Err(LoxError::Error(miette!("Undefined property '{name}'."))),
        }
    }

    fn visit_grouping_expr(&mut self, grouping: &Expr<'a>) -> crate::Result<LoxValue<'a>> {
        self.evaluate(grouping)
    }

    fn visit_logical_expr(
        &mut self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> crate::Result<LoxValue<'a>> {
        let lhs = self.evaluate(left)?;
        match operator {
            Token::And if !lhs.is_truthy() => Ok(lhs),
            Token::Or if lhs.is_truthy() => Ok(lhs),
            Token::And | Token::Or => self.evaluate(right),
            _ => Err(LoxError::Error(miette!("Invalid logical operator"))),
        }
    }

    fn visit_set_expr(
        &mut self,
        field: &Token<'a>,
        obj: &Expr<'a>,
        val: &Expr<'a>,
    ) -> crate::Result<LoxValue<'a>> {
        let field = identifier(field)?;
        // `obj` is the whole `object.field` target; only the object is evaluated.
        let ExprKind::Get(_, object) = &obj.kind else {
            return Err(LoxError::Error(miette!("Only instances have fields.")));
        };
        let LoxValue::Instance(instance) = self.evaluate(object)? else {
            return Err(LoxError::Error(miette!("Only instances have fields.")));
        };
        let value = self.evaluate(val)?;
        instance.borrow_mut().set_field(field, value.clone());
        Ok(value)
    }

    fn visit_super_expr(
        &mut self,
        obj: &Expr<'a>,
        _keyword: &Token<'a>,
        method: &Token<'a>,
    ) -> crate::Result<LoxValue<'a>> {
        let method = identifier(method)?;
        let invalid_super = || LoxError::Error(miette!("Invalid 'super' usage."));
        let distance = *self
            .locals
            .get(&obj.get_hash_code())
            .ok_or_else(invalid_super)?;
        // The resolver puts the `this` scope right inside the `super` scope.
        let this_distance = distance.checked_sub(1).ok_or_else(invalid_super)?;
        let superclass = self.environment.borrow().get_at(distance, SUPER)?;
        let this = self.environment.borrow().get_at(this_distance, THIS)?;
        let (LoxValue::Class(superclass), LoxValue::Instance(instance)) = (superclass, this) else {
            return Err(invalid_super());
        };
        match superclass.find_method(method) {
            Some(method) => Ok(LoxValue::Function(Rc::new(method.bind(instance)))),
            None => Err(LoxError::Error(miette!("Undefined property '{method}'."))),
        }
    }

    fn visit_this_expr(&mut self, obj: &Expr<'a>, _: &Token<'a>) -> crate::Result<LoxValue<'a>> {
        self.lookup_variable(obj, THIS)
    }

    fn visit_variable_expr(
        &mut self,
        obj: &Expr<'a>,
        name: &Token<'a>,
    ) -> crate::Result<LoxValue<'a>> {
        let id = identifier(name)?;
        self.lookup_variable(obj, id)
    }
}

impl<'a, W: std::io::Write> StmtVisitor<'a, crate::Result<()>> for Interpreter<'a, W> {
    fn visit_block_stmt(&mut self, body: &'a [crate::Result<Stmt<'a>>]) -> crate::Result<()> {
        let environment = Environment::child(self.environment.clone());
        self.execute_block(body, Rc::new(RefCell::new(environment)))
    }

    fn visit_class_stmt(
        &mut self,
        name: &Token<'a>,
        superclass: &Option<Box<Expr<'a>>>,
        methods: &'a [crate::Result<Stmt<'a>>],
    ) -> crate::Result<()> {
        let id = identifier(name)?;
        let superclass = match superclass {
            Some(expr) => match self.evaluate(expr)? {
                LoxValue::Class(class) => Some(class),
                _ => {
                    return Err(runtime_error(
                        expr.location.clone(),
                        "Superclass must be a class.",
                    ));
                }
            },
            None => None,
        };
        self.environment.borrow_mut().define(id, LoxValue::Nil);

        // Methods close over a scope holding `super`, mirroring the resolver.
        let method_env = if let Some(superclass) = &superclass {
            let mut env = Environment::child(self.environment.clone());
            env.define(SUPER, LoxValue::Class(superclass.clone()));
            Rc::new(RefCell::new(env))
        } else {
            self.environment.clone()
        };

        let mut table = HashMap::new();
        for method in methods {
            let method = method.as_ref().map_err(LoxError::duplicate)?;
            let StmtKind::Function(_, token, params, body) = &method.kind else {
                return Err(runtime_error(
                    method.location.clone(),
                    "Class body may only contain methods",
                ));
            };
            let method_name = identifier(token)?;
            let function = Function::new(
                method_name,
                parameter_names(params),
                function_body(body)?,
                method_env.clone(),
                method_name == INIT,
            );
            table.insert(method_name, Rc::new(function));
        }

        let class = Class::new(id, superclass, table);
        self.environment
            .borrow_mut()
            .define(id, LoxValue::Class(Rc::new(class)));
        Ok(())
    }

    fn visit_expression_stmt(&mut self, expr: &Expr<'a>) -> crate::Result<()> {
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_function_decl_stmt(
        &mut self,
        _kind: FunctionKind,
        token: &Token<'a>,
        params: &[Box<Expr<'a>>],
        body: &'a crate::Result<Stmt<'a>>,
    ) -> crate::Result<()> {
        let id = identifier(token)?;
        let function = Function::new(
            id,
            parameter_names(params),
            function_body(body)?,
            self.environment.clone(),
            false,
        );
        self.environment
            .borrow_mut()
            .define(id, LoxValue::Function(Rc::new(function)));
        Ok(())
    }

    fn visit_if_stmt(
        &mut self,
        cond: &Expr<'a>,
        then: &'a crate::Result<Stmt<'a>>,
        otherwise: &'a Option<Box<crate::Result<Stmt<'a>>>>,
    ) -> crate::Result<()> {
        if self.evaluate(cond)?.is_truthy() {
            self.execute(then)
        } else if let Some(otherwise) = otherwise {
            self.execute(otherwise)
        } else {
            Ok(())
        }
    }

    fn visit_print_stmt(&mut self, expr: &Expr<'a>) -> crate::Result<()> {
        let val = self.evaluate(expr)?;
        writeln!(self.writer, "{val}").map_err(|e| LoxError::Error(miette!(e)))
    }

    fn visit_return_stmt(&mut self, _keyword: &Token<'a>, value: &Expr<'a>) -> crate::Result<()> {
        self.returned = Some(self.evaluate(value)?);
        Err(LoxError::Return)
    }

    fn visit_variable_stmt(
        &mut self,
        name: &Token<'a>,
        initializer: &Option<Box<Expr<'a>>>,
    ) -> crate::Result<()> {
        let id = identifier(name)?;
        let value = match initializer {
            Some(init) => self.evaluate(init)?,
            None => LoxValue::Nil,
        };
        self.environment.borrow_mut().define(id, value);
        Ok(())
    }

    fn visit_while_stmt(
        &mut self,
        cond: &Expr<'a>,
        body: &'a crate::Result<Stmt<'a>>,
    ) -> crate::Result<()> {
        while self.evaluate(cond)?.is_truthy() {
            self.execute(body)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::Parser, resolver::Resolver};

    use super::*;
    use test_case::test_case;

    #[test_case("print 1+2;", "3")]
    #[test_case("var x; x = 2; var y = 4; print x+y;", "6")]
    #[test_case("var a = 1; var a; print a;", "nil")]
    #[test_case("var a; print a = \"arg\";", "arg")]
    #[test_case(
        "var a = 1; var b; { var a = 2; b = 3; print a; } print a; print b;",
        "2\n1\n3"
    )]
    #[test_case("var a = 1; if (a == 1) { print 10; } else { print 20; }", "10")]
    #[test_case("var a = 1; if (a != 1) { print 10; } else { print 20; }", "20")]
    #[test_case("var a = 1; if (a == 1) { print 10; }", "10")]
    #[test_case("var a = 1; if (a == 2) { print 10; }", "")]
    #[test_case("var a = false; if (a = true) { print 10; }", "10" ; "assignment in condition")]
    #[test_case("var a = 1; var b = 2; if (a < b and b > 1) { print 10; } else print 20;", "10" ; "and logic then")]
    #[test_case("var a = 1; var b = 2; if (a < b and b > 10) { print 10; } else print 20;", "20" ; "and logic otherwise")]
    #[test_case("var a = 1; var b = 1; if (a < b or b > 0) { print 10; } else print 20;", "10" ; "or logic then")]
    #[test_case("var a = 2; var b = 2; if (a < b or b < 1) { print 10; } else print 20;", "20" ; "or logic otherwise")]
    #[test_case("if (nil == 1) print 10;", "" ; "nil eq")]
    #[test_case("if (true == 1) print 10;", "" ; "bool eq")]
    #[test_case("if (nil != 1) print 10;", "10" ; "nil ne")]
    #[test_case("if (true != 1) print 10;", "10" ; "bool ne")]
    #[test_case("var i = 0; while (i < 10) i = i + 1; print i;", "10" ; "while test")]
    #[test_case("for(var i = 0; i < 3; i = i + 1) print i;", "0\n1\n2" ; "for test")]
    #[test_case("var i = 0; for(; i < 3; i = i + 1) print i;", "0\n1\n2" ; "for test without initializer")]
    #[test_case("print clock() - clock() <= 0;", "true" ; "simple clock call")]
    #[test_case("print sqrt(9);", "3" ; "use sqrt call")]
    #[test_case("print min(10, 20);", "10" ; "use min call")]
    #[test_case("print max(10, 20);", "20" ; "use max call")]
    #[test_case("if (clock() > 0) print \"good\"; else print \"impossible\";", "good" ; "call in predicate")]
    #[test_case("fun x(v) { } print x(10);", "nil" ; "empty function body")]
    #[test_case("fun x(v) { print v; } print x(10);", "10\nnil" ; "simple call one arg")]
    #[test_case("fun sum(a1, a2) { print a1 + a2; } sum(1, 2);", "3" ; "simple call two args")]
    #[test_case("fun sum_and_decr(a1, a2) { var x = a1 + a2 - 1; print x; } sum_and_decr(1, 2);", "2" ; "function with two statements")]
    #[test_case("fun foo(x) { return x + 1; } print foo(1);", "2" ; "function with return")]
    #[test_case("fun foo() { return bar; } fun bar(x, y) { return x + y; } print foo()(1, 2);", "3" ; "cascade call")]
    #[test_case("fun foo() { var i = 1; fun bar(x) { return i + x; } return bar; } print foo()(2);", "3" ; "closure")]
    #[test_case("fun fib(n) { if (n < 2) return n; return fib(n - 1) + fib(n - 2); } print fib(8);", "21" ; "fibonacci")]
    #[test_case("fun foo(n) { if (n < 2) return n; return 10; } print foo(1);", "1" ; "conditional return success")]
    #[test_case("fun foo(n) { if (n < 2) return n; return 10; } print foo(5);", "10" ; "conditional return fail")]
    #[test_case("class Foo { method(x) { print x;} }", "" ; "class")]
    #[test_case("class Bagel{} var b = Bagel(); print b;", "Bagel instance" ; "class instance empty")]
    #[test_case("class Bagel{} var b = Bagel(); { var b = Bagel(); b.field = 1; print b.field; } b.field = 2; print b.field;", "1\n2" ; "get/set class field complex")]
    #[test_case("class Bagel{} var b = Bagel(); { b.field = 1; } print b.field; b.field = 2; print b.field;", "1\n2" ; "get/set class field complex no shadowing")]
    #[test_case("class Bagel{} var b; b = Bagel(); { b.field = 1; } print b.field; b.field = 2; print b.field;", "1\n2" ; "get/set class field complex no shadowing assignment")]
    #[test_case("class Bagel{} var b = Bagel(); b.field = 1; print b.field;", "1" ; "get/set class field")]
    #[test_case("class Bagel{} var b; b = Bagel(); b.field = 1; print b.field;", "1" ; "class instance assign and get/set class field")]
    #[test_case("class Bagel{} var b; { b = Bagel(); b.field = 1; } b.field = 2; print b.field;", "2" ; "class instance init inside child scope and get/set class field")]
    #[test_case("class Bagel { method() { print 10;} } var b = Bagel(); b.method();", "10" ; "call class method")]
    #[test_case("class Foo { method() { } } var foo = Foo(); foo.a = 1; var i = 0; while (i < 2) { print foo.a; i = i + 1; }", "1\n1" ; "class with empty body method and use it from while")]
    #[test_case("class Foo { method() { } } var foo = Foo(); foo.a = 1; print foo.a;", "1" ; "class with empty body method and use it")]
    #[test_case("class Foo {} var foo = Foo(); fun bar() { print 10; } foo.meth = bar; print foo.meth();", "10\nnil" ; "call property with stored function")]
    #[test_case("class Bagel { method() { print 10;} } Bagel().method();", "10" ; "call class method without instance in var")]
    #[test_case("class Bagel { method() { print 10;} } var b = Bagel().method; b();", "10" ; "call class method from assigned var")]
    #[test_case("class Class { method() { print this.some; } } var c = Class(); c.some = 10; c.method();", "10" ; "this usage")]
    #[test_case("class Class { init() { this.some = 10; } method() { print this.some; } } var c = Class(); c.method();", "10" ; "class constructor")]
    #[test_case("class Class { init(x) { this.some = x; } method() { print this.some; } } var c = Class(10); c.method();", "10" ; "class constructor with arg")]
    #[test_case("class Class { init(x) { this.some = x; } method() { print this.some; } } var c = Class(0); c.init(10); c.method();", "10" ; "class constructor with arg and invoking ctor directly")]
    #[test_case("class Class { init(x) { this.some = x; } method() { print this.some; } } var c = Class(0).init(10); c.method();", "10" ; "class constructor with arg and invoking ctor directly from instance")]
    #[test_case("class A {} fun f() { class B < A {} return B; } print f();", "B" ; "Local class inherits from global")]
    #[test_case("class A { af() { print 10; }} class B < A { bf() { print 5; } } print B().af();", "10\nnil" ; "Call inherited method")]
    #[test_case("class A { af() { print 10; }} class B < A { bf() { this.af(); } } print B().bf();", "10\nnil" ; "Call inherited method inside other")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { super.method(); }} class C < B {} C().test();", "A" ; "Call super method inside grandchild class")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { super.method(); }} class C < B {} var c =C(); c.test();", "A" ; "Call super method inside grandchild class var variant")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { super.method(); }} B().test();", "A" ; "Call super method when shadowed defined in class")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { this.method(); }} B().test();", "B" ; "Call this method when shadowed defined in class")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { this.method(); }} class C < B {} C().test();", "B" ; "Call super method when shadowed defined in class and call shadowed")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { this.method(); }} class C < B {} var c = C(); c.test();", "B" ; "Call super method when shadowed defined in class and call shadowed var variant")]
    #[test_case("class A { init(param) { this.field = param; } test() { print this.field; } } class B < A {} var b = B(10); b.test();", "10" ; "Call superclass with parameter init subclass without parameter init")]
    #[test_case("class A { init(x) { this.f1 = x; } test() { return this.f1; } } class B < A { init(x, y) { this.f1 = x; this.f2 = y; } sum() { return this.test() + this.f1 + this.f2; } } var b = B(10, 20); print b.sum();", "40" ; "Call superclass with less init parameters then subclass")]
    #[test_case("class Foo{ init(arg) { print 1; } } fun init() { print 0; } init();", "0" ; "Plain function with init name")]
    #[test_case(r#"class Base {
      init(a) {
        this.a = a;
      }
    }

    class Derived < Base {
      init(a, b) {
        super.init(a);
        this.b = b;
      }
    }

    var derived = Derived("a", "b");
    print derived.a; // expect: a
    print derived.b; // expect: b"#, "a\nb" ; "this in superclass method")]
    #[test_case(
        "fun make() { var a = \"A\"; var b = \"B\"; fun read() { print a; print b; } return read; } var f = make(); f();",
        "A\nB" ; "closure return captures two locals"
    )]
    #[test_case(r#"
class A { m() { print this.x; } }
var a = A(); a.x = 1;
var b = A(); b.x = 2;
a.m();
"#, "1" ; "this test")]
    #[test_case("fun mk(x) { fun g() { return x; } return g; } var a = mk(1); var b = mk(2); print a(); print b();", "1\n2" ; "each closure keeps its own scope")]
    #[test_case("{ fun f() { return 1; } print f(); } { fun f() { return 2; } print f(); }", "1\n2" ; "same function name in sibling scopes")]
    #[test_case("var f = 1; { fun f() { return 2; } print f(); } print f;", "2\n1" ; "local function shadows global variable")]
    #[test_case("for (var i = 1; i < 3; i = i + 1) { fun f() { return i; } print f(); }", "1\n2" ; "function declared in loop body")]
    #[test_case("class A { m() { return \"method\"; } } var a = A(); a.m = \"field\"; print a.m;", "field" ; "field shadows method")]
    #[test_case("class A { get() { return this.n; } } var a = A(); a.n = 1; var b = A(); b.n = 2; var m = a.get; print b.get(); print m();", "2\n1" ; "bound method keeps its receiver")]
    #[test_case("class A {} var a = A(); print A == A; print a == a; print a == A(); fun f() {} print f == f;", "true\ntrue\nfalse\ntrue" ; "reference equality")]
    #[test_case("class A { init() { print \"init\"; return; print \"nope\"; } } var a = A(); print a.init();", "init\ninit\nA instance" ; "early return from initializer")]
    #[test_case("class Base { init(a, b) { print a + b; } } class Derived < Base { init() { super.init(\"x\", \"y\"); } } Derived();", "xy" ; "explicit super initializer with other arity")]
    #[test_case("class A { m() { return \"A\"; } } class B < A { m() { var s = super.m; return s(); } } print B().m();", "A" ; "super method as value")]
    #[test_case("fun foo() { return \"global\"; } class A { foo() { return foo(); } } print A().foo();", "global" ; "method name does not shadow global function")]
    #[test_case("class A {} class A {} print A;", "A" ; "class redeclaration at global scope")]
    #[test_case("print nil == false; print 0 == -0; var nan = 0/0; print nan == nan; print 1/0;", "false\ntrue\nfalse\ninf" ; "equality and IEEE division")]
    #[test_case("print 0.1 + 0.2 == 0.3; print 0.000001 == 0;", "false\nfalse" ; "exact number equality")]
    #[test_case("fun f() {} print f; print clock; class A { m() {} } print A().m;", "<fn f>\n<native fn>\n<fn m>" ; "callables formatting")]
    fn interpretation_positive(input: &str, expected: &str) {
        // Arrange
        let mut parser = Parser::new(input);
        let mut stdout = Vec::new();
        let interpreter = Interpreter::new(&mut stdout);
        let resolver = Resolver::new(interpreter);
        let stmts: Vec<crate::Result<Stmt>> = parser.collect();

        // Act
        let interpretation_result = resolver.interpret(&stmts);

        // Assert
        if let Err(e) = interpretation_result {
            panic!("interpretation_result should be Ok. But it was: {e:#?}. \nText: {input}");
        }

        let actual = String::from_utf8(stdout).unwrap();
        assert_eq!(actual.trim_end(), expected);
    }

    #[test_case("fun f() 123;" ; "Invalid function body")]
    #[test_case("fun foo() {} class Subclass < foo {}" ; "Inherit from function")]
    #[test_case("var Nil = nil; class Foo < Nil {}" ; "Inherit from nil")]
    #[test_case("class A {} class B < A { method() { super.; } } var b = B(); b.method();" ; "Super without name with run")]
    #[test_case("class A {} class B < A { method() { super.; } }" ; "Super without name no run")]
    fn interpretation_negative(input: &str) {
        // Arrange
        let mut parser = Parser::new(input);
        let mut stdout = Vec::new();
        let interpreter = Interpreter::new(&mut stdout);
        let resolver = Resolver::new(interpreter);
        let stmts: Vec<crate::Result<Stmt>> = parser.collect();

        // Act
        let interpretation_result = resolver.interpret(&stmts);

        // Assert
        assert!(
            interpretation_result.is_err(),
            "interpretation_result should be Error. But it was OK. \nText: {input}"
        )
    }

    #[test_case("print \"before\"; print -\"x\"; print \"after\";", "before", "Operand must be a number." ; "runtime error stops execution")]
    #[test_case("{ print 1; nil(); print 2; } print 3;", "1", "Can only call functions and classes." ; "runtime error inside block")]
    #[test_case("var secret = 1; class A {} print A().secret;", "", "Undefined property 'secret'." ; "field lookup does not reach enclosing scopes")]
    #[test_case("print \"a\" + 4;", "", "Operands must be two numbers or two strings." ; "no implicit string conversion")]
    #[test_case("print nil < false;", "", "Operands must be numbers." ; "only numbers and strings are ordered")]
    #[test_case("class A {} A(1, 2);", "", "Expected 0 arguments but got 2." ; "class without initializer takes no arguments")]
    #[test_case("class A { init(x) {} } class B < A { init() {} } B(1);", "", "Expected 0 arguments but got 1." ; "subclass initializer arity")]
    #[test_case("var NotClass = 1; class A < NotClass {}", "", "Superclass must be a class." ; "superclass must be a class")]
    #[test_case("print undefined;", "", "Undefined variable 'undefined'." ; "undefined variable")]
    fn interpretation_runtime_error(input: &str, expected_output: &str, message: &str) {
        // Arrange
        let mut parser = Parser::new(input);
        let mut stdout = Vec::new();
        let interpreter = Interpreter::new(&mut stdout);
        let resolver = Resolver::new(interpreter);
        let stmts: Vec<crate::Result<Stmt>> = parser.collect();

        // Act
        let result = resolver.interpret(&stmts);

        // Assert
        let Err(LoxError::Error(report)) = result else {
            panic!("expected a runtime error for: {input}");
        };
        let labels: Vec<String> = report
            .labels()
            .into_iter()
            .flatten()
            .filter_map(|l| l.label().map(str::to_owned))
            .collect();
        assert!(
            labels.iter().any(|l| l.contains(message)),
            "expected '{message}' in {labels:?}"
        );
        assert_eq!(
            String::from_utf8(stdout).unwrap().trim_end(),
            expected_output
        );
    }

    #[test_case("{ var a = 1; var a = 2; }" ; "duplicate local")]
    #[test_case("fun f(a) { var a; }" ; "local collides with parameter")]
    #[test_case("fun f(a, a) {}" ; "duplicate parameter")]
    #[test_case("class A { init() { return 1; } }" ; "value returned from initializer")]
    #[test_case("print 1; { var a = a; }" ; "local read in its own initializer")]
    fn static_error_prevents_execution(input: &str) {
        // Arrange
        let mut parser = Parser::new(input);
        let mut stdout = Vec::new();
        let interpreter = Interpreter::new(&mut stdout);
        let resolver = Resolver::new(interpreter);
        let stmts: Vec<crate::Result<Stmt>> = parser.collect();

        // Act
        let result = resolver.interpret(&stmts);

        // Assert
        assert!(result.is_err());
        assert!(stdout.is_empty());
    }

    #[test]
    fn class_with_superclass_extra_initializer_arguments_is_error() {
        // Arrange: initializers are not chained implicitly, so B(10, 20)
        // must match B's own `init(x)`.
        let input = "class A { init(x, y) {} } class B < A { init(x) {} } B(10, 20);";
        let mut parser = Parser::new(input);
        let mut stdout = Vec::new();
        let resolver = Resolver::new(Interpreter::new(&mut stdout));
        let stmts: Vec<crate::Result<Stmt>> = parser.collect();

        // Act
        let result = resolver.interpret(&stmts);

        // Assert
        assert!(result.is_err());
    }
}
