#![allow(clippy::missing_errors_doc)]

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use miette::{LabeledSpan, SourceSpan, miette};

use crate::{
    LoxError,
    ast::{Expr, ExprKind, ExprVisitor, FunctionKind, LoxValue, Stmt, StmtVisitor},
    call::{self, CallResult, Catalogue, Class, Clock, Function, LoxCallable},
    env::Environment,
    lexer::Token,
};

pub struct Interpreter<'a, W: std::io::Write> {
    /// Current environment that keeps current scope vars. Global by default
    environment: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    callables: Box<Catalogue<'a>>,
    writer: W,
    locals: HashMap<u64, usize>,
    class_methods: Vec<Function<'a>>,
}

impl<'a, W: std::io::Write> Interpreter<'a, W> {
    #[must_use]
    pub fn new(writer: W) -> Self {
        let environment = Rc::new(RefCell::new(Environment::new()));
        let globals = environment.clone();
        let mut callables = Box::new(Catalogue::new());
        globals.borrow_mut().define(
            call::CLOCK.to_string(),
            LoxValue::Callable("native", call::CLOCK.to_owned(), None),
        );
        callables.define(call::CLOCK, Rc::new(RefCell::new(Clock {})));
        Self {
            environment,
            globals,
            writer,
            callables,
            locals: HashMap::new(),
            class_methods: Vec::new(),
        }
    }

    pub fn evaluate(&mut self, expr: &Expr<'a>) -> crate::Result<LoxValue> {
        let loc = expr.location.clone();
        expr.accept(self).map_err(|e| {
            let labels = if let LoxError::Error(e) = e {
                if let Some(labels) = e.labels() {
                    let mut l: Vec<LabeledSpan> = labels.collect();
                    l.push(LabeledSpan::at(loc, e.to_string()));
                    l
                } else {
                    vec![LabeledSpan::at(loc, e.to_string())]
                }
            } else {
                vec![]
            };

            LoxError::Error(miette!(labels = labels, "Evaluation failed"))
        })
    }

    pub fn resolve(&mut self, obj: &Expr<'a>, depth: usize) {
        self.locals.insert(obj.get_hash_code(), depth);
    }

    pub fn interpret(&mut self, statements: &'a [crate::Result<Stmt<'a>>]) -> crate::Result<()> {
        let mut errors = vec![];

        let mut spans = HashSet::new();
        let mut add_error = |e: &crate::LoxError| {
            if let crate::LoxError::Error(e) = e {
                if let Some(label) = e.labels() {
                    for l in label {
                        if !spans.contains(&(l.len(), l.offset())) {
                            spans.insert((l.len(), l.offset()));
                            errors.push(l);
                        }
                    }
                }
            }
        };

        for stmt in statements {
            match stmt {
                Ok(s) => {
                    if let Err(e) = s.accept(self) {
                        if let LoxError::Return(_) = e {
                            // break interpretation
                            // error will be handled later
                            return Err(e);
                        }
                        add_error(&e);
                    }
                }
                Err(e) => add_error(e),
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(LoxError::Error(miette!(
                labels = errors,
                "Program completed with errors"
            )))
        }
    }

    fn interpret_one(&mut self, statement: &'a crate::Result<Stmt<'a>>) -> crate::Result<()> {
        match statement {
            Ok(s) => s.accept(self),
            Err(e) => {
                let mut errors = vec![];
                let mut spans = HashSet::new();
                if let crate::LoxError::Error(e) = e {
                    if let Some(label) = e.labels() {
                        for l in label {
                            if !spans.contains(&(l.len(), l.offset())) {
                                spans.insert((l.len(), l.offset()));
                                errors.push(l);
                            }
                        }
                    }
                }
                Err(LoxError::Error(miette!(
                    labels = errors,
                    "Invalid statement"
                )))
            }
        }
    }

    fn begin_scope(&mut self, enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let prev = self.environment.clone();
        let child = Environment::child(enclosing);
        self.environment = Rc::new(RefCell::new(child));
        prev
    }

    fn end_scope(&mut self, prev: Rc<RefCell<Environment>>) {
        self.environment = prev;
    }

    fn lookup_variable(&self, obj: &Expr<'a>, name: &'a str) -> crate::Result<LoxValue> {
        if let Some(distance) = self.locals.get(&obj.get_hash_code()) {
            let val = self.environment.borrow().get_at(*distance, name)?;
            if let LoxValue::Nil = val {
                Err(LoxError::Error(miette!(
                    "Using uninitialized variable '{name}'"
                )))
            } else {
                Ok(val)
            }
        } else {
            let val = self.globals.borrow().get(name)?;
            if let LoxValue::Nil = val {
                Err(LoxError::Error(miette!(
                    "Using uninitialized variable '{name}'"
                )))
            } else {
                Ok(val)
            }
        }
    }

    fn call_code(
        &mut self,
        arguments: Vec<LoxValue>,
        callee: std::cell::Ref<'_, dyn LoxCallable<'a>>,
    ) -> crate::Result<LoxValue> {
        match callee.call(arguments)? {
            CallResult::Value(lox_value) => Ok(lox_value),
            CallResult::Code(stmt, closure) => {
                let result = {
                    let prev = self.begin_scope(closure);
                    let result = self.interpret_one(stmt);
                    self.end_scope(prev);
                    result
                };
                match result {
                    Ok(()) => Ok(LoxValue::Nil),
                    Err(LoxError::Return(val)) => Ok(val),
                    Err(e) => Err(e),
                }
            }
        }
    }
}

fn map_operand_err<T>(
    err: crate::Result<T>,
    span: impl Into<SourceSpan>,
    label: &str,
) -> crate::Result<T> {
    err.map_err(|e| LoxError::Error(miette!(labels = vec![LabeledSpan::at(span, label)], "{e}")))
}

const RIGHT_OPERAND_ERR: &str = "right operand type not match left one";
const RIGHT_NUMBER_ERR: &str = "right operand must be number";
const LEFT_NUMBER_ERR: &str = "left operand must be number";

impl<'a, W: std::io::Write> ExprVisitor<'a, crate::Result<LoxValue>> for Interpreter<'a, W> {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> crate::Result<LoxValue> {
        match token {
            Some(t) => match t {
                Token::String(s) => Ok(LoxValue::String((*s).to_string())),
                Token::Number(n) => Ok(LoxValue::Number(*n)),
                Token::False => Ok(LoxValue::Bool(false)),
                Token::True => Ok(LoxValue::Bool(true)),
                Token::Nil => Ok(LoxValue::Nil),
                _ => Err(LoxError::Error(miette!("Invalid literal"))),
            },
            None => Ok(LoxValue::Nil),
        }
    }

    fn visit_binary_expr(
        &mut self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> crate::Result<LoxValue> {
        let left_loc = left.location.clone();
        let right_loc = right.location.clone();
        let lhs = self.evaluate(left)?;
        let rhs = self.evaluate(right)?;

        match operator {
            Token::Minus => {
                let l = map_operand_err(lhs.try_num(), left_loc, LEFT_NUMBER_ERR)?;
                let r = map_operand_err(rhs.try_num(), right_loc, RIGHT_NUMBER_ERR)?;
                let result = l - r;
                Ok(LoxValue::Number(result))
            }
            Token::Plus => {
                let lr = lhs.try_str();
                let rr = rhs.try_str();
                if lr.is_ok() || rr.is_ok() {
                    // concat strings here if any of operands is a string
                    if let Ok(l) = lr {
                        let result = l.to_owned() + &rhs.to_string();
                        return Ok(LoxValue::String(result));
                    } else if let Ok(r) = rr {
                        let result = lhs.to_string() + r;
                        return Ok(LoxValue::String(result));
                    }
                } else if let Ok(l) = lhs.try_num() {
                    let r = map_operand_err(rhs.try_num(), right_loc, RIGHT_NUMBER_ERR)?;
                    let result = l + r;
                    return Ok(LoxValue::Number(result));
                }
                let start = *left_loc.start();
                let end = *right_loc.end();
                Err(LoxError::Error(miette!(
                    labels = vec![LabeledSpan::at(start..=end, "Problem expression")],
                    "Invalid operands types for plus"
                )))
            }
            Token::Slash => {
                let err_loc = right_loc.clone();
                let l = map_operand_err(lhs.try_num(), left_loc, LEFT_NUMBER_ERR)?;
                let r = map_operand_err(rhs.try_num(), right_loc, RIGHT_NUMBER_ERR)?;

                if !r.is_normal() && !r.is_infinite() {
                    Err(LoxError::Error(miette!(
                        labels = vec![LabeledSpan::at(err_loc, "Zero division detected here")],
                        "Zero division detected"
                    )))
                } else {
                    let result = l / r;
                    Ok(LoxValue::Number(result))
                }
            }
            Token::Star => {
                let l = map_operand_err(lhs.try_num(), left_loc, LEFT_NUMBER_ERR)?;
                let r = map_operand_err(rhs.try_num(), right_loc, RIGHT_NUMBER_ERR)?;
                let result = l * r;
                Ok(LoxValue::Number(result))
            }
            Token::BangEqual => Ok(LoxValue::Bool(!lhs.equal(&rhs))),
            Token::EqualEqual => Ok(LoxValue::Bool(lhs.equal(&rhs))),
            Token::Greater => {
                let lt = map_operand_err(lhs.less(&rhs), right_loc, RIGHT_OPERAND_ERR)?;
                let gt = !lt && !lhs.equal(&rhs);
                Ok(LoxValue::Bool(gt))
            }
            Token::GreaterEqual => {
                let lt = map_operand_err(lhs.less(&rhs), right_loc, RIGHT_OPERAND_ERR)?;
                let ge = !lt || lhs.equal(&rhs);
                Ok(LoxValue::Bool(ge))
            }
            Token::Less => {
                let lt = map_operand_err(lhs.less(&rhs), right_loc, RIGHT_OPERAND_ERR)?;
                Ok(LoxValue::Bool(lt))
            }
            Token::LessEqual => {
                let lt = map_operand_err(lhs.less(&rhs), right_loc, RIGHT_OPERAND_ERR)?;
                let le = lt || lhs.equal(&rhs);
                Ok(LoxValue::Bool(le))
            }
            _ => Err(LoxError::Error(miette!("Invalid binary operator"))),
        }
    }

    fn visit_unary_expr(
        &mut self,
        operator: &Token<'a>,
        expr: &Expr<'a>,
    ) -> crate::Result<LoxValue> {
        let expr_loc = expr.location.clone();
        let val = self.evaluate(expr)?;
        match operator {
            Token::Minus => Ok(LoxValue::Number(-val.try_num()?)),
            Token::Bang => Ok(LoxValue::Bool(!val.is_truthy())),
            _ => Err(LoxError::Error(miette!(
                labels = vec![LabeledSpan::at(expr_loc, "Problem expression")],
                "Invalid unary operator"
            ))),
        }
    }

    fn visit_assign_expr(&mut self, lhs: &Expr<'a>, rhs: &Expr<'a>) -> crate::Result<LoxValue> {
        // Early return for invalid l-value expression
        let ExprKind::Variable(id) = &lhs.kind else {
            return Err(LoxError::Error(miette!(
                labels = vec![LabeledSpan::at(
                    lhs.location.clone(),
                    "Invalid l-value expression"
                )],
                "Assigment failed"
            )));
        };

        // Early return for invalid identifier
        let Token::Identifier(id) = id else {
            return Err(LoxError::Error(miette!(
                labels = vec![LabeledSpan::at(lhs.location.clone(), "Assigment failed")],
                "Assigment failed"
            )));
        };

        let location = rhs.location.clone();
        let val = rhs.accept(self)?;

        if let Some(distance) = self.locals.get(&lhs.get_hash_code()) {
            self.environment
                .borrow_mut()
                .assign_at(*distance, (*id).to_string(), val.clone())
                .map_err(|e| {
                    LoxError::Error(miette!(
                        labels = vec![LabeledSpan::at(location, e.to_string())],
                        "Assigment failed"
                    ))
                })?;
        } else {
            self.globals
                .borrow_mut()
                .assign((*id).to_string(), val.clone())
                .map_err(|e| {
                    LoxError::Error(miette!(
                        labels = vec![LabeledSpan::at(location, e.to_string())],
                        "Assigment failed"
                    ))
                })?;
        }

        Ok(val)
    }

    fn visit_call_expr(
        &mut self,
        paren: &Token<'a>,
        callee: &Expr<'a>,
        args: &[Box<Expr<'a>>],
    ) -> crate::Result<LoxValue> {
        let _ = paren;
        let location = callee.location.clone();
        let receiver = self.evaluate(callee)?;
        let mut arguments = vec![];
        for a in args {
            let a = self.evaluate(a)?;
            arguments.push(a);
        }
        let LoxValue::Callable(_, ref function, parent) = receiver else {
            return Err(LoxError::Error(miette!(
                labels = vec![LabeledSpan::at(location, "Invalid callable type")],
                "Invalid callable type"
            )));
        };
        if let Some(parent) = parent {
            let Ok(class) = self.callables.get(&parent) else {
                return Err(LoxError::Error(miette!(
                    labels = vec![LabeledSpan::at(
                        location,
                        format!("No class '{parent}' registered")
                    )],
                    "Undefined class"
                )));
            };

            let Some(method) = class.borrow().get(function) else {
                return Err(LoxError::Error(miette!(
                    labels = vec![LabeledSpan::at(
                        location,
                        format!("Class '{parent}' has no method {function}")
                    )],
                    "Class has no method"
                )));
            };
            let callee = method.clone();
            let callee = callee.borrow();
            self.call_code(arguments, callee)
        } else {
            let callee = self.callables.get(function)?;
            let callee = callee.borrow();
            if let Some(method) = callee.get("init") {
                // Call constructor if available
                let instance = self.call_code(vec![], callee);
                let ctor = method.borrow();
                self.call_code(arguments, ctor)?;
                instance
            } else {
                self.call_code(arguments, callee)
            }
        }
    }

    fn visit_get_expr(&mut self, name: &Token<'a>, object: &Expr<'a>) -> crate::Result<LoxValue> {
        let obj = self.evaluate(object)?;
        let Token::Identifier(identifier) = name else {
            return Err(LoxError::Error(miette!(
                "Field or method name must be an identifier"
            )));
        };
        let (class_name, closure) = match &obj {
            LoxValue::Instance(class_name, closure) => (class_name, closure),
            _ => {
                return Err(LoxError::Error(miette!(
                    "Only instances have properties or methods"
                )));
            }
        };
        if let Ok(class) = self.callables.get(class_name) {
            if class.borrow().get(identifier).is_some() {
                return Ok(LoxValue::Callable(
                    "fn",
                    (*identifier).to_string(),
                    Some(class_name.to_string()),
                ));
            }
        }
        let field = closure.borrow().get(identifier)?;
        Ok(field)
    }

    fn visit_set_expr(
        &mut self,
        field: &Token<'a>,
        obj: &Expr<'a>,
        val: &Expr<'a>,
    ) -> crate::Result<LoxValue> {
        // Early return for invalid field name
        let field = if let Token::Identifier(id) = field {
            (*id).to_string()
        } else {
            return Err(LoxError::Error(miette!("Field name must be an identifier")));
        };

        // Early return for invalid object type
        let ExprKind::Get(_, instance) = &obj.kind else {
            return Err(LoxError::Error(miette!("Only instances have fields")));
        };

        let instance = self.evaluate(instance)?;

        if let LoxValue::Instance(_class_name, closure) = &instance {
            let value = self.evaluate(val)?;
            closure.borrow_mut().define(field, value.clone());
            Ok(value)
        } else {
            Err(LoxError::Error(miette!("Only instances have properties")))
        }
    }

    fn visit_grouping_expr(&mut self, grouping: &Expr<'a>) -> crate::Result<LoxValue> {
        self.evaluate(grouping)
    }

    fn visit_logical_expr(
        &mut self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> crate::Result<LoxValue> {
        let lhs = self.evaluate(left)?;
        match operator {
            Token::And => {
                if lhs.is_truthy() {
                    self.evaluate(right)
                } else {
                    Ok(lhs)
                }
            }
            Token::Or => {
                if lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    self.evaluate(right)
                }
            }
            _ => Err(LoxError::Error(miette!("Invalid logical operator"))),
        }
    }

    fn visit_super_expr(
        &mut self,
        keyword: &Token<'a>,
        method: &Token<'a>,
    ) -> crate::Result<LoxValue> {
        let _ = method;
        let _ = keyword;
        todo!()
    }

    fn visit_this_expr(&mut self, obj: &Expr<'a>, _: &Token<'a>) -> crate::Result<LoxValue> {
        let _ = obj;
        self.lookup_variable(obj, "this")
    }

    fn visit_variable_expr(&mut self, obj: &Expr<'a>, name: &Token<'a>) -> crate::Result<LoxValue> {
        if let Token::Identifier(id) = name {
            self.lookup_variable(obj, id)
        } else {
            Err(LoxError::Error(miette!("Invalid identifier")))
        }
    }
}

impl<'a, W: std::io::Write> StmtVisitor<'a, crate::Result<()>> for Interpreter<'a, W> {
    fn visit_block_stmt(&mut self, body: &'a [crate::Result<Stmt<'a>>]) -> crate::Result<()> {
        let prev = self.begin_scope(self.environment.clone());
        let result = self.interpret(body);
        self.end_scope(prev);
        result
    }

    fn visit_class_stmt(
        &mut self,
        name: &Token<'a>,
        superclass: &Option<Box<Stmt<'a>>>,
        methods: &'a [crate::Result<Stmt<'a>>],
    ) -> crate::Result<()> {
        let _ = superclass;
        let Token::Identifier(id) = name else {
            return Err(LoxError::Error(miette!("Invalid class name")));
        };
        if self.environment.borrow().get(id).is_ok() {
            return Err(LoxError::Error(miette!("Class '{id}' redefinition")));
        }

        let enclosing = self.begin_scope(self.environment.clone());
        for method in methods {
            let method = match method {
                Ok(m) => m,
                Err(e) => return Err(LoxError::Error(miette!(e.to_string()))), // TODO: Handle all errors
            };
            method.accept(self)?;
        }

        let definition = LoxValue::Callable("class", (*id).to_string(), None);
        enclosing.borrow_mut().define((*id).to_string(), definition);

        let mut methods = vec![];
        methods.append(&mut self.class_methods);

        let class = Class::new(id, self.environment.clone(), methods);
        let callable = Rc::new(RefCell::new(class));
        self.callables.define(id, callable);
        self.end_scope(enclosing);
        Ok(())
    }

    fn visit_expression_stmt(&mut self, expr: &Expr<'a>) -> crate::Result<()> {
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_function_decl_stmt(
        &mut self,
        kind: FunctionKind,
        token: &Token<'a>,
        params: &[Box<Expr<'a>>],
        body: &'a crate::Result<Stmt<'a>>,
    ) -> crate::Result<()> {
        let Token::Identifier(id) = token else {
            return Err(LoxError::Error(miette!("Invalid {kind}")));
        };
        if self.environment.borrow().get(id).is_ok() {
            return Err(LoxError::Error(miette!(
                "{kind} or variable with '{id}' redefinition"
            )));
        }

        if let FunctionKind::Function = kind {
            self.environment.borrow_mut().define(
                (*id).to_string(),
                LoxValue::Callable("fn", (*id).to_string(), None),
            );
        }

        let mut parameters = vec![];
        let mut names = HashSet::new();
        for param in params {
            if let ExprKind::Variable(Token::Identifier(name)) = &param.kind {
                if names.contains(name) {
                    let location = param.location.clone();
                    return Err(LoxError::Error(miette!(
                        labels = vec![LabeledSpan::at(
                            location,
                            format!("Parameter '{name}' redefinition")
                        )],
                        "Parameter names must be unique"
                    )));
                }
                names.insert(*name);
                parameters.push(*name);
            }
        }
        let function = Function::new(id, parameters, body, self.environment.clone());
        match kind {
            FunctionKind::Function => {
                let callable = Rc::new(RefCell::new(function));
                self.callables.define(id, callable);
            }
            FunctionKind::Method => {
                self.class_methods.push(function);
            }
            FunctionKind::None => (),
        }

        Ok(())
    }

    fn visit_if_stmt(
        &mut self,
        cond: &Expr<'a>,
        then: &'a crate::Result<Stmt<'a>>,
        otherwise: &'a Option<Box<crate::Result<Stmt<'a>>>>,
    ) -> crate::Result<()> {
        match self.evaluate(cond)? {
            LoxValue::Bool(v) => {
                if v {
                    let then = match then {
                        Ok(s) => s,
                        Err(e) => return Err(LoxError::Error(miette!(e.to_string()))),
                    };
                    then.accept(self)
                } else if let Some(otherwise) = otherwise {
                    let otherwise = match &**otherwise {
                        Ok(s) => s,
                        Err(e) => return Err(LoxError::Error(miette!(e.to_string()))),
                    };
                    otherwise.accept(self)
                } else {
                    Ok(())
                }
            }
            LoxValue::String(_)
            | LoxValue::Number(_)
            | LoxValue::Callable(_, _, _)
            | LoxValue::Instance(_, _) => {
                let then = match then {
                    Ok(s) => s,
                    Err(e) => return Err(LoxError::Error(miette!(e.to_string()))),
                };
                then.accept(self)
            }
            LoxValue::Nil => {
                if let Some(otherwise) = otherwise {
                    let otherwise = match &(**otherwise) {
                        Ok(s) => s,
                        Err(e) => return Err(LoxError::Error(miette!(e.to_string()))),
                    };
                    otherwise.accept(self)
                } else {
                    Ok(())
                }
            }
        }
    }

    fn visit_print_stmt(&mut self, expr: &Expr<'a>) -> crate::Result<()> {
        match self.evaluate(expr) {
            Ok(val) => writeln!(self.writer, "{val}").map_err(|e| LoxError::Error(miette!(e))),
            Err(e) => Err(e),
        }
    }

    fn visit_return_stmt(&mut self, keyword: &Token<'a>, value: &Expr<'a>) -> crate::Result<()> {
        let _ = keyword;
        let val = self.evaluate(value)?;
        Err(LoxError::Return(val))
    }

    fn visit_variable_stmt(
        &mut self,
        name: &Token<'a>,
        initializer: &Option<Box<Expr<'a>>>,
    ) -> crate::Result<()> {
        let Token::Identifier(id) = name else {
            return Err(LoxError::Error(miette!("Invalid identifier")));
        };
        if let Some(v) = initializer {
            match v.accept(self) {
                Ok(val) => {
                    // TODO: Initializer may be class call (constructor) so we need to return instance

                    // var id = Class(); or var id = 1; or var id;
                    // so val may be Class() call or other value
                    self.environment.borrow_mut().define((*id).to_string(), val);
                }
                Err(e) => return Err(e),
            }
        } else {
            self.environment
                .borrow_mut()
                .define((*id).to_string(), LoxValue::Nil);
        }
        Ok(())
    }

    fn visit_while_stmt(
        &mut self,
        cond: &Expr<'a>,
        body: &'a crate::Result<Stmt<'a>>,
    ) -> crate::Result<()> {
        while self.evaluate(cond)?.is_truthy() {
            self.interpret_one(body)?;
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
    #[test_case("print clock() - clock();", "0" ; "simple clock call")]
    #[test_case("if (clock() > 0) print \"good\"; else print \"impossible\";", "good" ; "call in predicate")]
    #[test_case("fun x(v) { } print x(10);", "" ; "empty function body")]
    #[test_case("fun x(v) { print v; } print x(10);", "10" ; "simple call one arg")]
    #[test_case("fun sum(a1, a2) { print a1 + a2; } sum(1, 2);", "3" ; "simple call two args")]
    #[test_case("fun sum_and_decr(a1, a2) { var x = a1 + a2 - 1; print x; } sum_and_decr(1, 2);", "2" ; "function with two statements")]
    #[test_case("fun foo(x) { return x + 1; } print foo(1);", "2" ; "function with return")]
    #[test_case("fun foo() { return bar; } fun bar(x, y) { return x + y; } print foo()(1, 2);", "3" ; "cascade call")]
    #[test_case("fun foo() { var i = 1; fun bar(x) { return i + x; } return bar; } print foo()(2);", "3" ; "closure")]
    #[test_case("fun fib(n) { if (n < 2) return n; return fib(n - 1) + fib(n - 2); } print fib(8);", "21" ; "fibonacci")]
    #[test_case("fun foo(n) { if (n < 2) return n; return 10; } print foo(1);", "1" ; "conditional return success")]
    #[test_case("fun foo(n) { if (n < 2) return n; return 10; } print foo(5);", "10" ; "conditional return fail")]
    #[test_case("class Foo { method(x) { print x;} }", "" ; "class")]
    #[test_case("class Bagel{} var b = Bagel(); print b;", "insstance of Bagel" ; "class instance empty")]
    #[test_case("class Bagel{} var b = Bagel(); { var b = Bagel(); b.field = 1; print b.field; } b.field = 2; print b.field;", "1\n2" ; "get/set class field complex")]
    #[test_case("class Bagel{} var b = Bagel(); { b.field = 1; } print b.field; b.field = 2; print b.field;", "1\n2" ; "get/set class field complex no shadowing")]
    #[test_case("class Bagel{} var b; b = Bagel(); { b.field = 1; } print b.field; b.field = 2; print b.field;", "1\n2" ; "get/set class field complex no shadowing assignment")]
    #[test_case("class Bagel{} var b = Bagel(); b.field = 1; print b.field;", "1" ; "get/set class field")]
    #[test_case("class Bagel{} var b; b = Bagel(); b.field = 1; print b.field;", "1" ; "class instance assign and get/set class field")]
    #[test_case("class Bagel{} var b; { b = Bagel(); b.field = 1; } b.field = 2; print b.field;", "2" ; "class instance init inside child scope and get/set class field")]
    #[test_case("class Bagel { method() { print 10;} } var b = Bagel(); b.method();", "10" ; "call class method")]
    #[test_case("class Foo { method() { } } var foo = Foo(); foo.a = 1; var i = 0; while (i < 2) { print foo.a; i = i + 1; }", "1\n1" ; "class with empty body method and use it from while")]
    #[test_case("class Foo { method() { } } var foo = Foo(); foo.a = 1; print foo.a;", "1" ; "class with empty body method and use it")]
    #[test_case("class Foo {} var foo = Foo(); fun bar() { print 10; } foo.meth = bar; print foo.meth();", "10" ; "call property with stored function")]
    #[test_case("class Bagel { method() { print 10;} } Bagel().method();", "10" ; "call class method without instance in var")]
    #[test_case("class Bagel { method() { print 10;} } var b = Bagel().method; b();", "10" ; "call class method from assigned var")]
    #[test_case("class Class { method() { print this.some; } } var c = Class(); c.some = 10; c.method();", "10" ; "this usage")]
    #[test_case("class Class { init() { this.some = 10; } method() { print this.some; } } var c = Class(); c.method();", "10" ; "class constructor")]
    #[test_case("class Class { init(x) { this.some = x; } method() { print this.some; } } var c = Class(10); c.method();", "10" ; "class constructor with arg")]
    #[test_case("class Class { init(x) { this.some = x; } method() { print this.some; } } var c = Class(10); c.init(10); c.method();", "10" ; "class constructor with arg and invoking ctor directly")]
    fn eval_single_result_tests(input: &str, expected: &str) {
        // Arrange
        let mut parser = Parser::new(input);
        let mut stdout = Vec::new();
        let interpreter = Interpreter::new(&mut stdout);
        let resolver = Resolver::new(interpreter);
        let stmts: Vec<crate::Result<crate::ast::Stmt>> = parser.collect();

        // Act
        let iterpretation_result = resolver.interpret(&stmts);

        // Assert
        if let Err(e) = iterpretation_result {
            panic!("iterpretation_result should be Ok. But it was: {e:#?}. \nText: {input}");
        }

        let actual = String::from_utf8(stdout).unwrap();
        assert_eq!(actual.trim_end(), expected);
    }
}
