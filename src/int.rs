#![allow(clippy::missing_errors_doc)]

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use miette::{Diagnostic, LabeledSpan, SourceSpan, miette};
use thiserror::Error;

use crate::{
    ast::{Expr, ExprKind, ExprVisitor, FunctionKind, LoxValue, Stmt, StmtVisitor},
    call::{self, CallResult, Catalogue, Class, Clock, Function, LoxCallable},
    env::Environment,
    lexer::Token,
};

#[derive(Debug, Error, Diagnostic)]
#[error("Program flow error")]
#[diagnostic()]
pub enum ProgramError {
    Return(LoxValue),
}

pub struct Interpreter<'a, W: std::io::Write> {
    /// Current environment that keeps current scope vars. Global by default
    environment: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    callables: Box<Catalogue<'a>>,
    writer: W,
    locals: HashMap<u64, usize>,
    all_class_methods: HashMap<String, HashMap<String, Rc<RefCell<dyn LoxCallable<'a> + 'a>>>>,
    class_methods: Vec<Rc<RefCell<dyn LoxCallable<'a> + 'a>>>,
}

impl<'a, W: std::io::Write> Interpreter<'a, W> {
    #[must_use]
    pub fn new(writer: W) -> Self {
        let environment = Rc::new(RefCell::new(Environment::new()));
        let globals = environment.clone();
        let mut callables = Box::new(Catalogue::new());
        globals.borrow_mut().define(
            call::CLOCK.to_string(),
            LoxValue::Callable("native", call::CLOCK.to_owned()),
        );
        callables.define(call::CLOCK, Rc::new(RefCell::new(Clock {})));
        Self {
            environment,
            globals,
            writer,
            callables,
            locals: HashMap::new(),
            all_class_methods: HashMap::new(),
            class_methods: Vec::new(),
        }
    }

    pub fn evaluate(&mut self, expr: &Expr<'a>) -> miette::Result<LoxValue> {
        let loc = expr.location.clone();
        expr.accept(self).map_err(|e| {
            let mut labels = if let Some(labels) = e.labels() {
                labels.collect()
            } else {
                vec![]
            };
            labels.push(LabeledSpan::at(loc, e.to_string()));
            miette!(labels = labels, "Evaluation failed")
        })
    }

    pub fn resolve(&mut self, obj: &Expr<'a>, depth: usize) {
        self.locals.insert(obj.get_hash_code(), depth);
    }

    pub fn interpret(&mut self, statements: &'a [miette::Result<Stmt<'a>>]) -> miette::Result<()> {
        let mut errors = vec![];

        let mut spans = HashSet::new();
        let mut add_error = |e: &miette::Report| {
            if let Some(label) = e.labels() {
                for l in label {
                    if !spans.contains(&(l.len(), l.offset())) {
                        spans.insert((l.len(), l.offset()));
                        errors.push(l);
                    }
                }
            }
        };

        for stmt in statements {
            match stmt {
                Ok(s) => {
                    if let Err(e) = s.accept(self) {
                        if let Some(ProgramError::Return(_)) = e.downcast_ref::<ProgramError>() {
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
            Err(miette!(labels = errors, "Program completed with errors"))
        }
    }

    fn interpret_one(&mut self, statement: &'a miette::Result<Stmt<'a>>) -> miette::Result<()> {
        match statement {
            Ok(s) => s.accept(self),
            Err(e) => {
                let mut errors = vec![];
                let mut spans = HashSet::new();
                if let Some(label) = e.labels() {
                    for l in label {
                        if !spans.contains(&(l.len(), l.offset())) {
                            spans.insert((l.len(), l.offset()));
                            errors.push(l);
                        }
                    }
                }
                Err(miette!(labels = errors, "Program completed with errors"))
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

    fn lookup_variable(&self, obj: &Expr<'a>, name: &'a str) -> miette::Result<LoxValue> {
        if let Some(distance) = self.locals.get(&obj.get_hash_code()) {
            let val = self.environment.borrow().get_at(*distance, name)?;
            if let LoxValue::Nil = val {
                Err(miette!("Using uninitialized variable '{name}'"))
            } else {
                Ok(val)
            }
        } else {
            let val = self.globals.borrow().get(name)?;
            if let LoxValue::Nil = val {
                Err(miette!("Using uninitialized variable '{name}'"))
            } else {
                Ok(val)
            }
        }
    }

    fn call_code(
        &mut self,
        arguments: Vec<LoxValue>,
        callee: std::cell::Ref<'_, dyn LoxCallable<'a>>,
    ) -> Result<LoxValue, miette::Error> {
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
                    Err(e) => {
                        if let Some(ProgramError::Return(val)) = e.downcast_ref::<ProgramError>() {
                            // Return handling case
                            Ok(val.clone())
                        } else {
                            Err(e)
                        }
                    }
                }
            }
        }
    }
}

fn map_operand_err<T>(
    err: miette::Result<T>,
    span: impl Into<SourceSpan>,
    label: &str,
) -> miette::Result<T> {
    err.map_err(|e| {
        miette!(
            labels = vec![LabeledSpan::at(span, label)],
            "Invalid operand"
        )
        .wrap_err(e)
    })
}

const RIGHT_OPERAND_ERR: &str = "right operand type not match left one";
const RIGHT_NUMBER_ERR: &str = "right operand must be number";
const LEFT_NUMBER_ERR: &str = "left operand must be number";

impl<'a, W: std::io::Write> ExprVisitor<'a, miette::Result<LoxValue>> for Interpreter<'a, W> {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> miette::Result<LoxValue> {
        match token {
            Some(t) => match t {
                Token::String(s) => Ok(LoxValue::String((*s).to_string())),
                Token::Number(n) => Ok(LoxValue::Number(*n)),
                Token::False => Ok(LoxValue::Bool(false)),
                Token::True => Ok(LoxValue::Bool(true)),
                Token::Nil => Ok(LoxValue::Nil),
                _ => Err(miette!("Invalid literal")),
            },
            None => Ok(LoxValue::Nil),
        }
    }

    fn visit_binary_expr(
        &mut self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> miette::Result<LoxValue> {
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
                Err(miette!(
                    labels = vec![LabeledSpan::at(start..=end, "Problem expression")],
                    "Invalid operands types for plus"
                ))
            }
            Token::Slash => {
                let err_loc = right_loc.clone();
                let l = map_operand_err(lhs.try_num(), left_loc, LEFT_NUMBER_ERR)?;
                let r = map_operand_err(rhs.try_num(), right_loc, RIGHT_NUMBER_ERR)?;

                if !r.is_normal() && !r.is_infinite() {
                    Err(miette!(
                        labels = vec![LabeledSpan::at(err_loc, "Zero division detected here")],
                        "Zero division detected"
                    ))
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
            _ => Err(miette!("Invalid binary operator")),
        }
    }

    fn visit_unary_expr(
        &mut self,
        operator: &Token<'a>,
        expr: &Expr<'a>,
    ) -> miette::Result<LoxValue> {
        let expr_loc = expr.location.clone();
        let val = self.evaluate(expr)?;
        match operator {
            Token::Minus => Ok(LoxValue::Number(-val.try_num()?)),
            Token::Bang => Ok(LoxValue::Bool(!val.is_truthy())),
            _ => Err(miette!(
                labels = vec![LabeledSpan::at(expr_loc, "Problem expression")],
                "Invalid unary operator"
            )),
        }
    }

    fn visit_assign_expr(&mut self, lhs: &Expr<'a>, rhs: &Expr<'a>) -> miette::Result<LoxValue> {
        // Early return for invalid l-value expression
        let ExprKind::Variable(id) = &lhs.kind else {
            return Err(miette!(
                labels = vec![LabeledSpan::at(
                    lhs.location.clone(),
                    "Invalid l-value expression"
                )],
                "Assigment failed"
            ));
        };

        // Early return for invalid identifier
        let Token::Identifier(id) = id else {
            return Err(miette!(
                labels = vec![LabeledSpan::at(
                    lhs.location.clone(),
                    "Invalid l-value expression"
                )],
                "Assigment failed"
            ));
        };

        let location = rhs.location.clone();
        let val = rhs.accept(self)?;

        // Convert class to instance if needed
        let val = if let LoxValue::Class(class) = val {
            LoxValue::Instance(class.to_string(), (*id).to_string())
        } else {
            val
        };

        if let Some(distance) = self.locals.get(&lhs.get_hash_code()) {
            self.environment
                .borrow_mut()
                .assign_at(*distance, (*id).to_string(), val.clone())
                .map_err(|e| {
                    miette!(
                        labels = vec![LabeledSpan::at(location, e.to_string())],
                        "Assigment failed"
                    )
                })?;
        } else {
            self.globals
                .borrow_mut()
                .assign((*id).to_string(), val.clone())
                .map_err(|e| {
                    miette!(
                        labels = vec![LabeledSpan::at(location, e.to_string())],
                        "Assigment failed"
                    )
                })?;
        }

        Ok(val)
    }

    fn visit_call_expr(
        &mut self,
        paren: &Token<'a>,
        callee: &Expr<'a>,
        args: &[Box<Expr<'a>>],
    ) -> miette::Result<LoxValue> {
        let _ = paren;
        let location = callee.location.clone();
        let callee = self.evaluate(callee)?;
        let mut arguments = vec![];
        for a in args {
            let a = self.evaluate(a)?;
            arguments.push(a);
        }
        let function;
        let mut class: Option<&String> = None;
        match callee {
            LoxValue::Callable(_, ref id) => function = id,
            LoxValue::Instance(ref class_name, ref method) => {
                class = Some(class_name);
                function = method;
            }
            _ => {
                return Err(miette!(
                    labels = vec![LabeledSpan::at(location, "Invalid callable type")],
                    "Invalid callable type"
                ));
            }
        }
        if let Some(class) = class {
            let Some(methods) = self.all_class_methods.get(class) else {
                return Err(miette!("Undefined class: '{class}'"));
            };
            let Some(method) = methods.get(function) else {
                return Err(miette!("Undefined method: '{function}' in class '{class}'"));
            };
            let callee = method.clone();
            let callee = callee.borrow();
            self.call_code(arguments, callee)
        } else {
            let callee = self.callables.get(function)?;
            let callee = callee.borrow();
            self.call_code(arguments, callee)
        }
    }

    fn visit_get_expr(&mut self, name: &Token<'a>, object: &Expr<'a>) -> miette::Result<LoxValue> {
        let result = self.evaluate(object)?;
        match &result {
            LoxValue::Instance(class_name, id) => {
                let Token::Identifier(field_or_method) = name else {
                    return Err(miette!("Field name must be an identifier"));
                };

                if let Some(methods) = self.all_class_methods.get(class_name) {
                    if methods.contains_key(*field_or_method) {
                        if let Ok(class) = self.callables.get(class_name) {
                            let class = class.borrow();
                            if let Ok(CallResult::Value(lox_value)) =
                                class.call(vec![LoxValue::String((*field_or_method).to_string())])
                            {
                                return Ok(lox_value);
                            }
                        }
                    }
                }
                let field = if let Some(distance) = self.locals.get(&object.get_hash_code()) {
                    self.environment
                        .borrow()
                        .get_field_at(*distance, id, field_or_method)?
                } else {
                    self.globals.borrow().get_field(id, field_or_method)?
                };
                Ok(field)
            }
            LoxValue::Class(class) => {
                let callee = self.callables.get(class)?;
                let callee = callee.borrow();
                match callee.call(vec![])? {
                    CallResult::Value(lox_value) => {
                        // TODO: Implement class instance creation without var
                        Ok(lox_value)
                    }
                    CallResult::Code(stmt, closure) => {
                        let result = {
                            let prev = self.begin_scope(closure);
                            let result = self.interpret_one(stmt);
                            self.end_scope(prev);
                            result
                        };
                        match result {
                            Ok(()) => Ok(LoxValue::Nil),
                            Err(e) => {
                                if let Some(ProgramError::Return(val)) =
                                    e.downcast_ref::<ProgramError>()
                                {
                                    // Return handling case
                                    Ok(val.clone())
                                } else {
                                    Err(e)
                                }
                            }
                        }
                    }
                }
            }
            _ => Err(miette!("Only instances have properties")),
        }
    }

    fn visit_grouping_expr(&mut self, grouping: &Expr<'a>) -> miette::Result<LoxValue> {
        self.evaluate(grouping)
    }

    fn visit_logical_expr(
        &mut self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> miette::Result<LoxValue> {
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
            _ => Err(miette!("Invalid logical operator")),
        }
    }

    fn visit_set_expr(
        &mut self,
        field: &Token<'a>,
        obj: &Expr<'a>,
        val: &Expr<'a>,
    ) -> miette::Result<LoxValue> {
        // Early return for invalid field name
        let field = match field {
            Token::Identifier(id) => (*id).to_string(),
            _ => return Err(miette!("Field name must be an identifier")),
        };

        // Early return for invalid object type
        let ExprKind::Get(_, instance) = &obj.kind else {
            return Err(miette!("Only instances have fields"));
        };

        // Early return for invalid instance
        let ExprKind::Variable(id) = &instance.kind else {
            return Err(miette!("Invalid instance"));
        };

        // Early return for invalid instance name
        let id = match id {
            Token::Identifier(id) => (*id).to_string(),
            _ => return Err(miette!("Instance name must be an identifier")),
        };

        let v = self.evaluate(val)?;

        if let Some(distance) = self.locals.get(&instance.get_hash_code()) {
            self.environment
                .borrow_mut()
                .set_field_at(*distance, id, field, v.clone());
        } else {
            self.globals.borrow_mut().set_field(id, field, v.clone());
        }

        Ok(v)
    }

    fn visit_super_expr(
        &mut self,
        keyword: &Token<'a>,
        method: &Token<'a>,
    ) -> miette::Result<LoxValue> {
        let _ = method;
        let _ = keyword;
        todo!()
    }

    fn visit_this_expr(&mut self, keyword: &Token<'a>) -> miette::Result<LoxValue> {
        let _ = keyword;
        todo!()
    }

    fn visit_variable_expr(
        &mut self,
        obj: &Expr<'a>,
        name: &Token<'a>,
    ) -> miette::Result<LoxValue> {
        if let Token::Identifier(id) = name {
            self.lookup_variable(obj, id)
        } else {
            Err(miette!("Invalid identifier"))
        }
    }
}

impl<'a, W: std::io::Write> StmtVisitor<'a, miette::Result<()>> for Interpreter<'a, W> {
    fn visit_block_stmt(&mut self, body: &'a [miette::Result<Stmt<'a>>]) -> miette::Result<()> {
        let prev = self.begin_scope(self.environment.clone());
        let result = self.interpret(body);
        self.end_scope(prev);
        result
    }

    fn visit_class_stmt(
        &mut self,
        name: &Token<'a>,
        superclass: &Option<Box<Stmt<'a>>>,
        methods: &'a [miette::Result<Stmt<'a>>],
    ) -> miette::Result<()> {
        let _ = superclass;
        let Token::Identifier(id) = name else {
            return Err(miette!("Invalid class name"));
        };
        if self.environment.borrow().get(id).is_ok() {
            return Err(miette!("Class '{id}' redefinition"));
        }

        for method in methods {
            let method = match method {
                Ok(m) => m,
                Err(e) => return Err(miette!(e.to_string())), // TODO: Handle all errors
            };
            method.accept(self)?;
        }
        let definition = LoxValue::Callable("class", (*id).to_string());
        self.environment
            .borrow_mut()
            .define((*id).to_string(), definition);

        let mut methods = vec![];
        methods.append(&mut self.class_methods);

        let methods: HashMap<String, Rc<RefCell<dyn LoxCallable<'a> + 'a>>> = methods
            .into_iter()
            .map(|f| (f.borrow().name().to_string(), f.clone()))
            .collect();
        self.all_class_methods.insert((*id).to_string(), methods);

        let class = Class::new((*id).to_string());
        let callable = Rc::new(RefCell::new(class));
        self.callables.define(id, callable);
        Ok(())
    }

    fn visit_expression_stmt(&mut self, expr: &Expr<'a>) -> miette::Result<()> {
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_function_decl_stmt(
        &mut self,
        kind: FunctionKind,
        token: &Token<'a>,
        params: &[Box<Expr<'a>>],
        body: &'a miette::Result<Stmt<'a>>,
    ) -> miette::Result<()> {
        let Token::Identifier(id) = token else {
            return Err(miette!("Invalid {kind}"));
        };
        if self.environment.borrow().get(id).is_ok() {
            return Err(miette!("{kind} or variable with '{id}' redefinition"));
        }
        self.environment.borrow_mut().define(
            (*id).to_string(),
            LoxValue::Callable("fn", (*id).to_string()),
        );

        let mut parameters = vec![];
        let mut names = HashSet::new();
        for param in params {
            if let ExprKind::Variable(Token::Identifier(name)) = &param.kind {
                if names.contains(name) {
                    let location = param.location.clone();
                    return Err(miette!(
                        labels = vec![LabeledSpan::at(
                            location,
                            format!("Parameter '{name}' redefinition")
                        )],
                        "Parameter names must be unique"
                    ));
                }
                names.insert(*name);
                parameters.push(*name);
            }
        }
        let callable = Function::new(id, parameters, body, self.environment.clone());
        let callable = Rc::new(RefCell::new(callable));
        match kind {
            FunctionKind::Function => {
                self.callables.define(id, callable);
            }
            FunctionKind::Method => {
                self.class_methods.push(callable);
            }
            FunctionKind::None => (),
        }

        Ok(())
    }

    fn visit_if_stmt(
        &mut self,
        cond: &Expr<'a>,
        then: &'a miette::Result<Stmt<'a>>,
        otherwise: &'a Option<Box<miette::Result<Stmt<'a>>>>,
    ) -> miette::Result<()> {
        match self.evaluate(cond)? {
            LoxValue::Bool(v) => {
                if v {
                    let then = match then {
                        Ok(s) => s,
                        Err(e) => return Err(miette!(e.to_string())),
                    };
                    then.accept(self)
                } else if let Some(otherwise) = otherwise {
                    let otherwise = match &**otherwise {
                        Ok(s) => s,
                        Err(e) => return Err(miette!(e.to_string())),
                    };
                    otherwise.accept(self)
                } else {
                    Ok(())
                }
            }
            LoxValue::String(_)
            | LoxValue::Number(_)
            | LoxValue::Callable(_, _)
            | LoxValue::Instance(_, _)
            | LoxValue::Class(_) => {
                let then = match then {
                    Ok(s) => s,
                    Err(e) => return Err(miette!(e.to_string())),
                };
                then.accept(self)
            }
            LoxValue::Nil => {
                if let Some(otherwise) = otherwise {
                    let otherwise = match &**otherwise {
                        Ok(s) => s,
                        Err(e) => return Err(miette!(e.to_string())),
                    };
                    otherwise.accept(self)
                } else {
                    Ok(())
                }
            }
        }
    }

    fn visit_print_stmt(&mut self, expr: &Expr<'a>) -> miette::Result<()> {
        match self.evaluate(expr) {
            Ok(val) => {
                writeln!(self.writer, "{val}").map_err(|e| miette!(e))?;
            }
            Err(e) => {
                return Err(e);
            }
        }
        Ok(())
    }

    fn visit_return_stmt(&mut self, keyword: &Token<'a>, value: &Expr<'a>) -> miette::Result<()> {
        let _ = keyword;
        let val = self.evaluate(value)?;
        Err(ProgramError::Return(val).into())
    }

    fn visit_variable_stmt(
        &mut self,
        name: &Token<'a>,
        initializer: &Option<Box<Expr<'a>>>,
    ) -> miette::Result<()> {
        let Token::Identifier(id) = name else {
            return Err(miette!("Invalid identifier"));
        };
        if let Some(v) = initializer {
            match v.accept(self) {
                Ok(val) => {
                    // Convert class to instance if needed
                    let val = if let LoxValue::Class(class) = val {
                        LoxValue::Instance(class.to_string(), (*id).to_string())
                    } else {
                        val
                    };
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
        body: &'a miette::Result<Stmt<'a>>,
    ) -> miette::Result<()> {
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
    #[test_case("class Bagel{} var b = Bagel(); print b;", "b: Bagel" ; "class instance empty")]
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
    fn eval_single_result_tests(input: &str, expected: &str) {
        // Arrange
        let mut parser = Parser::new(input);
        let mut stdout = Vec::new();
        let interpreter = Interpreter::new(&mut stdout);
        let resolver = Resolver::new(interpreter);
        let stmts: Vec<miette::Result<crate::ast::Stmt>> = parser.collect();

        // Act
        let iterpretation_result = resolver.interpret(&stmts);

        // Assert
        if let Err(e) = iterpretation_result {
            panic!("iterpretation_result should be Ok. But it was: {e:#?}");
        }

        let actual = String::from_utf8(stdout).unwrap();
        assert_eq!(actual.trim_end(), expected);
    }
}
