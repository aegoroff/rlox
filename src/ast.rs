#![allow(clippy::missing_errors_doc)]

use std::{
    cell::RefCell, collections::HashSet, fmt::Display, iter::once, ops::RangeInclusive, rc::Rc,
};

use miette::{Diagnostic, LabeledSpan, SourceSpan, miette};
use thiserror::Error;

use crate::{
    call::{self, Catalogue, Clock, Function},
    env::Environment,
    lexer::Token,
};

// Traits

pub trait ExprVisitor<'a, R> {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> R;
    fn visit_binary_expr(&mut self, operator: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) -> R;
    fn visit_unary_expr(&mut self, operator: &Token<'a>, expr: &Expr<'a>) -> R;
    fn visit_assign_expr(&mut self, name: &Token<'a>, value: &Expr<'a>) -> R;
    fn visit_call_expr(
        &mut self,
        paren: &Token<'a>,
        callee: &Expr<'a>,
        args: &[Box<Expr<'a>>],
    ) -> R;
    fn visit_get_expr(&mut self, name: &Token<'a>, object: &Expr<'a>) -> R;
    fn visit_grouping_expr(&mut self, grouping: &Expr<'a>) -> R;
    fn visit_logical_expr(&mut self, operator: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) -> R;
    fn visit_set_expr(&mut self, name: &Token<'a>, obj: &Expr<'a>, val: &Expr<'a>) -> R;
    fn visit_super_expr(&mut self, keyword: &Token<'a>, method: &Token<'a>) -> R;
    fn visit_this_expr(&mut self, keyword: &Token<'a>) -> R;
    fn visit_variable_expr(&mut self, name: &Token<'a>) -> R;
}

pub trait StmtVisitor<'a, R> {
    fn visit_block_stmt(&mut self, body: &'a [miette::Result<Stmt<'a>>]) -> R;
    fn visit_class_stmt(
        &self,
        name: &Token<'a>,
        superclass: &Stmt<'a>,
        methods: &[Box<Stmt<'a>>],
    ) -> R;
    fn visit_expression_stmt(&mut self, expr: &Expr<'a>) -> R;
    fn visit_function_decl_stmt(
        &mut self,
        token: &Token<'a>,
        params: &[Box<Expr<'a>>],
        body: &'a miette::Result<Stmt<'a>>,
    ) -> R;
    fn visit_if_stmt(
        &mut self,
        cond: &Expr<'a>,
        then: &'a miette::Result<Stmt<'a>>,
        otherwise: &'a Option<Box<miette::Result<Stmt<'a>>>>,
    ) -> R;
    fn visit_print_stmt(&mut self, expr: &Expr<'a>) -> R;
    fn visit_return_stmt(&mut self, keyword: &Token<'a>, value: &Expr<'a>) -> R;
    fn visit_variable_stmt(&mut self, name: &Token<'a>, initializer: &Option<Box<Expr<'a>>>) -> R;
    fn visit_while_stmt(&mut self, cond: &Expr<'a>, body: &'a miette::Result<Stmt<'a>>) -> R;
}

pub trait LoxCallable<'a> {
    fn arity(&self) -> usize;
    fn call(&self, arguments: Vec<LoxValue>) -> CallResult<'a>;
}

pub enum CallResult<'a> {
    Value(LoxValue),
    Code(&'a miette::Result<Stmt<'a>>, Rc<RefCell<Environment>>),
}

#[derive(Debug, Error, Diagnostic)]
#[error("Program flow error")]
#[diagnostic()]
pub enum ProgramError {
    Return(LoxValue),
}

// Expressions

#[derive(Debug)]
pub enum ExprKind<'a> {
    Literal(Option<Token<'a>>),
    Binary(Token<'a>, Box<Expr<'a>>, Box<Expr<'a>>),
    Unary(Token<'a>, Box<Expr<'a>>),
    Assign(Token<'a>, Box<Expr<'a>>),
    /// paren, callee, args
    Call(Token<'a>, Box<Expr<'a>>, Vec<Box<Expr<'a>>>),
    Get(Token<'a>, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Logical(Token<'a>, Box<Expr<'a>>, Box<Expr<'a>>),
    /// name, object, value
    Set(Token<'a>, Box<Expr<'a>>, Box<Expr<'a>>),
    /// keyword, method
    Super(Token<'a>, Token<'a>),
    This(Token<'a>),
    Variable(Token<'a>),
}

#[derive(Debug)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub location: RangeInclusive<usize>,
}

impl<'a> Expr<'a> {
    pub fn accept<R>(&self, visitor: &mut impl ExprVisitor<'a, R>) -> R {
        match &self.kind {
            ExprKind::Literal(token) => visitor.visit_literal(token),
            ExprKind::Binary(operator, left, right) => {
                visitor.visit_binary_expr(operator, left, right)
            }
            ExprKind::Unary(operator, expr) => visitor.visit_unary_expr(operator, expr),
            ExprKind::Assign(name, value) => visitor.visit_assign_expr(name, value),
            ExprKind::Call(paren, callee, args) => visitor.visit_call_expr(paren, callee, args),
            ExprKind::Get(name, object) => visitor.visit_get_expr(name, object),
            ExprKind::Grouping(grouping) => visitor.visit_grouping_expr(grouping),
            ExprKind::Logical(token, left, right) => visitor.visit_logical_expr(token, left, right),
            ExprKind::Set(name, obj, val) => visitor.visit_set_expr(name, obj, val),
            ExprKind::Super(keyword, method) => visitor.visit_super_expr(keyword, method),
            ExprKind::This(keyword) => visitor.visit_this_expr(keyword),
            ExprKind::Variable(name) => visitor.visit_variable_expr(name),
        }
    }
}

// Statements

#[derive(Debug)]
pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub location: RangeInclusive<usize>,
}

impl<'a> Stmt<'a> {
    pub fn accept<R>(&'a self, visitor: &mut impl StmtVisitor<'a, R>) -> R {
        match &self.kind {
            StmtKind::Block(stmts) => visitor.visit_block_stmt(stmts),
            StmtKind::Class(token, stmt, stmts) => visitor.visit_class_stmt(token, stmt, stmts),
            StmtKind::Expression(expr) => visitor.visit_expression_stmt(expr),
            StmtKind::Function(token, params, body) => {
                visitor.visit_function_decl_stmt(token, params, body)
            }
            StmtKind::If(cond, then, otherwise) => visitor.visit_if_stmt(cond, then, otherwise),
            StmtKind::Print(expr) => visitor.visit_print_stmt(expr),
            StmtKind::Return(keyword, value) => visitor.visit_return_stmt(keyword, value),
            StmtKind::Variable(name, initializer) => visitor.visit_variable_stmt(name, initializer),
            StmtKind::While(cond, body) => visitor.visit_while_stmt(cond, body),
        }
    }
}

#[derive(Debug)]
pub enum StmtKind<'a> {
    Block(Vec<miette::Result<Stmt<'a>>>),
    /// name, superclass, methods
    Class(Token<'a>, Box<Stmt<'a>>, Vec<Box<Stmt<'a>>>),
    Expression(Box<Expr<'a>>),
    /// token, params, body
    Function(Token<'a>, Vec<Box<Expr<'a>>>, Box<miette::Result<Stmt<'a>>>),
    /// condition, then, else
    If(
        Box<Expr<'a>>,
        Box<miette::Result<Stmt<'a>>>,
        Option<Box<miette::Result<Stmt<'a>>>>,
    ),
    Print(Box<Expr<'a>>),
    Return(Token<'a>, Box<Expr<'a>>),
    Variable(Token<'a>, Option<Box<Expr<'a>>>),
    While(Box<Expr<'a>>, Box<miette::Result<Stmt<'a>>>),
}

// Values

#[derive(Clone, Debug)]
pub enum LoxValue {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
    Callable(&'static str, String),
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::String(s) => write!(f, "{s}"),
            LoxValue::Callable(kind, val) => write!(f, "<{kind} {val}>"),
            LoxValue::Number(n) => write!(f, "{n}"),
            LoxValue::Bool(b) => write!(f, "{b}"),
            LoxValue::Nil => write!(f, ""),
        }
    }
}

impl LoxValue {
    pub fn try_num(&self) -> miette::Result<f64> {
        if let LoxValue::Number(n) = self {
            Ok(*n)
        } else {
            Err(miette!("Number expected"))
        }
    }

    pub fn try_str(&self) -> miette::Result<&String> {
        if let LoxValue::String(s) = self {
            Ok(s)
        } else {
            Err(miette!("String expected"))
        }
    }

    pub fn try_bool(&self) -> miette::Result<bool> {
        match self {
            LoxValue::Bool(b) => Ok(*b),
            LoxValue::Nil => Ok(false),
            _ => Err(miette!("Boolean expected or Nil")),
        }
    }

    #[must_use]
    pub fn is_truthy(&self) -> bool {
        self.try_bool().unwrap_or(true)
    }

    #[must_use]
    pub fn equal(&self, other: &LoxValue) -> bool {
        if let Ok(l) = self.try_num() {
            let Ok(r) = other.try_num() else {
                return false;
            };
            (l - r).abs() < ERROR_MARGIN
        } else if let Ok(l) = self.try_bool() {
            let Ok(r) = other.try_bool() else {
                return false;
            };
            l == r
        } else if let Ok(l) = self.try_str() {
            let Ok(r) = other.try_str() else {
                return false;
            };
            l == r
        } else if let LoxValue::Nil = self {
            matches!(other, LoxValue::Nil)
        } else if let LoxValue::Nil = other {
            matches!(self, LoxValue::Nil)
        } else {
            false
        }
    }

    pub fn less(&self, other: &LoxValue) -> miette::Result<bool> {
        if let Ok(l) = self.try_num() {
            let r = other.try_num()?;
            Ok(l < r)
        } else if let Ok(l) = self.try_bool() {
            let r = other.try_bool()?;
            Ok(!l & r)
        } else if let Ok(l) = self.try_str() {
            let r = other.try_str()?;
            Ok(l < r)
        } else {
            Err(miette!("Invalid operands types for less"))
        }
    }
}

// Interpreter

const ERROR_MARGIN: f64 = 0.00001;

pub struct Interpreter<'a, W: std::io::Write> {
    /// Current environment that keeps current scope vars. Global by default
    environment: Rc<RefCell<Environment>>,
    callables: Box<Catalogue<'a>>,
    writer: W,
}

impl<'a, W: std::io::Write> Interpreter<'a, W> {
    #[must_use]
    pub fn new(writer: W) -> Self {
        let environment = Rc::new(RefCell::new(Environment::new()));
        let mut callables = Box::new(Catalogue::new());
        environment.borrow_mut().define(
            call::CLOCK.to_string(),
            LoxValue::Callable("native", call::CLOCK.to_owned()),
        );
        callables.define(call::CLOCK, Rc::new(RefCell::new(Clock {})));
        Self {
            environment,
            writer,
            callables,
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

    pub fn interpret(
        &mut self,
        statements: impl Iterator<Item = miette::Result<Stmt<'a>>>,
    ) -> miette::Result<()> {
        // HACK: Box::leak dangerous thing
        let refs = statements
            .map(|x| &*Box::leak(Box::new(x)))
            .map(|x| Rc::new(RefCell::new(x)));
        self.accept_all(refs)
    }

    fn accept_all(
        &mut self,
        statements: impl Iterator<Item = Rc<RefCell<&'a miette::Result<Stmt<'a>>>>>,
    ) -> miette::Result<()> {
        let mut errors = vec![];

        let mut add_error = |e: &miette::Report| {
            if let Some(label) = e.labels() {
                for l in label {
                    errors.push(l);
                }
            }
        };

        for stmt in statements {
            let stmt = stmt.borrow();
            match stmt.as_ref() {
                Ok(s) => {
                    if let Err(e) = s.accept(self) {
                        if let Some(ProgramError::Return(_)) = e.downcast_ref::<ProgramError>() {
                            // break interpretation
                            // error will be handled later
                            return Err(e);
                        } else {
                            add_error(&e);
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

    fn visit_block(
        &mut self,
        statements: impl Iterator<Item = Rc<RefCell<&'a miette::Result<Stmt<'a>>>>>,
        enclosing: Rc<RefCell<Environment>>,
    ) -> miette::Result<()> {
        let prev = self.environment.clone();
        let child = Environment::child(enclosing);
        self.environment = Rc::new(RefCell::new(child));
        let result = self.accept_all(statements);
        self.environment = prev;
        result
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

    fn visit_assign_expr(
        &mut self,
        name: &Token<'a>,
        value: &Expr<'a>,
    ) -> miette::Result<LoxValue> {
        let location = value.location.clone();
        if let Token::Identifier(id) = name {
            match value.accept(self) {
                Ok(val) => {
                    self.environment
                        .borrow_mut()
                        .assign(id.to_string(), val.clone())
                        .map_err(|e| {
                            miette!(
                                labels = vec![LabeledSpan::at(location, e.to_string())],
                                "Assigment failed"
                            )
                        })?;
                    Ok(val)
                }
                Err(e) => Err(e),
            }
        } else {
            Err(miette!(
                labels = vec![LabeledSpan::at(location, "Invalid l-value expression")],
                "Assigment failed"
            ))
        }
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
        if let LoxValue::Callable(_, ref id) = callee {
            let callee = self.callables.get(id)?;
            let callee = callee.borrow();

            let expected = callee.arity();
            let actual = arguments.len();
            if expected != actual {
                return Err(miette!(
                    labels = vec![LabeledSpan::at(
                        location,
                        format!(
                            "Invalid arguments number passed to '{id}'. Expected: {expected} passed: {actual}"
                        )
                    )],
                    "Invalid arguments number"
                ));
            }

            match callee.call(arguments) {
                CallResult::Value(lox_value) => Ok(lox_value),
                CallResult::Code(stmt, closure) => {
                    let body = Rc::new(RefCell::new(stmt));
                    let result = self.visit_block(once(body), closure);
                    match result {
                        Ok(_) => Ok(LoxValue::Nil),
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
        } else {
            Err(miette!(
                labels = vec![LabeledSpan::at(location, "Invalid callable type")],
                "Invalid callable type"
            ))
        }
    }

    fn visit_get_expr(&mut self, name: &Token<'a>, object: &Expr<'a>) -> miette::Result<LoxValue> {
        let _ = name;
        self.evaluate(object)
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
        name: &Token<'a>,
        obj: &Expr<'a>,
        val: &Expr<'a>,
    ) -> miette::Result<LoxValue> {
        let _ = val;
        let _ = obj;
        let _ = name;
        todo!()
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

    fn visit_variable_expr(&mut self, name: &Token<'a>) -> miette::Result<LoxValue> {
        if let Token::Identifier(id) = name {
            let val = self.environment.borrow().get(id)?;
            if let LoxValue::Nil = val {
                Err(miette!("Using uninitialized variable '{id}'"))
            } else {
                Ok(val)
            }
        } else {
            Err(miette!("Invalid identifier"))
        }
    }
}

impl<'a, W: std::io::Write> StmtVisitor<'a, miette::Result<()>> for Interpreter<'a, W> {
    fn visit_block_stmt(&mut self, body: &'a [miette::Result<Stmt<'a>>]) -> miette::Result<()> {
        let it = body.iter().map(|item| Rc::new(RefCell::new(item)));
        self.visit_block(it, self.environment.clone())?;
        Ok(())
    }

    fn visit_class_stmt(
        &self,
        name: &Token<'a>,
        superclass: &Stmt<'a>,
        methods: &[Box<Stmt<'a>>],
    ) -> miette::Result<()> {
        let _ = methods;
        let _ = superclass;
        let _ = name;
        todo!()
    }

    fn visit_expression_stmt(&mut self, expr: &Expr<'a>) -> miette::Result<()> {
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_function_decl_stmt(
        &mut self,
        token: &Token<'a>,
        params: &[Box<Expr<'a>>],
        body: &'a miette::Result<Stmt<'a>>,
    ) -> miette::Result<()> {
        if let Token::Identifier(id) = token {
            if self.environment.borrow().get(id).is_ok() {
                return Err(miette!("function or variable with '{id}' redefinition"));
            }
            self.environment
                .borrow_mut()
                .define(id.to_string(), LoxValue::Callable("fn", id.to_string()));

            let mut parameters = vec![];
            let mut names = HashSet::new();
            for param in params {
                if let ExprKind::Variable(Token::Identifier(name)) = &param.kind {
                    if !names.contains(name) {
                        names.insert(*name);
                        parameters.push(*name);
                    } else {
                        let location = param.location.clone();
                        return Err(miette!(
                            labels = vec![LabeledSpan::at(
                                location,
                                format!("Parameter '{name}' redefinition")
                            )],
                            "Parameter names must be unique"
                        ));
                    }
                }
            }

            let callable = Function::new(parameters, body, self.environment.clone());
            let callable = Rc::new(RefCell::new(callable));
            self.callables.define(id, callable);
            Ok(())
        } else {
            Err(miette!("Invalid function"))
        }
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
            LoxValue::String(_) | LoxValue::Number(_) | LoxValue::Callable(_, _) => {
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
        if let Token::Identifier(id) = name {
            if let Some(v) = initializer {
                match v.accept(self) {
                    Ok(val) => self.environment.borrow_mut().define(id.to_string(), val),
                    Err(e) => return Err(e),
                }
            } else {
                self.environment
                    .borrow_mut()
                    .define(id.to_string(), LoxValue::Nil);
            }
            Ok(())
        } else {
            Err(miette!("Invalid identifier"))
        }
    }

    fn visit_while_stmt(
        &mut self,
        cond: &Expr<'a>,
        body: &'a miette::Result<Stmt<'a>>,
    ) -> miette::Result<()> {
        let body = match body {
            Ok(s) => s,
            Err(e) => return Err(miette!(e.to_string())),
        };
        while self.evaluate(cond)?.is_truthy() {
            body.accept(self)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;

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
    #[test_case("fun x(v) { print v; } print x(10);", "10" ; "simple call one arg")]
    #[test_case("fun sum(a1, a2) { print a1 + a2; } sum(1, 2);", "3" ; "simple call two args")]
    #[test_case("fun sum_and_decr(a1, a2) { var x = a1 + a2 - 1; print x; } sum_and_decr(1, 2);", "2" ; "function with two statements")]
    #[test_case("fun foo(x) { return x + 1; } print foo(1);", "2" ; "function with return")]
    #[test_case("fun foo() { return bar; } fun bar(x, y) { return x + y; } print foo()(1, 2);", "3" ; "cascade call")]
    #[test_case("fun foo() { var i = 1; fun bar(x) { return i + x; } return bar; } print foo()(2);", "3" ; "closure")]
    fn eval_single_result_tests(input: &str, expected: &str) {
        // Arrange
        let mut parser = Parser::new(input);
        let mut stdout = Vec::new();
        let mut interpreter = Interpreter::new(&mut stdout);

        // Act
        let iterpretation_result = interpreter.interpret(&mut parser);

        // Assert
        if let Err(e) = iterpretation_result {
            panic!("iterpretation_result should be Ok. But it was: {e:#?}");
        }

        let actual = String::from_utf8(stdout).unwrap();
        assert_eq!(actual.trim_end(), expected);
    }
}
