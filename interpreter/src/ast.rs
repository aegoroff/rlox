#![allow(clippy::missing_errors_doc)]

use std::{
    cell::RefCell,
    fmt::Display,
    hash::{DefaultHasher, Hash, Hasher},
    ops::Range,
    rc::Rc,
};

use crate::{LoxError, env::Environment};
use scanner::Token;

// Traits

pub trait ExprVisitor<'a, R> {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> R;
    fn visit_binary_expr(&mut self, operator: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) -> R;
    fn visit_unary_expr(&mut self, operator: &Token<'a>, expr: &Expr<'a>) -> R;
    fn visit_assign_expr(&mut self, lhs: &Expr<'a>, rhs: &Expr<'a>) -> R;
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
    fn visit_super_expr(&mut self, obj: &Expr<'a>, keyword: &Token<'a>, method: &Token<'a>) -> R;
    fn visit_this_expr(&mut self, obj: &Expr<'a>, keyword: &Token<'a>) -> R;
    fn visit_variable_expr(&mut self, obj: &Expr<'a>, name: &Token<'a>) -> R;
}

pub trait StmtVisitor<'a, R> {
    fn visit_block_stmt(&mut self, body: &'a [crate::Result<Stmt<'a>>]) -> R;
    fn visit_class_stmt(
        &mut self,
        name: &Token<'a>,
        superclass: &Option<Box<Expr<'a>>>,
        methods: &'a [crate::Result<Stmt<'a>>],
    ) -> R;
    fn visit_expression_stmt(&mut self, expr: &Expr<'a>) -> R;
    fn visit_function_decl_stmt(
        &mut self,
        kind: FunctionKind,
        token: &Token<'a>,
        params: &[Box<Expr<'a>>],
        body: &'a crate::Result<Stmt<'a>>,
    ) -> R;
    fn visit_if_stmt(
        &mut self,
        cond: &Expr<'a>,
        then: &'a crate::Result<Stmt<'a>>,
        otherwise: &'a Option<Box<crate::Result<Stmt<'a>>>>,
    ) -> R;
    fn visit_print_stmt(&mut self, expr: &Expr<'a>) -> R;
    fn visit_return_stmt(&mut self, keyword: &Token<'a>, value: &Expr<'a>) -> R;
    fn visit_variable_stmt(&mut self, name: &Token<'a>, initializer: &Option<Box<Expr<'a>>>) -> R;
    fn visit_while_stmt(&mut self, cond: &Expr<'a>, body: &'a crate::Result<Stmt<'a>>) -> R;
}

// Expressions

#[derive(Debug, Hash)]
pub enum ExprKind<'a> {
    Literal(Option<Token<'a>>),
    Binary(Token<'a>, Box<Expr<'a>>, Box<Expr<'a>>),
    Unary(Token<'a>, Box<Expr<'a>>),
    Assign(Box<Expr<'a>>, Box<Expr<'a>>),
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

#[derive(Debug, Hash)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub location: Range<usize>,
}

impl<'a> Expr<'a> {
    pub fn accept<R>(&self, visitor: &mut impl ExprVisitor<'a, R>) -> R {
        match &self.kind {
            ExprKind::Literal(token) => visitor.visit_literal(token),
            ExprKind::Binary(operator, left, right) => {
                visitor.visit_binary_expr(operator, left, right)
            }
            ExprKind::Unary(operator, expr) => visitor.visit_unary_expr(operator, expr),
            ExprKind::Assign(lhs, rhs) => visitor.visit_assign_expr(lhs, rhs),
            ExprKind::Call(paren, callee, args) => visitor.visit_call_expr(paren, callee, args),
            ExprKind::Get(name, object) => visitor.visit_get_expr(name, object),
            ExprKind::Grouping(grouping) => visitor.visit_grouping_expr(grouping),
            ExprKind::Logical(token, left, right) => visitor.visit_logical_expr(token, left, right),
            ExprKind::Set(name, obj, val) => visitor.visit_set_expr(name, obj, val),
            ExprKind::Super(keyword, method) => visitor.visit_super_expr(self, keyword, method),
            ExprKind::This(keyword) => visitor.visit_this_expr(self, keyword),
            ExprKind::Variable(name) => visitor.visit_variable_expr(self, name),
        }
    }

    #[must_use]
    pub fn get_hash_code(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

// Statements

#[derive(Debug)]
pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub location: Range<usize>,
}

impl<'a> Stmt<'a> {
    pub fn accept<R>(&'a self, visitor: &mut impl StmtVisitor<'a, R>) -> R {
        match &self.kind {
            StmtKind::Block(stmts) => visitor.visit_block_stmt(stmts),
            StmtKind::Class(token, stmt, stmts) => visitor.visit_class_stmt(token, stmt, stmts),
            StmtKind::Expression(expr) => visitor.visit_expression_stmt(expr),
            StmtKind::Function(kind, token, params, body) => {
                visitor.visit_function_decl_stmt(*kind, token, params, body)
            }
            StmtKind::If(cond, then, otherwise) => visitor.visit_if_stmt(cond, then, otherwise),
            StmtKind::Print(expr) => visitor.visit_print_stmt(expr),
            StmtKind::Return(keyword, value) => visitor.visit_return_stmt(keyword, value),
            StmtKind::Variable(name, initializer) => visitor.visit_variable_stmt(name, initializer),
            StmtKind::While(cond, body) => visitor.visit_while_stmt(cond, body),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionKind {
    None = 0,
    Function = 1,
    Method = 2,
    Initializer = 3,
}

impl Display for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::None => write!(f, "none"),
            FunctionKind::Function => write!(f, "function"),
            FunctionKind::Method => write!(f, "method"),
            FunctionKind::Initializer => write!(f, "initializer"),
        }
    }
}

#[derive(Debug)]
pub enum StmtKind<'a> {
    Block(Vec<crate::Result<Stmt<'a>>>),
    /// name, superclass, methods
    Class(
        Token<'a>,
        Option<Box<Expr<'a>>>,
        Vec<crate::Result<Stmt<'a>>>,
    ),
    Expression(Box<Expr<'a>>),
    /// kind, token, params, body
    Function(
        FunctionKind,
        Token<'a>,
        Vec<Box<Expr<'a>>>,
        Box<crate::Result<Stmt<'a>>>,
    ),
    /// condition, then, else
    If(
        Box<Expr<'a>>,
        Box<crate::Result<Stmt<'a>>>,
        Option<Box<crate::Result<Stmt<'a>>>>,
    ),
    Print(Box<Expr<'a>>),
    Return(Token<'a>, Box<Expr<'a>>),
    Variable(Token<'a>, Option<Box<Expr<'a>>>),
    While(Box<Expr<'a>>, Box<crate::Result<Stmt<'a>>>),
}

// Values

const ERROR_MARGIN: f64 = 0.00001;

#[derive(Clone, Debug)]
pub enum LoxValue {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
    Callable(&'static str, String, Option<String>),
    Instance(String, Rc<RefCell<Environment>>),
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::String(s) => write!(f, "{s}"),
            LoxValue::Callable(kind, val, parent) => {
                if let Some(parent) = parent {
                    write!(f, "<{kind} {parent}.{val}>")
                } else {
                    write!(f, "<{kind} {val}>")
                }
            }
            LoxValue::Number(n) => write!(f, "{n}"),
            LoxValue::Bool(b) => write!(f, "{b}"),
            LoxValue::Nil => write!(f, ""),
            LoxValue::Instance(class, _) => write!(f, "instance of {class}"),
        }
    }
}

impl LoxValue {
    pub fn try_num(&self) -> crate::Result<f64> {
        if let LoxValue::Number(n) = self {
            Ok(*n)
        } else {
            Err(LoxError::Error(miette::miette!("Expected number")))
        }
    }

    pub fn try_str(&self) -> crate::Result<&String> {
        if let LoxValue::String(s) = self {
            Ok(s)
        } else {
            Err(LoxError::Error(miette::miette!("Expected string")))
        }
    }

    pub fn try_bool(&self) -> crate::Result<bool> {
        match self {
            LoxValue::Bool(b) => Ok(*b),
            LoxValue::Nil => Ok(false),
            _ => Err(LoxError::Error(miette::miette!("Expected boolean"))),
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

    pub fn less(&self, other: &LoxValue) -> crate::Result<bool> {
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
            Err(LoxError::Error(miette::miette!("Operands must be numbers")))
        }
    }
}
