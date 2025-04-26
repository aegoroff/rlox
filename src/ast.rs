#![allow(clippy::missing_errors_doc)]

use std::{fmt::Display, hash::DefaultHasher, hash::Hash, hash::Hasher, ops::RangeInclusive};

use miette::miette;

use crate::lexer::Token;

// Traits

pub trait ExprVisitor<'a, R> {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> R;
    fn visit_binary_expr(&mut self, operator: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) -> R;
    fn visit_unary_expr(&mut self, operator: &Token<'a>, expr: &Expr<'a>) -> R;
    fn visit_assign_expr(&mut self, to: &Expr<'a>, name: &Token<'a>, value: &Expr<'a>) -> R;
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
    fn visit_variable_expr(&mut self, obj: &Expr<'a>, name: &Token<'a>) -> R;
}

pub trait StmtVisitor<'a, R> {
    fn visit_block_stmt(&mut self, body: &'a [miette::Result<Stmt<'a>>]) -> R;
    fn visit_class_stmt(
        &mut self,
        name: &Token<'a>,
        superclass: &Option<Box<Stmt<'a>>>,
        methods: &[miette::Result<Stmt<'a>>],
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

// Expressions

#[derive(Debug, Hash)]
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

#[derive(Debug, Hash)]
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
            ExprKind::Assign(name, value) => visitor.visit_assign_expr(self, name, value),
            ExprKind::Call(paren, callee, args) => visitor.visit_call_expr(paren, callee, args),
            ExprKind::Get(name, object) => visitor.visit_get_expr(name, object),
            ExprKind::Grouping(grouping) => visitor.visit_grouping_expr(grouping),
            ExprKind::Logical(token, left, right) => visitor.visit_logical_expr(token, left, right),
            ExprKind::Set(name, obj, val) => visitor.visit_set_expr(name, obj, val),
            ExprKind::Super(keyword, method) => visitor.visit_super_expr(keyword, method),
            ExprKind::This(keyword) => visitor.visit_this_expr(keyword),
            ExprKind::Variable(name) => visitor.visit_variable_expr(self, name),
        }
    }

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
    Class(
        Token<'a>,
        Option<Box<Stmt<'a>>>,
        Vec<miette::Result<Stmt<'a>>>,
    ),
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

const ERROR_MARGIN: f64 = 0.00001;

#[derive(Clone, Debug)]
pub enum LoxValue {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
    Callable(&'static str, String),
    Instance(String),
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::String(s) => write!(f, "{s}"),
            LoxValue::Callable(kind, val) => write!(f, "<{kind} {val}>"),
            LoxValue::Number(n) => write!(f, "{n}"),
            LoxValue::Bool(b) => write!(f, "{b}"),
            LoxValue::Nil => write!(f, ""),
            LoxValue::Instance(class) => write!(f, "{class} instance"),
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
