#![allow(clippy::missing_errors_doc)]

use std::{fmt::Display, ops::RangeInclusive};

use miette::{Diagnostic, LabeledSpan, miette};
use thiserror::Error;

use crate::lexer::Token;

// Expressions

pub trait ExprVisitor<'a, R> {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> R;
    fn visit_binary_expr(&self, operator: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) -> R;
    fn visit_unary_expr(&self, operator: &Token<'a>, expr: &Expr<'a>) -> R;
    fn visit_assign_expr(&self, name: &Token<'a>, value: &Expr<'a>) -> R;
    fn visit_call_expr(&self, paren: &Token<'a>, callee: &Expr<'a>, args: &[Box<Expr<'a>>]) -> R;
    fn visit_get_expr(&self, name: &Token<'a>, object: &Expr<'a>) -> R;
    fn visit_grouping_expr(&self, grouping: &Expr<'a>) -> R;
    fn visit_logical_expr(&self, operator: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) -> R;
    fn visit_set_expr(&self, name: &Token<'a>, obj: &Expr<'a>, val: &Expr<'a>) -> R;
    fn visit_super_expr(&self, keyword: &Token<'a>, method: &Token<'a>) -> R;
    fn visit_this_expr(&self, keyword: &Token<'a>) -> R;
    fn visit_variable_expr(&self, name: &Token<'a>) -> R;
}

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
    pub fn accept<R>(&self, visitor: &impl ExprVisitor<'a, R>) -> R {
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
    pub fn accept<R>(&self, visitor: &impl StmtVisitor<'a, R>) -> R {
        match &self.kind {
            StmtKind::Block(stmts) => visitor.visit_block_stmt(stmts),
            StmtKind::Class(token, stmt, stmts) => visitor.visit_class_stmt(token, stmt, stmts),
            StmtKind::Expression(expr) => visitor.visit_expression_stmt(expr),
            StmtKind::Function(token, params, body) => {
                visitor.visit_function_stmt(token, params, body)
            }
            StmtKind::If(cond, then, otherwise) => visitor.visit_if_stmt(cond, then, otherwise),
            StmtKind::Print(expr) => visitor.visit_print_stmt(expr),
            StmtKind::Return(keyword, value) => visitor.visit_return_stmt(keyword, value),
            StmtKind::Variable(name, initializer) => {
                visitor.visit_variable_stmt(name, initializer.as_deref())
            }
            StmtKind::While(cond, body) => visitor.visit_while_stmt(cond, body),
        }
    }
}

#[derive(Debug)]
pub enum StmtKind<'a> {
    Block(Vec<Box<Stmt<'a>>>),
    /// name, superclass, methods
    Class(Token<'a>, Box<Stmt<'a>>, Vec<Box<Stmt<'a>>>),
    Expression(Box<Expr<'a>>),
    /// token, params, body
    Function(Token<'a>, Vec<Box<Stmt<'a>>>, Vec<Box<Stmt<'a>>>),
    /// condition, then, else
    If(Box<Expr<'a>>, Box<Stmt<'a>>, Box<Stmt<'a>>),
    Print(Box<Expr<'a>>),
    Return(Token<'a>, Box<Expr<'a>>),
    Variable(Token<'a>, Option<Box<Expr<'a>>>),
    While(Box<Expr<'a>>, Box<Stmt<'a>>),
}

impl<'a> StmtKind<'a> {
    pub fn accept<R>(&self, visitor: &impl StmtVisitor<'a, R>) -> R {
        match self {
            StmtKind::Block(body) => visitor.visit_block_stmt(body),
            StmtKind::Class(name, superclass, methods) => {
                visitor.visit_class_stmt(name, superclass, methods)
            }
            StmtKind::Expression(expr) => visitor.visit_expression_stmt(expr),
            StmtKind::Function(token, params, body) => {
                visitor.visit_function_stmt(token, params, body)
            }
            StmtKind::If(cond, then, otherwise) => visitor.visit_if_stmt(cond, then, otherwise),
            StmtKind::Print(expr) => visitor.visit_print_stmt(expr),
            StmtKind::Return(keyword, value) => visitor.visit_return_stmt(keyword, value),
            StmtKind::Variable(name, initializer) => {
                visitor.visit_variable_stmt(name, initializer.as_deref())
            }
            StmtKind::While(cond, body) => visitor.visit_while_stmt(cond, body),
        }
    }
}

pub trait StmtVisitor<'a, R> {
    fn visit_block_stmt(&self, body: &[Box<Stmt<'a>>]) -> R;
    fn visit_class_stmt(
        &self,
        name: &Token<'a>,
        superclass: &Stmt<'a>,
        methods: &[Box<Stmt<'a>>],
    ) -> R;
    fn visit_expression_stmt(&self, expr: &Expr<'a>) -> R;
    fn visit_function_stmt(
        &self,
        token: &Token<'a>,
        params: &[Box<Stmt<'a>>],
        body: &[Box<Stmt<'a>>],
    ) -> R;
    fn visit_if_stmt(&self, cond: &Expr<'a>, then: &Stmt<'a>, otherwise: &Stmt<'a>) -> R;
    fn visit_print_stmt(&self, expr: &Expr<'a>) -> R;
    fn visit_return_stmt(&self, keyword: &Token<'a>, value: &Expr<'a>) -> R;
    fn visit_variable_stmt(&self, name: &Token<'a>, initializer: Option<&Expr<'a>>) -> R;
    fn visit_while_stmt(&self, cond: &Expr<'a>, body: &Stmt<'a>) -> R;
}

// Values

pub enum LoxValue {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::String(s) => write!(f, "{s}"),
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

    pub fn equal(&self, other: &LoxValue) -> miette::Result<bool> {
        if let Ok(l) = self.try_num() {
            let r = other.try_num()?;
            Ok((l - r).abs() < ERROR_MARGIN)
        } else if let Ok(l) = self.try_bool() {
            let r = other.try_bool()?;
            Ok(l == r)
        } else if let Ok(l) = self.try_str() {
            let r = other.try_str()?;
            Ok(l == r)
        } else if let LoxValue::Nil = self {
            if let LoxValue::Nil = other {
                Ok(true)
            } else {
                Ok(false)
            }
        } else if let LoxValue::Nil = other {
            if let LoxValue::Nil = self {
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Err(miette!("Invalid operands types for equality"))
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

#[derive(Debug, Error, Diagnostic)]
#[error("Program completed with errors")]
#[diagnostic()]
pub struct ProgramError {
    #[related]
    errors: Vec<miette::Report>,
}

const ERROR_MARGIN: f64 = 0.00001;

pub struct Interpreter {}

impl Interpreter {
    pub fn evaluate(&self, expr: &Expr<'_>) -> miette::Result<LoxValue> {
        expr.accept(&self)
    }

    pub fn interpret<'a>(
        &self,
        statements: impl Iterator<Item = miette::Result<Stmt<'a>>>,
    ) -> miette::Result<()> {
        let mut errors = vec![];

        for stmt in statements {
            match stmt {
                Ok(s) => {
                    if let Err(e) = s.accept(&self) {
                        errors.push(e);
                    }
                }
                Err(e) => errors.push(e),
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(ProgramError { errors }.into())
        }
    }
}

fn map_operand_err<T>(err: miette::Result<T>, op: &Expr<'_>) -> miette::Result<T> {
    err.map_err(|e| {
        miette!(
            labels = vec![LabeledSpan::at(op.location.clone(), "Problem expression")],
            "Invalid operand"
        )
        .wrap_err(e)
    })
}

impl<'a> ExprVisitor<'a, miette::Result<LoxValue>> for &Interpreter {
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
        &self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> miette::Result<LoxValue> {
        let lhs = self.evaluate(left)?;
        let rhs = self.evaluate(right)?;

        match operator {
            Token::Minus => {
                let l = map_operand_err(lhs.try_num(), left)?;
                let r = map_operand_err(rhs.try_num(), right)?;
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
                    let r = map_operand_err(rhs.try_num(), right)?;
                    let result = l + r;
                    return Ok(LoxValue::Number(result));
                }
                let start = *left.location.start();
                let end = *right.location.end();
                Err(miette!(
                    labels = vec![LabeledSpan::at(start..=end, "Problem expression")],
                    "Invalid operands types for plus"
                ))
            }
            Token::Slash => {
                let l = map_operand_err(lhs.try_num(), left)?;
                let r = map_operand_err(rhs.try_num(), right)?;

                if !r.is_normal() && !r.is_infinite() {
                    Err(miette!(
                        labels = vec![LabeledSpan::at(
                            right.location.clone(),
                            "Zero division detected here"
                        )],
                        "Zero division detected"
                    ))
                } else {
                    let result = l / r;
                    Ok(LoxValue::Number(result))
                }
            }
            Token::Star => {
                let l = map_operand_err(lhs.try_num(), left)?;
                let r = map_operand_err(rhs.try_num(), right)?;
                let result = l * r;
                Ok(LoxValue::Number(result))
            }
            Token::BangEqual => {
                let eq = map_operand_err(lhs.equal(&rhs), right)?;
                Ok(LoxValue::Bool(!eq))
            }
            Token::EqualEqual => {
                let eq = map_operand_err(lhs.equal(&rhs), right)?;
                Ok(LoxValue::Bool(eq))
            }
            Token::Greater => {
                let lt = map_operand_err(lhs.less(&rhs), right)?;
                let eq = map_operand_err(lhs.equal(&rhs), right)?;
                let gt = !lt && !eq;
                Ok(LoxValue::Bool(gt))
            }
            Token::GreaterEqual => {
                let lt = map_operand_err(lhs.less(&rhs), right)?;
                let eq = map_operand_err(lhs.equal(&rhs), right)?;
                let ge = !lt || eq;
                Ok(LoxValue::Bool(ge))
            }
            Token::Less => {
                let lt = map_operand_err(lhs.less(&rhs), right)?;
                Ok(LoxValue::Bool(lt))
            }
            Token::LessEqual => {
                let lt = map_operand_err(lhs.less(&rhs), right)?;
                let eq = map_operand_err(lhs.equal(&rhs), right)?;
                let le = lt || eq;
                Ok(LoxValue::Bool(le))
            }
            _ => Err(miette!("Invalid binary operator")),
        }
    }

    fn visit_unary_expr(&self, operator: &Token<'a>, expr: &Expr<'a>) -> miette::Result<LoxValue> {
        let val = self.evaluate(expr)?;
        match operator {
            Token::Minus => Ok(LoxValue::Number(-val.try_num()?)),
            Token::Bang => Ok(LoxValue::Bool(!val.try_bool()?)),
            _ => Err(miette!(
                labels = vec![LabeledSpan::at(expr.location.clone(), "Problem expression")],
                "Invalid unary operator"
            )),
        }
    }

    fn visit_assign_expr(&self, name: &Token<'a>, value: &Expr<'a>) -> miette::Result<LoxValue> {
        let _ = value;
        let _ = name;
        todo!()
    }

    fn visit_call_expr(
        &self,
        paren: &Token<'a>,
        callee: &Expr<'a>,
        args: &[Box<Expr<'a>>],
    ) -> miette::Result<LoxValue> {
        let _ = args;
        let _ = callee;
        let _ = paren;
        todo!()
    }

    fn visit_get_expr(&self, name: &Token<'a>, object: &Expr<'a>) -> miette::Result<LoxValue> {
        let _ = name;
        self.evaluate(object)
    }

    fn visit_grouping_expr(&self, grouping: &Expr<'a>) -> miette::Result<LoxValue> {
        self.evaluate(grouping)
    }

    fn visit_logical_expr(
        &self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> miette::Result<LoxValue> {
        let _ = right;
        let _ = left;
        let _ = operator;
        todo!()
    }

    fn visit_set_expr(
        &self,
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
        &self,
        keyword: &Token<'a>,
        method: &Token<'a>,
    ) -> miette::Result<LoxValue> {
        let _ = method;
        let _ = keyword;
        todo!()
    }

    fn visit_this_expr(&self, keyword: &Token<'a>) -> miette::Result<LoxValue> {
        let _ = keyword;
        todo!()
    }

    fn visit_variable_expr(&self, name: &Token<'a>) -> miette::Result<LoxValue> {
        match name {
            Token::Identifier(id) => Ok(LoxValue::String((*id).to_string())),
            _ => Err(miette!("Invalid identifier")),
        }
    }
}

impl<'a> StmtVisitor<'a, miette::Result<()>> for &Interpreter {
    fn visit_block_stmt(&self, body: &[Box<Stmt<'a>>]) -> miette::Result<()> {
        let _ = body;
        todo!()
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

    fn visit_expression_stmt(&self, expr: &Expr<'a>) -> miette::Result<()> {
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_function_stmt(
        &self,
        token: &Token<'a>,
        params: &[Box<Stmt<'a>>],
        body: &[Box<Stmt<'a>>],
    ) -> miette::Result<()> {
        let _ = body;
        let _ = params;
        let _ = token;
        todo!()
    }

    fn visit_if_stmt(
        &self,
        cond: &Expr<'a>,
        then: &Stmt<'a>,
        otherwise: &Stmt<'a>,
    ) -> miette::Result<()> {
        let _ = otherwise;
        let _ = then;
        let _ = cond;
        todo!()
    }

    fn visit_print_stmt(&self, expr: &Expr<'a>) -> miette::Result<()> {
        match self.evaluate(expr) {
            Ok(e) => println!("{e}"),
            Err(e) => {
                return Err(e);
            }
        }
        Ok(())
    }

    fn visit_return_stmt(&self, keyword: &Token<'a>, value: &Expr<'a>) -> miette::Result<()> {
        let _ = value;
        let _ = keyword;
        todo!()
    }

    fn visit_variable_stmt(
        &self,
        name: &Token<'a>,
        initializer: Option<&Expr<'a>>,
    ) -> miette::Result<()> {
        let _ = name;
        let _ = initializer;
        // TODO: add var into scope
        Ok(())
    }

    fn visit_while_stmt(&self, cond: &Expr<'a>, body: &Stmt<'a>) -> miette::Result<()> {
        let _ = body;
        let _ = cond;
        todo!()
    }
}

#[cfg(test)]
mod tests {}
