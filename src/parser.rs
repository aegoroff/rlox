use std::iter::Peekable;

use miette::miette;

use crate::lexer::{Lexer, Token};

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
pub enum Expr<'a> {
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

impl<'a> Expr<'a> {
    pub fn accept<R>(&self, visitor: &impl ExprVisitor<'a, R>) -> R {
        match self {
            Expr::Literal(token) => visitor.visit_literal(token),
            Expr::Binary(operator, left, right) => visitor.visit_binary_expr(operator, left, right),
            Expr::Unary(operator, expr) => visitor.visit_unary_expr(operator, expr),
            Expr::Assign(name, value) => visitor.visit_assign_expr(name, value),
            Expr::Call(paren, callee, args) => visitor.visit_call_expr(paren, callee, args),
            Expr::Get(name, object) => visitor.visit_get_expr(name, object),
            Expr::Grouping(grouping) => visitor.visit_grouping_expr(grouping),
            Expr::Logical(token, left, right) => visitor.visit_logical_expr(token, left, right),
            Expr::Set(name, obj, val) => visitor.visit_set_expr(name, obj, val),
            Expr::Super(keyword, method) => visitor.visit_super_expr(keyword, method),
            Expr::This(keyword) => visitor.visit_this_expr(keyword),
            Expr::Variable(name) => visitor.visit_variable_expr(name),
        }
    }
}

// Statements

pub enum Stmt<'a> {
    Block(Vec<Box<Stmt<'a>>>),
    /// name, superclass, methods
    Class(Token<'a>, Box<Stmt<'a>>, Vec<Box<Stmt<'a>>>),
    Expression(Expr<'a>),
    /// token, params, body
    Function(Token<'a>, Vec<Box<Stmt<'a>>>, Vec<Box<Stmt<'a>>>),
    /// condition, then, else
    If(Expr<'a>, Box<Stmt<'a>>, Box<Stmt<'a>>),
    Print(Expr<'a>),
    Return(Token<'a>, Expr<'a>),
    Variable(Token<'a>, Expr<'a>),
    While(Expr<'a>, Box<Stmt<'a>>),
}

impl<'a> Stmt<'a> {
    pub fn accept<R>(&self, visitor: &impl StmtVisitor<'a, R>) -> R {
        match self {
            Stmt::Block(body) => visitor.visit_block_stmt(body),
            Stmt::Class(name, superclass, methods) => {
                visitor.visit_class_stmt(name, superclass, methods)
            }
            Stmt::Expression(expr) => visitor.visit_expression_stmt(expr),
            Stmt::Function(token, params, body) => visitor.visit_function_stmt(token, params, body),
            Stmt::If(cond, then, otherwise) => visitor.visit_if_stmt(cond, then, otherwise),
            Stmt::Print(expr) => visitor.visit_print_stmt(expr),
            Stmt::Return(keyword, value) => visitor.visit_return_stmt(keyword, value),
            Stmt::Variable(name, initializer) => visitor.visit_variable_stmt(name, initializer),
            Stmt::While(cond, body) => visitor.visit_while_stmt(cond, body),
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
    fn visit_variable_stmt(&self, name: &Token<'a>, initializer: &Expr<'a>) -> R;
    fn visit_while_stmt(&self, cond: &Expr<'a>, body: &Stmt<'a>) -> R;
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(content: &'a str) -> Self {
        Self {
            lexer: Lexer::new(content).peekable(),
        }
    }

    pub fn parse(&mut self) -> Option<miette::Result<Expr<'a>>> {
        self.expression()
    }

    fn expression(&mut self) -> Option<miette::Result<Expr<'a>>> {
        self.equality()
    }

    fn equality(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let expr = match self.comparison()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };
        match self.lexer.peek()? {
            Ok(tok) => {
                if let Token::Bang | Token::BangEqual = tok {
                    let operator = self.lexer.next()?.unwrap(); // TODO
                    match self.comparison()? {
                        Ok(r) => Some(Ok(Expr::Binary(operator, Box::new(expr), Box::new(r)))),
                        Err(e) => Some(Err(e)),
                    }
                } else {
                    Some(Ok(expr))
                }
            }
            Err(_e) => Some(Err(miette!("Unexpected equality error"))), // TODO
        }
    }

    fn comparison(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let expr = match self.term()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };
        match self.lexer.peek()? {
            Ok(tok) => {
                if let Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual = tok {
                    let operator = self.lexer.next()?.unwrap(); // TODO
                    match self.term()? {
                        Ok(r) => Some(Ok(Expr::Binary(operator, Box::new(expr), Box::new(r)))),
                        Err(e) => Some(Err(e)),
                    }
                } else {
                    Some(Ok(expr))
                }
            }
            Err(_e) => Some(Err(miette!("Unexpected comparison error"))), // TODO
        }
    }

    fn term(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let expr = match self.factor()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };
        match self.lexer.peek()? {
            Ok(tok) => {
                if let Token::Plus | Token::Minus = tok {
                    let operator = self.lexer.next()?.unwrap(); // TODO
                    match self.factor()? {
                        Ok(r) => Some(Ok(Expr::Binary(operator, Box::new(expr), Box::new(r)))),
                        Err(e) => Some(Err(e)),
                    }
                } else {
                    Some(Ok(expr))
                }
            }
            Err(_e) => Some(Err(miette!("Unexpected term error"))), // TODO
        }
    }

    fn factor(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let expr = match self.unary()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };
        match self.lexer.peek()? {
            Ok(tok) => {
                if let Token::Star | Token::Slash = tok {
                    let operator = self.lexer.next()?.unwrap(); // TODO
                    match self.unary()? {
                        Ok(r) => Some(Ok(Expr::Binary(operator, Box::new(expr), Box::new(r)))),
                        Err(e) => Some(Err(e)),
                    }
                } else {
                    Some(Ok(expr))
                }
            }
            Err(_e) => Some(Err(miette!("Unexpected factor error"))), // TODO
        }
    }

    fn unary(&mut self) -> Option<miette::Result<Expr<'a>>> {
        match self.lexer.peek()? {
            Ok(tok) => {
                if let Token::Bang | Token::Minus = tok {
                    let operator = self.lexer.next()?.unwrap(); // TODO
                    match self.unary()? {
                        Ok(r) => Some(Ok(Expr::Unary(operator, Box::new(r)))),
                        Err(e) => Some(Err(e)),
                    }
                } else {
                    self.primary()
                }
            }
            Err(_e) => Some(Err(miette!("Unexpected unary error"))), // TODO
        }
    }

    fn primary(&mut self) -> Option<miette::Result<Expr<'a>>> {
        if let Ok(tok) = self.lexer.peek()? {
            match tok {
                Token::String(_) | Token::Number(_) | Token::False | Token::Nil | Token::True => {
                    let t = self.lexer.next()?.unwrap(); // TODO
                    return Some(Ok(Expr::Literal(Some(t))));
                }
                Token::LeftParen => match self.expression()? {
                    Ok(expr) => {
                        if let Ok(tok) = self.lexer.next()? {
                            if let Token::RightParen = tok {
                                let g = Expr::Grouping(Box::new(expr));
                                return Some(Ok(g));
                            }
                            return Some(Err(miette!("Expect ')' after expression.")));
                        }
                    }
                    Err(e) => return Some(Err(e)),
                },
                _ => return self.expression(),
            }
        }
        self.expression()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test_case("1 + 2 * 3")]
    #[test_case("1 + 2 - 4")]
    #[test_case("(1 + 2) * 5")]
    fn parser_tests(input: &str) {
        // Arrange
        let mut parser = Parser::new(input);

        // Act
        let actual = parser.parse();

        // Assert
        assert!(actual.is_none());
    }
}
