use std::iter::Peekable;

use miette::{LabeledSpan, miette};

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

pub struct AstPrinter {}

impl AstPrinter {
    fn parenthesize(&self, name: &str, expressions: &[&Expr<'_>]) -> String {
        let expressions = expressions
            .iter()
            .map(|e| e.accept(&self))
            .collect::<Vec<String>>()
            .join(" ");

        format!("({name} {expressions})")
    }

    pub fn print(&self, expr: &Expr<'_>) {
        println!("{}", expr.accept(&self));
    }
}

impl<'a> ExprVisitor<'a, String> for &AstPrinter {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> String {
        match token {
            Some(t) => format!("{t}"),
            None => "null".to_owned(),
        }
    }

    fn visit_binary_expr(&self, operator: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) -> String {
        let op = format!("{operator}");
        self.parenthesize(&op, &[left, right])
    }

    fn visit_unary_expr(&self, operator: &Token<'a>, expr: &Expr<'a>) -> String {
        let op = format!("{operator}");
        self.parenthesize(&op, &[expr])
    }

    fn visit_assign_expr(&self, name: &Token<'a>, value: &Expr<'a>) -> String {
        let _ = value;
        let _ = name;
        todo!()
    }

    fn visit_call_expr(
        &self,
        paren: &Token<'a>,
        callee: &Expr<'a>,
        args: &[Box<Expr<'a>>],
    ) -> String {
        let _ = args;
        let _ = callee;
        let _ = paren;
        todo!()
    }

    fn visit_get_expr(&self, name: &Token<'a>, object: &Expr<'a>) -> String {
        let _ = object;
        let _ = name;
        todo!()
    }

    fn visit_grouping_expr(&self, grouping: &Expr<'a>) -> String {
        self.parenthesize("group", &[grouping])
    }

    fn visit_logical_expr(
        &self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> String {
        let op = format!("{operator}");
        self.parenthesize(&op, &[left, right])
    }

    fn visit_set_expr(&self, name: &Token<'a>, obj: &Expr<'a>, val: &Expr<'a>) -> String {
        let _ = val;
        let _ = obj;
        let _ = name;
        todo!()
    }

    fn visit_super_expr(&self, keyword: &Token<'a>, method: &Token<'a>) -> String {
        let _ = method;
        let _ = keyword;
        todo!()
    }

    fn visit_this_expr(&self, keyword: &Token<'a>) -> String {
        let _ = keyword;
        todo!()
    }

    fn visit_variable_expr(&self, name: &Token<'a>) -> String {
        let _ = name;
        todo!()
    }
}

pub enum LoxValue<'a> {
    String(&'a str),
    Number(f64),
    Bool(bool),
}

pub struct Evaluator {}

impl<'a> ExprVisitor<'a, LoxValue<'a>> for Evaluator {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> LoxValue<'a> {
        let _ = token;
        todo!()
    }

    fn visit_binary_expr(
        &self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> LoxValue<'a> {
        let _ = right;
        let _ = left;
        let _ = operator;
        todo!()
    }

    fn visit_unary_expr(&self, operator: &Token<'a>, expr: &Expr<'a>) -> LoxValue<'a> {
        let _ = operator;
        let _ = expr;
        todo!()
    }

    fn visit_assign_expr(&self, name: &Token<'a>, value: &Expr<'a>) -> LoxValue<'a> {
        let _ = value;
        let _ = name;
        todo!()
    }

    fn visit_call_expr(
        &self,
        paren: &Token<'a>,
        callee: &Expr<'a>,
        args: &[Box<Expr<'a>>],
    ) -> LoxValue<'a> {
        let _ = args;
        let _ = callee;
        let _ = paren;
        todo!()
    }

    fn visit_get_expr(&self, name: &Token<'a>, object: &Expr<'a>) -> LoxValue<'a> {
        let _ = object;
        let _ = name;
        todo!()
    }

    fn visit_grouping_expr(&self, grouping: &Expr<'a>) -> LoxValue<'a> {
        let _ = grouping;
        todo!()
    }

    fn visit_logical_expr(
        &self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> LoxValue<'a> {
        let _ = right;
        let _ = left;
        let _ = operator;
        todo!()
    }

    fn visit_set_expr(&self, name: &Token<'a>, obj: &Expr<'a>, val: &Expr<'a>) -> LoxValue<'a> {
        let _ = val;
        let _ = obj;
        let _ = name;
        todo!()
    }

    fn visit_super_expr(&self, keyword: &Token<'a>, method: &Token<'a>) -> LoxValue<'a> {
        let _ = method;
        let _ = keyword;
        todo!()
    }

    fn visit_this_expr(&self, keyword: &Token<'a>) -> LoxValue<'a> {
        let _ = keyword;
        todo!()
    }

    fn visit_variable_expr(&self, name: &Token<'a>) -> LoxValue<'a> {
        let _ = name;
        todo!()
    }
}

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(content: &'a str) -> Self {
        Self {
            tokens: Lexer::new(content).peekable(),
        }
    }

    pub fn parse(&mut self) -> Option<miette::Result<Expr<'a>>> {
        self.expression()
    }

    fn expression(&mut self) -> Option<miette::Result<Expr<'a>>> {
        self.equality()
    }

    fn equality(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let mut expr = match self.comparison()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };

        loop {
            let Some(current) = self.tokens.peek() else {
                return Some(Ok(expr));
            };
            let Ok(next_tok) = current else {
                // Consume token if it's not a valid
                match self.tokens.next()? {
                    Ok(_) => unreachable!(),
                    Err(e) => return Some(Err(e)),
                }
            };

            if !matches!(next_tok, (_, Token::EqualEqual | Token::BangEqual, _)) {
                break;
            }

            // Consume operator
            let (s, operator, f) = match self.tokens.next()? {
                Ok(tok) => tok,
                Err(e) => return Some(Err(e)),
            };

            let Some(comparison) = self.comparison() else {
                return Some(Err(miette!(
                    labels = vec![LabeledSpan::at(
                        s..=f,
                        "Missing comparison expression in the right part of binary expression"
                    )],
                    "Invalid syntax"
                )));
            };

            let right = match comparison {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };

            expr = Expr::Binary(operator, Box::new(expr), Box::new(right));
        }

        Some(Ok(expr))
    }

    fn comparison(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let mut expr = match self.term()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };

        loop {
            let Some(current) = self.tokens.peek() else {
                return Some(Ok(expr));
            };
            let Ok(next_tok) = current else {
                // Consume token if it's not a valid
                match self.tokens.next()? {
                    Ok(_) => unreachable!(),
                    Err(e) => return Some(Err(e)),
                }
            };

            if !matches!(
                next_tok,
                (
                    _,
                    Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual,
                    _
                )
            ) {
                break;
            }

            // Consume operator
            let (s, operator, f) = match self.tokens.next()? {
                Ok(tok) => tok,
                Err(e) => return Some(Err(e)),
            };

            let Some(term) = self.term() else {
                return Some(Err(miette!(
                    labels = vec![LabeledSpan::at(
                        s..=f,
                        "Missing term expression in the right part of binary expression"
                    )],
                    "Invalid syntax"
                )));
            };

            let right = match term {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };

            expr = Expr::Binary(operator, Box::new(expr), Box::new(right));
        }

        Some(Ok(expr))
    }

    fn term(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let mut expr = match self.factor()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };

        loop {
            let Some(current) = self.tokens.peek() else {
                return Some(Ok(expr));
            };
            let Ok(next_tok) = current else {
                // Consume token if it's not a valid
                match self.tokens.next()? {
                    Ok(_) => unreachable!(),
                    Err(e) => return Some(Err(e)),
                }
            };

            if !matches!(next_tok, (_, Token::Plus | Token::Minus, _)) {
                break;
            }

            // Consume operator
            let (s, operator, f) = match self.tokens.next()? {
                Ok(tok) => tok,
                Err(e) => return Some(Err(e)),
            };

            let Some(factor) = self.factor() else {
                return Some(Err(miette!(
                    labels = vec![LabeledSpan::at(
                        s..=f,
                        "Missing factor expression in the right part of binary expression"
                    )],
                    "Invalid syntax"
                )));
            };

            let right = match factor {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };

            expr = Expr::Binary(operator, Box::new(expr), Box::new(right));
        }

        Some(Ok(expr))
    }

    fn factor(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let mut expr = match self.unary()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };
        loop {
            let Some(current) = self.tokens.peek() else {
                return Some(Ok(expr));
            };
            let Ok(next_tok) = current else {
                // Consume token if it's not a valid
                match self.tokens.next()? {
                    Ok(_) => unreachable!(),
                    Err(e) => return Some(Err(e)),
                }
            };

            if !matches!(next_tok, (_, Token::Star | Token::Slash, _)) {
                break;
            }

            // Consume operator
            let (s, operator, f) = match self.tokens.next()? {
                Ok(tok) => tok,
                Err(e) => return Some(Err(e)),
            };

            let Some(unary) = self.unary() else {
                return Some(Err(miette!(
                    labels = vec![LabeledSpan::at(
                        s..=f,
                        "Missing unary expression in the right part of binary expression"
                    )],
                    "Invalid syntax"
                )));
            };

            let right = match unary {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };

            expr = Expr::Binary(operator, Box::new(expr), Box::new(right));
        }

        Some(Ok(expr))
    }

    fn unary(&mut self) -> Option<miette::Result<Expr<'a>>> {
        match self.tokens.peek()? {
            Ok(tok) => {
                if let (_, Token::Bang | Token::Minus, _) = tok {
                    // Consume operator
                    let (s, operator, f) = match self.tokens.next()? {
                        Ok(tok) => tok,
                        Err(e) => return Some(Err(e)),
                    };
                    let Some(unary) = self.unary() else {
                        return Some(Err(miette!(
                            labels = vec![LabeledSpan::at(
                                s..=f,
                                format!("Dangling {} operator in unary expression", operator)
                            )],
                            "Invalid syntax"
                        )));
                    };

                    match unary {
                        Ok(r) => Some(Ok(Expr::Unary(operator, Box::new(r)))),
                        Err(e) => Some(Err(e)),
                    }
                } else {
                    self.primary()
                }
            }
            Err(_) => {
                // Consume token if it's not a valid unary operator
                match self.tokens.next()? {
                    Ok(_) => unreachable!(),
                    Err(e) => Some(Err(e)),
                }
            }
        }
    }

    fn primary(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let (start, tok, finish) = match self.tokens.next()? {
            Ok(t) => t,
            Err(e) => return Some(Err(e)),
        };
        match tok {
            Token::String(_) | Token::Number(_) | Token::False | Token::Nil | Token::True => {
                Some(Ok(Expr::Literal(Some(tok))))
            }
            Token::LeftParen => {
                let Some(expr) = self.expression() else {
                    return Some(Err(miette!(
                        labels = vec![LabeledSpan::at(
                            start..=finish,
                            "Expect expression after '('"
                        )],
                        "Expect expression after '('"
                    )));
                };
                match expr {
                    Ok(expr) => {
                        let Some(next) = self.tokens.next() else {
                            return Some(Err(miette!(
                                labels = vec![LabeledSpan::at(
                                    start..=finish,
                                    "Expect ')' after grouping expression that starts here."
                                )],
                                "Expect ')' after expression."
                            )));
                        };

                        if let Ok((_, Token::RightParen, _)) = next {
                            let g = Expr::Grouping(Box::new(expr));
                            Some(Ok(g))
                        } else {
                            Some(Err(miette!(
                                labels = vec![LabeledSpan::at(
                                    start..=finish,
                                    "Expect ')' after grouping expression that starts here."
                                )],
                                "Expect ')' after expression."
                            )))
                        }
                    }
                    Err(e) => Some(Err(e)),
                }
            }
            _ => Some(Err(miette!(
                labels = vec![LabeledSpan::at(
                    start..finish,
                    format!("Unexpected primary token: {tok}.")
                )],
                "Invalid syntax"
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test_case("1 + 2 * 3")]
    #[test_case("1 + 2 - 4")]
    #[test_case("1 * 2 * 6")]
    #[test_case("(1 + 2) * 5")]
    #[test_case("2 == 3")]
    #[test_case("(1 + 0 + 1)")]
    #[test_case("(1 == 2 == 17)")]
    #[test_case("(1 <= 2 > 10)")]
    #[test_case("--1")]
    #[test_case("!!2")]
    fn parser_tests(input: &str) {
        // Arrange
        let mut parser = Parser::new(input);

        // Act
        let actual = parser.parse();

        // Assert
        assert!(actual.is_some());
    }
}
