use std::iter::Peekable;

use miette::{LabeledSpan, miette};

use crate::{
    ast::{Expr, ExprKind, Stmt, StmtKind},
    lexer::{Lexer, Token},
};

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

    pub fn parse(&mut self) -> Option<Vec<miette::Result<Stmt<'a>>>> {
        let mut result = vec![];

        while let Some(stmt) = self.statement() {
            result.push(stmt);
        }
        Some(result)
    }

    fn statement(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        let current = self.tokens.peek()?;
        if let Ok((_, Token::Print, _)) = current {
            self.print_statement()
        } else {
            self.expr_statement()
        }
    }

    fn print_statement(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        self.tokens.next(); // consume print token TODO: include print start position into stmt location
        match self.semicolon_terminated_expression()? {
            Ok(expr) => {
                let location = expr.location.clone();
                let kind = StmtKind::Print(Box::new(expr));
                Some(Ok(Stmt { kind, location }))
            }
            Err(e) => Some(Err(e)),
        }
    }

    fn expr_statement(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        match self.semicolon_terminated_expression()? {
            Ok(expr) => {
                let location = expr.location.clone();
                let kind = StmtKind::Expression(Box::new(expr));
                Some(Ok(Stmt { kind, location }))
            }
            Err(e) => Some(Err(e)),
        }
    }

    fn semicolon_terminated_expression(&mut self) -> Option<miette::Result<Expr<'a>>> {
        match self.expression()? {
            Ok(expr) => {
                let location = expr.location.clone();
                let semicolon = self.tokens.peek()?;
                if let Ok((_, Token::Semicolon, _)) = semicolon {
                    self.tokens.next(); // consume semicolon token
                } else {
                    let s = location.end();
                    let f = location.end();
                    return Some(Err(miette!(
                        labels = vec![LabeledSpan::at(*s..=*f, "Semicolon expected here")],
                        "Missing semicolon"
                    )));
                }

                Some(Ok(expr))
            }
            Err(e) => Some(Err(e)),
        }
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

            let start = *expr.location.start();
            let end = *right.location.end() - 1;
            let kind = ExprKind::Binary(operator, Box::new(expr), Box::new(right));

            expr = Expr {
                kind,
                location: start..=end,
            };
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

            let start = *expr.location.start();
            let end = *right.location.end();
            let kind = ExprKind::Binary(operator, Box::new(expr), Box::new(right));

            expr = Expr {
                kind,
                location: start..=end,
            };
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

            let start = *expr.location.start();
            let end = *right.location.end();
            let kind = ExprKind::Binary(operator, Box::new(expr), Box::new(right));

            expr = Expr {
                kind,
                location: start..=end,
            };
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

            let start = *expr.location.start();
            let end = *right.location.end();
            let kind = ExprKind::Binary(operator, Box::new(expr), Box::new(right));

            expr = Expr {
                kind,
                location: start..=end,
            };
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
                                format!("Dangling {operator} operator in unary expression")
                            )],
                            "Invalid syntax"
                        )));
                    };

                    match unary {
                        Ok(r) => {
                            let end = *r.location.end() - 1;
                            let kind = ExprKind::Unary(operator, Box::new(r));
                            Some(Ok(Expr {
                                kind,
                                location: s..=end,
                            }))
                        }
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
                let end = finish - 1;
                Some(Ok(Expr {
                    kind: ExprKind::Literal(Some(tok)),
                    location: start..=end,
                }))
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

                        if let Ok((_, Token::RightParen, finish)) = next {
                            let end = finish - 1;
                            Some(Ok(Expr {
                                kind: ExprKind::Grouping(Box::new(expr)),
                                location: start..=end,
                            }))
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
    use crate::ast::{Evaluator, LoxValue};

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

    #[test_case("--1", 1.0)]
    #[test_case("1 - 1", 0.0)]
    #[test_case("1 - 2", -1.0)]
    #[test_case("2 - 1", 1.0)]
    #[test_case("2 + 3", 5.0)]
    #[test_case("2 + 3 - 1", 4.0)]
    #[test_case("3 + 3 / 3", 4.0)]
    #[test_case("(3 + 3) / 3", 2.0)]
    #[test_case("4 / 2", 2.0)]
    #[test_case("4 / 1", 4.0)]
    #[test_case("5 / -1", -5.0)]
    #[test_case("(5 - (3-1)) + -1", 2.0)]
    #[test_case("(5 - (3-1)) * -1", -3.0)]
    #[test_case("((5 - (3-1)) * -2) / 4", -1.5)]
    #[test_case("((5 - (3-1) + 3) * -2) / 4", -3.0)]
    fn iterpreter_numeric_positive_tests(input: &str, expected: f64) {
        // Arrange
        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap().unwrap();
        let eval = Evaluator {};

        // Act
        let actual = eval.evaluate(&expr);

        // Assert
        assert!(actual.is_ok());
        let actual = actual.unwrap();
        if let LoxValue::Number(actual) = actual {
            assert_eq!(actual, expected)
        } else {
            todo!()
        }
    }

    #[test_case("(\"a\" + \"b\") + \"c\"", "abc")]
    #[test_case("(\"a\" + 4) + \"c\"", "a4c")]
    #[test_case("(4 + \"a\") + \"c\"", "4ac")]
    #[test_case("(true + \"a\") + \"c\"", "trueac")]
    #[test_case("(nil + \"a\") + \"c\"", "ac")]
    fn iterpreter_string_positive_tests(input: &str, expected: &str) {
        // Arrange
        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap().unwrap();
        let eval = Evaluator {};

        // Act
        let actual = eval.evaluate(&expr);

        // Assert
        assert!(actual.is_ok());
        let actual = actual.unwrap();
        if let LoxValue::String(actual) = actual {
            assert_eq!(actual, expected)
        } else {
            todo!()
        }
    }

    #[test_case("(\"a\" == \"b\")", false)]
    #[test_case("(\"a\" != \"c\")", true)]
    #[test_case("(\"ab\" == \"ab\")", true)]
    #[test_case("(\"aa\" > \"bb\")", false)]
    #[test_case("(\"bb\" > \"aa\")", true)]
    #[test_case("(\"bba\" >= \"aaa\")", true)]
    #[test_case("(\"bba\" <= \"aaa\")", false)]
    #[test_case("1 == 2", false)]
    #[test_case("2 == 2", true)]
    #[test_case("3 >= 3", true)]
    #[test_case("3 >= 2", true)]
    #[test_case("3 <= 1", false)]
    #[test_case("(3 - 1) * 200 <= 1", false)]
    #[test_case("3 > 1 == true", true)]
    #[test_case("20 <= 20", true)]
    #[test_case("40 <= 50", true)]
    #[test_case("nil <= false", true ; "nil lrs less or equal")]
    #[test_case("nil < false", false ; "nil lrs less")]
    #[test_case("nil == false", true ; "nil lrs equal")]
    #[test_case("!nil", true ; "not nil")]
    fn iterpreter_predicates_tests(input: &str, expected: bool) {
        // Arrange
        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap().unwrap();
        let eval = Evaluator {};

        // Act
        let actual = eval.evaluate(&expr);

        // Assert
        assert!(actual.is_ok());
        let actual = actual.unwrap();
        if let LoxValue::Bool(actual) = actual {
            assert_eq!(actual, expected)
        } else {
            todo!()
        }
    }
}
