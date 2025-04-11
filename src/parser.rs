use std::iter::Peekable;

use miette::{LabeledSpan, miette};

use crate::{
    ast::Expr,
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
