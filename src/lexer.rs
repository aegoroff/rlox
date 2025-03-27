use std::{fmt::Display, str::CharIndices};

use miette::LabeledSpan;

pub struct Lexer<'a> {
    chars: std::iter::Peekable<CharIndices<'a>>,
    whole: &'a str,
}

#[derive(PartialEq, Debug)]
pub enum Token<'a> {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier(&'a str),
    String(&'a str),
    Number(f64),
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl<'a> Lexer<'a> {
    pub fn new(content: &'a str) -> Self {
        Self {
            chars: content.char_indices().peekable(),
            whole: content,
        }
    }

    fn two_char_token(
        &mut self,
        next: char,
        success: Token<'a>,
        failure: Token<'a>,
    ) -> Option<miette::Result<Token<'a>>> {
        if let Some((_, peek)) = self.chars.peek() {
            if next == *peek {
                self.chars.next(); // if match advance iterator
                Some(Ok(success))
            } else {
                Some(Ok(failure))
            }
        } else {
            Some(Ok(failure))
        }
    }

    fn skip_comment_or(
        &mut self,
        start: usize,
        token: Token<'a>,
    ) -> Option<miette::Result<Token<'a>>> {
        if let Some((_, peek)) = self.chars.peek() {
            if '/' == *peek {
                // skip all char until EOL (end of line)
                for (_, c) in self.chars.by_ref() {
                    if c == '\n' {
                        break;
                    }
                }
                None
            } else if '*' == *peek {
                // multiline comments
                // skip all char until next star
                let mut prev = *peek;
                for (_, c) in self.chars.by_ref() {
                    if c == '/' && prev == '*' {
                        // correct multiline comment termination
                        return None;
                    }
                    prev = c;
                }
                let problem_ix = if let Some((f, _)) = self.chars.peek() {
                    *f
                } else {
                    self.whole.len() - 1
                };
                let report = miette::miette!(
                    labels = vec![LabeledSpan::at(
                        start..=problem_ix,
                        "Unterminated multiline comment"
                    )],
                    "Comment syntax error"
                );
                Some(Err(report))
            } else {
                Some(Ok(token))
            }
        } else {
            Some(Ok(token))
        }
    }

    fn string(&mut self, start: usize) -> Option<miette::Result<Token<'a>>> {
        for (finish, next) in self.chars.by_ref() {
            if next == '"' {
                return Some(Ok(Token::String(&self.whole[start + 1..finish])));
            }
        }
        let problem_ix = if let Some((f, _)) = self.chars.peek() {
            *f
        } else {
            self.whole.len() - 1
        };
        let report = miette::miette!(
            labels = vec![LabeledSpan::at(start..=problem_ix, "Unterminated string")],
            "String parsing error"
        );
        Some(Err(report))
    }

    fn number(&mut self, start: usize) -> Option<miette::Result<Token<'a>>> {
        let mut with_fractional = false;
        let mut finish = start;

        while let Some((i, next)) = self.chars.peek() {
            finish = *i;

            match *next {
                '0'..='9' => {
                    self.chars.next();
                    continue;
                }
                '.' => {
                    if with_fractional {
                        let report = miette::miette!(
                            labels = vec![LabeledSpan::at(finish..=finish, "Dot not allowed here")],
                            "Parsing fractional f64 failed"
                        );
                        return Some(Err(report));
                    }
                    self.chars.next();
                    with_fractional = true;
                    continue;
                }
                'a'..='z' | 'A'..='Z' | '"' => {
                    let report = miette::miette!(
                        labels = vec![LabeledSpan::at(start..=finish, "Invalid number")],
                        "Parsing fractional f64 failed"
                    );
                    return Some(Err(report));
                }
                _ => {
                    finish -= 1;
                    break;
                }
            }
        }
        let range = start..=finish;
        let result = self.whole[range.clone()].parse().map_err(|e| {
            miette::miette!(
                labels = vec![LabeledSpan::at(range, format!("Problem is here: {e}"))],
                "Parsing fractional f64 failed"
            )
        });
        match result {
            Ok(value) => Some(Ok(Token::Number(value))),
            Err(e) => Some(Err(e)),
        }
    }

    fn identifier_or_keyword(&mut self, start: usize) -> Option<miette::Result<Token<'a>>> {
        let mut finish = start;
        while let Some((i, next)) = self.chars.peek() {
            finish = *i;
            match *next {
                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                    self.chars.next();
                    continue;
                }
                _ => {
                    finish -= 1;
                    break;
                }
            }
        }
        let id = &self.whole[start..=finish];
        match id {
            "and" => Some(Ok(Token::And)),
            "class" => Some(Ok(Token::Class)),
            "else" => Some(Ok(Token::Else)),
            "false" => Some(Ok(Token::False)),
            "fun" => Some(Ok(Token::Fun)),
            "for" => Some(Ok(Token::For)),
            "if" => Some(Ok(Token::If)),
            "nil" => Some(Ok(Token::Nil)),
            "or" => Some(Ok(Token::Or)),
            "print" => Some(Ok(Token::Print)),
            "return" => Some(Ok(Token::Return)),
            "super" => Some(Ok(Token::Super)),
            "this" => Some(Ok(Token::This)),
            "true" => Some(Ok(Token::True)),
            "var" => Some(Ok(Token::Var)),
            "while" => Some(Ok(Token::While)),
            _ => Some(Ok(Token::Identifier(id))),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = miette::Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (i, current) = self.chars.next()?;
            return match current {
                '(' => Some(Ok(Token::LeftParen)),
                ')' => Some(Ok(Token::RightParen)),
                '{' => Some(Ok(Token::LeftBrace)),
                '}' => Some(Ok(Token::RightBrace)),
                ',' => Some(Ok(Token::Comma)),
                '.' => Some(Ok(Token::Dot)),
                '-' => Some(Ok(Token::Minus)),
                '+' => Some(Ok(Token::Plus)),
                ';' => Some(Ok(Token::Semicolon)),
                '*' => Some(Ok(Token::Star)),
                '=' => self.two_char_token('=', Token::EqualEqual, Token::Equal),
                '>' => self.two_char_token('=', Token::GreaterEqual, Token::Greater),
                '<' => self.two_char_token('=', Token::LessEqual, Token::Less),
                '!' => self.two_char_token('=', Token::BangEqual, Token::BangEqual),
                '/' => {
                    if let Some(t) = self.skip_comment_or(i, Token::Slash) {
                        Some(t)
                    } else {
                        continue;
                    }
                }
                '"' => self.string(i),
                'a'..='z' | 'A'..='Z' | '_' => self.identifier_or_keyword(i),
                '0'..='9' => self.number(i),
                ' ' | '\t' | '\r' | '\n' => continue, // skip whitespaces
                _ => Some(Err(miette::miette!(
                    labels = vec![LabeledSpan::at(
                        i..i + 1,
                        format!("Unexpected char {current} at {i}")
                    ),],
                    "Unexpected char"
                ))),
            };
        }
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::String(s) => write!(f, "STRING \"{s}\""),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Minus => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Semicolon => write!(f, ";"),
            Token::Slash => write!(f, "/"),
            Token::Star => write!(f, "*"),
            Token::Bang => write!(f, "!"),
            Token::BangEqual => write!(f, "!="),
            Token::Equal => write!(f, "="),
            Token::EqualEqual => write!(f, "=="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Identifier(s) => write!(f, "IDENTIFIER \"{s}\""),
            Token::Number(n) => write!(f, "NUMBER {n}"),
            Token::And => write!(f, "AND"),
            Token::Class => write!(f, "CLASS"),
            Token::Else => write!(f, "ELSE"),
            Token::False => write!(f, "FALSE"),
            Token::Fun => write!(f, "FUN"),
            Token::For => write!(f, "FOR"),
            Token::If => write!(f, "IF"),
            Token::Nil => write!(f, "NIL"),
            Token::Or => write!(f, "OR"),
            Token::Print => write!(f, "PRINT"),
            Token::Return => write!(f, "RETURN"),
            Token::Super => write!(f, "SUPER"),
            Token::This => write!(f, "THIS"),
            Token::True => write!(f, "TRUE"),
            Token::Var => write!(f, "VAR"),
            Token::While => write!(f, "WHILE"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test_case("(", vec![Token::LeftParen] ; "Left paren")]
    #[test_case(")", vec![Token::RightParen] ; "Right paren")]
    #[test_case("()", vec![Token::LeftParen, Token::RightParen] ; "Both paren")]
    #[test_case("var x = 2+3;", vec![Token::Var, Token::Identifier("x"), Token::Equal, Token::Number(2.0), Token::Plus, Token::Number(3.0), Token::Semicolon] ; "Expression")]
    #[test_case(r#""str""#, vec![Token::String("str")] ; "String")]
    #[test_case(r#""str" // Comment"#, vec![Token::String("str")] ; "String + Comment")]
    #[test_case(r#"1.2.3 4"#, vec![Token::Dot, Token::Number(3.0), Token::Number(4.0)] ; "Bad number")]
    #[test_case(r#"id"#, vec![Token::Identifier("id")] ; "Single identifier")]
    #[test_case(r#"var"#, vec![Token::Var] ; "Single var")]
    #[test_case(r#"1.2"#, vec![Token::Number(1.2)] ; "Single number")]
    fn positive_tests(input: &str, expected: Vec<Token>) {
        // Arrange
        let lexer = Lexer::new(input);

        // Act
        let actual: Vec<Token> = lexer.into_iter().filter_map(|t| t.ok()).collect();

        // Assert
        assert_eq!(expected, actual);
    }
}
