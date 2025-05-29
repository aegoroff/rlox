use std::{fmt::Display, str::CharIndices};

use miette::LabeledSpan;

pub type Spanned<Tok, Loc> = miette::Result<(Loc, Tok, Loc)>;

pub struct Lexer<'a> {
    chars: std::iter::Peekable<CharIndices<'a>>,
    whole: &'a str,
    pub line: usize,
    pub line_begin: usize,
    pub begin: usize,
    pub end: usize,
}

pub const THIS: &str = "this";
pub const SUPER: &str = "super";
pub const INIT: &str = "init";

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
    Eof,
}

impl std::hash::Hash for Token<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl<'a> Lexer<'a> {
    #[must_use]
    pub fn new(content: &'a str) -> Self {
        Self {
            chars: content.char_indices().peekable(),
            whole: content,
            line: 1,
            line_begin: 0,
            begin: 0,
            end: 0,
        }
    }

    fn two_char_token(
        &mut self,
        start: usize,
        next: char,
        success: Token<'a>,
        failure: Token<'a>,
    ) -> (usize, Token<'a>, usize) {
        if let Some((i, peek)) = self.chars.peek() {
            if next == *peek {
                let start = *i;
                self.chars.next(); // if match advance iterator
                (start, success, start + 1)
            } else {
                (*i, failure, *i)
            }
        } else {
            (start, failure, start)
        }
    }

    fn skip_comment_or(
        &mut self,
        start: usize,
        token: Token<'a>,
    ) -> Option<Spanned<Token<'a>, usize>> {
        if let Some((_, peek)) = self.chars.peek() {
            if '/' == *peek {
                // skip all char until EOL (end of line)
                for (_, c) in self.chars.by_ref() {
                    if c == '\n' {
                        self.line += 1;
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
                Some(Ok((start, token, start)))
            }
        } else {
            Some(Ok((start, token, start)))
        }
    }

    fn string(&mut self, start: usize) -> Spanned<Token<'a>, usize> {
        let mut problem_ix = start;
        for (finish, next) in self.chars.by_ref() {
            problem_ix = finish;
            if next == '"' {
                let tok = Token::String(&self.whole[start + 1..finish]);
                return Ok((start + 1, tok, finish));
            }
        }
        let report = miette::miette!(
            labels = vec![LabeledSpan::at(start..=problem_ix, "Unterminated string")],
            "String parsing error"
        );
        Err(report)
    }

    fn number(&mut self, start: usize) -> Spanned<Token<'a>, usize> {
        let mut finish = self.skip_digits(start);

        if let Some((i, next)) = self.chars.peek() {
            if *next == '.' {
                let next_ix = *i + 1;
                if let Some('0'..='9') = self.whole.chars().nth(next_ix) {
                    self.chars.next(); // consume dot
                    finish = self.skip_digits(next_ix);
                }
            }
        }
        let result = self.whole[start..=finish].parse().map_err(|e| {
            miette::miette!(
                labels = vec![LabeledSpan::at(
                    start..=finish,
                    format!("Problem is here: {e}")
                )],
                "Parsing fractional f64 failed"
            )
        });
        match result {
            Ok(value) => Ok((start, Token::Number(value), finish + 1)),
            Err(e) => Err(e),
        }
    }

    fn skip_digits(&mut self, start: usize) -> usize {
        let mut finish = start;
        while let Some((i, next)) = self.chars.peek() {
            match *next {
                '0'..='9' => {
                    finish = *i;
                    self.chars.next();
                }
                _ => break,
            }
        }
        finish
    }

    fn identifier_or_keyword(&mut self, start: usize) -> (usize, Token<'a>, usize) {
        let mut finish = start;
        while let Some((i, next)) = self.chars.peek() {
            finish = *i;
            match *next {
                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                    self.chars.next();
                }
                _ => {
                    finish -= 1;
                    break;
                }
            }
        }
        let id = &self.whole[start..=finish];
        let tok = match id {
            "and" => Token::And,
            "class" => Token::Class,
            "else" => Token::Else,
            "false" => Token::False,
            "fun" => Token::Fun,
            "for" => Token::For,
            "if" => Token::If,
            "nil" => Token::Nil,
            "or" => Token::Or,
            "print" => Token::Print,
            "return" => Token::Return,
            SUPER => Token::Super,
            THIS => Token::This,
            "true" => Token::True,
            "var" => Token::Var,
            "while" => Token::While,
            _ => Token::Identifier(id),
        };
        (start, tok, finish + 1)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Spanned<Token<'a>, usize>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (i, current) = self.chars.next()?;
            let t = match current {
                '(' => Some(Ok((i, Token::LeftParen, i))),
                ')' => Some(Ok((i, Token::RightParen, i))),
                '{' => Some(Ok((i, Token::LeftBrace, i))),
                '}' => Some(Ok((i, Token::RightBrace, i))),
                ',' => Some(Ok((i, Token::Comma, i))),
                '.' => Some(Ok((i, Token::Dot, i))),
                '-' => Some(Ok((i, Token::Minus, i))),
                '+' => Some(Ok((i, Token::Plus, i))),
                ';' => Some(Ok((i, Token::Semicolon, i))),
                '*' => Some(Ok((i, Token::Star, i))),
                '=' => Some(Ok(self.two_char_token(
                    i,
                    '=',
                    Token::EqualEqual,
                    Token::Equal,
                ))),
                '>' => Some(Ok(self.two_char_token(
                    i,
                    '=',
                    Token::GreaterEqual,
                    Token::Greater,
                ))),
                '<' => Some(Ok(self.two_char_token(
                    i,
                    '=',
                    Token::LessEqual,
                    Token::Less,
                ))),
                '!' => Some(Ok(self.two_char_token(
                    i,
                    '=',
                    Token::BangEqual,
                    Token::Bang,
                ))),
                '/' => {
                    if let Some(t) = self.skip_comment_or(i, Token::Slash) {
                        Some(t)
                    } else {
                        continue;
                    }
                }
                '"' => Some(self.string(i)),
                'a'..='z' | 'A'..='Z' | '_' => Some(Ok(self.identifier_or_keyword(i))),
                '0'..='9' => Some(self.number(i)),
                ' ' | '\t' | '\r' => continue, // skip whitespaces
                '\n' => {
                    self.line += 1;
                    self.line_begin = i;
                    continue; // skip line break
                }
                _ => Some(Err(miette::miette!(
                    labels = vec![LabeledSpan::at(
                        i..=i,
                        format!("Unexpected char {current} at {i}")
                    ),],
                    "Unexpected char"
                ))),
            };
            if let Some(Ok((begin, _, end))) = t {
                self.begin = begin;
                self.end = end;
            }
            return t;
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
            Token::Eof => write!(f, "EOF"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test_case("(", vec![Token::LeftParen] ; "Left paren")]
    #[test_case("!!=", vec![Token::Bang, Token::BangEqual] ; "Bang tests")]
    #[test_case(">>=", vec![Token::Greater, Token::GreaterEqual] ; "Greater tests")]
    #[test_case("<<=", vec![Token::Less, Token::LessEqual] ; "Less tests")]
    #[test_case("===", vec![Token::EqualEqual, Token::Equal] ; "Equal tests")]
    #[test_case(")", vec![Token::RightParen] ; "Right paren")]
    #[test_case("()", vec![Token::LeftParen, Token::RightParen] ; "Both paren")]
    #[test_case("var x = 2+3;", vec![Token::Var, Token::Identifier("x"), Token::Equal, Token::Number(2.0), Token::Plus, Token::Number(3.0), Token::Semicolon] ; "Expression")]
    #[test_case(r#""str""#, vec![Token::String("str")] ; "String")]
    #[test_case(r#""str" // Comment"#, vec![Token::String("str")] ; "String + Comment")]
    #[test_case(r#"1.2.3 4"#, vec![Token::Number(1.2), Token::Dot, Token::Number(3.0), Token::Number(4.0)] ; "Bad number")]
    #[test_case(r#"id"#, vec![Token::Identifier("id")] ; "Single identifier")]
    #[test_case(r#"var"#, vec![Token::Var] ; "Single var")]
    #[test_case(r#"1.2"#, vec![Token::Number(1.2)] ; "Single number")]
    #[test_case(r#"3 4"#, vec![Token::Number(3.0), Token::Number(4.0)] ; "Couple nums separated space")]
    #[test_case(r#"3 45"#, vec![Token::Number(3.0), Token::Number(45.0)] ; "Couple nums separated space second above 10")]
    #[test_case(r#"123."#, vec![Token::Number(123.0), Token::Dot] ; "Number with trailing dot")]
    #[test_case(r#" .456 123. "#, vec![Token::Dot, Token::Number(456.0), Token::Number(123.0),
        Token::Dot] ; "Number with starting dot and number with trailing dot")]
    fn positive_tests(input: &str, expected: Vec<Token>) {
        // Arrange
        let lexer = Lexer::new(input);

        // Act
        let actual: Vec<Token> = lexer
            .into_iter()
            .filter_map(|t| t.ok())
            .map(|(_, t, _)| t)
            .collect();

        // Assert
        assert_eq!(expected, actual);
    }
}
