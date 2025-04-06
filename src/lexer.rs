use std::{fmt::Display, ops::RangeInclusive, str::CharIndices};

use miette::LabeledSpan;

pub struct Lexer<'a> {
    chars: std::iter::Peekable<CharIndices<'a>>,
    whole: &'a str,
}

#[derive(PartialEq, Debug)]
pub enum Token<'a> {
    LeftParen(RangeInclusive<usize>),
    RightParen(RangeInclusive<usize>),
    LeftBrace(RangeInclusive<usize>),
    RightBrace(RangeInclusive<usize>),
    Comma(RangeInclusive<usize>),
    Dot(RangeInclusive<usize>),
    Minus(RangeInclusive<usize>),
    Plus(RangeInclusive<usize>),
    Semicolon(RangeInclusive<usize>),
    Slash(RangeInclusive<usize>),
    Star(RangeInclusive<usize>),
    Bang(RangeInclusive<usize>),
    BangEqual(RangeInclusive<usize>),
    Equal(RangeInclusive<usize>),
    EqualEqual(RangeInclusive<usize>),
    Greater(RangeInclusive<usize>),
    GreaterEqual(RangeInclusive<usize>),
    Less(RangeInclusive<usize>),
    LessEqual(RangeInclusive<usize>),
    Identifier(RangeInclusive<usize>, &'a str),
    String(RangeInclusive<usize>, &'a str),
    Number(RangeInclusive<usize>, f64),
    And(RangeInclusive<usize>),
    Class(RangeInclusive<usize>),
    Else(RangeInclusive<usize>),
    False(RangeInclusive<usize>),
    Fun(RangeInclusive<usize>),
    For(RangeInclusive<usize>),
    If(RangeInclusive<usize>),
    Nil(RangeInclusive<usize>),
    Or(RangeInclusive<usize>),
    Print(RangeInclusive<usize>),
    Return(RangeInclusive<usize>),
    Super(RangeInclusive<usize>),
    This(RangeInclusive<usize>),
    True(RangeInclusive<usize>),
    Var(RangeInclusive<usize>),
    While(RangeInclusive<usize>),
}

impl<'a> Lexer<'a> {
    #[must_use]
    pub fn new(content: &'a str) -> Self {
        Self {
            chars: content.char_indices().peekable(),
            whole: content,
        }
    }

    fn two_char_token(&mut self, next: char, success: Token<'a>, failure: Token<'a>) -> Token<'a> {
        if let Some((_, peek)) = self.chars.peek() {
            if next == *peek {
                self.chars.next(); // if match advance iterator
                success
            } else {
                failure
            }
        } else {
            failure
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

    fn string(&mut self, start: usize) -> miette::Result<Token<'a>> {
        let mut problem_ix = start;
        for (finish, next) in self.chars.by_ref() {
            problem_ix = finish;
            if next == '"' {
                return Ok(Token::String(
                    start..=finish,
                    &self.whole[start + 1..finish],
                ));
            }
        }
        let report = miette::miette!(
            labels = vec![LabeledSpan::at(start..=problem_ix, "Unterminated string")],
            "String parsing error"
        );
        Err(report)
    }

    fn number(&mut self, start: usize) -> miette::Result<Token<'a>> {
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
            Ok(value) => Ok(Token::Number(start..=finish, value)),
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

    fn identifier_or_keyword(&mut self, start: usize) -> Token<'a> {
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
        let range = start..=finish;
        let id = &self.whole[range.clone()];
        match id {
            "and" => Token::And(range),
            "class" => Token::Class(range),
            "else" => Token::Else(range),
            "false" => Token::False(range),
            "fun" => Token::Fun(range),
            "for" => Token::For(range),
            "if" => Token::If(range),
            "nil" => Token::Nil(range),
            "or" => Token::Or(range),
            "print" => Token::Print(range),
            "return" => Token::Return(range),
            "super" => Token::Super(range),
            "this" => Token::This(range),
            "true" => Token::True(range),
            "var" => Token::Var(range),
            "while" => Token::While(range),
            _ => Token::Identifier(range, id),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = miette::Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (i, current) = self.chars.next()?;
            let t = match current {
                '(' => Some(Ok(Token::LeftParen(i..=i))),
                ')' => Some(Ok(Token::RightParen(i..=i))),
                '{' => Some(Ok(Token::LeftBrace(i..=i))),
                '}' => Some(Ok(Token::RightBrace(i..=i))),
                ',' => Some(Ok(Token::Comma(i..=i))),
                '.' => Some(Ok(Token::Dot(i..=i))),
                '-' => Some(Ok(Token::Minus(i..=i))),
                '+' => Some(Ok(Token::Plus(i..=i))),
                ';' => Some(Ok(Token::Semicolon(i..=i))),
                '*' => Some(Ok(Token::Star(i..=i))),
                '=' => Some(Ok(self.two_char_token(
                    '=',
                    Token::EqualEqual(i..=(i + 1)),
                    Token::Equal(i..=i),
                ))),
                '>' => Some(Ok(self.two_char_token(
                    '=',
                    Token::GreaterEqual(i..=(i + 1)),
                    Token::Greater(i..=i),
                ))),
                '<' => Some(Ok(self.two_char_token(
                    '=',
                    Token::LessEqual(i..=(i + 1)),
                    Token::Less(i..=i),
                ))),
                '!' => Some(Ok(self.two_char_token(
                    '=',
                    Token::BangEqual(i..=(i + 1)),
                    Token::Bang(i..=i),
                ))),
                '/' => {
                    if let Some(t) = self.skip_comment_or(i, Token::Slash(i..=i)) {
                        Some(t)
                    } else {
                        continue;
                    }
                }
                '"' => Some(self.string(i)),
                'a'..='z' | 'A'..='Z' | '_' => Some(Ok(self.identifier_or_keyword(i))),
                '0'..='9' => Some(self.number(i)),
                ' ' | '\t' | '\r' | '\n' => continue, // skip whitespaces
                _ => Some(Err(miette::miette!(
                    labels = vec![LabeledSpan::at(
                        i..=i,
                        format!("Unexpected char {current} at {i}")
                    ),],
                    "Unexpected char"
                ))),
            };
            return t;
        }
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::String(_, s) => write!(f, "STRING \"{s}\""),
            Token::LeftParen(_) => write!(f, "("),
            Token::RightParen(_) => write!(f, ")"),
            Token::LeftBrace(_) => write!(f, "{{"),
            Token::RightBrace(_) => write!(f, "}}"),
            Token::Comma(_) => write!(f, ","),
            Token::Dot(_) => write!(f, "."),
            Token::Minus(_) => write!(f, "-"),
            Token::Plus(_) => write!(f, "+"),
            Token::Semicolon(_) => write!(f, ";"),
            Token::Slash(_) => write!(f, "/"),
            Token::Star(_) => write!(f, "*"),
            Token::Bang(_) => write!(f, "!"),
            Token::BangEqual(_) => write!(f, "!="),
            Token::Equal(_) => write!(f, "="),
            Token::EqualEqual(_) => write!(f, "=="),
            Token::Greater(_) => write!(f, ">"),
            Token::GreaterEqual(_) => write!(f, ">="),
            Token::Less(_) => write!(f, "<"),
            Token::LessEqual(_) => write!(f, "<="),
            Token::Identifier(_, s) => write!(f, "IDENTIFIER \"{s}\""),
            Token::Number(_, n) => write!(f, "NUMBER {n}"),
            Token::And(_) => write!(f, "AND"),
            Token::Class(_) => write!(f, "CLASS"),
            Token::Else(_) => write!(f, "ELSE"),
            Token::False(_) => write!(f, "FALSE"),
            Token::Fun(_) => write!(f, "FUN"),
            Token::For(_) => write!(f, "FOR"),
            Token::If(_) => write!(f, "IF"),
            Token::Nil(_) => write!(f, "NIL"),
            Token::Or(_) => write!(f, "OR"),
            Token::Print(_) => write!(f, "PRINT"),
            Token::Return(_) => write!(f, "RETURN"),
            Token::Super(_) => write!(f, "SUPER"),
            Token::This(_) => write!(f, "THIS"),
            Token::True(_) => write!(f, "TRUE"),
            Token::Var(_) => write!(f, "VAR"),
            Token::While(_) => write!(f, "WHILE"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test_case("(", vec![Token::LeftParen(0..=0)] ; "Left paren")]
    #[test_case("!!=", vec![Token::Bang(0..=0), Token::BangEqual(1..=2)] ; "Bang tests")]
    #[test_case(">>=", vec![Token::Greater(0..=0), Token::GreaterEqual(1..=2)] ; "Greater tests")]
    #[test_case("<<=", vec![Token::Less(0..=0), Token::LessEqual(1..=2)] ; "Less tests")]
    #[test_case("===", vec![Token::EqualEqual(0..=1), Token::Equal(2..=2)] ; "Equal tests")]
    #[test_case(")", vec![Token::RightParen(0..=0)] ; "Right paren")]
    #[test_case("()", vec![Token::LeftParen(0..=0), Token::RightParen(1..=1)] ; "Both paren")]
    #[test_case("var x = 2+3;", vec![Token::Var(0..=2), Token::Identifier(4..=4, "x"), Token::Equal(6..=6), Token::Number(8..=8, 2.0), Token::Plus(9..=9), Token::Number(10..=10, 3.0), Token::Semicolon(11..=11)] ; "Expression")]
    #[test_case(r#""str""#, vec![Token::String(0..=4, "str")] ; "String")]
    #[test_case(r#""str" // Comment"#, vec![Token::String(0..=4, "str")] ; "String + Comment")]
    #[test_case(r#"1.2.3 4"#, vec![Token::Number(0..=2, 1.2), Token::Dot(3..=3), Token::Number(4..=4, 3.0), Token::Number(6..=6, 4.0)] ; "Bad number")]
    #[test_case(r#"id"#, vec![Token::Identifier(0..=1, "id")] ; "Single identifier")]
    #[test_case(r#"var"#, vec![Token::Var(0..=2)] ; "Single var")]
    #[test_case(r#"1.2"#, vec![Token::Number(0..=2, 1.2)] ; "Single number")]
    #[test_case(r#"3 4"#, vec![Token::Number(0..=0, 3.0), Token::Number(2..=2, 4.0)] ; "Couple nums separated space")]
    #[test_case(r#"3 45"#, vec![Token::Number(0..=0, 3.0), Token::Number(2..=3, 45.0)] ; "Couple nums separated space second above 10")]
    #[test_case(r#"123."#, vec![Token::Number(0..=2, 123.0), Token::Dot(3..=3)] ; "Number with trailing dot")]
    #[test_case(r#" .456 123. "#, vec![Token::Dot(1..=1), Token::Number(2..=4, 456.0), Token::Number(6..=8, 123.0),
        Token::Dot(9..=9)] ; "Number with starting dot and number with trailing dot")]
    fn positive_tests(input: &str, expected: Vec<Token>) {
        // Arrange
        let lexer = Lexer::new(input);

        // Act
        let actual: Vec<Token> = lexer.into_iter().filter_map(|t| t.ok()).collect();

        // Assert
        assert_eq!(expected, actual);
    }
}
