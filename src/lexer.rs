use std::{fmt::Display, str::CharIndices};

use miette::LabeledSpan;

pub struct Lexer<'a> {
    chars: std::iter::Peekable<CharIndices<'a>>,
    whole: &'a str,
}

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
        next: char,
        token: Token<'a>,
    ) -> Option<miette::Result<Token<'a>>> {
        if let Some((_, peek)) = self.chars.peek() {
            if next == *peek {
                // skip all char until EOL (end of line)
                for (_, c) in self.chars.by_ref() {
                    if c == '\n' {
                        break;
                    }
                }
                None
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
            labels = vec![LabeledSpan::at(start..problem_ix, "Problem is here")],
            "Unterminated string"
        );
        Some(Err(report))
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
                    if let Some(t) = self.skip_comment_or('/', Token::Slash) {
                        Some(t)
                    } else {
                        continue;
                    }
                }
                '"' => self.string(i),
                ' ' | '\t' | '\r' | '\n' => continue, // skip whitespaces
                _ => Some(Err(miette::miette!(
                    labels = vec![LabeledSpan::at(i..i + 1, "Problem is here"),],
                    "Unexpected char {} at {}",
                    current,
                    i
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
            Token::Number(n) => write!(f, "NUMBER \"{n}\""),
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
