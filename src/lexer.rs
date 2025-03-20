use std::fmt::Display;

pub struct Lexer<'a> {
    whole: &'a str,
    rest: &'a str,
    byte: usize,
}

pub enum Token<'a> {
    String(&'a str),
}

impl<'a> Lexer<'a> {
    pub fn new(content: &'a str) -> Self {
        Self {
            whole: content,
            rest: content,
            byte: 0,
        }
    }

    pub fn scan_tokens(&self) -> Vec<Token> {
        vec![]
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::String(s) => write!(f, "STRING \"{s}\""),
        }
    }
}
