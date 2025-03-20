use std::fmt::Display;

pub struct Scanner {
    content: String,
}

pub enum Token {}

impl Scanner {
    pub fn new(content: String) -> Self {
        Self { content }
    }

    pub fn scan_tokens(&self) -> Vec<Token> {
        vec![]
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}