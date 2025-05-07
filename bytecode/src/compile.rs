use std::iter::Peekable;

use scanner::Lexer;

use crate::chunk::Chunk;

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

    pub fn compile(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        self.expression()?;
        Ok(())
    }

    fn expression(&mut self) -> crate::Result<()> {
        Ok(())
    }
}
