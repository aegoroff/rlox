use scanner::{Lexer, Token};

use crate::{CompileError, chunk::Chunk};

pub struct Parser<'a> {
    tokens: Lexer<'a>,
}

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(content: &'a str) -> Self {
        Self {
            tokens: Lexer::new(content),
        }
    }

    pub fn compile(&mut self, _chunk: &mut Chunk) -> crate::Result<()> {
        let _current = self.advance()?;
        self.expression()?;
        Ok(())
    }

    fn expression(&mut self) -> crate::Result<()> {
        Ok(())
    }

    fn advance(&mut self) -> crate::Result<Token<'a>> {
        match self.tokens.next() {
            Some(Ok((_, t, _))) => Ok(t),
            Some(Err(r)) => Err(CompileError::CompileError(r)),
            None => Err(CompileError::CompileError(miette::miette!(
                "Unexpected EOF"
            ))),
        }
    }

    fn consume(&mut self, token: &Token) -> crate::Result<Token<'a>> {
        let current = self.advance()?;
        if &current == token {
            Ok(current)
        } else {
            Err(CompileError::CompileError(miette::miette!(
                "Unexpected token: '{current}' Expected: '{token}'"
            )))
        }
    }

    fn emit_byte(chunk: &mut Chunk, byte: u8, line: usize) {
        chunk.write_operand(byte, line);
    }
}
