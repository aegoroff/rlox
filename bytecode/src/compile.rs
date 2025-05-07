use scanner::lexer::{Lexer, Token};

use crate::{CompileError, chunk::Chunk};

#[derive(Default)]
pub struct Parser<'a> {
    current: Option<Token<'a>>,
    prev: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            current: None,
            prev: None,
        }
    }

    pub fn compile(&mut self, content: &'a str, chunk: &mut Chunk) -> crate::Result<()> {
        let lex = Lexer::new(content).peekable();
        for t in lex {
            let (_, curr, _) = t.map_err(CompileError::CompileError)?;
            self.current = Some(curr);
        }
        Ok(())
    }
}
