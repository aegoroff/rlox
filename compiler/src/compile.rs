use std::{cell::RefCell, rc::Rc};

use scanner::{Lexer, Token};

use crate::{
    CompileError,
    chunk::{Chunk, OpCode},
    value::LoxValue,
};

pub struct Parser<'a> {
    tokens: Lexer<'a>,
    current: Rc<RefCell<Token<'a>>>,
    previous: Rc<RefCell<Token<'a>>>,
}

enum Precedence {
    None = 0,
    Assignment = 1,
    Or = 2,
    And = 3,
    Equality = 4,
    Comparison = 5,
    Term = 6,
    Factor = 7,
    Unary = 8,
    Call = 9,
    Primary = 10,
}

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(content: &'a str) -> Self {
        Self {
            tokens: Lexer::new(content),
            current: Rc::new(RefCell::new(Token::Eof)),
            previous: Rc::new(RefCell::new(Token::Eof)),
        }
    }

    pub fn compile(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        self.advance()?;
        self.expression(chunk)?;
        self.end_compiler(chunk);
        Ok(())
    }

    fn expression(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        self.parse_precedence(Precedence::Assignment)?;
        Ok(())
    }

    fn unary(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        let previous = self.previous.clone();
        self.parse_precedence(Precedence::Unary)?;
        if let Token::Minus = *previous.borrow() {
            self.emit_opcode(chunk, OpCode::Negate);
        }
        Ok(())
    }

    fn grouping(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        self.expression(chunk)?;
        self.consume(&Token::RightParen)?;
        Ok(())
    }

    fn number(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        if let Token::Number(number) = *self.previous.borrow() {
            self.emit_constant(chunk, LoxValue::Number(number));
            Ok(())
        } else {
            Err(CompileError::CompileError(miette::miette!(
                "Unexpected token: '{}' Expected: 'number'",
                self.previous.borrow()
            )))
        }
    }

    fn parse_precedence(&mut self, _precedence: Precedence) -> crate::Result<()> {
        Ok(())
    }

    fn advance(&mut self) -> crate::Result<()> {
        match self.tokens.next() {
            Some(Ok((_, t, _))) => {
                self.previous = self.current.clone();
                self.current = Rc::new(RefCell::new(t));
                Ok(())
            }
            Some(Err(r)) => Err(CompileError::CompileError(r)),
            None => Err(CompileError::CompileError(miette::miette!(
                "Unexpected EOF"
            ))),
        }
    }

    fn consume(&mut self, token: &Token) -> crate::Result<()> {
        if *self.current.borrow() == *token {
            self.advance()?;
            Ok(())
        } else {
            Err(CompileError::CompileError(miette::miette!(
                "Unexpected token: '{}' Expected: '{token}'",
                self.current.borrow()
            )))
        }
    }

    fn emit_constant(&self, chunk: &mut Chunk, value: LoxValue) {
        chunk.write_constant(value, self.tokens.line);
    }

    fn emit_opcode(&self, chunk: &mut Chunk, opcode: OpCode) {
        chunk.write_code(opcode, self.tokens.line);
    }

    fn emit_byte(&self, chunk: &mut Chunk, byte: u8) {
        chunk.write_operand(byte, self.tokens.line);
    }

    fn emit_return(&self, chunk: &mut Chunk) {
        chunk.write_code(OpCode::Return, self.tokens.line);
    }

    fn end_compiler(&self, chunk: &mut Chunk) {
        self.emit_return(chunk);
    }
}
