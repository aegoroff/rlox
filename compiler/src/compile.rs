use std::{cell::RefCell, rc::Rc};

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
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

#[repr(u8)]
#[derive(FromPrimitive, Clone, Copy)]
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
        self.consume(&Token::Eof)?;
        self.end_compiler(chunk);
        #[cfg(feature = "printcode")]
        {
            chunk.disassembly("main");
        }
        Ok(())
    }

    fn expression(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        self.parse_precedence(chunk, Precedence::Assignment)?;
        Ok(())
    }

    fn binary(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        let previous = self.previous.clone();
        let precedence = Parser::get_precedence(&previous.borrow());
        let precedence = precedence as u8 + 1;
        let precedence = Precedence::from_u8(precedence).ok_or(CompileError::CompileError(
            miette::miette!("Invalid precedence: {}", precedence),
        ))?;
        self.parse_precedence(chunk, precedence)?;
        match *previous.borrow() {
            Token::Minus => {
                self.emit_opcode(chunk, OpCode::Subtract);
            }
            Token::Slash => {
                self.emit_opcode(chunk, OpCode::Divide);
            }
            Token::Star => {
                self.emit_opcode(chunk, OpCode::Multiply);
            }
            Token::Plus => {
                self.emit_opcode(chunk, OpCode::Add);
            }
            _ => (),
        }
        Ok(())
    }

    fn unary(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        let previous = self.previous.clone();
        self.parse_precedence(chunk, Precedence::Unary)?;
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

    fn parse_precedence(&mut self, chunk: &mut Chunk, precedence: Precedence) -> crate::Result<()> {
        self.advance()?;
        let previous = self.previous.clone();
        self.call_prefix(chunk, &previous.borrow())?;
        while Parser::get_precedence(&self.current.borrow()) as u8 >= precedence as u8 {
            self.advance()?;
            let previous = self.previous.clone();
            self.call_infix(chunk, &previous.borrow())?;
        }
        Ok(())
    }

    fn get_precedence(token: &Token) -> Precedence {
        match token {
            Token::Minus | Token::Plus => Precedence::Term,
            Token::Slash | Token::Star => Precedence::Factor,
            _ => Precedence::None,
        }
    }

    fn call_infix(&mut self, chunk: &mut Chunk, token: &Token) -> crate::Result<()> {
        match token {
            Token::Minus | Token::Plus | Token::Slash | Token::Star => self.binary(chunk),
            _ => Ok(()),
        }
    }

    fn call_prefix(&mut self, chunk: &mut Chunk, token: &Token) -> crate::Result<()> {
        match token {
            Token::Minus => self.unary(chunk),
            Token::LeftParen => self.grouping(chunk),
            Token::Number(_) => self.number(chunk),
            _ => Ok(()),
        }
    }

    fn advance(&mut self) -> crate::Result<()> {
        match self.tokens.next() {
            Some(Ok((_, t, _))) => {
                self.previous = self.current.clone();
                self.current = Rc::new(RefCell::new(t));
                Ok(())
            }
            Some(Err(r)) => Err(CompileError::CompileError(r)),
            None => {
                self.previous = self.current.clone();
                self.current = Rc::new(RefCell::new(Token::Eof));
                Ok(())
            }
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

    fn emit_return(&self, chunk: &mut Chunk) {
        chunk.write_code(OpCode::Return, self.tokens.line);
    }

    fn end_compiler(&self, chunk: &mut Chunk) {
        self.emit_return(chunk);
    }
}
