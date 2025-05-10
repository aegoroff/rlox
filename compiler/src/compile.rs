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
        while !self.matches(&Token::Eof)? {
            self.declaration(chunk)?;
        }
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

    fn declaration(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        if self.matches(&Token::Var)? {
            self.var_declaration(chunk)
        } else {
            self.statement(chunk)
        }
    }

    fn var_declaration(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        let global = self.parse_variable(chunk)?;
        //print!("var: '{global}' ");
        if self.matches(&Token::Equal)? {
            //println!(" Init by expression");
            self.expression(chunk)?;
        } else {
            //println!(" Init by nil");
            self.emit_opcode(chunk, OpCode::Nil);
        }
        self.consume(&Token::Semicolon)?;
        self.define_variable(chunk, global)?;
        Ok(())
    }

    fn parse_variable(&mut self, chunk: &mut Chunk) -> crate::Result<usize> {
        let Token::Identifier(id) = *self.current.borrow() else {
            return Err(CompileError::CompileError(miette::miette!(
                "Identifier expected"
            )));
        };
        self.advance()?;
        let constant = self.identifier_constant(chunk, id)?;
        //print!("var: '{id}' constant ix: '{constant}' ");
        Ok(constant)
    }

    fn identifier_constant(&mut self, chunk: &mut Chunk, id: &str) -> crate::Result<usize> {
        let constant = self.make_constant(chunk, LoxValue::String(id.to_string()));
        Ok(constant)
    }

    fn define_variable(&mut self, chunk: &mut Chunk, global: usize) -> crate::Result<()> {
        if global > 255 {
            self.emit_opcode(chunk, OpCode::DefineGlobalLong);
        } else {
            self.emit_opcode(chunk, OpCode::DefineGlobal);
        }
        self.emit_operand(chunk, global);

        Ok(())
    }

    fn statement(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        if self.matches(&Token::Print)? {
            self.print_statement(chunk)
        } else {
            self.expression_statement(chunk)
        }
    }

    fn print_statement(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        self.expression(chunk)?;
        self.consume(&Token::Semicolon)?;
        self.emit_opcode(chunk, OpCode::Print);
        Ok(())
    }

    fn expression_statement(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        self.expression(chunk)?;
        self.consume(&Token::Semicolon)?;
        self.emit_opcode(chunk, OpCode::Pop);
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
            Token::BangEqual => {
                self.emit_opcode(chunk, OpCode::Equal);
                self.emit_opcode(chunk, OpCode::Not);
            }
            Token::EqualEqual => {
                self.emit_opcode(chunk, OpCode::Equal);
            }
            Token::Greater => {
                self.emit_opcode(chunk, OpCode::Greater);
            }
            Token::GreaterEqual => {
                self.emit_opcode(chunk, OpCode::Less);
                self.emit_opcode(chunk, OpCode::Not);
            }
            Token::Less => {
                self.emit_opcode(chunk, OpCode::Less);
            }
            Token::LessEqual => {
                self.emit_opcode(chunk, OpCode::Greater);
                self.emit_opcode(chunk, OpCode::Not);
            }
            _ => (),
        }
        Ok(())
    }

    fn unary(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        let previous = self.previous.clone();
        self.parse_precedence(chunk, Precedence::Unary)?;
        match *previous.borrow() {
            Token::Minus => {
                self.emit_opcode(chunk, OpCode::Negate);
            }
            Token::Bang => {
                self.emit_opcode(chunk, OpCode::Not);
            }
            _ => (),
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

    fn string(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        if let Token::String(str) = *self.previous.borrow() {
            self.emit_constant(chunk, LoxValue::String(str.to_owned()));
            Ok(())
        } else {
            Err(CompileError::CompileError(miette::miette!(
                "Unexpected token: '{}' Expected: 'string'",
                self.previous.borrow()
            )))
        }
    }

    fn literal(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        match *self.previous.borrow() {
            Token::True => {
                self.emit_opcode(chunk, OpCode::True);
                Ok(())
            }
            Token::False => {
                self.emit_opcode(chunk, OpCode::False);
                Ok(())
            }
            Token::Nil => {
                self.emit_opcode(chunk, OpCode::Nil);
                Ok(())
            }
            _ => Err(CompileError::CompileError(miette::miette!(
                "Unexpected token: '{}' Expected one of: 'true', 'false', 'nil'",
                self.previous.borrow()
            ))),
        }
    }

    fn variable(&mut self, chunk: &mut Chunk, can_assign: bool) -> crate::Result<()> {
        self.named_variable(chunk, self.previous.clone(), can_assign)
    }

    fn named_variable(
        &mut self,
        chunk: &mut Chunk,
        token: Rc<RefCell<Token>>,
        can_assign: bool,
    ) -> crate::Result<()> {
        let Token::Identifier(id) = *token.borrow() else {
            return Err(CompileError::CompileError(miette::miette!(
                "Unexpected token: '{}' Expected: 'idenitifier'",
                self.previous.borrow()
            )));
        };
        let arg = self.identifier_constant(chunk, id)?;

        if can_assign && self.matches(&Token::Equal)? {
            self.expression(chunk)?;
            if arg > 255 {
                self.emit_opcode(chunk, OpCode::SetGlobalLong);
            } else {
                self.emit_opcode(chunk, OpCode::SetGlobal);
            }
        } else if arg > 255 {
            self.emit_opcode(chunk, OpCode::GetGlobalLong);
        } else {
            self.emit_opcode(chunk, OpCode::GetGlobal);
        }
        self.emit_operand(chunk, arg);

        Ok(())
    }

    fn parse_precedence(&mut self, chunk: &mut Chunk, precedence: Precedence) -> crate::Result<()> {
        self.advance()?;
        let previous = self.previous.clone();
        let can_assign = precedence as u8 <= Precedence::Assignment as u8;
        self.call_prefix(chunk, &previous.borrow(), can_assign)?;
        while Parser::get_precedence(&self.current.borrow()) as u8 >= precedence as u8 {
            self.advance()?;
            let previous = self.previous.clone();
            self.call_infix(chunk, &previous.borrow())?;
        }
        if can_assign && self.matches(&Token::Equal)? {
            Err(CompileError::CompileError(miette::miette!(
                "Invalid assignment target."
            )))
        } else {
            Ok(())
        }
    }

    fn get_precedence(token: &Token) -> Precedence {
        match token {
            Token::Minus | Token::Plus => Precedence::Term,
            Token::Slash | Token::Star => Precedence::Factor,
            Token::BangEqual | Token::EqualEqual => Precedence::Equality,
            Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual => {
                Precedence::Comparison
            }
            _ => Precedence::None,
        }
    }

    fn call_infix(&mut self, chunk: &mut Chunk, token: &Token) -> crate::Result<()> {
        match token {
            Token::Minus
            | Token::Plus
            | Token::Slash
            | Token::Star
            | Token::BangEqual
            | Token::EqualEqual
            | Token::Greater
            | Token::GreaterEqual
            | Token::Less
            | Token::LessEqual => self.binary(chunk),
            _ => Ok(()),
        }
    }

    fn call_prefix(
        &mut self,
        chunk: &mut Chunk,
        token: &Token,
        can_assign: bool,
    ) -> crate::Result<()> {
        match token {
            Token::Minus | Token::Bang => self.unary(chunk),
            Token::LeftParen => self.grouping(chunk),
            Token::Number(_) => self.number(chunk),
            Token::String(_) => self.string(chunk),
            Token::Identifier(_) => self.variable(chunk, can_assign),
            Token::True | Token::False | Token::Nil => self.literal(chunk),
            _ => Ok(()),
        }
    }

    fn advance(&mut self) -> crate::Result<()> {
        let next = self.tokens.next().unwrap_or(Ok((0, Token::Eof, 0)));
        match next {
            Ok((_, t, _)) => {
                self.previous = self.current.clone();
                self.current = Rc::new(RefCell::new(t));
                Ok(())
            }
            Err(r) => Err(CompileError::CompileError(r)),
        }
    }

    fn matches(&mut self, token: &Token) -> crate::Result<bool> {
        if self.check(token) {
            self.advance()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn consume(&mut self, token: &Token) -> crate::Result<()> {
        if self.matches(token)? {
            Ok(())
        } else {
            Err(CompileError::CompileError(miette::miette!(
                "Unexpected token: '{}' Expected: '{token}'",
                self.current.borrow()
            )))
        }
    }

    fn check(&self, token: &Token) -> bool {
        *self.current.borrow() == *token
    }

    fn emit_constant(&self, chunk: &mut Chunk, value: LoxValue) {
        chunk.write_constant(value, self.tokens.line)
    }

    fn make_constant(&self, chunk: &mut Chunk, value: LoxValue) -> usize {
        chunk.add_constant(value)
    }

    fn emit_opcode(&self, chunk: &mut Chunk, opcode: OpCode) {
        chunk.write_code(opcode, self.tokens.line);
    }

    fn emit_operand(&self, chunk: &mut Chunk, value: usize) {
        chunk.write_operand(value, self.tokens.line);
    }

    fn emit_return(&self, chunk: &mut Chunk) {
        chunk.write_code(OpCode::Return, self.tokens.line);
    }

    fn end_compiler(&self, chunk: &mut Chunk) {
        self.emit_return(chunk);
    }
}
