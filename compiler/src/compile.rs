use std::{cell::RefCell, rc::Rc};

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use scanner::{Lexer, Token};

use crate::{
    CompileError,
    chunk::{Chunk, MAX_SHORT_VALUE, OpCode},
    value::LoxValue,
};

pub struct Parser<'a> {
    tokens: Lexer<'a>,
    compiler: Compiler<'a>,
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

struct Local<'a> {
    name: &'a str,
    depth: Option<usize>,
}

impl<'a> Local<'a> {
    fn new(name: &'a str, depth: Option<usize>) -> Self {
        Self { name, depth }
    }
}

struct Compiler<'a> {
    locals: Vec<Local<'a>>,
    scope_depth: usize,
}

impl Compiler<'_> {
    fn new() -> Self {
        Self {
            locals: vec![],
            scope_depth: 0,
        }
    }
}

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(content: &'a str) -> Self {
        Self {
            tokens: Lexer::new(content),
            current: Rc::new(RefCell::new(Token::Eof)),
            previous: Rc::new(RefCell::new(Token::Eof)),
            compiler: Compiler::new(),
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
        if self.matches(&Token::Equal)? {
            self.expression(chunk)?;
        } else {
            self.emit_opcode(chunk, OpCode::Nil);
        }
        self.consume(&Token::Semicolon)?;
        self.define_variable(chunk, global);
        Ok(())
    }

    fn parse_variable(&mut self, chunk: &mut Chunk) -> crate::Result<usize> {
        let Token::Identifier(id) = *self.current.borrow() else {
            return Err(CompileError::CompileError(miette::miette!(
                "Identifier expected"
            )));
        };
        self.advance()?;

        self.declare_variable(id)?;
        if self.compiler.scope_depth > 0 {
            return Ok(0);
        }

        let constant = self.identifier_constant(chunk, id);
        Ok(constant)
    }

    fn declare_variable(&mut self, name: &'a str) -> crate::Result<()> {
        if self.compiler.scope_depth == 0 {
            return Ok(());
        }

        let existing = self.compiler.locals.iter().rev().any(|local| {
            if let Some(local_depth) = local.depth {
                local.name == name && local_depth == self.compiler.scope_depth
            } else {
                local.name == name
            }
        });
        if existing {
            return Err(CompileError::CompileError(miette::miette!(
                "Already a variables with this name '{name}' in the same scope"
            )));
        }
        self.add_local(name)?;
        Ok(())
    }

    fn add_local(&mut self, name: &'a str) -> crate::Result<()> {
        if self.compiler.scope_depth == 0 {
            return Ok(());
        }
        if self.compiler.locals.len() >= MAX_SHORT_VALUE {
            return Err(CompileError::CompileError(miette::miette!(
                "Too many local variables in function."
            )));
        }
        let local = Local::new(name, None);
        self.compiler.locals.push(local);
        Ok(())
    }

    fn identifier_constant(&mut self, chunk: &mut Chunk, id: &str) -> usize {
        self.make_constant(chunk, LoxValue::String(id.to_string()))
    }

    fn define_variable(&mut self, chunk: &mut Chunk, global: usize) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        if global > MAX_SHORT_VALUE {
            self.emit_opcode(chunk, OpCode::DefineGlobalLong);
        } else {
            self.emit_opcode(chunk, OpCode::DefineGlobal);
        }
        self.emit_operand(chunk, global);
    }

    fn mark_initialized(&mut self) {
        if let Some(v) = self.compiler.locals.last_mut() {
            v.depth = Some(self.compiler.scope_depth);
        }
    }

    fn statement(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        if self.matches(&Token::Print)? {
            self.print_statement(chunk)
        } else if self.matches(&Token::LeftBrace)? {
            self.begin_scope();
            let block_result = self.block(chunk);
            self.end_scope(chunk);
            block_result
        } else if self.matches(&Token::For)? {
            self.for_statement(chunk)
        } else if self.matches(&Token::If)? {
            self.if_statement(chunk)
        } else if self.matches(&Token::While)? {
            self.while_statement(chunk)
        } else {
            self.expression_statement(chunk)
        }
    }

    fn block(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        while !self.check(&Token::RightBrace) && !self.check(&Token::Eof) {
            self.declaration(chunk)?;
        }
        self.consume(&Token::RightBrace)?;
        Ok(())
    }

    fn for_statement(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        self.begin_scope();
        self.consume(&Token::LeftParen)?;

        // Initializer
        if self.matches(&Token::Semicolon)? {
        } else if self.matches(&Token::Var)? {
            self.var_declaration(chunk)?;
        } else {
            self.expression_statement(chunk)?;
        }

        // Condition
        let mut loop_start = chunk.code.len();
        let mut exit_jump = None;

        if !self.matches(&Token::Semicolon)? {
            self.expression(chunk)?;
            self.consume(&Token::Semicolon)?;
            exit_jump = Some(self.emit_jump(chunk, OpCode::JumpIfFalse));
        }
        // Increment

        if !self.matches(&Token::RightParen)? {
            let body_jump = self.emit_jump(chunk, OpCode::Jump);
            let increment_start = chunk.code.len();
            self.expression(chunk)?;
            self.emit_opcode(chunk, OpCode::Pop);
            self.consume(&Token::RightParen)?;
            self.emit_loop(chunk, loop_start)?;
            loop_start = increment_start;
            chunk.patch_jump(body_jump);
        }

        self.statement(chunk)?;
        self.emit_loop(chunk, loop_start)?;

        if let Some(jmp) = exit_jump {
            chunk.patch_jump(jmp);
            self.emit_opcode(chunk, OpCode::Pop);
        }
        self.end_scope(chunk);
        Ok(())
    }

    fn while_statement(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        let loop_start = chunk.code.len();
        self.consume(&Token::LeftParen)?;
        self.expression(chunk)?;
        self.consume(&Token::RightParen)?;

        let exit_jump = self.emit_jump(chunk, OpCode::JumpIfFalse);
        self.emit_opcode(chunk, OpCode::Pop);
        self.statement(chunk)?;
        self.emit_loop(chunk, loop_start)?;

        chunk.patch_jump(exit_jump);
        self.emit_opcode(chunk, OpCode::Pop);
        Ok(())
    }

    fn if_statement(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        self.consume(&Token::LeftParen)?;
        self.expression(chunk)?;
        self.consume(&Token::RightParen)?;
        let then_jump = self.emit_jump(chunk, OpCode::JumpIfFalse);
        self.emit_opcode(chunk, OpCode::Pop);
        self.statement(chunk)?;
        let else_jump = self.emit_jump(chunk, OpCode::Jump);
        chunk.patch_jump(then_jump);
        self.emit_opcode(chunk, OpCode::Pop);
        if self.matches(&Token::Else)? {
            self.statement(chunk)?;
        }
        chunk.patch_jump(else_jump);
        Ok(())
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

    fn and(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        let and_jump = self.emit_jump(chunk, OpCode::JumpIfFalse);
        self.emit_opcode(chunk, OpCode::Pop);
        self.parse_precedence(chunk, Precedence::And)?;
        chunk.patch_jump(and_jump);
        Ok(())
    }

    fn or(&mut self, chunk: &mut Chunk) -> crate::Result<()> {
        let else_jump = self.emit_jump(chunk, OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(chunk, OpCode::Jump);

        chunk.patch_jump(else_jump);
        self.emit_opcode(chunk, OpCode::Pop);

        self.parse_precedence(chunk, Precedence::Or)?;
        chunk.patch_jump(end_jump);
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

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self, chunk: &mut Chunk) {
        self.compiler.scope_depth -= 1;

        while !self.compiler.locals.is_empty() {
            let i = self.compiler.locals.len() - 1;
            if self.compiler.locals[i].depth.unwrap_or(0) > self.compiler.scope_depth {
                self.emit_opcode(chunk, OpCode::Pop);
                self.compiler.locals.pop();
            } else {
                break;
            }
        }
    }

    fn named_variable(
        &mut self,
        chunk: &mut Chunk,
        token: Rc<RefCell<Token<'a>>>,
        can_assign: bool,
    ) -> crate::Result<()> {
        let Token::Identifier(id) = *token.borrow() else {
            return Err(CompileError::CompileError(miette::miette!(
                "Unexpected token: '{}' Expected: 'idenitifier'",
                self.previous.borrow()
            )));
        };
        let mut set_code = OpCode::SetGlobal;
        let mut get_code = OpCode::GetGlobal;
        let arg = if let Some(i) = self.resolve_local(id)? {
            set_code = OpCode::SetLocal;
            get_code = OpCode::GetLocal;
            i
        } else {
            self.identifier_constant(chunk, id)
        };

        if can_assign && self.matches(&Token::Equal)? {
            self.expression(chunk)?;
            if arg > MAX_SHORT_VALUE {
                self.emit_opcode(chunk, OpCode::SetGlobalLong);
            } else {
                self.emit_opcode(chunk, set_code);
            }
        } else if arg > MAX_SHORT_VALUE {
            self.emit_opcode(chunk, OpCode::GetGlobalLong);
        } else {
            self.emit_opcode(chunk, get_code);
        }
        self.emit_operand(chunk, arg);

        Ok(())
    }

    fn resolve_local(&self, name: &'a str) -> crate::Result<Option<usize>> {
        let Some((i, l)) = self
            .compiler
            .locals
            .iter()
            .rev()
            .enumerate()
            .find(|(_, local)| local.name == name)
        else {
            return Ok(None);
        };
        if l.depth.is_none() {
            Err(CompileError::CompileError(miette::miette!(
                "Can't read local variable in its own initializer."
            )))
        } else {
            Ok(Some(self.compiler.locals.len() - 1 - i))
        }
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
            Token::And => Precedence::And,
            Token::Or => Precedence::Or,
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
            Token::And => self.and(chunk),
            Token::Or => self.or(chunk),
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
        chunk.write_constant(value, self.tokens.line);
    }

    fn make_constant(&self, chunk: &mut Chunk, value: LoxValue) -> usize {
        chunk.add_constant(value)
    }

    fn emit_jump(&self, chunk: &mut Chunk, opcode: OpCode) -> usize {
        chunk.write_code(opcode, self.tokens.line);
        self.emit_operand(chunk, 0xFF);
        self.emit_operand(chunk, 0xFF);
        chunk.code.len() - 2
    }

    fn emit_loop(&self, chunk: &mut Chunk, loop_start: usize) -> crate::Result<()> {
        chunk.write_code(OpCode::Loop, self.tokens.line);

        let offset = chunk.code.len() - loop_start + 2;
        if offset > u16::MAX as usize {
            return Err(CompileError::CompileError(miette::miette!(
                "Loop body too large."
            )));
        }
        chunk.write_two_bytes(offset);
        Ok(())
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
