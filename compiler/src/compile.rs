use std::{cell::RefCell, rc::Rc};

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use scanner::{Lexer, Token};

use crate::{
    CompileError,
    chunk::{MAX_SHORT_VALUE, OpCode},
    object::Function,
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

#[derive(Default)]
pub struct Compiler<'a> {
    locals: Vec<Local<'a>>,
    scope_depth: usize,
    pub function: Function<'a>,
    function_type: FunctionType,
}

#[derive(Default)]
enum FunctionType {
    Function,
    #[default]
    Script,
}

impl Compiler<'_> {
    pub fn new() -> Self {
        Self {
            // TODO: locals: vec![Local::new("", Some(0))],
            locals: vec![],
            scope_depth: 0,
            function: Function::new(),
            function_type: FunctionType::Script,
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

    pub fn compile(&mut self) -> crate::Result<&mut Function<'a>> {
        self.advance()?;
        while !self.matches(&Token::Eof)? {
            self.declaration()?;
        }
        self.end_compiler();

        #[cfg(feature = "printcode")]
        {
            self.compiler
                .function
                .chunk
                .disassembly(self.compiler.function.name.unwrap_or("<script>"));
        }
        Ok(&mut self.compiler.function)
    }

    fn expression(&mut self) -> crate::Result<()> {
        self.parse_precedence(Precedence::Assignment)?;
        Ok(())
    }

    fn declaration(&mut self) -> crate::Result<()> {
        if self.matches(&Token::Var)? {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> crate::Result<()> {
        let global = self.parse_variable()?;
        if self.matches(&Token::Equal)? {
            self.expression()?;
        } else {
            self.emit_opcode(OpCode::Nil);
        }
        self.consume(&Token::Semicolon)?;
        self.define_variable(global);
        Ok(())
    }

    fn parse_variable(&mut self) -> crate::Result<usize> {
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

        let constant = self.identifier_constant(id);
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

    fn identifier_constant(&mut self, id: &str) -> usize {
        self.make_constant(LoxValue::String(id.to_string()))
    }

    fn define_variable(&mut self, global: usize) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        if global > MAX_SHORT_VALUE {
            self.emit_opcode(OpCode::DefineGlobalLong);
        } else {
            self.emit_opcode(OpCode::DefineGlobal);
        }
        self.emit_operand(global);
    }

    fn mark_initialized(&mut self) {
        if let Some(v) = self.compiler.locals.last_mut() {
            v.depth = Some(self.compiler.scope_depth);
        }
    }

    fn statement(&mut self) -> crate::Result<()> {
        if self.matches(&Token::Print)? {
            self.print_statement()
        } else if self.matches(&Token::LeftBrace)? {
            self.begin_scope();
            let block_result = self.block();
            self.end_scope();
            block_result
        } else if self.matches(&Token::For)? {
            self.for_statement()
        } else if self.matches(&Token::If)? {
            self.if_statement()
        } else if self.matches(&Token::While)? {
            self.while_statement()
        } else {
            self.expression_statement()
        }
    }

    fn block(&mut self) -> crate::Result<()> {
        while !self.check(&Token::RightBrace) && !self.check(&Token::Eof) {
            self.declaration()?;
        }
        self.consume(&Token::RightBrace)?;
        Ok(())
    }

    fn for_statement(&mut self) -> crate::Result<()> {
        self.begin_scope();
        self.consume(&Token::LeftParen)?;

        // Initializer
        if self.matches(&Token::Semicolon)? {
        } else if self.matches(&Token::Var)? {
            self.var_declaration()?;
        } else {
            self.expression_statement()?;
        }

        // Condition
        let mut loop_start = self.chunk_code_size();
        let mut exit_jump = None;

        if !self.matches(&Token::Semicolon)? {
            self.expression()?;
            self.consume(&Token::Semicolon)?;
            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse));
        }
        // Increment

        if !self.matches(&Token::RightParen)? {
            let body_jump = self.emit_jump(OpCode::Jump);
            let increment_start = self.chunk_code_size();
            self.expression()?;
            self.emit_opcode(OpCode::Pop);
            self.consume(&Token::RightParen)?;
            self.emit_loop(loop_start)?;
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement()?;
        self.emit_loop(loop_start)?;

        if let Some(jmp) = exit_jump {
            self.patch_jump(jmp);
            self.emit_opcode(OpCode::Pop);
        }
        self.end_scope();
        Ok(())
    }

    fn while_statement(&mut self) -> crate::Result<()> {
        let loop_start = self.chunk_code_size();
        self.consume(&Token::LeftParen)?;
        self.expression()?;
        self.consume(&Token::RightParen)?;

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_opcode(OpCode::Pop);
        self.statement()?;
        self.emit_loop(loop_start)?;

        self.patch_jump(exit_jump);
        self.emit_opcode(OpCode::Pop);
        Ok(())
    }

    fn if_statement(&mut self) -> crate::Result<()> {
        self.consume(&Token::LeftParen)?;
        self.expression()?;
        self.consume(&Token::RightParen)?;
        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_opcode(OpCode::Pop);
        self.statement()?;
        let else_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(then_jump);
        self.emit_opcode(OpCode::Pop);
        if self.matches(&Token::Else)? {
            self.statement()?;
        }
        self.patch_jump(else_jump);
        Ok(())
    }

    fn print_statement(&mut self) -> crate::Result<()> {
        self.expression()?;
        self.consume(&Token::Semicolon)?;
        self.emit_opcode(OpCode::Print);
        Ok(())
    }

    fn expression_statement(&mut self) -> crate::Result<()> {
        self.expression()?;
        self.consume(&Token::Semicolon)?;
        self.emit_opcode(OpCode::Pop);
        Ok(())
    }

    fn binary(&mut self) -> crate::Result<()> {
        let previous = self.previous.clone();
        let precedence = Parser::get_precedence(&previous.borrow());
        let precedence = precedence as u8 + 1;
        let precedence = Precedence::from_u8(precedence).ok_or(CompileError::CompileError(
            miette::miette!("Invalid precedence: {}", precedence),
        ))?;
        self.parse_precedence(precedence)?;
        match *previous.borrow() {
            Token::Minus => {
                self.emit_opcode(OpCode::Subtract);
            }
            Token::Slash => {
                self.emit_opcode(OpCode::Divide);
            }
            Token::Star => {
                self.emit_opcode(OpCode::Multiply);
            }
            Token::Plus => {
                self.emit_opcode(OpCode::Add);
            }
            Token::BangEqual => {
                self.emit_opcode(OpCode::Equal);
                self.emit_opcode(OpCode::Not);
            }
            Token::EqualEqual => {
                self.emit_opcode(OpCode::Equal);
            }
            Token::Greater => {
                self.emit_opcode(OpCode::Greater);
            }
            Token::GreaterEqual => {
                self.emit_opcode(OpCode::Less);
                self.emit_opcode(OpCode::Not);
            }
            Token::Less => {
                self.emit_opcode(OpCode::Less);
            }
            Token::LessEqual => {
                self.emit_opcode(OpCode::Greater);
                self.emit_opcode(OpCode::Not);
            }
            _ => (),
        }
        Ok(())
    }

    fn and(&mut self) -> crate::Result<()> {
        let and_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_opcode(OpCode::Pop);
        self.parse_precedence(Precedence::And)?;
        self.patch_jump(and_jump);
        Ok(())
    }

    fn or(&mut self) -> crate::Result<()> {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(else_jump);
        self.emit_opcode(OpCode::Pop);

        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    fn unary(&mut self) -> crate::Result<()> {
        let previous = self.previous.clone();
        self.parse_precedence(Precedence::Unary)?;
        match *previous.borrow() {
            Token::Minus => {
                self.emit_opcode(OpCode::Negate);
            }
            Token::Bang => {
                self.emit_opcode(OpCode::Not);
            }
            _ => (),
        }
        Ok(())
    }

    fn grouping(&mut self) -> crate::Result<()> {
        self.expression()?;
        self.consume(&Token::RightParen)?;
        Ok(())
    }

    fn number(&mut self) -> crate::Result<()> {
        let Token::Number(number) = *self.previous.borrow() else {
            return Err(CompileError::CompileError(miette::miette!(
                "Unexpected token: '{}' Expected: 'number'",
                self.previous.borrow()
            )));
        };
        self.emit_constant(LoxValue::Number(number));
        Ok(())
    }

    fn string(&mut self) -> crate::Result<()> {
        let Token::String(str) = *self.previous.borrow() else {
            return Err(CompileError::CompileError(miette::miette!(
                "Unexpected token: '{}' Expected: 'string'",
                self.previous.borrow()
            )));
        };
        let s = str.to_owned();
        let value = LoxValue::String(s);
        self.emit_constant(value);
        Ok(())
    }

    fn literal(&mut self) -> crate::Result<()> {
        match *self.previous.clone().borrow() {
            Token::True => {
                self.emit_opcode(OpCode::True);
                Ok(())
            }
            Token::False => {
                self.emit_opcode(OpCode::False);
                Ok(())
            }
            Token::Nil => {
                self.emit_opcode(OpCode::Nil);
                Ok(())
            }
            _ => Err(CompileError::CompileError(miette::miette!(
                "Unexpected token: '{}' Expected one of: 'true', 'false', 'nil'",
                self.previous.borrow()
            ))),
        }
    }

    fn variable(&mut self, can_assign: bool) -> crate::Result<()> {
        self.named_variable(self.previous.clone(), can_assign)
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;

        while !self.compiler.locals.is_empty() {
            let i = self.compiler.locals.len() - 1;
            if self.compiler.locals[i].depth.unwrap_or(0) > self.compiler.scope_depth {
                self.emit_opcode(OpCode::Pop);
                self.compiler.locals.pop();
            } else {
                break;
            }
        }
    }

    fn named_variable(
        &mut self,
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
            self.identifier_constant(id)
        };

        if can_assign && self.matches(&Token::Equal)? {
            self.expression()?;
            if arg > MAX_SHORT_VALUE {
                self.emit_opcode(OpCode::SetGlobalLong);
            } else {
                self.emit_opcode(set_code);
            }
        } else if arg > MAX_SHORT_VALUE {
            self.emit_opcode(OpCode::GetGlobalLong);
        } else {
            self.emit_opcode(get_code);
        }
        self.emit_operand(arg);

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

    fn parse_precedence(&mut self, precedence: Precedence) -> crate::Result<()> {
        self.advance()?;
        let previous = self.previous.clone();
        let can_assign = precedence as u8 <= Precedence::Assignment as u8;
        self.call_prefix(&previous.borrow(), can_assign)?;
        while Parser::get_precedence(&self.current.borrow()) as u8 >= precedence as u8 {
            self.advance()?;
            let previous = self.previous.clone();
            self.call_infix(&previous.borrow())?;
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

    fn call_infix(&mut self, token: &Token) -> crate::Result<()> {
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
            | Token::LessEqual => self.binary(),
            Token::And => self.and(),
            Token::Or => self.or(),
            _ => Ok(()),
        }
    }

    fn call_prefix(&mut self, token: &Token, can_assign: bool) -> crate::Result<()> {
        match token {
            Token::Minus | Token::Bang => self.unary(),
            Token::LeftParen => self.grouping(),
            Token::Number(_) => self.number(),
            Token::String(_) => self.string(),
            Token::Identifier(_) => self.variable(can_assign),
            Token::True | Token::False | Token::Nil => self.literal(),
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

    fn patch_jump(&mut self, exit_jump: usize) {
        self.compiler.function.chunk.patch_jump(exit_jump);
    }

    fn emit_constant(&mut self, value: LoxValue) {
        self.compiler
            .function
            .chunk
            .write_constant(value, self.tokens.line);
    }

    fn make_constant(&mut self, value: LoxValue) -> usize {
        self.compiler.function.chunk.add_constant(value)
    }

    fn emit_jump(&mut self, opcode: OpCode) -> usize {
        self.compiler
            .function
            .chunk
            .write_code(opcode, self.tokens.line);
        self.emit_operand(0xFF);
        self.emit_operand(0xFF);
        self.chunk_code_size() - 2
    }

    fn emit_loop(&mut self, loop_start: usize) -> crate::Result<()> {
        self.compiler
            .function
            .chunk
            .write_code(OpCode::Loop, self.tokens.line);

        let offset = self.chunk_code_size() - loop_start + 2;
        if offset > u16::MAX as usize {
            return Err(CompileError::CompileError(miette::miette!(
                "Loop body too large."
            )));
        }
        self.compiler.function.chunk.write_two_bytes(offset);
        Ok(())
    }

    fn chunk_code_size(&self) -> usize {
        self.compiler.function.chunk.code.len()
    }

    fn emit_opcode(&mut self, opcode: OpCode) {
        self.compiler
            .function
            .chunk
            .write_code(opcode, self.tokens.line);
    }

    fn emit_operand(&mut self, value: usize) {
        self.compiler
            .function
            .chunk
            .write_operand(value, self.tokens.line);
    }

    fn emit_return(&mut self) {
        self.compiler
            .function
            .chunk
            .write_code(OpCode::Return, self.tokens.line);
    }

    fn end_compiler(&mut self) {
        self.emit_return();
    }
}
