use num_traits::FromPrimitive;

use crate::{
    CompileError,
    chunk::{Chunk, OpCode},
    compile::Parser,
    value::LoxValue,
};

#[derive(Default)]
pub struct VirtualMachine {
    chunk: Chunk,
    ip: usize,
    stack: Vec<LoxValue>,
}

macro_rules! binary_op {
    ($self:ident, $op:tt) => {{
        let b = $self.pop()?;
        let a = $self.pop()?;
        let a = a.try_num()?;
        let b = b.try_num()?;
        $self.push(LoxValue::Number(a $op b));
        $self.ip += 1;
    }}
}

impl VirtualMachine {
    #[must_use]
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self, content: &str) -> crate::Result<()> {
        self.chunk = Chunk::new();
        self.ip = 0;
        let mut parser = Parser::new(content);
        parser.compile(&mut self.chunk)?;
        self.run()
    }

    pub fn init(&mut self) {
        self.stack.clear();
    }

    fn push(&mut self, value: LoxValue) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> crate::Result<LoxValue> {
        self.stack
            .pop()
            .ok_or(CompileError::RuntimeError(miette::miette!(
                "Instructions stack empty"
            )))
    }

    fn peek(&mut self, distance: usize) -> crate::Result<&LoxValue> {
        if self.stack.len() < distance {
            Err(CompileError::RuntimeError(miette::miette!(
                "Not enough stack capacity for distance {distance}. Current stack size is {}",
                self.stack.len()
            )))
        } else {
            Ok(&self.stack[self.stack.len() - 1 - distance])
        }
    }

    fn run(&mut self) -> crate::Result<()> {
        while self.ip < self.chunk.code.len() {
            let code =
                OpCode::from_u8(self.chunk.code[self.ip]).ok_or(CompileError::CompileError(
                    miette::miette!("Invalid instruction: {}", self.chunk.code[self.ip]),
                ))?;
            #[cfg(feature = "disassembly")]
            {
                for value in &self.stack {
                    println!("[{value}]");
                }
                self.chunk.disassembly_instruction(self.ip);
            }
            match code {
                OpCode::Constant => {
                    let constant = self.chunk.read_constant(self.ip);
                    self.push(constant);
                    self.ip += 2;
                }
                OpCode::Return => {
                    let value = self.pop()?;
                    println!("{value}");
                    self.ip += 1;
                    return Ok(());
                }
                OpCode::ConstantLong => {
                    let constant = self.chunk.read_constant(self.ip);
                    self.push(constant);
                    self.ip += 4;
                }
                OpCode::Negate => {
                    let value = self.pop()?;
                    let value = value.try_num()?;
                    self.push(LoxValue::Number(-value));
                    self.ip += 1;
                }
                OpCode::Add => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let lr = a.try_str();
                    let rr = b.try_str();
                    if lr.is_ok() || rr.is_ok() {
                        // concat strings here if any of the operands is a string
                        if let Ok(l) = lr {
                            let result = l.to_owned() + &b.to_string();
                            self.push(LoxValue::String(result));
                        } else if let Ok(r) = rr {
                            let result = a.to_string() + r;
                            self.push(LoxValue::String(result));
                        }
                    } else if let Ok(l) = a.try_num() {
                        let r = b.try_num()?;
                        self.push(LoxValue::Number(l + r));
                    }

                    self.ip += 1;
                }
                OpCode::Subtract => binary_op!(self, -),
                OpCode::Multiply => binary_op!(self, *),
                OpCode::Divide => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let a = a.try_num()?;
                    let b = b.try_num()?;
                    if b == 0.0 {
                        return Err(CompileError::RuntimeError(miette::miette!(
                            "Division by zero"
                        )));
                    }
                    self.push(LoxValue::Number(a / b));
                    self.ip += 1;
                }
                OpCode::Nil => {
                    self.push(LoxValue::Nil);
                    self.ip += 1;
                }
                OpCode::True => {
                    self.push(LoxValue::Bool(true));
                    self.ip += 1;
                }
                OpCode::False => {
                    self.push(LoxValue::Bool(false));
                    self.ip += 1;
                }
                OpCode::Not => {
                    let value = self.pop()?;
                    let val = value.try_bool()?;
                    self.push(LoxValue::Bool(!val));
                    self.ip += 1;
                }
                OpCode::Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = a.equal(&b);
                    self.push(LoxValue::Bool(result));
                    self.ip += 1;
                }
                OpCode::Less => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = a.less(&b)?;
                    self.push(LoxValue::Bool(result));
                    self.ip += 1;
                }
                OpCode::Greater => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let lt = a.less(&b)?;
                    let gt = !lt && !a.equal(&b);
                    self.push(LoxValue::Bool(gt));
                    self.ip += 1;
                }
            }
        }
        Ok(())
    }
}
