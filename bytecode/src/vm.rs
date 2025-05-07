use num_traits::FromPrimitive;
use scanner::lexer::Lexer;

use crate::{
    CompileError,
    chunk::{Chunk, OpCode},
    value::LoxValue,
};

#[derive(Default)]
pub struct VirtualMachine {
    chunk: Chunk,
    ip: usize,
    stack: Vec<LoxValue>,
}

pub enum InterpretResult {
    Ok = 0,
    CompileError = 1,
    RuntimeError = 2,
}

macro_rules! binary_op {
    ($this:ident, $op:tt) => {{
        let b = $this.pop_number()?;
        let a = $this.pop_number()?;
        $this.push(LoxValue::Number(a $op b));
        $this.ip += 1;
    }}
}

impl VirtualMachine {
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
        let lexer = Lexer::new(content);
        for t in lexer {
            let _ = t.map_err(CompileError::CompileError)?;
        }

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

    fn pop_number(&mut self) -> crate::Result<f64> {
        let value = self.pop()?;
        let LoxValue::Number(n) = value else {
            return Err(CompileError::CompileError(miette::miette!(
                "Number expectet but was: {value}"
            )));
        };
        Ok(n)
    }

    fn run(&mut self) -> crate::Result<()> {
        while self.ip < self.chunk.code.len() {
            let code =
                OpCode::from_u8(self.chunk.code[self.ip]).ok_or(CompileError::CompileError(
                    miette::miette!("Invalid instruction: {}", self.chunk.code[self.ip]),
                ))?;
            #[cfg(feature = "disassembly")]
            {
                for value in self.stack.iter() {
                    println!("[{value}]");
                }
                chunk.disassembly_instruction(self.ip);
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
                    self.ip += 4
                }
                OpCode::Negate => {
                    let value = self.pop_number()?;
                    self.push(LoxValue::Number(-value));
                    self.ip += 1;
                }
                OpCode::Add => binary_op!(self, +),
                OpCode::Subtract => binary_op!(self, -),
                OpCode::Multiply => binary_op!(self, *),
                OpCode::Divide => {
                    let b = self.pop_number()?;
                    let a = self.pop_number()?;
                    if b == 0.0 {
                        return Err(CompileError::RuntimeError(miette::miette!(
                            "Divizion by zero"
                        )));
                    }
                    self.push(LoxValue::Number(a / b));
                    self.ip += 1;
                }
            }
        }
        Ok(())
    }
}
