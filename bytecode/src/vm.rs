use num_traits::FromPrimitive;

use crate::{
    chunk::{Chunk, OpCode},
    value::LoxValue,
};

#[derive(Default)]
pub struct VirtualMachine<'a> {
    chunk: Option<&'a Chunk>,
    ip: usize,
    stack: Vec<LoxValue>,
}

pub enum InterpretResult {
    Ok = 0,
    CompileError = 1,
    RuntimeError = 2,
}

impl<'a> VirtualMachine<'a> {
    pub fn new() -> Self {
        Self {
            chunk: None,
            ip: 0,
            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self, chunk: &'a Chunk) -> InterpretResult {
        self.chunk = Some(chunk);
        self.ip = 0;
        self.run()
    }

    pub fn init(&mut self) {
        self.stack.clear();
    }

    pub fn push(&mut self, value: LoxValue) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Option<LoxValue> {
        self.stack.pop()
    }

    fn run(&mut self) -> InterpretResult {
        let Some(chunk) = self.chunk else {
            return InterpretResult::Ok;
        };
        let instr = &chunk.instructions;
        while self.ip < instr.len() {
            let Some(code) = OpCode::from_u8(instr[self.ip]) else {
                return InterpretResult::RuntimeError;
            };
            #[cfg(feature = "disassembly")]
            {
                for value in self.stack.iter() {
                    println!("[{value}]");
                }
                chunk.disassembly_instruction(self.ip);
            }
            match code {
                OpCode::Constant => {
                    let constant = chunk.read_constant(self.ip);
                    self.push(constant);
                    self.ip += 2;
                }
                OpCode::Return => {
                    let Some(value) = self.pop() else {
                        return InterpretResult::RuntimeError;
                    };
                    println!("{value}");
                    self.ip += 1;
                    return InterpretResult::Ok;
                }
                OpCode::ConstantLong => {
                    let constant = chunk.read_constant(self.ip);
                    self.push(constant);
                    self.ip += 4
                }
                OpCode::Negate => {
                    let Some(value) = self.pop() else {
                        return InterpretResult::RuntimeError;
                    };
                    let LoxValue::Number(n) = value else {
                        return InterpretResult::RuntimeError;
                    };
                    self.push(LoxValue::Number(-n));
                    self.ip += 1;
                }
            }
        }
        InterpretResult::Ok
    }
}
