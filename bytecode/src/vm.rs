use num_traits::FromPrimitive;

use crate::chunk::{Chunk, OpCode};

#[derive(Default)]
pub struct VM<'a> {
    chunk: Option<&'a Chunk>,
    ip: usize,
}

pub enum InterpretResult {
    Ok = 0,
    CompileError = 1,
    RuntimeError = 2,
}

impl<'a> VM<'a> {
    pub fn new() -> Self {
        Self { chunk: None, ip: 0 }
    }

    pub fn interpret(&mut self, chunk: &'a Chunk) -> InterpretResult {
        self.chunk = Some(chunk);
        self.ip = 0;
        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        let Some(chunk) = self.chunk else {
            return InterpretResult::Ok;
        };
        let instr = &chunk.instructions;
        let constants = &chunk.constants;
        while self.ip < instr.len() {
            let Some(code) = OpCode::from_u8(instr[self.ip]) else {
                return InterpretResult::RuntimeError;
            };
            match code {
                OpCode::Constant => {
                    let op1 = instr[self.ip + 1];
                    let constant = &constants[op1 as usize];
                    println!("{constant}");
                    self.ip += 2
                }
                OpCode::Return => {
                    self.ip += 1;
                    return InterpretResult::Ok;
                }
                OpCode::ConstantLong => {
                    let op1 = instr[self.ip + 1];
                    let op2 = instr[self.ip + 2];
                    let op3 = instr[self.ip + 3];
                    let ix: usize = (op3 as usize) << 16 | (op2 as usize) << 8 | (op1 as usize);
                    let constant = &constants[ix];
                    println!("{constant}");
                    self.ip += 4
                }
            }
        }
        InterpretResult::Ok
    }
}
