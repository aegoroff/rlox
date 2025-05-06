use std::fmt::Display;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::value::LoxValue;

#[repr(u8)]
#[derive(FromPrimitive)]
pub enum OpCode {
    Constant = 0,
    Return = 1,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Return => write!(f, "OP_RETURN"),
            OpCode::Constant => write!(f, "OP_CONSTANT"),
        }
    }
}

#[derive(Default)]
pub struct Chunk {
    instructions: Vec<u8>,
    constants: Vec<LoxValue>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            constants: vec![],
        }
    }

    pub fn write_code(&mut self, code: OpCode) {
        self.instructions.push(code as u8);
    }

    pub fn write_operand(&mut self, value: u8) {
        self.instructions.push(value);
    }

    pub fn add_constant(&mut self, value: LoxValue) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1) as u8
    }

    pub fn disassembly(&self, name: &str) {
        println!("=== {name} ===");
        let mut offset = 0;
        while offset < self.instructions.len() {
            offset = self.disassembly_instruction(offset);
        }
    }

    fn disassembly_instruction(&self, offset: usize) -> usize {
        let code = self.instructions[offset];
        match OpCode::from_u8(code) {
            Some(OpCode::Constant) => {
                let op1 = self.instructions[offset + 1];
                let constant = &self.constants[op1 as usize];
                println!("{offset:04} {code} {constant}");
                offset + 2
            }
            Some(OpCode::Return) => {
                println!("{offset:04} {code}");
                offset + 1
            }
            None => offset + 1,
        }
    }
}
