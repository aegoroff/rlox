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
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn write_code(&mut self, code: OpCode, line: usize) {
        self.write_operand(code as u8, line);
    }

    pub fn write_operand(&mut self, value: u8, line: usize) {
        self.instructions.push(value);
        self.lines.push(line);
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
        let Some(code) = OpCode::from_u8(self.instructions[offset]) else {
            return offset + 1;
        };
        print!("{offset:04} ");
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[offset]);
        }
        match code {
            OpCode::Constant => self.disassembly_constant(offset, &code),
            OpCode::Return => self.disassembly_return(offset, &code),
        }
    }

    fn disassembly_constant(&self, offset: usize, code: &OpCode) -> usize {
        let op1 = self.instructions[offset + 1]; // first operand defines constant index in the constants vector
        let constant = &self.constants[op1 as usize];
        println!("{code:-16} {op1:4} '{constant}'");
        offset + 2
    }

    fn disassembly_return(&self, offset: usize, code: &OpCode) -> usize {
        println!("{code}");
        offset + 1
    }
}
