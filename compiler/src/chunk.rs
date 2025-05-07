use std::fmt::Display;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::value::LoxValue;

#[repr(u8)]
#[derive(FromPrimitive)]
pub enum OpCode {
    Constant = 0,
    ConstantLong = 1,
    Add = 2,
    Subtract = 3,
    Multiply = 4,
    Divide = 5,
    Negate = 6,
    Return = 7,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Return => write!(f, "OP_RETURN"),
            OpCode::Constant => write!(f, "OP_CONSTANT"),
            OpCode::ConstantLong => write!(f, "OP_CONSTANT_LONG"),
            OpCode::Negate => write!(f, "OP_NEGATE"),
            OpCode::Add => write!(f, "OP_ADD"),
            OpCode::Subtract => write!(f, "OP_SUBTRACT"),
            OpCode::Multiply => write!(f, "OP_MULTIPLY"),
            OpCode::Divide => write!(f, "OP_DIVIDE"),
        }
    }
}

#[derive(Default)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<LoxValue>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn write_code(&mut self, code: OpCode, line: usize) {
        self.write_operand(code as u8, line);
    }

    pub fn write_operand(&mut self, value: u8, line: usize) {
        self.code.push(value);
        self.lines.push(line);
    }

    pub fn write_constant(&mut self, value: LoxValue, line: usize) {
        let constant = self.add_constant(value);
        if constant > 255 {
            self.write_code(OpCode::ConstantLong, line);
            for b in into_three_bytes(constant) {
                self.write_operand(b, line);
            }
        } else {
            self.write_code(OpCode::Constant, line);
            self.write_operand(constant as u8, line);
        }
    }

    pub fn read_constant(&self, offset: usize) -> LoxValue {
        let ix = self.get_constant_ix(offset);
        self.constants[ix].clone()
    }

    pub fn disassembly(&self, name: &str) {
        println!("=== {name} ===");
        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassembly_instruction(offset);
        }
    }

    pub fn disassembly_instruction(&self, offset: usize) -> usize {
        let Some(code) = OpCode::from_u8(self.code[offset]) else {
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
            OpCode::ConstantLong => self.disassembly_constant_long(offset, &code),
            OpCode::Negate => self.disassembly_negate(offset, &code),
            OpCode::Add => self.disassembly_add(offset, &code),
            OpCode::Subtract => self.disassembly_subtract(offset, &code),
            OpCode::Multiply => self.disassembly_multiply(offset, &code),
            OpCode::Divide => self.disassembly_divide(offset, &code),
        }
    }

    fn disassembly_constant(&self, offset: usize, code: &OpCode) -> usize {
        let ix = self.get_constant_ix(offset);
        let constant = &self.constants[ix];
        println!("{code:-16} {ix:4} '{constant}'");
        offset + 2
    }

    fn disassembly_constant_long(&self, offset: usize, code: &OpCode) -> usize {
        let ix = self.get_constant_ix(offset);
        let constant = &self.constants[ix];
        println!("{code:-16} {ix:4} '{constant}'");
        offset + 4
    }

    fn disassembly_negate(&self, offset: usize, code: &OpCode) -> usize {
        println!("{code}");
        offset + 1
    }

    fn disassembly_add(&self, offset: usize, code: &OpCode) -> usize {
        println!("{code}");
        offset + 1
    }

    fn disassembly_subtract(&self, offset: usize, code: &OpCode) -> usize {
        println!("{code}");
        offset + 1
    }

    fn disassembly_multiply(&self, offset: usize, code: &OpCode) -> usize {
        println!("{code}");
        offset + 1
    }

    fn disassembly_divide(&self, offset: usize, code: &OpCode) -> usize {
        println!("{code}");
        offset + 1
    }
    fn add_constant(&mut self, value: LoxValue) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn get_constant_ix(&self, offset: usize) -> usize {
        let Some(code) = OpCode::from_u8(self.code[offset]) else {
            return 0;
        };
        match code {
            OpCode::Constant => self.code[offset + 1] as usize,
            OpCode::ConstantLong => {
                let op1 = self.code[offset + 1]; // first operand defines constant index in the constants vector
                let op2 = self.code[offset + 2]; // second operand defines constant index in the constants vector
                let op3 = self.code[offset + 3]; // third operand defines constant index in the constants vector

                (op3 as usize) << 16 | (op2 as usize) << 8 | (op1 as usize)
            }
            _ => 0,
        }
    }

    fn disassembly_return(&self, offset: usize, code: &OpCode) -> usize {
        println!("{code}");
        offset + 1
    }
}

fn into_three_bytes(value: usize) -> [u8; 3] {
    let op1 = (value & 0xFF) as u8;
    let op2 = ((value & 0xFF00) >> 8) as u8;
    let op3 = ((value & 0xFF0000) >> 16) as u8;
    [op1, op2, op3]
}
