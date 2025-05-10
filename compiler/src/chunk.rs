use std::fmt::Display;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::value::LoxValue;

#[repr(u8)]
#[derive(FromPrimitive)]
pub enum OpCode {
    Constant = 0,
    ConstantLong = 1,
    Nil = 2,
    True = 3,
    False = 4,
    Pop = 5,
    GetGlobal = 6,
    GetGlobalLong = 7,
    DefineGlobal = 8,
    DefineGlobalLong = 9,
    SetGlobal = 10,
    SetGlobalLong = 11,
    Equal = 12,
    Greater = 13,
    Less = 14,
    Add = 15,
    Subtract = 16,
    Multiply = 17,
    Divide = 18,
    Not = 19,
    Negate = 20,
    Print = 21,
    Return = 22,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Return => write!(f, "OP_RETURN"),
            OpCode::Constant => write!(f, "OP_CONSTANT"),
            OpCode::ConstantLong => write!(f, "OP_CONSTANT_LONG"),
            OpCode::Nil => write!(f, "OP_NIL"),
            OpCode::True => write!(f, "OP_TRUE"),
            OpCode::False => write!(f, "OP_FALSE"),
            OpCode::Negate => write!(f, "OP_NEGATE"),
            OpCode::Add => write!(f, "OP_ADD"),
            OpCode::Subtract => write!(f, "OP_SUBTRACT"),
            OpCode::Multiply => write!(f, "OP_MULTIPLY"),
            OpCode::Divide => write!(f, "OP_DIVIDE"),
            OpCode::Not => write!(f, "OP_NOT"),
            OpCode::Equal => write!(f, "OP_EQUAL"),
            OpCode::Greater => write!(f, "OP_GREATER"),
            OpCode::Less => write!(f, "OP_LESS"),
            OpCode::Print => write!(f, "OP_PRINT"),
            OpCode::Pop => write!(f, "OP_POP"),
            OpCode::DefineGlobal => write!(f, "OP_DEFINE_GLOBAL"),
            OpCode::DefineGlobalLong => write!(f, "OP_DEFINE_LONG"),
            OpCode::GetGlobal => write!(f, "OP_GET_GLOBAL"),
            OpCode::GetGlobalLong => write!(f, "OP_GET_GLOBAL_LONG"),
            OpCode::SetGlobal => write!(f, "OP_SET_GLOBAL"),
            OpCode::SetGlobalLong => write!(f, "OP_SET_GLOBAL_LONG"),
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
        self.write_operand(code as usize, line);
    }

    pub fn write_constant(&mut self, value: LoxValue, line: usize) {
        let constant = self.add_constant(value);
        if constant > 255 {
            self.write_code(OpCode::ConstantLong, line);
        } else {
            self.write_code(OpCode::Constant, line);
        }
        self.write_operand(constant, line);
    }

    pub fn write_operand(&mut self, value: usize, line: usize) {
        if value > 255 {
            for b in into_three_bytes(value) {
                self.code.push(b);
            }
            self.lines.push(line);
        } else {
            self.code.push(value as u8);
            self.lines.push(line);
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
            OpCode::Constant | OpCode::DefineGlobal | OpCode::GetGlobal | OpCode::SetGlobal => {
                self.disassembly_constant(offset, &code)
            }
            OpCode::Return
            | OpCode::Nil
            | OpCode::True
            | OpCode::False
            | OpCode::Negate
            | OpCode::Add
            | OpCode::Subtract
            | OpCode::Multiply
            | OpCode::Divide
            | OpCode::Not
            | OpCode::Equal
            | OpCode::Greater
            | OpCode::Less
            | OpCode::Print
            | OpCode::Pop => self.disassembly_simple_instruction(offset, &code),
            OpCode::GetGlobalLong
            | OpCode::SetGlobalLong
            | OpCode::DefineGlobalLong
            | OpCode::ConstantLong => self.disassembly_constant_long(offset, &code),
        }
    }

    fn disassembly_constant(&self, offset: usize, code: &OpCode) -> usize {
        let ix = self.get_constant_ix(offset);
        let constant = &self.constants[ix];
        println!("{:<16} {ix:4} '{constant}'", code.to_string());
        offset + 2
    }

    fn disassembly_constant_long(&self, offset: usize, code: &OpCode) -> usize {
        let ix = self.get_constant_ix(offset);
        let constant = &self.constants[ix];
        println!("{:<16} {ix:4} '{constant}'", code.to_string());
        offset + 4
    }

    fn disassembly_simple_instruction(&self, offset: usize, code: &OpCode) -> usize {
        println!("{code}");
        offset + 1
    }

    pub fn add_constant(&mut self, value: LoxValue) -> usize {
        let Some((i, _)) = self
            .constants
            .iter()
            .enumerate()
            .find(|(_, c)| *c == &value)
        else {
            self.constants.push(value);
            return self.constants.len() - 1;
        };
        i
    }

    fn get_constant_ix(&self, offset: usize) -> usize {
        let Some(code) = OpCode::from_u8(self.code[offset]) else {
            return 0;
        };
        match code {
            OpCode::Constant | OpCode::DefineGlobal | OpCode::GetGlobal | OpCode::SetGlobal => {
                self.code[offset + 1] as usize
            }
            OpCode::ConstantLong
            | OpCode::DefineGlobalLong
            | OpCode::GetGlobalLong
            | OpCode::SetGlobalLong => {
                let op1 = self.code[offset + 1]; // first operand defines constant index in the constants vector
                let op2 = self.code[offset + 2]; // second operand defines constant index in the constants vector
                let op3 = self.code[offset + 3]; // third operand defines constant index in the constants vector

                (op3 as usize) << 16 | (op2 as usize) << 8 | (op1 as usize)
            }
            _ => 0,
        }
    }
}

fn into_three_bytes(value: usize) -> [u8; 3] {
    let op1 = (value & 0xFF) as u8;
    let op2 = ((value & 0xFF00) >> 8) as u8;
    let op3 = ((value & 0xFF0000) >> 16) as u8;
    [op1, op2, op3]
}
