use std::fmt::Display;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::{ProgramError, value::LoxValue};

#[repr(u8)]
#[derive(FromPrimitive)]
pub enum OpCode {
    Constant = 0,
    ConstantLong = 1,
    Nil = 2,
    True = 3,
    False = 4,
    Pop = 5,
    GetLocal = 6,
    SetLocal = 7,
    GetGlobal = 8,
    GetGlobalLong = 9,
    DefineGlobal = 10,
    DefineGlobalLong = 11,
    SetGlobal = 12,
    SetGlobalLong = 13,
    GetUpvalue = 14,
    SetUpvalue = 15,
    GetProperty = 16,
    SetProperty = 17,
    Equal = 18,
    Greater = 19,
    Less = 20,
    Add = 21,
    Subtract = 22,
    Multiply = 23,
    Divide = 24,
    Not = 25,
    Negate = 26,
    Print = 27,
    Jump = 28,
    JumpIfFalse = 29,
    Loop = 30,
    Call = 31,
    Closure = 32,
    CloseUpvalue = 33,
    Return = 34,
    Class = 35,
    Method = 36,
}

pub const MAX_SHORT_VALUE: usize = 255;

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
            OpCode::GetLocal => write!(f, "OP_GET_LOCAL"),
            OpCode::SetLocal => write!(f, "OP_SET_LOCAL"),
            OpCode::JumpIfFalse => write!(f, "OP_JUMP_IF_FALSE"),
            OpCode::Jump => write!(f, "OP_JUMP"),
            OpCode::Loop => write!(f, "OP_LOOP"),
            OpCode::Call => write!(f, "OP_CALL"),
            OpCode::Closure => write!(f, "OP_CLOSURE"),
            OpCode::GetUpvalue => write!(f, "OP_GET_UPVALUE"),
            OpCode::SetUpvalue => write!(f, "OP_SET_UPVALUE"),
            OpCode::CloseUpvalue => write!(f, "OP_CLOSE_UPVALUE"),
            OpCode::Class => write!(f, "OP_CLASS"),
            OpCode::GetProperty => write!(f, "OP_GET_PROPERTY"),
            OpCode::SetProperty => write!(f, "OP_SET_PROPERTY"),
            OpCode::Method => write!(f, "OP_METHOD"),
        }
    }
}

#[derive(Default, Debug, PartialEq)]
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
        if constant > MAX_SHORT_VALUE {
            self.write_code(OpCode::ConstantLong, line);
        } else {
            self.write_code(OpCode::Constant, line);
        }
        self.write_operand(constant, line);
    }

    pub fn write_operand(&mut self, value: usize, line: usize) {
        if value > MAX_SHORT_VALUE {
            for b in into_three_bytes(value) {
                self.code.push(b);
            }
            self.lines.push(line);
        } else {
            self.code.push(value as u8);
            self.lines.push(line);
        }
    }

    pub fn read_opcode(&self, offset: usize) -> Result<OpCode, ProgramError> {
        OpCode::from_u8(self.code[offset]).ok_or(ProgramError::InvalidInstruction(offset))
    }

    pub fn read_constant(&self, offset: usize, constant_size: usize) -> LoxValue {
        let ix = self.get_constant_ix(offset, constant_size);
        self.constants[ix].clone()
    }

    pub fn ref_constant(&self, offset: usize, constant_size: usize) -> &LoxValue {
        let ix = self.get_constant_ix(offset, constant_size);
        &self.constants[ix]
    }

    #[inline]
    pub fn read_byte(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    #[inline]
    pub fn read_short(&self, offset: usize) -> usize {
        let op1 = self.code[offset];
        let op2 = self.code[offset + 1];
        (op2 as usize) << 8 | (op1 as usize)
    }

    #[inline]
    pub fn read_three_bytes(&self, offset: usize) -> usize {
        let op1 = self.code[offset]; // first operand defines constant index in the constant's vector
        let op2 = self.code[offset + 1]; // second operand defines constant index in the constant's vector
        let op3 = self.code[offset + 2]; // third operand defines constant index in the constant's vector

        (op3 as usize) << 16 | (op2 as usize) << 8 | (op1 as usize)
    }

    pub fn patch_jump(&mut self, offset: usize) {
        // -2 to adjust for the bytecode for the jump offset itself.
        let jump = self.code.len() - 2 - offset;
        let bytes = into_two_bytes(jump);
        self.code[offset] = bytes[0];
        self.code[offset + 1] = bytes[1];
    }

    pub fn write_two_bytes(&mut self, value: usize) {
        let bytes = into_two_bytes(value);
        self.code.push(bytes[0]);
        self.code.push(bytes[1]);
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
        let line_ix = self.line_index(offset);
        if line_ix > 0 && self.lines[line_ix] == self.lines[line_ix - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[line_ix]);
        }
        match code {
            OpCode::Constant | OpCode::DefineGlobal | OpCode::GetGlobal | OpCode::SetGlobal => {
                self.disassembly_constant(offset, &code, 1)
            }
            OpCode::SetLocal
            | OpCode::GetLocal
            | OpCode::Call
            | OpCode::GetUpvalue
            | OpCode::Class
            | OpCode::Method
            | OpCode::GetProperty
            | OpCode::SetProperty
            | OpCode::SetUpvalue => self.disassembly_byte_instruction(offset, &code),
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
            | OpCode::CloseUpvalue
            | OpCode::Pop => self.disassembly_simple_instruction(offset, &code),
            OpCode::GetGlobalLong
            | OpCode::SetGlobalLong
            | OpCode::DefineGlobalLong
            | OpCode::ConstantLong => self.disassembly_constant(offset, &code, 3),
            OpCode::JumpIfFalse | OpCode::Jump => {
                self.disassembly_jump_instruction(offset, &code, 1)
            }
            OpCode::Loop => self.disassembly_jump_instruction(offset, &code, -1),
            OpCode::Closure => self.disassembly_closure_instruction(offset),
        }
    }

    #[inline]
    pub fn line(&self, offset: usize) -> usize {
        let line_ix = offset.min(self.lines.len() - 1);
        self.lines[line_ix]
    }

    #[inline]
    fn line_index(&self, offset: usize) -> usize {
        offset.min(self.lines.len() - 1)
    }

    fn disassembly_byte_instruction(&self, offset: usize, code: &OpCode) -> usize {
        let ix = self.code[offset + 1];
        println!("{:<16} {ix:4}", code.to_string());
        offset + 2
    }

    fn disassembly_closure_instruction(&self, offset: usize) -> usize {
        let function_ix = self.code[offset + 1];

        let mut offset = offset + 2;
        let val = &self.constants[function_ix as usize];
        if let LoxValue::Function(func) = val {
            println!("{:<16} {function_ix:4} {func}", OpCode::Closure.to_string());
            for _ in 0..func.upvalue_count {
                let is_local = self.code[offset];
                let is_local = if is_local == 1 { "local" } else { "upvalue" };
                let index = self.code[offset + 1];
                println!("{offset:04}    |                     {is_local} {index}");
                offset += 2;
            }
        } else {
            println!("{:<16} {function_ix:4}", OpCode::Closure.to_string());
        }
        offset
    }

    fn disassembly_jump_instruction(&self, offset: usize, code: &OpCode, sign: i32) -> usize {
        let jump = self.read_short(offset);

        println!(
            "{:<16} {offset:4} -> {}",
            code.to_string(),
            offset as i32 + 3 + sign * jump as i32
        );
        offset + 3
    }

    fn disassembly_constant(&self, offset: usize, code: &OpCode, constant_size: usize) -> usize {
        let ix = self.get_constant_ix(offset + 1, constant_size);
        let constant = &self.constants[ix];
        println!("{:<16} {ix:4} '{constant}'", code.to_string());
        offset + constant_size + 1 // + 1 for opcode itself
    }

    fn disassembly_simple_instruction(&self, offset: usize, code: &OpCode) -> usize {
        println!("{code}");
        offset + 1
    }

    pub fn add_constant(&mut self, value: LoxValue) -> usize {
        if let Some((constant_index, _)) = self
            .constants
            .iter()
            .enumerate()
            .find(|(_, c)| *c == &value)
        {
            constant_index
        } else {
            self.constants.push(value);
            self.constants.len() - 1
        }
    }

    #[inline]
    fn get_constant_ix(&self, offset: usize, constant_size: usize) -> usize {
        match constant_size {
            1 => self.read_byte(offset) as usize,
            3 => self.read_three_bytes(offset),
            _ => usize::MAX, // so as to crash app if error
        }
    }
}

fn into_three_bytes(value: usize) -> [u8; 3] {
    let op1 = (value & 0xFF) as u8;
    let op2 = ((value & 0xFF00) >> 8) as u8;
    let op3 = ((value & 0xFF0000) >> 16) as u8;
    [op1, op2, op3]
}

fn into_two_bytes(value: usize) -> [u8; 2] {
    let op1 = (value & 0xFF) as u8;
    let op2 = ((value & 0xFF00) >> 8) as u8;
    [op1, op2]
}
