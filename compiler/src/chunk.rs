#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_possible_truncation)]
use std::fmt::Display;
use std::rc::Rc;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::{RuntimeError, object::ObjectStore, value::LoxValue};

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
    GetLocalLong = 8,
    SetLocalLong = 9,
    GetGlobal = 10,
    GetGlobalLong = 11,
    DefineGlobal = 12,
    DefineGlobalLong = 13,
    SetGlobal = 14,
    SetGlobalLong = 15,
    GetUpvalue = 16,
    SetUpvalue = 17,
    GetProperty = 18,
    SetProperty = 19,
    GetSuper = 20,
    Equal = 21,
    Greater = 22,
    Less = 23,
    Add = 24,
    Subtract = 25,
    Multiply = 26,
    Divide = 27,
    Not = 28,
    Negate = 29,
    Print = 30,
    Jump = 31,
    JumpIfFalse = 32,
    Loop = 33,
    Call = 34,
    Invoke = 35,
    SuperInvoke = 36,
    Closure = 37,
    CloseUpvalue = 38,
    Return = 39,
    Class = 40,
    Inherit = 41,
    Method = 42,
    ClosureLong = 43,
    ClassLong = 44,
    MethodLong = 45,
    GetPropertyLong = 46,
    SetPropertyLong = 47,
    GetSuperLong = 48,
    InvokeLong = 49,
    SuperInvokeLong = 50,
}

pub const MAX_SHORT_VALUE: usize = 255;
/// The largest index a 3-byte (`*Long`) operand can encode.
pub const MAX_LONG_VALUE: usize = 0x00FF_FFFF;
/// A `Closure` operand describing one captured variable: an `is_local` byte,
/// then the slot or enclosing upvalue index. The index is always three bytes
/// wide because a captured local may sit past slot 255.
pub const UPVALUE_OPERAND_SIZE: usize = 4;

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
            OpCode::GetLocalLong => write!(f, "OP_GET_LOCAL_LONG"),
            OpCode::SetLocalLong => write!(f, "OP_SET_LOCAL_LONG"),
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
            OpCode::Invoke => write!(f, "OP_INVOKE"),
            OpCode::Inherit => write!(f, "OP_INHERIT"),
            OpCode::GetSuper => write!(f, "OP_GET_SUPER"),
            OpCode::SuperInvoke => write!(f, "OP_SUPER_INVOKE"),
            OpCode::ClassLong => write!(f, "OP_CLASS_LONG"),
            OpCode::MethodLong => write!(f, "OP_METHOD_LONG"),
            OpCode::ClosureLong => write!(f, "OP_CLOSURE_LONG"),
            OpCode::GetPropertyLong => write!(f, "OP_GET_PROPERTY_LONG"),
            OpCode::SetPropertyLong => write!(f, "OP_SET_PROPERTY_LONG"),
            OpCode::InvokeLong => write!(f, "OP_INVOKE_LONG"),
            OpCode::GetSuperLong => write!(f, "OP_GET_SUPER_LONG"),
            OpCode::SuperInvokeLong => write!(f, "OP_SUPER_INVOKE_LONG"),
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct Chunk {
    pub code: Rc<Vec<u8>>,
    pub constants: Rc<Vec<LoxValue>>,
    pub(crate) lines: Rc<Vec<usize>>,
}

impl PartialEq for Chunk {
    fn eq(&self, other: &Self) -> bool {
        self.code.as_ref() == other.code.as_ref()
            && self.constants.as_ref() == other.constants.as_ref()
            && self.lines.as_ref() == other.lines.as_ref()
    }
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Rc::new(vec![]),
            constants: Rc::new(vec![]),
            lines: Rc::new(vec![]),
        }
    }

    pub fn first_line(&self) -> usize {
        *self.lines.first().unwrap_or(&1)
    }

    pub fn write_code(&mut self, code: OpCode, line: usize) {
        self.write_byte(code as u8, line);
    }

    pub fn write_byte(&mut self, value: u8, line: usize) {
        Rc::make_mut(&mut self.code).push(value);
        Rc::make_mut(&mut self.lines).push(line);
    }

    /// Writes a 3-byte little-endian operand. `value` must not exceed [`MAX_LONG_VALUE`].
    pub fn write_u24(&mut self, value: usize, line: usize) {
        debug_assert!(value <= MAX_LONG_VALUE);
        for b in into_three_bytes(value) {
            self.write_byte(b, line);
        }
    }

    #[inline]
    pub fn read_opcode(&self, offset: usize) -> Result<OpCode, RuntimeError> {
        OpCode::from_u8(self.code[offset]).ok_or(RuntimeError::InvalidInstruction(offset))
    }

    #[inline]
    pub fn read_constant(&self, offset: usize, constant_size: usize) -> LoxValue {
        let ix = self.get_constant_ix(offset, constant_size);
        self.constants[ix]
    }

    #[inline]
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
        let code = Rc::make_mut(&mut self.code);
        let jump = code.len() - 2 - offset;
        let bytes = into_two_bytes(jump);
        code[offset] = bytes[0];
        code[offset + 1] = bytes[1];
    }

    pub fn write_two_bytes(&mut self, value: usize, line: usize) {
        let bytes = into_two_bytes(value);
        let code = Rc::make_mut(&mut self.code);
        let lines = Rc::make_mut(&mut self.lines);
        code.push(bytes[0]);
        lines.push(line);
        code.push(bytes[1]);
        lines.push(line);
    }

    pub fn disassembly(&self, name: &str, store: &ObjectStore) {
        println!("=== {name} ===");
        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassembly_instruction(offset, store);
        }
    }

    pub fn disassembly_instruction(&self, offset: usize, store: &ObjectStore) -> usize {
        let Some(code) = OpCode::from_u8(self.code[offset]) else {
            return offset + 1;
        };
        print!("{offset:04} ");
        let line_ix = offset;
        if line_ix > 0 && self.lines[line_ix] == self.lines[line_ix - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[line_ix]);
        }
        match code {
            OpCode::Constant
            | OpCode::DefineGlobal
            | OpCode::GetGlobal
            | OpCode::SetGlobal
            | OpCode::GetSuper
            | OpCode::Class
            | OpCode::Method
            | OpCode::GetProperty
            | OpCode::SetProperty => self.disassembly_constant(offset, &code, 1),
            OpCode::SetLocal
            | OpCode::GetLocal
            | OpCode::Call
            | OpCode::GetUpvalue
            | OpCode::SetUpvalue => self.disassembly_byte_instruction(offset, &code, 1),
            OpCode::GetLocalLong | OpCode::SetLocalLong => {
                self.disassembly_byte_instruction(offset, &code, 3)
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
            | OpCode::CloseUpvalue
            | OpCode::Inherit
            | OpCode::Pop => self.disassembly_simple_instruction(offset, &code),
            OpCode::GetGlobalLong
            | OpCode::SetGlobalLong
            | OpCode::DefineGlobalLong
            | OpCode::ConstantLong
            | OpCode::GetSuperLong
            | OpCode::ClassLong
            | OpCode::MethodLong
            | OpCode::GetPropertyLong
            | OpCode::SetPropertyLong => self.disassembly_constant(offset, &code, 3),
            OpCode::JumpIfFalse | OpCode::Jump => {
                self.disassembly_jump_instruction(offset, &code, 1)
            }
            OpCode::Loop => self.disassembly_jump_instruction(offset, &code, -1),
            OpCode::Closure => self.disassembly_closure_instruction(offset, &code, store, 1),
            OpCode::ClosureLong => self.disassembly_closure_instruction(offset, &code, store, 3),
            OpCode::Invoke | OpCode::SuperInvoke => {
                self.disassembly_invoke_instruction(offset, &code, 1)
            }
            OpCode::InvokeLong | OpCode::SuperInvokeLong => {
                self.disassembly_invoke_instruction(offset, &code, 3)
            }
        }
    }

    #[inline]
    pub fn line(&self, offset: usize) -> usize {
        self.lines[offset]
    }

    pub fn add_constant(&mut self, value: LoxValue) -> usize {
        let constants = Rc::make_mut(&mut self.constants);
        if let Some((constant_index, _)) = constants.iter().enumerate().find(|(_, c)| *c == &value)
        {
            constant_index
        } else {
            constants.push(value);
            constants.len() - 1
        }
    }

    fn disassembly_byte_instruction(
        &self,
        offset: usize,
        code: &OpCode,
        operand_size: usize,
    ) -> usize {
        let ix = self.get_constant_ix(offset + 1, operand_size);
        println!("{:<16} {ix:4}", code.to_string());
        offset + 1 + operand_size
    }

    fn disassembly_invoke_instruction(
        &self,
        offset: usize,
        code: &OpCode,
        constant_size: usize,
    ) -> usize {
        let constant = self.get_constant_ix(offset + 1, constant_size);
        let arg_count = self.code[offset + 1 + constant_size];
        let val = &self.constants[constant];
        println!(
            "{:<16}    ({arg_count} args) {constant:4} '{val}'",
            code.to_string()
        );
        offset + constant_size + 2
    }

    fn disassembly_closure_instruction(
        &self,
        offset: usize,
        code: &OpCode,
        store: &ObjectStore,
        constant_size: usize,
    ) -> usize {
        let function_ix = self.get_constant_ix(offset + 1, constant_size);

        let mut offset = offset + 1 + constant_size;
        let val = self.constants[function_ix];
        if let Ok(function_id) = val.try_function() {
            if let (Ok(function), Ok(name)) = (
                store.function(function_id),
                store
                    .function(function_id)
                    .and_then(|function| store.string(function.name)),
            ) {
                let name = name.chars.as_str();
                println!("{:<16} {function_ix:4} {name}", code.to_string());
                let upvalue_count = function.upvalue_count;
                for _ in 0..upvalue_count {
                    let is_local = self.code[offset];
                    let is_local = if is_local == 1 { "local" } else { "upvalue" };
                    let index = self.read_three_bytes(offset + 1);
                    println!("{offset:04}    |                     {is_local} {index}");
                    offset += UPVALUE_OPERAND_SIZE;
                }
            } else {
                println!("{:<16} {function_ix:4}", code.to_string());
            }
        } else {
            println!("{:<16} {function_ix:4}", code.to_string());
        }
        offset
    }

    fn disassembly_jump_instruction(&self, offset: usize, code: &OpCode, sign: i32) -> usize {
        let jump = self.read_short(offset + 1);

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
    let op3 = ((value & 0x00FF_0000) >> 16) as u8;
    [op1, op2, op3]
}

fn into_two_bytes(value: usize) -> [u8; 2] {
    let op1 = (value & 0xFF) as u8;
    let op2 = ((value & 0xFF00) >> 8) as u8;
    [op1, op2]
}
