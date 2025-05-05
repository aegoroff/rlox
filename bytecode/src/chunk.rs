use std::fmt::Display;

pub enum OpCode {
    Return,
    Constant(usize),
}

#[derive(Clone, Debug)]
pub enum LoxValue {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Return => write!(f, "OP_RETURN"),
            OpCode::Constant(index) => write!(f, "OP_CONSTANT {}", index),
        }
    }
}

#[derive(Default)]
pub struct Chunk {
    instructions: Vec<OpCode>,
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
        self.instructions.push(code);
    }

    pub fn add_constant(&mut self, value: LoxValue) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn disasembly(&self, name: &str) {
        println!("=== {name} ===");
        for (offset, instruction) in self.instructions.iter().enumerate() {
            Chunk::disasembly_instruction(instruction, offset);
        }
    }

    fn disasembly_instruction(code: &OpCode, offset: usize) {
        println!("{offset:04} {code}");
    }
}
