use std::fmt::Display;

#[repr(u8)]
pub enum OpCode {
    Constant = 0,
    Return = 1,
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

    pub fn add_constant(&mut self, value: LoxValue) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn disassembly(&self, name: &str) {
        println!("=== {name} ===");
        for (offset, instruction) in self.instructions.iter().enumerate() {
            Chunk::disassembly_instruction(instruction, offset);
        }
    }

    fn disassembly_instruction(code: &u8, offset: usize) {
        println!("{offset:04} {code}");
    }
}
