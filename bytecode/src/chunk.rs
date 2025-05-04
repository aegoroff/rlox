use std::fmt::Display;

pub enum OpCode {
    Return = 0,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Return => write!(f, "OP_RETURN"),
        }
    }
}

#[derive(Default)]
pub struct Chunk {
    instructions: Vec<OpCode>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
        }
    }

    pub fn write_code(&mut self, code: OpCode) {
        self.instructions.push(code);
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
