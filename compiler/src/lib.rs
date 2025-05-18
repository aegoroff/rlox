use std::fmt::Display;

mod chunk;
pub mod compile;
pub mod value;
pub mod vm;

extern crate num_derive;

pub type Result<T, E = miette::Report> = core::result::Result<T, E>;

pub enum ProgramError {
    Compile(String),
    Runtime(String),
    InvalidInstruction(usize),
}

impl Display for ProgramError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProgramError::Compile(s) => write!(f, "{s}"),
            ProgramError::Runtime(s) => write!(f, "{s}"),
            ProgramError::InvalidInstruction(code) => write!(f, "Invalid instruction: {code}"),
        }
    }
}
