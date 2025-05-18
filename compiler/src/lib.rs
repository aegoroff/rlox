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
    InstructionsStackEmpty,
    NotEnoughStackCapacity(usize, usize),
    DivizionByZero,
    InvalidCallable,
    OperandsMustBeNumbers,
    ExpectedNumber,
    ExpectedString,
    ExpectedBool,
}

impl Display for ProgramError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProgramError::Compile(s) => write!(f, "{s}"),
            ProgramError::Runtime(s) => write!(f, "{s}"),
            ProgramError::InvalidInstruction(code) => write!(f, "Invalid instruction: {code}"),
            ProgramError::InstructionsStackEmpty => write!(f, "Instructions stack empty"),
            ProgramError::NotEnoughStackCapacity(distance, size) => write!(
                f,
                "Not enough stack capacity for distance {distance}. Current stack size is {size}"
            ),
            ProgramError::DivizionByZero => write!(f, "Division by zero"),
            ProgramError::InvalidCallable => write!(f, "Can only call functions and classes."),
            ProgramError::OperandsMustBeNumbers => write!(f, "Operands must be numbers"),
            ProgramError::ExpectedNumber => write!(f, "Expected number"),
            ProgramError::ExpectedString => write!(f, "Expected string"),
            ProgramError::ExpectedBool => write!(f, "Expected boolean"),
        }
    }
}
