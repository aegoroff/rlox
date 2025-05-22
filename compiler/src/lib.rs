use std::fmt::Display;

mod builtin;
mod chunk;
pub mod compile;
pub mod value;
pub mod vm;

extern crate num_derive;

pub type Result<T, E = miette::Report> = core::result::Result<T, E>;

pub enum ProgramError {
    Runtime(String),
    InvalidInstruction(usize),
    InstructionsStackEmpty,
    NotEnoughStackCapacity(usize, usize),
    DivisionByZero,
    InvalidCallable,
    OperandsMustBeNumbers,
    ExpectedNumber,
    ExpectedString,
    ExpectedBool,
    ExpectedFunction,
    InvalidFunctionArgsCount(usize, usize),
    UndefinedGlobal(String),
}

impl Display for ProgramError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProgramError::Runtime(s) => write!(f, "{s}"),
            ProgramError::InvalidInstruction(code) => write!(f, "Invalid instruction: {code}"),
            ProgramError::InstructionsStackEmpty => write!(f, "Instructions stack empty"),
            ProgramError::NotEnoughStackCapacity(distance, size) => write!(
                f,
                "Not enough stack capacity for distance {distance}. Current stack size is {size}"
            ),
            ProgramError::DivisionByZero => write!(f, "Division by zero"),
            ProgramError::InvalidCallable => write!(f, "Can only call functions and classes."),
            ProgramError::OperandsMustBeNumbers => write!(f, "Operands must be numbers"),
            ProgramError::ExpectedNumber => write!(f, "Expected number"),
            ProgramError::ExpectedString => write!(f, "Expected string"),
            ProgramError::ExpectedBool => write!(f, "Expected boolean"),
            ProgramError::ExpectedFunction => write!(f, "Expected function"),
            ProgramError::InvalidFunctionArgsCount(arity, args_count) => {
                write!(f, "Expected {arity} arguments but got {args_count}")
            }
            ProgramError::UndefinedGlobal(value) => {
                write!(f, "Undefined global variable '{value}'")
            }
        }
    }
}
