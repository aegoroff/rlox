use std::fmt::Display;

mod builtin;
mod chunk;
pub mod compile;
pub mod value;
pub mod vm;

extern crate num_derive;

pub type Result<T, E = miette::Report> = core::result::Result<T, E>;

pub enum RuntimeError {
    Common(String),
    InvalidInstruction(usize),
    InstructionsStackEmpty,
    NotEnoughStackCapacity(usize, usize),
    InvalidCallable,
    OperandsMustBeNumbers(usize),
    ExpectedNumber(usize),
    ExpectedString(usize),
    ExpectedClass(usize),
    ExpectedBool(usize),
    ExpectedFunction(usize),
    ExpectedInstance(usize),
    InvalidFunctionArgsCount(usize, usize),
    UndefinedGlobal(usize, String),
    UndefinedMethodOrProperty(usize, String),
    StackOverflow,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Common(s) => write!(f, "{s}"),
            RuntimeError::InvalidInstruction(code) => write!(f, "Invalid instruction: {code}"),
            RuntimeError::InstructionsStackEmpty => write!(f, "Instructions stack empty"),
            RuntimeError::NotEnoughStackCapacity(distance, size) => write!(
                f,
                "Not enough stack capacity for distance {distance}. Current stack size is {size}"
            ),
            RuntimeError::InvalidCallable => write!(f, "Can only call functions and classes."),
            RuntimeError::OperandsMustBeNumbers(line) => {
                write!(f, "Operands must be numbers at {line}")
            }
            RuntimeError::ExpectedNumber(line) => write!(f, "Expected number at {line}"),
            RuntimeError::ExpectedString(line) => write!(f, "Expected string at {line}"),
            RuntimeError::ExpectedClass(line) => write!(f, "Expected class at {line}"),
            RuntimeError::ExpectedBool(line) => write!(f, "Expected boolean at {line}"),
            RuntimeError::ExpectedFunction(line) => write!(f, "Expected function at {line}"),
            RuntimeError::InvalidFunctionArgsCount(arity, args_count) => {
                write!(f, "Expected {arity} arguments but got {args_count}")
            }
            RuntimeError::UndefinedGlobal(line, value) => {
                write!(f, "Undefined global variable '{value}' in line {line}")
            }
            RuntimeError::ExpectedInstance(line) => {
                write!(f, "Only instances can have properties in line {line}")
            }
            RuntimeError::UndefinedMethodOrProperty(line, value) => {
                write!(f, "Undefined method or property '{value}' in line {line}")
            }
            RuntimeError::StackOverflow => {
                write!(f, "Stack overflow. Too much nestings call max available 64")
            }
        }
    }
}
