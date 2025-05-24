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
    OperandsMustBeNumbers,
    ExpectedNumber,
    ExpectedString,
    ExpectedClass,
    ExpectedBool,
    ExpectedFunction(usize),
    ExpectedInstance(usize),
    InvalidFunctionArgsCount(usize, usize),
    UndefinedGlobal(usize, String),
    UndefinedMethodOrProperty(usize, String),
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
            RuntimeError::OperandsMustBeNumbers => write!(f, "Operands must be numbers"),
            RuntimeError::ExpectedNumber => write!(f, "Expected number"),
            RuntimeError::ExpectedString => write!(f, "Expected string"),
            RuntimeError::ExpectedClass => write!(f, "Expected class"),
            RuntimeError::ExpectedBool => write!(f, "Expected boolean"),
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
        }
    }
}
