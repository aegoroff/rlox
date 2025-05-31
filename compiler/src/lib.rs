use std::fmt::Display;

use value::LoxValue;

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
    InvalidCallable(LoxValue),
    OperandsMustBeNumbers,
    ExpectedNumber,
    ExpectedString,
    ExpectedClass,
    ExpectedBool,
    ExpectedFunction,
    ExpectedInstance,
    InvalidFunctionArgsCount(usize, usize),
    UndefinedGlobal(String),
    UndefinedMethodOrProperty(String),
    StackOverflow,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Common(s) => write!(f, "{s}"),
            RuntimeError::InvalidInstruction(code) => {
                write!(f, "Invalid instruction: {code}")
            }
            RuntimeError::InstructionsStackEmpty => {
                write!(f, "Instructions stack empty")
            }
            RuntimeError::NotEnoughStackCapacity(distance, size) => write!(
                f,
                "Not enough stack capacity for distance {distance}. Current stack size is {size}"
            ),
            RuntimeError::InvalidCallable(value) => {
                write!(f, "Can only call functions and classes but was '{value}'")
            }
            RuntimeError::OperandsMustBeNumbers => {
                write!(f, "Operands must be numbers")
            }
            RuntimeError::ExpectedNumber => write!(f, "Expected number"),
            RuntimeError::ExpectedString => write!(f, "Expected string"),
            RuntimeError::ExpectedClass => write!(f, "Expected class"),
            RuntimeError::ExpectedBool => write!(f, "Expected boolean"),
            RuntimeError::ExpectedFunction => write!(f, "Expected function"),
            RuntimeError::InvalidFunctionArgsCount(arity, args_count) => {
                write!(f, "Expected {arity} arguments but got {args_count}")
            }
            RuntimeError::UndefinedGlobal(value) => {
                write!(f, "Undefined global variable '{value}'")
            }
            RuntimeError::ExpectedInstance => {
                write!(f, "Only instances can have properties")
            }
            RuntimeError::UndefinedMethodOrProperty(value) => {
                write!(f, "Undefined method or property '{value}'")
            }
            RuntimeError::StackOverflow => {
                write!(f, "Stack overflow. Too much nestings call max available 64")
            }
        }
    }
}
