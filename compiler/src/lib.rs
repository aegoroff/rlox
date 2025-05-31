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
    OperandsMustBeNumbers(Box<LoxValue>, Box<LoxValue>),
    ExpectedNumber(LoxValue),
    ExpectedString(LoxValue),
    ExpectedClass(LoxValue),
    ExpectedBool(LoxValue),
    ExpectedFunction(LoxValue),
    ExpectedInstance(LoxValue),
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
                write!(f, "Can only call functions and classes not '{value:?}'")
            }
            RuntimeError::OperandsMustBeNumbers(first, second) => {
                write!(
                    f,
                    "Operands must be numbers. But first is '{first}' and second is '{second}'"
                )
            }
            RuntimeError::ExpectedNumber(val) => write!(f, "Expected number but was '{val:?}'"),
            RuntimeError::ExpectedString(val) => write!(f, "Expected string but was '{val:?}'"),
            RuntimeError::ExpectedClass(val) => write!(f, "Expected class but was '{val:?}'"),
            RuntimeError::ExpectedBool(val) => write!(f, "Expected boolean but was '{val:?}'"),
            RuntimeError::ExpectedFunction(val) => write!(f, "Expected function but was '{val:?}'"),
            RuntimeError::InvalidFunctionArgsCount(arity, args_count) => {
                write!(f, "Expected {arity} arguments but got {args_count}")
            }
            RuntimeError::UndefinedGlobal(value) => {
                write!(f, "Undefined global variable '{value}'")
            }
            RuntimeError::ExpectedInstance(val) => {
                write!(f, "Only instances can have properties not '{val:?}'")
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
