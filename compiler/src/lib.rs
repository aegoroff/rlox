use std::fmt::Display;

use object::ObjId;
use value::LoxValue;

mod builtin;
mod chunk;
pub mod compile;
pub mod object;
pub mod value;
pub mod vm;

extern crate num_derive;

pub type Result<T, E = miette::Report> = core::result::Result<T, E>;

#[derive(Debug)]
pub enum RuntimeError {
    Common(String),
    InvalidInstruction(usize),
    InstructionsStackEmpty,
    NotEnoughStackCapacity(usize, usize),
    InvalidCallable(LoxValue),
    OperandsMustBeNumbers(LoxValue, LoxValue),
    OperandsMustBeNumbersOrStrings,
    ExpectedNumber(LoxValue),
    ExpectedString(LoxValue),
    ExpectedClass(LoxValue),
    ExpectedBool(LoxValue),
    ExpectedFunction(LoxValue),
    ExpectedClosure(LoxValue),
    ExpectedNative(LoxValue),
    ExpectedBoundMethod(LoxValue),
    ExpectedInstance(LoxValue),
    ExpectedInstanceField,
    InvalidFunctionArgsCount(usize, usize),
    UndefinedGlobal(String),
    UndefinedMethodOrProperty(String),
    StackOverflow,
    ObjectUnavailable(ObjId),
    TooManyObjects,
    UnexpectedObjectType(ObjId, crate::object::ObjType),
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
            RuntimeError::InvalidCallable(_) => {
                write!(f, "Can only call functions and classes.")
            }
            RuntimeError::OperandsMustBeNumbers(first, second) => {
                write!(
                    f,
                    "Operands must be numbers. But first is '{first}' and second is '{second}'"
                )
            }
            RuntimeError::OperandsMustBeNumbersOrStrings => {
                write!(f, "Operands must be two numbers or two strings.")
            }
            RuntimeError::ExpectedNumber(val) => write!(f, "Expected number but was '{val}'"),
            RuntimeError::ExpectedString(val) => write!(f, "Expected string but was '{val}'"),
            RuntimeError::ExpectedClass(val) => write!(f, "Expected class but was '{val}'"),
            RuntimeError::ExpectedBool(val) => write!(f, "Expected boolean but was '{val}'"),
            RuntimeError::ExpectedFunction(val) => write!(f, "Expected function but was '{val}'"),
            RuntimeError::ExpectedClosure(val) => write!(f, "Expected closure but was '{val}'"),
            RuntimeError::ExpectedNative(val) => write!(f, "Expected native but was '{val}'"),
            RuntimeError::ExpectedBoundMethod(val) => {
                write!(f, "Expected bound method but was '{val}'")
            }
            RuntimeError::InvalidFunctionArgsCount(arity, args_count) => {
                write!(f, "Expected {arity} arguments but got {args_count}")
            }
            RuntimeError::UndefinedGlobal(value) => {
                write!(f, "Undefined variable '{value}'.")
            }
            RuntimeError::ExpectedInstance(_) => {
                write!(f, "Only instances have properties.")
            }
            RuntimeError::ExpectedInstanceField => {
                write!(f, "Only instances have fields.")
            }
            RuntimeError::UndefinedMethodOrProperty(value) => {
                write!(f, "Undefined property '{value}'.")
            }
            RuntimeError::StackOverflow => {
                write!(f, "Stack overflow. Too much nestings call max available 64")
            }
            RuntimeError::ObjectUnavailable(id) => {
                write!(f, "Object {id} is no longer available")
            }
            RuntimeError::TooManyObjects => write!(f, "Too many heap objects"),
            RuntimeError::UnexpectedObjectType(id, ty) => {
                write!(f, "Object {id} has unexpected type (expected {ty:?})")
            }
        }
    }
}
