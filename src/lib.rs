pub mod ast;
mod call;
mod env;
pub mod int;
pub mod lexer;
pub mod parser;
pub mod resolver;

use ast::LoxValue;
use miette::Diagnostic;
use thiserror::Error;

pub type Result<T, E = LoxError> = core::result::Result<T, E>;

#[derive(Debug, Error, Diagnostic)]
#[error("Program error")]
#[diagnostic()]
pub enum LoxError {
    Error(miette::Report),
    Return(LoxValue),
}
