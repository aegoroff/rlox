use int::LoxError;

pub mod ast;
mod call;
mod env;
pub mod int;
pub mod lexer;
pub mod parser;
pub mod resolver;

pub type Result<T, E = LoxError> = core::result::Result<T, E>;
