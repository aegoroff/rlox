mod chunk;
pub mod compile;
pub mod value;
pub mod vm;

use miette::Diagnostic;
use thiserror::Error;

extern crate num_derive;

pub type Result<T, E = CompileError> = core::result::Result<T, E>;

#[derive(Debug, Error, Diagnostic)]
#[error("Program error")]
#[diagnostic()]
pub enum CompileError {
    CompileError(miette::Report),
    RuntimeError(miette::Report),
}
