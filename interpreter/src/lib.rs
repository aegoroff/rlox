pub mod ast;
mod call;
mod env;
pub mod int;
pub mod parser;
pub mod resolver;

use miette::Diagnostic;
use thiserror::Error;

pub type Result<T, E = LoxError> = core::result::Result<T, E>;

#[derive(Debug, Error, Diagnostic)]
#[error("Program error")]
#[diagnostic()]
pub enum LoxError {
    Error(miette::Report),
    /// Unwinds execution up to the enclosing call. The returned value is kept
    /// by the interpreter because runtime values borrow the syntax tree.
    Return,
}

impl LoxError {
    /// Rebuilds the error so a stored diagnostic (e.g. a parse error kept in
    /// the syntax tree) can be reported again. `miette::Report` is not `Clone`.
    pub(crate) fn duplicate(&self) -> LoxError {
        match self {
            LoxError::Error(report) => {
                let labels: Vec<miette::LabeledSpan> =
                    report.labels().into_iter().flatten().collect();
                LoxError::Error(miette::miette!(labels = labels, "{report}"))
            }
            LoxError::Return => LoxError::Return,
        }
    }
}
