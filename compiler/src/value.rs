#![allow(clippy::missing_errors_doc)]

use std::fmt::Display;

use crate::{CompileError, chunk::Chunk};

#[derive(Clone, Debug, PartialEq)]
pub enum LoxValue {
    String(String),
    Number(f64),
    Bool(bool),
    Function(Function),
    Native(NativeFunction),
    Nil,
}

const ERROR_MARGIN: f64 = 0.00001;

impl LoxValue {
    #[must_use]
    pub fn equal(&self, other: &LoxValue) -> bool {
        if let Ok(l) = self.try_num() {
            let Ok(r) = other.try_num() else {
                return false;
            };
            (l - r).abs() < ERROR_MARGIN
        } else if let Ok(l) = self.try_bool() {
            let Ok(r) = other.try_bool() else {
                return false;
            };
            l == r
        } else if let Ok(l) = self.try_str() {
            let Ok(r) = other.try_str() else {
                return false;
            };
            l == r
        } else if let LoxValue::Nil = self {
            matches!(other, LoxValue::Nil)
        } else if let LoxValue::Nil = other {
            matches!(self, LoxValue::Nil)
        } else {
            false
        }
    }

    pub fn less(&self, other: &LoxValue) -> crate::Result<bool> {
        if let Ok(l) = self.try_num() {
            let r = other.try_num()?;
            Ok(l < r)
        } else if let Ok(l) = self.try_bool() {
            let r = other.try_bool()?;
            Ok(!l & r)
        } else if let Ok(l) = self.try_str() {
            let r = other.try_str()?;
            Ok(l < r)
        } else {
            Err(CompileError::CompileError(miette::miette!(
                "Operands must be numbers"
            )))
        }
    }

    pub fn try_num(&self) -> crate::Result<f64> {
        if let LoxValue::Number(n) = self {
            Ok(*n)
        } else {
            Err(CompileError::CompileError(miette::miette!(
                "Expected number. But was {self}"
            )))
        }
    }

    pub fn try_str(&self) -> crate::Result<&String> {
        if let LoxValue::String(s) = self {
            Ok(s)
        } else {
            Err(CompileError::CompileError(miette::miette!(
                "Expected string. But was {self}"
            )))
        }
    }

    pub fn try_bool(&self) -> crate::Result<bool> {
        match self {
            LoxValue::Bool(b) => Ok(*b),
            LoxValue::Nil => Ok(false),
            _ => Err(CompileError::CompileError(miette::miette!(
                "Expected boolean. But was {self}"
            ))),
        }
    }

    #[must_use]
    pub fn is_truthy(&self) -> bool {
        self.try_bool().unwrap_or(true)
    }
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::String(s) => write!(f, "{s}"),
            LoxValue::Number(n) => write!(f, "{n}"),
            LoxValue::Bool(b) => write!(f, "{b}"),
            LoxValue::Nil => write!(f, ""),
            LoxValue::Function(func) => write!(f, "{func}"),
            LoxValue::Native(native) => write!(f, "{native}"),
        }
    }
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl Function {
    #[must_use]
    pub fn new(name: &str) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: name.to_owned(),
        }
    }

    pub fn disassembly(&self) {
        self.chunk.disassembly(self.name.as_str());
    }
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct NativeFunction {
    pub arity: usize,
    pub name: String,
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn {}>", self.name)
    }
}

impl NativeFunction {
    #[must_use]
    pub fn new(name: &str, arity: usize) -> Self {
        Self {
            arity,
            name: name.to_owned(),
        }
    }
}
