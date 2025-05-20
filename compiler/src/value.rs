#![allow(clippy::missing_errors_doc)]

use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::{ProgramError, chunk::Chunk};

#[derive(Clone, Debug, PartialEq)]
pub enum LoxValue {
    String(String),
    Number(f64),
    Bool(bool),
    Function(Function),
    Native(NativeFunction),
    Closure(Closure),
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

    pub fn less(&self, other: &LoxValue) -> Result<bool, ProgramError> {
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
            Err(ProgramError::OperandsMustBeNumbers)
        }
    }

    pub fn try_num(&self) -> Result<f64, ProgramError> {
        if let LoxValue::Number(n) = self {
            Ok(*n)
        } else {
            Err(ProgramError::ExpectedNumber)
        }
    }

    pub fn try_str(&self) -> Result<&String, ProgramError> {
        if let LoxValue::String(s) = self {
            Ok(s)
        } else {
            Err(ProgramError::ExpectedString)
        }
    }

    pub fn try_bool(&self) -> Result<bool, ProgramError> {
        match self {
            LoxValue::Bool(b) => Ok(*b),
            LoxValue::Nil => Ok(false),
            _ => Err(ProgramError::ExpectedBool),
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
            LoxValue::Closure(closure) => write!(f, "{closure}"),
        }
    }
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Function {
    pub arity: usize,
    pub chunk: Rc<RefCell<Chunk>>,
    pub name: String,
    pub upvalue_count: usize,
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
            chunk: Rc::new(RefCell::new(Chunk::new())),
            name: name.to_owned(),
            upvalue_count: 0,
        }
    }

    pub fn disassembly(&self) {
        self.chunk.borrow().disassembly(self.name.as_str());
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

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Closure {
    pub function: Function,
    pub upvalues: Vec<Upvalue>,
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.function.name)
    }
}

impl Closure {
    #[must_use]
    pub fn new(function: Function) -> Self {
        let upvalues_count = function.upvalue_count;
        Self {
            function,
            upvalues: Vec::with_capacity(upvalues_count),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Upvalue {
    pub location: usize,
}

impl Display for Upvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "upvalue")
    }
}

impl Upvalue {
    #[must_use]
    pub fn new(location: usize) -> Self {
        Self { location }
    }
}
