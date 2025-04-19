#![allow(clippy::borrowed_box)]

use miette::miette;
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::ast::{Interpreter, LoxCallable};

pub const CLOCK: &str = "clock";

pub struct Catalogue<'a> {
    storage: HashMap<&'a str, Box<dyn LoxCallable>>,
}

impl<'a> Catalogue<'a> {
    pub fn new() -> Self {
        Self {
            storage: HashMap::new(),
        }
    }

    pub fn get(&mut self, id: &str) -> miette::Result<&mut Box<dyn LoxCallable>> {
        if let Some(var) = self.storage.get_mut(id) {
            Ok(var)
        } else {
            Err(miette!("Undefined identifier: '{id}'"))
        }
    }

    pub fn define(&mut self, id: &'a str, initializer: Box<dyn LoxCallable>) {
        self.storage.entry(id).or_insert(initializer);
    }
}

pub struct Clock {}

impl LoxCallable for Clock {
    fn arity(&self) -> usize {
        0
    }

    fn call(&mut self, _: &[crate::ast::LoxValue]) -> crate::ast::LoxValue {
        let start = SystemTime::now();
        let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap_or_default();
        let seconds = since_the_epoch.as_secs();
        crate::ast::LoxValue::Number(seconds as f64)
    }
}

pub struct Function<'a, W: std::io::Write> {
    interpreter: Rc<RefCell<Interpreter<'a, W>>>,
    arity: usize,
}

impl<W: std::io::Write> LoxCallable for Function<'_, W> {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(&mut self, arguments: &[crate::ast::LoxValue]) -> crate::ast::LoxValue {
        todo!()
    }
}

impl<'a, W: std::io::Write> Function<'a, W> {
    pub fn new(interpreter: Rc<RefCell<Interpreter<'a, W>>>, arity: usize) -> Self {
        Self { interpreter, arity }
    }
}
