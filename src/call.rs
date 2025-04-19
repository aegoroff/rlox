#![allow(clippy::borrowed_box)]

use miette::miette;
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::ast::{LoxCallable, Stmt};

pub const CLOCK: &str = "clock";

pub struct Catalogue<'a> {
    storage: HashMap<&'a str, Rc<RefCell<dyn LoxCallable + 'a>>>,
}

impl<'a> Catalogue<'a> {
    pub fn new() -> Self {
        Self {
            storage: HashMap::new(),
        }
    }

    pub fn get(&mut self, id: &str) -> miette::Result<Rc<RefCell<dyn LoxCallable + 'a>>> {
        if let Some(var) = self.storage.get_mut(id) {
            Ok(var.clone())
        } else {
            Err(miette!("Undefined identifier: '{id}'"))
        }
    }

    pub fn define(&mut self, id: &'a str, initializer: Rc<RefCell<dyn LoxCallable + 'a>>) {
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

pub struct Function<'a> {
    arity: usize,
    body: &'a Stmt<'a>,
}

impl<'a> LoxCallable for Function<'a> {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(&mut self, arguments: &[crate::ast::LoxValue]) -> crate::ast::LoxValue {
        todo!()
    }
}

impl<'a> Function<'a> {
    pub fn new(arity: usize, body: &'a Stmt<'a>) -> Self {
        Self { arity, body }
    }
}
