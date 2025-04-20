#![allow(clippy::borrowed_box)]

use miette::miette;
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::ast::{CallResult, LoxCallable, LoxValue, Stmt};

pub const CLOCK: &str = "clock";

pub struct Catalogue<'a> {
    storage: HashMap<&'a str, Rc<RefCell<dyn LoxCallable<'a> + 'a>>>,
}

impl<'a> Catalogue<'a> {
    pub fn new() -> Self {
        Self {
            storage: HashMap::new(),
        }
    }

    pub fn get(&mut self, id: &str) -> miette::Result<Rc<RefCell<dyn LoxCallable<'a> + 'a>>> {
        if let Some(var) = self.storage.get_mut(id) {
            Ok(var.clone())
        } else {
            Err(miette!("Undefined identifier: '{id}'"))
        }
    }

    pub fn define(&mut self, id: &'a str, initializer: Rc<RefCell<dyn LoxCallable<'a> + 'a>>) {
        self.storage.entry(id).or_insert(initializer);
    }
}

pub struct Clock {}

impl<'a> LoxCallable<'a> for Clock {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _: Vec<LoxValue>) -> CallResult<'a> {
        let start = SystemTime::now();
        let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap_or_default();
        let seconds = since_the_epoch.as_secs();
        let val = LoxValue::Number(seconds as f64);
        CallResult::Value(val)
    }
}

pub struct Function<'a> {
    parameters: Vec<&'a str>,
    body: &'a miette::Result<Stmt<'a>>,
}

impl<'a> LoxCallable<'a> for Function<'a> {
    fn arity(&self) -> usize {
        self.parameters.len()
    }

    fn call(&self, arguments: Vec<LoxValue>) -> CallResult<'a> {
        let mapping: Vec<(String, LoxValue)> = self
            .parameters
            .iter()
            .enumerate()
            .map(|(i, v)| (v.to_string(), arguments[i].clone()))
            .collect();
        CallResult::Code(self.body, mapping)
    }
}

impl<'a> Function<'a> {
    pub fn new(parameters: Vec<&'a str>, body: &'a miette::Result<Stmt<'a>>) -> Self {
        Self { parameters, body }
    }
}
