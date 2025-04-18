#![allow(clippy::borrowed_box)]

use miette::miette;
use std::{
    collections::HashMap,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::ast::LoxCallable;

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

    pub fn get(&self, id: &str) -> miette::Result<&Box<dyn LoxCallable>> {
        if let Some(var) = self.storage.get(id) {
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

    fn call(&self, _: &[crate::ast::LoxValue]) -> crate::ast::LoxValue {
        let start = SystemTime::now();
        let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap_or_default();
        let seconds = since_the_epoch.as_secs();
        crate::ast::LoxValue::Number(seconds as f64)
    }
}
