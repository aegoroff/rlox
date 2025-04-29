#![allow(clippy::borrowed_box)]

use miette::miette;
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    LoxError,
    ast::{LoxValue, Stmt},
    env::Environment,
};

pub enum CallResult<'a> {
    Value(LoxValue),
    Code(&'a crate::Result<Stmt<'a>>, Rc<RefCell<Environment>>),
    Instance(String, Rc<RefCell<Environment>>),
}

pub trait LoxCallable<'a> {
    fn arity(&self) -> usize;
    fn name(&self) -> &'a str;
    fn call(&self, arguments: Vec<LoxValue>) -> crate::Result<CallResult<'a>>;
}

pub const CLOCK: &str = "clock";

#[derive(Default)]
pub struct Catalogue<'a> {
    storage: HashMap<&'a str, Rc<RefCell<dyn LoxCallable<'a> + 'a>>>,
}

impl<'a> Catalogue<'a> {
    pub fn new() -> Self {
        Self {
            storage: HashMap::new(),
        }
    }

    pub fn get(&self, id: &str) -> crate::Result<Rc<RefCell<dyn LoxCallable<'a> + 'a>>> {
        if let Some(var) = self.storage.get(id) {
            Ok(var.clone())
        } else {
            let report = miette!("Undefined identifier: '{id}'");
            Err(LoxError::Error(report))
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

    fn call(&self, _: Vec<LoxValue>) -> crate::Result<CallResult<'a>> {
        let start = SystemTime::now();
        let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap_or_default();
        let seconds = since_the_epoch.as_secs();
        let val = LoxValue::Number(seconds as f64);
        Ok(CallResult::Value(val))
    }

    fn name(&self) -> &'a str {
        ""
    }
}

pub struct Function<'a> {
    pub name: &'a str,
    parameters: Vec<&'a str>,
    body: &'a crate::Result<Stmt<'a>>,
    closure: Rc<RefCell<Environment>>,
}

impl<'a> LoxCallable<'a> for Function<'a> {
    fn arity(&self) -> usize {
        self.parameters.len()
    }

    fn call(&self, arguments: Vec<LoxValue>) -> crate::Result<CallResult<'a>> {
        let expected = self.arity();
        let actual = arguments.len();
        if expected != actual {
            let report = miette!(
                "Invalid arguments number passed to '{}'. Expected: {} passed: {}",
                self.name,
                expected,
                actual
            );
            return Err(LoxError::Error(report));
        }

        // We need new closure here to support recursive calls for example fibonacci calculation
        let closure = Rc::new(RefCell::new(Environment::child(self.closure.clone())));

        for (i, name) in self.parameters.iter().enumerate() {
            closure
                .borrow_mut()
                .define((*name).to_string(), arguments[i].clone());
        }

        Ok(CallResult::Code(self.body, closure))
    }

    fn name(&self) -> &'a str {
        self.name
    }
}

impl<'a> Function<'a> {
    pub fn new(
        name: &'a str,
        parameters: Vec<&'a str>,
        body: &'a crate::Result<Stmt<'a>>,
        closure: Rc<RefCell<Environment>>,
    ) -> Self {
        Self {
            name,
            parameters,
            body,
            closure,
        }
    }
}

pub struct Class {
    name: String,
    closure: Rc<RefCell<Environment>>,
}

impl<'a> LoxCallable<'a> for Class {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, arguments: Vec<LoxValue>) -> crate::Result<CallResult<'a>> {
        let closure = Rc::new(RefCell::new(Environment::child(self.closure.clone())));
        closure
            .borrow_mut()
            .define("this".to_string(), LoxValue::Nil);

        for (i, name) in arguments.iter().enumerate() {
            closure
                .borrow_mut()
                .define((*name).to_string(), arguments[i].clone());
        }

        Ok(CallResult::Instance(self.name.clone(), closure))
    }

    fn name(&self) -> &'a str {
        ""
    }
}

impl Class {
    pub fn new(name: String, closure: Rc<RefCell<Environment>>) -> Self {
        Self { name, closure }
    }
}
