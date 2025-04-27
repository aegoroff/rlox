#![allow(clippy::borrowed_box)]

use miette::miette;
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    ast::{LoxValue, Stmt},
    env::Environment,
};

pub enum CallResult<'a> {
    Value(LoxValue),
    Code(&'a miette::Result<Stmt<'a>>, Rc<RefCell<Environment>>),
}

pub trait LoxCallable<'a> {
    fn arity(&self) -> usize;
    fn name(&self) -> &'a str;
    fn call(&self, arguments: Vec<LoxValue>) -> miette::Result<CallResult<'a>>;
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

    pub fn get(&self, id: &str) -> miette::Result<Rc<RefCell<dyn LoxCallable<'a> + 'a>>> {
        if let Some(var) = self.storage.get(id) {
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

    fn call(&self, _: Vec<LoxValue>) -> miette::Result<CallResult<'a>> {
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
    body: &'a miette::Result<Stmt<'a>>,
    closure: Rc<RefCell<Environment>>,
}

impl<'a> LoxCallable<'a> for Function<'a> {
    fn arity(&self) -> usize {
        self.parameters.len()
    }

    fn call(&self, arguments: Vec<LoxValue>) -> miette::Result<CallResult<'a>> {
        let expected = self.arity();
        let actual = arguments.len();
        if expected != actual {
            return Err(miette!(
                "Invalid arguments number passed to '{}'. Expected: {} passed: {}",
                self.name,
                expected,
                actual
            ));
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
        body: &'a miette::Result<Stmt<'a>>,
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
}

impl<'a> LoxCallable<'a> for Class {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, args: Vec<LoxValue>) -> miette::Result<CallResult<'a>> {
        if args.len() == 1 {
            let method = &args[0];
            if let LoxValue::String(method) = method {
                Ok(CallResult::Value(LoxValue::Instance(
                    self.name.clone(),
                    method.clone(),
                )))
            } else {
                Ok(CallResult::Value(LoxValue::Class(self.name.clone())))
            }
        } else {
            Ok(CallResult::Value(LoxValue::Class(self.name.clone())))
        }
    }

    fn name(&self) -> &'a str {
        ""
    }
}

impl Class {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}
