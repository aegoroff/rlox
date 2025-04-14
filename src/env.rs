use crate::ast::LoxValue;
use miette::miette;
use std::collections::HashMap;

#[derive(Default)]
pub struct Environment<'a> {
    storage: HashMap<&'a str, LoxValue>,
    enclosing: Option<Box<Environment<'a>>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            storage: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn child(enclosing: Box<Environment<'a>>) -> Self {
        Self {
            storage: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn get(&'a self, id: &'a str) -> miette::Result<&'a LoxValue> {
        if let Some(var) = self.storage.get(id) {
            Ok(var)
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.get(id)
        } else {
            Err(miette!("Undefined variable: '{id}'"))
        }
    }

    pub fn define(&mut self, id: &'a str, initializer: LoxValue) {
        if self.storage.contains_key(id) {
            let _ = self.assign(id, initializer);
        } else {
            self.storage.entry(id).or_insert(initializer);
        }
    }

    pub fn assign(&mut self, id: &'a str, initializer: LoxValue) -> miette::Result<()> {
        if self.storage.contains_key(id) {
            self.storage.entry(id).and_modify(|e| *e = initializer);
            Ok(())
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.assign(id, initializer)
        } else {
            Err(miette!("assignment to undefined variable '{id}'"))
        }
    }
}
