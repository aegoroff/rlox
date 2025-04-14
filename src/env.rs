use crate::ast::Expr;
use miette::miette;
use std::collections::HashMap;

#[derive(Default)]
pub struct Environment<'a> {
    storage: HashMap<&'a str, Option<Box<Expr<'a>>>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            storage: HashMap::new(),
        }
    }

    pub fn get(&'a self, id: &'a str) -> miette::Result<&'a Option<Box<Expr<'a>>>> {
        if let Some(var) = self.storage.get(id) {
            Ok(var)
        } else {
            Err(miette!("Undefined variable: '{id}'"))
        }
    }

    pub fn define(&mut self, id: &'a str, initializer: Option<Box<Expr<'a>>>) {
        if self.storage.contains_key(id) {
            self.storage.entry(id).and_modify(|e| *e = initializer);
        } else {
            self.storage.entry(id).or_insert(initializer);
        }
    }
}
