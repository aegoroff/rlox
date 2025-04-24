use crate::ast::LoxValue;
use miette::miette;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Default)]
pub struct Environment {
    storage: HashMap<String, LoxValue>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            storage: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn child(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            storage: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn get(&self, id: &str) -> miette::Result<LoxValue> {
        if let Some(var) = self.storage.get(id) {
            Ok(var.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(id)
        } else {
            Err(miette!("Undefined identifier: '{id}'"))
        }
    }

    pub fn get_at(&self, distance: usize, id: &str) -> miette::Result<LoxValue> {
        if distance == 0 {
            self.get(id)
        } else {
            let mut child = self.enclosing.clone();
            for _ in 1..distance {
                if let Some(e) = child {
                    child = e.borrow().enclosing.clone();
                }
            }
            if let Some(e) = child {
                e.borrow().get(id)
            } else {
                Err(miette!("Undefined identifier: '{id}'"))
            }
        }
    }

    pub fn define(&mut self, id: String, initializer: LoxValue) {
        if self.storage.contains_key(&id) {
            let _ = self.assign(id, initializer);
        } else {
            self.storage.entry(id).or_insert(initializer);
        }
    }

    pub fn assign(&mut self, id: String, initializer: LoxValue) -> miette::Result<()> {
        if self.storage.contains_key(&id) {
            self.storage.entry(id).and_modify(|e| *e = initializer);
            Ok(())
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.borrow_mut().assign(id, initializer)
        } else {
            Err(miette!("assignment to undefined variable '{id}'"))
        }
    }

    pub fn assign_at(
        &mut self,
        distance: usize,
        id: String,
        initializer: LoxValue,
    ) -> miette::Result<()> {
        if distance == 0 {
            self.assign(id, initializer)
        } else {
            let mut child = self.enclosing.clone();
            for _ in 1..distance {
                if let Some(e) = child {
                    child = e.borrow().enclosing.clone();
                }
            }
            if let Some(e) = child {
                e.borrow_mut().assign(id, initializer)
            } else {
                Err(miette!("Undefined identifier: '{id}'"))
            }
        }
    }
}
