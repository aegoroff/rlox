use crate::ast::LoxValue;
use miette::miette;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Default)]
pub struct Environment {
    values: HashMap<String, LoxValue>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn child(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn get(&self, id: &str) -> miette::Result<LoxValue> {
        if let Some(var) = self.values.get(id) {
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
            let mut parent = self.enclosing.clone();
            for _ in 1..distance {
                if let Some(e) = parent {
                    parent = e.borrow().enclosing.clone();
                }
            }
            if let Some(e) = parent {
                e.borrow().get(id)
            } else {
                Err(miette!("Undefined identifier: '{id}'"))
            }
        }
    }

    pub fn define(&mut self, id: String, initializer: LoxValue) {
        if self.values.contains_key(&id) {
            let _ = self.assign(id, initializer);
        } else {
            self.values.entry(id).or_insert(initializer);
        }
    }

    pub fn assign(&mut self, id: String, initializer: LoxValue) -> miette::Result<()> {
        if self.values.contains_key(&id) {
            self.values.entry(id).and_modify(|e| *e = initializer);
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
            let mut parent = self.enclosing.clone();
            for _ in 1..distance {
                if let Some(e) = parent {
                    parent = e.borrow().enclosing.clone();
                }
            }
            if let Some(e) = parent {
                e.borrow_mut().assign(id, initializer)
            } else {
                Err(miette!("Undefined identifier: '{id}'"))
            }
        }
    }
}
