use crate::{LoxError, ast::LoxValue};
use miette::miette;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Default, Debug)]
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

    pub fn get(&self, id: &str) -> crate::Result<LoxValue> {
        if let Some(var) = self.values.get(id) {
            Ok(var.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(id)
        } else {
            Err(LoxError::Error(miette!("Undefined identifier: '{id}'")))
        }
    }

    pub fn get_at(&self, distance: usize, id: &str) -> crate::Result<LoxValue> {
        if distance == 0 {
            self.get(id)
        } else {
            let mut parent = self.enclosing.clone();
            for _ in 1..distance {
                if let Some(e) = parent {
                    let mut temp: Option<Rc<RefCell<Environment>>> = None;
                    temp.clone_from(&e.borrow().enclosing);
                    parent = temp;
                }
            }
            if let Some(e) = parent {
                e.borrow().get(id)
            } else {
                Err(LoxError::Error(miette!("Undefined identifier: '{id}'")))
            }
        }
    }

    pub fn define(&mut self, id: String, initializer: LoxValue) {
        if self.values.contains_key(&id) {
            self.values.entry(id).and_modify(|e| *e = initializer);
        } else {
            self.values.entry(id).or_insert(initializer);
        }
    }

    pub fn assign(&mut self, id: String, initializer: LoxValue) -> crate::Result<()> {
        if self.values.contains_key(&id) {
            self.values.entry(id).and_modify(|e| *e = initializer);
            Ok(())
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.borrow_mut().assign(id, initializer)
        } else {
            Err(LoxError::Error(miette!(
                "assignment to undefined variable '{id}'"
            )))
        }
    }

    pub fn assign_at(
        &mut self,
        distance: usize,
        id: String,
        initializer: LoxValue,
    ) -> crate::Result<()> {
        if distance == 0 {
            self.assign(id, initializer)
        } else {
            let mut parent: Option<Rc<RefCell<Environment>>> = self.enclosing.clone();
            for _ in 1..distance {
                if let Some(e) = parent {
                    let mut temp: Option<Rc<RefCell<Environment>>> = None;
                    temp.clone_from(&e.borrow().enclosing);
                    parent = temp;
                }
            }
            if let Some(e) = parent {
                e.borrow_mut().assign(id, initializer)
            } else {
                Err(LoxError::Error(miette!("Undefined identifier: '{id}'")))
            }
        }
    }
}
