use crate::{LoxError, ast::LoxValue};
use miette::miette;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Default, Debug)]
pub struct Environment {
    values: HashMap<String, LoxValue>,
    fields: HashMap<String, HashMap<String, LoxValue>>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
            fields: HashMap::new(),
        }
    }

    pub fn child(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            values: HashMap::new(),
            fields: HashMap::new(),
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
                    parent = e.borrow().enclosing.clone();
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

    pub fn set_field(&mut self, id: String, field: String, initializer: LoxValue) {
        if self.fields.contains_key(&id) {
            self.fields.entry(id).and_modify(|e| {
                e.insert(field, initializer);
            });
        } else {
            let mut fields = HashMap::new();
            fields.insert(field, initializer);
            self.fields.entry(id).or_insert(fields);
        }
    }

    pub fn set_field_at(
        &mut self,
        distance: usize,
        id: String,
        field: String,
        initializer: LoxValue,
    ) {
        if distance == 0 {
            self.set_field(id, field, initializer);
        } else {
            let mut parent = self.enclosing.clone();
            for _ in 1..distance {
                if let Some(e) = parent {
                    parent = e.borrow().enclosing.clone();
                }
            }
            if let Some(e) = parent {
                e.borrow_mut().set_field(id, field, initializer);
            }
        }
    }

    pub fn get_field(&self, id: &str, field: &str) -> crate::Result<LoxValue> {
        if let Some(fields) = self.fields.get(id) {
            if let Some(field) = fields.get(field) {
                Ok(field.clone())
            } else {
                Err(LoxError::Error(miette!("Undefined field: '{field}'")))
            }
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get_field(id, field)
        } else {
            Err(LoxError::Error(miette!("No any field set for: '{id}'")))
        }
    }

    pub fn get_field_at(&self, distance: usize, id: &str, field: &str) -> crate::Result<LoxValue> {
        if distance == 0 {
            self.get_field(id, field)
        } else {
            let mut parent = self.enclosing.clone();
            for _ in 1..distance {
                if let Some(e) = parent {
                    parent = e.borrow().enclosing.clone();
                }
            }
            if let Some(e) = parent {
                e.borrow().get_field(id, field)
            } else {
                Err(LoxError::Error(miette!("Undefined identifier: '{id}'")))
            }
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
            let mut parent = self.enclosing.clone();
            for _ in 1..distance {
                if let Some(e) = parent {
                    parent = e.borrow().enclosing.clone();
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
