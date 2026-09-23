use crate::{LoxError, ast::LoxValue};
use miette::miette;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Default, Debug)]
pub struct Environment<'a> {
    values: HashMap<&'a str, LoxValue<'a>>,
    enclosing: Option<Rc<RefCell<Environment<'a>>>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn child(enclosing: Rc<RefCell<Environment<'a>>>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn get(&self, id: &str) -> crate::Result<LoxValue<'a>> {
        if let Some(var) = self.values.get(id) {
            Ok(var.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(id)
        } else {
            Err(undefined(id))
        }
    }

    /// Reads a variable from the scope `distance` hops up the chain, as
    /// computed by the resolver. Unlike [`Environment::get`] it does not fall
    /// back to outer scopes.
    pub fn get_at(&self, distance: usize, id: &str) -> crate::Result<LoxValue<'a>> {
        if distance == 0 {
            return self.get_here(id);
        }
        let env = self.ancestor(distance).ok_or_else(|| undefined(id))?;
        env.borrow().get_here(id)
    }

    pub fn define(&mut self, id: &'a str, value: LoxValue<'a>) {
        self.values.insert(id, value);
    }

    pub fn assign(&mut self, id: &'a str, value: LoxValue<'a>) -> crate::Result<()> {
        if let Some(slot) = self.values.get_mut(id) {
            *slot = value;
            Ok(())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(id, value)
        } else {
            Err(undefined(id))
        }
    }

    pub fn assign_at(
        &mut self,
        distance: usize,
        id: &'a str,
        value: LoxValue<'a>,
    ) -> crate::Result<()> {
        if distance == 0 {
            return self.assign_here(id, value);
        }
        let env = self.ancestor(distance).ok_or_else(|| undefined(id))?;
        env.borrow_mut().assign_here(id, value)
    }

    fn get_here(&self, id: &str) -> crate::Result<LoxValue<'a>> {
        self.values.get(id).cloned().ok_or_else(|| undefined(id))
    }

    fn assign_here(&mut self, id: &str, value: LoxValue<'a>) -> crate::Result<()> {
        let slot = self.values.get_mut(id).ok_or_else(|| undefined(id))?;
        *slot = value;
        Ok(())
    }

    fn ancestor(&self, distance: usize) -> Option<Rc<RefCell<Environment<'a>>>> {
        let mut env = self.enclosing.clone()?;
        for _ in 1..distance {
            let next = env.borrow().enclosing.clone()?;
            env = next;
        }
        Some(env)
    }
}

fn undefined(id: &str) -> LoxError {
    LoxError::Error(miette!("Undefined variable '{id}'."))
}
