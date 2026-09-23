use miette::miette;
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Debug,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    LoxError,
    ast::{LoxValue, Stmt},
    env::Environment,
};

use scanner::{INIT, THIS};

type NativeFn = for<'a> fn(&[LoxValue<'a>]) -> crate::Result<LoxValue<'a>>;

/// Built-in function implemented in Rust.
#[derive(Clone, Copy)]
pub struct Native {
    name: &'static str,
    arity: usize,
    func: NativeFn,
}

impl Debug for Native {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native {}>", self.name)
    }
}

impl Native {
    #[must_use]
    pub fn name(&self) -> &'static str {
        self.name
    }

    #[must_use]
    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn call<'a>(&self, args: &[LoxValue<'a>]) -> crate::Result<LoxValue<'a>> {
        (self.func)(args)
    }
}

pub const NATIVES: [Native; 4] = [
    Native {
        name: "clock",
        arity: 0,
        func: clock,
    },
    Native {
        name: "sqrt",
        arity: 1,
        func: sqrt,
    },
    Native {
        name: "min",
        arity: 2,
        func: min,
    },
    Native {
        name: "max",
        arity: 2,
        func: max,
    },
];

#[allow(clippy::unnecessary_wraps)] // the signature is fixed by `NativeFn`
fn clock<'a>(_: &[LoxValue<'a>]) -> crate::Result<LoxValue<'a>> {
    let since_the_epoch = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    Ok(LoxValue::Number(since_the_epoch.as_secs_f64()))
}

fn sqrt<'a>(args: &[LoxValue<'a>]) -> crate::Result<LoxValue<'a>> {
    Ok(LoxValue::Number(number(&args[0])?.sqrt()))
}

fn min<'a>(args: &[LoxValue<'a>]) -> crate::Result<LoxValue<'a>> {
    Ok(LoxValue::Number(number(&args[0])?.min(number(&args[1])?)))
}

fn max<'a>(args: &[LoxValue<'a>]) -> crate::Result<LoxValue<'a>> {
    Ok(LoxValue::Number(number(&args[0])?.max(number(&args[1])?)))
}

fn number(value: &LoxValue) -> crate::Result<f64> {
    if let LoxValue::Number(n) = value {
        Ok(*n)
    } else {
        Err(LoxError::Error(miette!(
            "Expected number but was '{value}'"
        )))
    }
}

/// User-defined function or method together with the scope it closes over.
pub struct Function<'a> {
    name: &'a str,
    parameters: Vec<&'a str>,
    body: &'a [crate::Result<Stmt<'a>>],
    closure: Rc<RefCell<Environment<'a>>>,
    is_initializer: bool,
}

impl Debug for Function<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl<'a> Function<'a> {
    #[must_use]
    pub fn new(
        name: &'a str,
        parameters: Vec<&'a str>,
        body: &'a [crate::Result<Stmt<'a>>],
        closure: Rc<RefCell<Environment<'a>>>,
        is_initializer: bool,
    ) -> Self {
        Self {
            name,
            parameters,
            body,
            closure,
            is_initializer,
        }
    }

    #[must_use]
    pub fn name(&self) -> &'a str {
        self.name
    }

    #[must_use]
    pub fn arity(&self) -> usize {
        self.parameters.len()
    }

    #[must_use]
    pub fn parameters(&self) -> &[&'a str] {
        &self.parameters
    }

    pub fn body(&self) -> &'a [crate::Result<Stmt<'a>>] {
        self.body
    }

    #[must_use]
    pub fn closure(&self) -> Rc<RefCell<Environment<'a>>> {
        self.closure.clone()
    }

    #[must_use]
    pub fn is_initializer(&self) -> bool {
        self.is_initializer
    }

    /// Returns a copy of the method whose scope has `this` bound to `instance`.
    #[must_use]
    pub fn bind(&self, instance: Rc<RefCell<Instance<'a>>>) -> Function<'a> {
        let mut env = Environment::child(self.closure.clone());
        env.define(THIS, LoxValue::Instance(instance));
        Function {
            name: self.name,
            parameters: self.parameters.clone(),
            body: self.body,
            closure: Rc::new(RefCell::new(env)),
            is_initializer: self.is_initializer,
        }
    }
}

pub struct Class<'a> {
    name: &'a str,
    superclass: Option<Rc<Class<'a>>>,
    methods: HashMap<&'a str, Rc<Function<'a>>>,
}

impl Debug for Class<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<class {}>", self.name)
    }
}

impl<'a> Class<'a> {
    #[must_use]
    pub fn new(
        name: &'a str,
        superclass: Option<Rc<Class<'a>>>,
        methods: HashMap<&'a str, Rc<Function<'a>>>,
    ) -> Self {
        Self {
            name,
            superclass,
            methods,
        }
    }

    #[must_use]
    pub fn name(&self) -> &'a str {
        self.name
    }

    #[must_use]
    pub fn find_method(&self, name: &str) -> Option<Rc<Function<'a>>> {
        self.methods.get(name).cloned().or_else(|| {
            self.superclass
                .as_ref()
                .and_then(|superclass| superclass.find_method(name))
        })
    }

    #[must_use]
    pub fn arity(&self) -> usize {
        self.find_method(INIT).map_or(0, |init| init.arity())
    }
}

pub struct Instance<'a> {
    class: Rc<Class<'a>>,
    fields: HashMap<&'a str, LoxValue<'a>>,
}

impl Debug for Instance<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class.name)
    }
}

impl<'a> Instance<'a> {
    #[must_use]
    pub fn new(class: Rc<Class<'a>>) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }

    #[must_use]
    pub fn class_name(&self) -> &'a str {
        self.class.name
    }

    #[must_use]
    pub fn class(&self) -> Rc<Class<'a>> {
        self.class.clone()
    }

    #[must_use]
    pub fn field(&self, name: &str) -> Option<LoxValue<'a>> {
        self.fields.get(name).cloned()
    }

    pub fn set_field(&mut self, name: &'a str, value: LoxValue<'a>) {
        self.fields.insert(name, value);
    }
}
