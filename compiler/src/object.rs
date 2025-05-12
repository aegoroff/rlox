use std::fmt::Display;

use crate::chunk::Chunk;

#[derive(Default)]
pub struct Function<'a> {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Option<&'a str>,
}

impl Display for Function<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name.unwrap_or("script"))
    }
}

impl Function<'_> {
    pub fn new() -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: None,
        }
    }
}
