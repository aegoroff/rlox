use std::collections::HashMap;

use num_traits::FromPrimitive;

use crate::{
    CompileError,
    chunk::{Chunk, OpCode},
    compile::Parser,
    value::LoxValue,
};

pub struct VirtualMachine<W: std::io::Write> {
    chunk: Chunk,
    ip: usize,
    stack: Vec<LoxValue>,
    writer: W,
    globals: HashMap<String, LoxValue>,
}

macro_rules! binary_op {
    ($self:ident, $op:tt) => {{
        let b = $self.pop()?;
        let a = $self.pop()?;
        let a = a.try_num()?;
        let b = b.try_num()?;
        $self.push(LoxValue::Number(a $op b));
        $self.ip += 1;
    }}
}

impl<W: std::io::Write> VirtualMachine<W> {
    #[must_use]
    pub fn new(writer: W) -> Self {
        Self {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::new(),
            writer,
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, content: &str) -> crate::Result<()> {
        self.chunk = Chunk::new();
        self.ip = 0;
        let mut parser = Parser::new(content);
        parser.compile(&mut self.chunk)?;
        self.run()
    }

    pub fn init(&mut self) {
        self.stack.clear();
    }

    fn push(&mut self, value: LoxValue) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> crate::Result<LoxValue> {
        self.stack
            .pop()
            .ok_or(CompileError::RuntimeError(miette::miette!(
                "Instructions stack empty"
            )))
    }

    fn peek(&mut self, distance: usize) -> crate::Result<&LoxValue> {
        if self.stack.len() < distance + 1 {
            Err(CompileError::RuntimeError(miette::miette!(
                "Not enough stack capacity for distance {distance}. Current stack size is {}",
                self.stack.len()
            )))
        } else {
            Ok(&self.stack[self.stack.len() - 1 - distance])
        }
    }

    fn run(&mut self) -> crate::Result<()> {
        #[cfg(feature = "disassembly")]
        {
            println!("--- start run ---");
        }
        while self.ip < self.chunk.code.len() {
            let code =
                OpCode::from_u8(self.chunk.code[self.ip]).ok_or(CompileError::CompileError(
                    miette::miette!("Invalid instruction: {}", self.chunk.code[self.ip]),
                ))?;
            #[cfg(feature = "disassembly")]
            {
                for value in &self.stack {
                    println!("[{value}]");
                }
                self.chunk.disassembly_instruction(self.ip);
            }
            match code {
                OpCode::Constant => {
                    let constant = self.chunk.read_constant(self.ip);
                    self.push(constant);
                    self.ip += 2;
                }
                OpCode::Return => {
                    return Ok(());
                }
                OpCode::ConstantLong => {
                    let constant = self.chunk.read_constant(self.ip);
                    self.push(constant);
                    self.ip += 4;
                }
                OpCode::Negate => {
                    let value = self.pop()?;
                    let value = value.try_num()?;
                    self.push(LoxValue::Number(-value));
                    self.ip += 1;
                }
                OpCode::Add => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let lr = a.try_str();
                    let rr = b.try_str();
                    if lr.is_ok() || rr.is_ok() {
                        // concat strings here if any of the operands is a string
                        if let Ok(l) = lr {
                            let result = l.to_owned() + &b.to_string();
                            self.push(LoxValue::String(result));
                        } else if let Ok(r) = rr {
                            let result = a.to_string() + r;
                            self.push(LoxValue::String(result));
                        }
                    } else if let Ok(l) = a.try_num() {
                        let r = b.try_num()?;
                        self.push(LoxValue::Number(l + r));
                    }

                    self.ip += 1;
                }
                OpCode::Subtract => binary_op!(self, -),
                OpCode::Multiply => binary_op!(self, *),
                OpCode::Divide => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let a = a.try_num()?;
                    let b = b.try_num()?;
                    if b == 0.0 {
                        return Err(CompileError::RuntimeError(miette::miette!(
                            "Division by zero"
                        )));
                    }
                    self.push(LoxValue::Number(a / b));
                    self.ip += 1;
                }
                OpCode::Nil => {
                    self.push(LoxValue::Nil);
                    self.ip += 1;
                }
                OpCode::True => {
                    self.push(LoxValue::Bool(true));
                    self.ip += 1;
                }
                OpCode::False => {
                    self.push(LoxValue::Bool(false));
                    self.ip += 1;
                }
                OpCode::Not => {
                    let value = self.pop()?;
                    let val = value.try_bool()?;
                    self.push(LoxValue::Bool(!val));
                    self.ip += 1;
                }
                OpCode::Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = a.equal(&b);
                    self.push(LoxValue::Bool(result));
                    self.ip += 1;
                }
                OpCode::Less => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = a.less(&b)?;
                    self.push(LoxValue::Bool(result));
                    self.ip += 1;
                }
                OpCode::Greater => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let lt = a.less(&b)?;
                    let gt = !lt && !a.equal(&b);
                    self.push(LoxValue::Bool(gt));
                    self.ip += 1;
                }
                OpCode::Print => {
                    let value = self.pop()?;
                    writeln!(self.writer, "{value}")
                        .map_err(|e| CompileError::RuntimeError(miette::miette!(e)))?;
                    self.ip += 1;
                }
                OpCode::Pop => {
                    self.pop()?;
                    self.ip += 1;
                }
                OpCode::DefineGlobal => {
                    self.define_global()?;
                    self.ip += 2;
                }
                OpCode::DefineGlobalLong => {
                    self.define_global()?;
                    self.ip += 4;
                }
                OpCode::GetGlobal => {
                    self.get_global()?;
                    self.ip += 2;
                }
                OpCode::GetGlobalLong => {
                    self.get_global()?;
                    self.ip += 4;
                }
                OpCode::SetGlobal => {
                    self.set_global()?;
                    self.ip += 2;
                }
                OpCode::SetGlobalLong => {
                    self.set_global()?;
                    self.ip += 4;
                }
                OpCode::GetLocal => {
                    let val = self.chunk.read_byte(self.ip + 1);
                    let val = &self.stack[val as usize];
                    let val = val.clone();
                    self.push(val);
                    self.ip += 2;
                }
                OpCode::SetLocal => {
                    let val = self.chunk.read_byte(self.ip + 1);
                    let value = self.peek(0)?;
                    self.stack[val as usize] = value.clone();
                    self.ip += 2;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.chunk.read_short(self.ip + 1);
                    let top = self.peek(0)?;
                    let falsey = !top.is_truthy();
                    self.ip += 3;
                    if falsey {
                        self.ip += offset;
                    }
                }
                OpCode::Jump => {
                    let offset = self.chunk.read_short(self.ip + 1);
                    self.ip += 3;
                    self.ip += offset;
                }
                OpCode::Loop => {
                    let offset = self.chunk.read_short(self.ip + 1);
                    self.ip += 3;
                    self.ip -= offset;
                }
            }
        }
        Ok(())
    }

    fn set_global(&mut self) -> crate::Result<()> {
        let name = self.chunk.read_constant(self.ip);
        let name = name.try_str()?;
        if !self.globals.contains_key(name) {
            return Err(CompileError::RuntimeError(miette::miette!(
                "Undefined variable '{name}'"
            )));
        }
        let value = self.peek(0)?;
        let value = value.clone();
        self.globals.insert(name.clone(), value);
        Ok(())
    }

    fn get_global(&mut self) -> crate::Result<()> {
        let name = self.chunk.read_constant(self.ip);
        let name = name.try_str()?;
        let Some(val) = self.globals.get(name) else {
            return Err(CompileError::RuntimeError(miette::miette!(
                "Undefined variable '{name}'"
            )));
        };
        self.push(val.clone());
        Ok(())
    }

    fn define_global(&mut self) -> crate::Result<()> {
        let name = self.chunk.read_constant(self.ip);
        let name = name.try_str()?;
        let value = self.peek(0)?;
        let value = value.clone();
        self.globals.insert(name.clone(), value);
        self.pop()?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test_case("print (\"a\" + \"b\") + \"c\";", "abc")]
    #[test_case("print (\"a\" + 4) + \"c\";", "a4c")]
    #[test_case("print (4 + \"a\") + \"c\";", "4ac")]
    #[test_case("print (true + \"a\") + \"c\";", "trueac")]
    #[test_case("print (nil + \"a\") + \"c\";", "ac")]
    #[test_case("print (\"a\" == \"b\");", "false")]
    #[test_case("print (\"a\" != \"c\");", "true")]
    #[test_case("print (\"ab\" == \"ab\");", "true")]
    #[test_case("print (\"aa\" > \"bb\");", "false")]
    #[test_case("print (\"bb\" > \"aa\");", "true")]
    #[test_case("print (\"bba\" >= \"aaa\");", "true")]
    #[test_case("print (\"bba\" <= \"aaa\");", "false")]
    #[test_case("print 1 == 2;", "false")]
    #[test_case("print 2 == 2;", "true")]
    #[test_case("print 3 >= 3;", "true")]
    #[test_case("print 3 >= 2;", "true")]
    #[test_case("print 3 <= 1;", "false")]
    #[test_case("print (3 - 1) * 200 <= 1;", "false")]
    #[test_case("print 3 > 1 == true;", "true")]
    #[test_case("print 20 <= 20;", "true")]
    #[test_case("print 40 <= 50;", "true")]
    #[test_case("print nil <= false;", "true" ; "nil lrs less or equal")]
    #[test_case("print nil < false;", "false" ; "nil lrs less")]
    #[test_case("print nil == false;", "true" ; "nil lrs equal")]
    #[test_case("print !nil;", "true" ; "not nil")]
    #[test_case("print 40 <= 50 and 1 > 2 or 2 < 3;", "true" ; "two ands + or")]
    #[test_case("print 40 <= 50 and 1 < 2 and 2 < 3;", "true" ; "three ands")]
    #[test_case("print --1;", "1")]
    #[test_case("print 1 - 1;", "0")]
    #[test_case("print 1 - 2;", "-1")]
    #[test_case("print 2 - 1;", "1")]
    #[test_case("print 2 + 3;", "5")]
    #[test_case("print 2 + 3 - 1;", "4")]
    #[test_case("print 3 + 3 / 3;", "4")]
    #[test_case("print (3 + 3) / 3;", "2")]
    #[test_case("print 4 / 2;", "2")]
    #[test_case("print 4 / 1;", "4")]
    #[test_case("print 5 / -1;", "-5")]
    #[test_case("print (5 - (3-1)) + -1;", "2")]
    #[test_case("print (5 - (3-1)) * -1;", "-3")]
    #[test_case("print ((5 - (3-1)) * -2) / 4;", "-1.5")]
    #[test_case("print ((5 - (3-1) + 3) * -2) / 4;", "-3")]
    #[test_case("var x = 1; var y = x + 1; print x; print y;", "1\n2")]
    #[test_case("print 1; print 2;", "1\n2")]
    #[test_case("print 1; { print 3; }", "1\n3")]
    #[test_case("var y = 1; { var x = 2; print x; } print y;", "2\n1")]
    #[test_case("var x = 1; { var x = 2; print x; }", "2")]
    #[test_case(
        "var x = 1; { var x = 2; print x; { var x = 3; print x; } } print x;",
        "2\n3\n1"
    )]
    #[test_case("var x = 1; if (x > 0) { print x; }", "1")]
    #[test_case("var x = -1; if (x > 0) { print x; } print 2;", "2")]
    #[test_case("var x = 1; if (x > 0) { print x; } else { print 2; }", "1")]
    #[test_case("var x = -1; if (x > 0) { print x; } else { print 2; }", "2")]
    #[test_case("var i = 0; while (i < 10) i = i + 1; print i;", "10" ; "while test")]
    #[test_case("for(var i = 0; i < 3; i = i + 1) print i;", "0\n1\n2" ; "for test")]
    #[test_case("var i = 0; for(; i < 3; i = i + 1) print i;", "0\n1\n2" ; "for test without initializer")]
    fn vm_positive_tests(input: &str, expected: &str) {
        // Arrange
        let mut stdout = Vec::new();
        let mut vm = VirtualMachine::new(&mut stdout);
        vm.init();

        // Act
        let actual = vm.interpret(input);

        // Assert
        assert!(actual.is_ok());
        let actual = String::from_utf8(stdout).unwrap();
        assert_eq!(actual.trim_end(), expected);
    }
}
