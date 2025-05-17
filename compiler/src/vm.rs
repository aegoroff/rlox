#![allow(clippy::missing_errors_doc)]

use std::collections::HashMap;

use num_traits::FromPrimitive;

use crate::{
    CompileError,
    chunk::{Chunk, OpCode},
    compile::Parser,
    value::{Function, LoxValue, NativeFunction},
};

const FRAMES_MAX: usize = 64;

#[derive(Default)]
struct CallFrame {
    function: Function,
    ip: usize,               // caller's ip
    pub slots_offset: usize, // points to vm's value's stack first value it can use
}

impl CallFrame {
    fn new() -> Self {
        Self {
            function: Function::new(""),
            ip: 0,
            slots_offset: 1, // caller function itself
        }
    }
}

pub struct VirtualMachine<W: std::io::Write> {
    stack: Vec<LoxValue>,
    writer: W,
    globals: HashMap<String, LoxValue>,
    frames: [CallFrame; FRAMES_MAX],
    frame_count: usize,
}

impl<W: std::io::Write> VirtualMachine<W> {
    #[must_use]
    pub fn new(writer: W) -> Self {
        Self {
            stack: Vec::new(),
            writer,
            globals: HashMap::new(),
            frames: core::array::from_fn(|_| CallFrame::new()),
            frame_count: 0,
        }
    }

    pub fn interpret(&mut self, content: &str) -> crate::Result<()> {
        let mut parser = Parser::new(content);
        let function = parser.compile()?;
        self.push(LoxValue::Function(function.clone()));
        self.call(function, 0)
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

    fn pop_stack_n_times(&mut self, num_to_pop: usize) -> crate::Result<()> {
        for _ in 0..num_to_pop {
            self.pop()?;
        }
        Ok(())
    }

    fn peek(&self, distance: usize) -> crate::Result<&LoxValue> {
        if self.stack.len() < distance + 1 {
            Err(CompileError::RuntimeError(miette::miette!(
                "Not enough stack capacity for distance {distance}. Current stack size is {}",
                self.stack.len()
            )))
        } else {
            Ok(&self.stack[self.stack.len() - 1 - distance])
        }
    }

    fn frame(&mut self) -> &mut CallFrame {
        &mut self.frames[self.frame_count - 1]
    }

    fn run(&mut self) -> crate::Result<()> {
        let function = self.frame().function.clone();
        let chunk = function.chunk.borrow();
        #[cfg(feature = "disassembly")]
        {
            println!("--- start run ---");
        }
        let mut ip = self.frame().ip;
        while ip < chunk.code.len() {
            let code = OpCode::from_u8(chunk.code[ip]).ok_or(CompileError::CompileError(
                miette::miette!("Invalid instruction: {}", chunk.code[ip]),
            ))?;
            #[cfg(feature = "disassembly")]
            {
                for value in &self.stack {
                    println!("[{value}]");
                }
                chunk.disassembly_instruction(ip);
            }
            match code {
                OpCode::Constant => {
                    let constant = chunk.read_constant(ip);
                    self.push(constant);
                    ip += 2;
                }
                OpCode::Return => {
                    let value = self.pop()?;
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        return Ok(());
                    }

                    let num_to_pop =
                        self.stack.len() - self.frame().slots_offset + self.frame().function.arity;
                    self.pop_stack_n_times(num_to_pop)?;
                    self.push(value);
                    break;
                }
                OpCode::ConstantLong => {
                    let constant = chunk.read_constant(ip);
                    self.push(constant);
                    ip += 4;
                }
                OpCode::Negate => {
                    let value = self.pop()?;
                    let value = value.try_num()?;
                    self.push(LoxValue::Number(-value));
                    ip += 1;
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

                    ip += 1;
                }
                OpCode::Subtract => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let a = a.try_num()?;
                    let b = b.try_num()?;
                    self.push(LoxValue::Number(a - b));
                    ip += 1;
                }
                OpCode::Multiply => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let a = a.try_num()?;
                    let b = b.try_num()?;
                    self.push(LoxValue::Number(a * b));
                    ip += 1;
                }
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
                    ip += 1;
                }
                OpCode::Nil => {
                    self.push(LoxValue::Nil);
                    ip += 1;
                }
                OpCode::True => {
                    self.push(LoxValue::Bool(true));
                    ip += 1;
                }
                OpCode::False => {
                    self.push(LoxValue::Bool(false));
                    ip += 1;
                }
                OpCode::Not => {
                    let value = self.pop()?;
                    let val = value.try_bool()?;
                    self.push(LoxValue::Bool(!val));
                    ip += 1;
                }
                OpCode::Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = a.equal(&b);
                    self.push(LoxValue::Bool(result));
                    ip += 1;
                }
                OpCode::Less => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = a.less(&b)?;
                    self.push(LoxValue::Bool(result));
                    ip += 1;
                }
                OpCode::Greater => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let lt = a.less(&b)?;
                    let gt = !lt && !a.equal(&b);
                    self.push(LoxValue::Bool(gt));
                    ip += 1;
                }
                OpCode::Print => {
                    let value = self.pop()?;
                    writeln!(self.writer, "{value}")
                        .map_err(|e| CompileError::RuntimeError(miette::miette!(e)))?;
                    ip += 1;
                }
                OpCode::Pop => {
                    self.pop()?;
                    ip += 1;
                }
                OpCode::DefineGlobal => {
                    self.define_global(&chunk, ip)?;
                    ip += 2;
                }
                OpCode::DefineGlobalLong => {
                    self.define_global(&chunk, ip)?;
                    ip += 4;
                }
                OpCode::GetGlobal => {
                    self.get_global(&chunk, ip)?;
                    ip += 2;
                }
                OpCode::GetGlobalLong => {
                    self.get_global(&chunk, ip)?;
                    ip += 4;
                }
                OpCode::SetGlobal => {
                    self.set_global(&chunk, ip)?;
                    ip += 2;
                }
                OpCode::SetGlobalLong => {
                    self.set_global(&chunk, ip)?;
                    ip += 4;
                }
                OpCode::GetLocal => {
                    let slots_offset = self.frame().slots_offset;
                    let val = chunk.read_byte(ip + 1);
                    let val = &self.stack[slots_offset + val as usize - 1];
                    let val = val.clone();
                    self.push(val);
                    ip += 2;
                }
                OpCode::SetLocal => {
                    let slots_offset = self.frame().slots_offset;
                    let val = chunk.read_byte(ip + 1);
                    let value = self.peek(0)?;
                    let value = value.clone();
                    self.stack[slots_offset + val as usize - 1] = value;
                    ip += 2;
                }
                OpCode::JumpIfFalse => {
                    let offset = chunk.read_short(ip + 1);
                    let top = self.peek(0)?;
                    let falsey = !top.is_truthy();
                    ip += 3;
                    if falsey {
                        ip += offset;
                    }
                }
                OpCode::Jump => {
                    let offset = chunk.read_short(ip + 1);
                    ip += 3;
                    ip += offset;
                }
                OpCode::Loop => {
                    let offset = chunk.read_short(ip + 1);
                    ip += 3;
                    ip -= offset;
                }
                OpCode::Call => {
                    let args_count = chunk.read_byte(ip + 1);
                    let func = self.peek(args_count as usize)?;
                    self.call_value(func.clone(), args_count as usize)?;
                    ip += 2;
                }
            }
        }
        Ok(())
    }

    fn call_value(&mut self, callee: LoxValue, args_count: usize) -> crate::Result<()> {
        match callee {
            LoxValue::Function(func) => self.call(func, args_count),
            LoxValue::Native(func) => self.call_native(func, args_count),
            _ => Err(CompileError::RuntimeError(miette::miette!(
                "Can only call functions and classes."
            ))),
        }
    }

    fn call(&mut self, func: Function, args_count: usize) -> crate::Result<()> {
        self.frame_count += 1;
        self.frame().slots_offset = self.stack.len() - args_count;
        self.frame().function = func;
        self.run()
    }

    fn call_native(&mut self, func: NativeFunction, args_count: usize) -> crate::Result<()> {
        self.frame_count += 1;
        self.frame().slots_offset = self.stack.len() - args_count;
        //self.frame().function = func;
        Ok(())
    }

    fn set_global(&mut self, chunk: &Chunk, offset: usize) -> crate::Result<()> {
        let name = chunk.read_constant(offset);
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

    fn get_global(&mut self, chunk: &Chunk, offset: usize) -> crate::Result<()> {
        let name = chunk.read_constant(offset);
        let name = name.try_str()?;
        let Some(val) = self.globals.get(name) else {
            return Err(CompileError::RuntimeError(miette::miette!(
                "Undefined variable '{name}'"
            )));
        };
        self.push(val.clone());
        Ok(())
    }

    fn define_global(&mut self, chunk: &Chunk, offset: usize) -> crate::Result<()> {
        let name = chunk.read_constant(offset);
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
    #[test_case("fun foo() { print 10; } print foo();", "10" ; "simple call no args")]
    #[test_case("fun foo(v) { print v; } print foo(10);", "10" ; "simple call one arg")]
    #[test_case("fun sum(a1, a2) { print a1 + a2; } sum(1, 2);", "3" ; "simple call two args")]
    #[test_case("fun foo(x) { return x + 1; } print foo(1);", "2" ; "function with return")]
    #[test_case("fun fib(n) { if (n < 2) return n; return fib(n - 1) + fib(n - 2); } print fib(8);", "21" ; "fibonacci")]
    #[test_case("fun foo(n) { if (n < 2) return n; return 10; } print foo(1);", "1" ; "conditional return success")]
    #[test_case("fun foo(n) { if (n < 2) return n; return 10; } print foo(5);", "10" ; "conditional return fail")]
    fn vm_positive_tests(input: &str, expected: &str) {
        // Arrange
        let mut stdout = Vec::new();
        let mut vm = VirtualMachine::new(&mut stdout);
        vm.init();

        // Act
        let actual = vm.interpret(input);

        // Assert
        if actual.is_err() {
            println!("{actual:?}");
        }
        assert!(actual.is_ok());
        let actual = String::from_utf8(stdout).unwrap();
        assert_eq!(actual.trim_end(), expected);
    }
}
