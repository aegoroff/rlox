#![allow(clippy::missing_errors_doc)]

use crate::ProgramError;
use crate::chunk::Chunk;
use crate::value::{Closure, Upvalue};
use crate::{
    chunk::OpCode,
    compile::Parser,
    value::{Function, LoxValue, NativeFunction},
};
use fnv::FnvHashMap;
use std::cell::Ref;
use std::time::{SystemTime, UNIX_EPOCH};

const FRAMES_MAX: usize = 64;

#[derive(Default)]
struct CallFrame {
    closure: Closure,
    ip: usize,               // caller's ip
    pub slots_offset: usize, // points to vm's value's stack first value it can use
}

impl CallFrame {
    fn new() -> Self {
        Self {
            closure: Closure::new(Function::new("")),
            ip: 0,
            slots_offset: 1, // caller function itself
        }
    }
}

pub struct VirtualMachine<W: std::io::Write> {
    stack: Vec<LoxValue>,
    writer: W,
    globals: FnvHashMap<String, LoxValue>,
    frames: [CallFrame; FRAMES_MAX],
    frame_count: usize,
}

impl<W: std::io::Write> VirtualMachine<W> {
    #[must_use]
    pub fn new(writer: W) -> Self {
        Self {
            stack: Vec::new(),
            writer,
            globals: FnvHashMap::default(),
            frames: core::array::from_fn(|_| CallFrame::new()),
            frame_count: 0,
        }
    }

    pub fn interpret(&mut self, content: &str) -> crate::Result<()> {
        let mut parser = Parser::new(content);
        let function = parser.compile()?;
        self.push(LoxValue::Function(function.clone()));
        let closure = Closure::new(function);
        self.pop().map_err(|e| miette::miette!(e.to_string()))?;
        self.push(LoxValue::Closure(closure.clone()));
        self.call(closure, 0)
            .map_err(|e| miette::miette!(e.to_string()))
    }

    pub fn init(&mut self) {
        self.stack.clear();
        let clock_name = "clock".to_string();
        let clock = LoxValue::Native(NativeFunction {
            arity: 0,
            name: clock_name.clone(),
        });
        self.globals.insert(clock_name, clock);
    }

    #[inline]
    fn push(&mut self, value: LoxValue) {
        self.stack.push(value);
    }

    #[inline]
    fn pop(&mut self) -> Result<LoxValue, ProgramError> {
        self.stack.pop().ok_or(ProgramError::InstructionsStackEmpty)
    }

    #[inline]
    fn pop_stack_n_times(&mut self, num_to_pop: usize) -> Result<(), ProgramError> {
        for _ in 0..num_to_pop {
            self.pop()?;
        }
        Ok(())
    }

    #[inline]
    fn peek(&self, distance: usize) -> Result<&LoxValue, ProgramError> {
        if self.stack.len() < distance + 1 {
            Err(ProgramError::NotEnoughStackCapacity(
                distance,
                self.stack.len(),
            ))
        } else {
            Ok(&self.stack[self.stack.len() - 1 - distance])
        }
    }

    #[inline]
    fn frame(&mut self) -> &mut CallFrame {
        &mut self.frames[self.frame_count - 1]
    }

    #[inline]
    fn chunk(&self) -> Ref<Chunk> {
        self.frames[self.frame_count - 1]
            .closure
            .function
            .chunk
            .borrow()
    }

    fn run(&mut self) -> Result<(), ProgramError> {
        #[cfg(feature = "disassembly")]
        {
            println!("--- start run ---");
        }
        let mut ip = self.frame().ip;
        let code_size = self.chunk().code.len();
        while ip < code_size {
            let code = self.chunk().read_opcode(ip)?;
            #[cfg(feature = "disassembly")]
            {
                for value in &self.stack {
                    println!("[{value}]");
                }
                self.chunk().disassembly_instruction(ip);
            }
            match code {
                OpCode::Constant => {
                    let constant = self.chunk().read_constant(ip);
                    self.push(constant);
                    ip += 2;
                }
                OpCode::Return => {
                    let value = self.pop()?;
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        break;
                    }

                    let num_to_pop = self.frame().closure.function.arity + 1;
                    self.pop_stack_n_times(num_to_pop)?;
                    self.push(value);
                    break;
                }
                OpCode::ConstantLong => {
                    let constant = self.chunk().read_constant(ip);
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
                        return Err(ProgramError::DivizionByZero);
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
                        .map_err(|e| ProgramError::Runtime(e.to_string()))?;
                    ip += 1;
                }
                OpCode::Pop => {
                    self.pop()?;
                    ip += 1;
                }
                OpCode::DefineGlobal => {
                    self.define_global(ip)?;
                    ip += 2;
                }
                OpCode::DefineGlobalLong => {
                    self.define_global(ip)?;
                    ip += 4;
                }
                OpCode::GetGlobal => {
                    self.get_global(ip)?;
                    ip += 2;
                }
                OpCode::GetGlobalLong => {
                    self.get_global(ip)?;
                    ip += 4;
                }
                OpCode::SetGlobal => {
                    self.set_global(ip)?;
                    ip += 2;
                }
                OpCode::SetGlobalLong => {
                    self.set_global(ip)?;
                    ip += 4;
                }
                OpCode::GetLocal => {
                    let slots_offset = self.frame().slots_offset;
                    let val = self.chunk().read_byte(ip + 1);
                    let val = &self.stack[slots_offset + val as usize - 1];
                    let val = val.clone();
                    self.push(val);
                    ip += 2;
                }
                OpCode::SetLocal => {
                    let slots_offset = self.frame().slots_offset;
                    let val = self.chunk().read_byte(ip + 1);
                    let value = self.peek(0)?;
                    let value = value.clone();
                    self.stack[slots_offset + val as usize - 1] = value;
                    ip += 2;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.chunk().read_short(ip + 1);
                    let top = self.peek(0)?;
                    let falsey = !top.is_truthy();
                    ip += 3;
                    if falsey {
                        ip += offset;
                    }
                }
                OpCode::Jump => {
                    let offset = self.chunk().read_short(ip + 1);
                    ip += 3;
                    ip += offset;
                }
                OpCode::Loop => {
                    let offset = self.chunk().read_short(ip + 1);
                    ip += 3;
                    ip -= offset;
                }
                OpCode::Call => {
                    let args_count = self.chunk().read_byte(ip + 1);
                    let func = self.peek(args_count as usize)?;
                    self.call_value(func.clone(), args_count as usize)?;
                    ip += 2;
                }
                OpCode::Closure => {
                    let function_ix = self.chunk().read_constant(ip);
                    let LoxValue::Function(f) = function_ix else {
                        return Err(ProgramError::ExpectedFunction);
                    };
                    let upvalues_count = f.upvalue_count;
                    ip += 2;

                    let mut closure = Closure::new(f);
                    for _ in 0..upvalues_count {
                        let is_local = self.chunk().read_byte(ip);
                        let index = self.chunk().read_byte(ip + 1);
                        ip += 2;
                        if is_local == 1 {
                            let slots_offset = self.frame().slots_offset;
                            let val = &self.stack[slots_offset + index as usize - 1];
                            closure.upvalues.push(Upvalue::new(val.clone()));
                        } else {
                            let upvalue = &self.frame().closure.upvalues[index as usize];
                            closure.upvalues.push(upvalue.clone());
                        }
                    }

                    let val = LoxValue::Closure(closure);
                    self.push(val);
                }
                OpCode::GetUpvalue => {
                    let slot = self.chunk().read_byte(ip + 1);
                    // TODO:
                    ip += 2;
                }
                OpCode::SetUpvalue => {
                    let slot = self.chunk().read_byte(ip + 1);
                    // TODO:
                    ip += 2;
                }
            }
        }
        Ok(())
    }

    #[inline]
    fn call_value(&mut self, callee: LoxValue, args_count: usize) -> Result<(), ProgramError> {
        match callee {
            LoxValue::Closure(closure) => self.call(closure, args_count),
            LoxValue::Native(func) => self.call_native(func, args_count),
            _ => Err(ProgramError::InvalidCallable),
        }
    }

    #[inline]
    fn call(&mut self, closure: Closure, args_count: usize) -> Result<(), ProgramError> {
        self.frame_count += 1;
        self.frame().slots_offset = self.stack.len() - args_count;
        self.frame().closure = closure;
        self.run()
    }

    #[inline]
    fn call_native(&mut self, func: NativeFunction, args_count: usize) -> Result<(), ProgramError> {
        let slots_offset = self.stack.len() - args_count;
        let result = match func.name.as_str() {
            "clock" => self.clock_native(slots_offset, args_count),
            _ => {
                return Err(ProgramError::Runtime(format!(
                    "Undefined native function '{}'",
                    func.name
                )));
            }
        };
        let num_to_pop = func.arity + 1;
        self.pop_stack_n_times(num_to_pop)?;
        self.push(result);
        Ok(())
    }

    #[inline]
    fn set_global(&mut self, offset: usize) -> Result<(), ProgramError> {
        let name = self.chunk().read_constant(offset);
        let name = name.try_str()?;
        if !self.globals.contains_key(name) {
            return Err(ProgramError::Runtime(format!(
                "Undefined variable '{name}'"
            )));
        }
        let value = self.peek(0)?;
        let value = value.clone();
        self.globals.insert(name.clone(), value);
        Ok(())
    }

    #[inline]
    fn get_global(&mut self, offset: usize) -> Result<(), ProgramError> {
        let name = self.chunk().read_constant(offset);
        let name = name.try_str()?;
        let Some(val) = self.globals.get(name) else {
            return Err(ProgramError::Runtime(format!(
                "Undefined variable '{name}'"
            )));
        };
        self.push(val.clone());
        Ok(())
    }

    #[inline]
    fn define_global(&mut self, offset: usize) -> Result<(), ProgramError> {
        let name = self.chunk().read_constant(offset);
        let name = name.try_str()?;
        let value = self.peek(0)?;
        let value = value.clone();
        self.globals.insert(name.clone(), value);
        self.pop()?;
        Ok(())
    }

    #[inline]
    fn clock_native(&self, _: usize, _: usize) -> LoxValue {
        let start = SystemTime::now();
        let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap_or_default();
        let millis = since_the_epoch.as_millis();
        LoxValue::Number(millis as f64 * 0.001)
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
    #[test_case("print clock() - clock();", "0" ; "simple clock call")]
    #[test_case("fun foo() { var i = 1; fun bar(x) { return i + x; } return bar; } print foo()(2);", "3" ; "closure")]
    #[test_case("var f; { var local = \"local\"; fun f_() { print local; } f = f_; } f();", "local" ; "closure1")]
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
