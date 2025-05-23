#![allow(clippy::missing_errors_doc)]

use crate::chunk::Chunk;
use crate::value::{Class, Closure, Instance, Upvalue};
use crate::{ProgramError, builtin};
use crate::{
    chunk::OpCode,
    compile::Parser,
    value::{Function, LoxValue, NativeFunction},
};
use fnv::FnvHashMap;
use std::cell::{Ref, RefCell};
use std::rc::Rc;

const FRAMES_MAX: usize = 64;
const CONST_SIZE: usize = 1;
const CONST_LONG_SIZE: usize = 3;

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
    open_upvalues: Vec<Rc<RefCell<Upvalue>>>,
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
            open_upvalues: vec![],
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
            func: builtin::clock,
        });
        let sqrt_name = "sqrt".to_string();
        let sqrt = LoxValue::Native(NativeFunction {
            arity: 1,
            name: sqrt_name.clone(),
            func: builtin::sqrt,
        });
        self.globals.insert(clock_name, clock);
        self.globals.insert(sqrt_name, sqrt);
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
    fn peek_mut(&mut self, distance: usize) -> Result<&mut LoxValue, ProgramError> {
        if self.stack.len() < distance + 1 {
            Err(ProgramError::NotEnoughStackCapacity(
                distance,
                self.stack.len(),
            ))
        } else {
            let len = self.stack.len();
            Ok(&mut self.stack[len - 1 - distance])
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
            ip += 1; // shift opcode offset itself
            #[cfg(feature = "disassembly")]
            {
                for value in &self.stack {
                    println!("[{value}]");
                }
                self.chunk().disassembly_instruction(ip);
            }
            match code {
                OpCode::Constant => {
                    let constant = self.chunk().read_constant(ip, CONST_SIZE);
                    self.push(constant);
                    ip += CONST_SIZE;
                }
                OpCode::ConstantLong => {
                    let constant = self.chunk().read_constant(ip, CONST_LONG_SIZE);
                    self.push(constant);
                    ip += CONST_LONG_SIZE;
                }
                OpCode::Return => {
                    self.close_upvalues(self.stack.len() - 1);
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
                OpCode::Negate => {
                    let value = self.pop()?;
                    let value = value.try_num()?;
                    self.push(LoxValue::Number(-value));
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
                }
                OpCode::Subtract => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let a = a.try_num()?;
                    let b = b.try_num()?;
                    self.push(LoxValue::Number(a - b));
                }
                OpCode::Multiply => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let a = a.try_num()?;
                    let b = b.try_num()?;
                    self.push(LoxValue::Number(a * b));
                }
                OpCode::Divide => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let a = a.try_num()?;
                    let b = b.try_num()?;
                    if b == 0.0 {
                        self.push(LoxValue::NaN);
                    } else {
                        self.push(LoxValue::Number(a / b));
                    }
                }
                OpCode::Nil => self.push(LoxValue::Nil),
                OpCode::True => self.push(LoxValue::Bool(true)),
                OpCode::False => self.push(LoxValue::Bool(false)),
                OpCode::Not => {
                    let value = self.pop()?;
                    let val = value.try_bool()?;
                    self.push(LoxValue::Bool(!val));
                }
                OpCode::Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = a.equal(&b);
                    self.push(LoxValue::Bool(result));
                }
                OpCode::Less => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = a.less(&b)?;
                    self.push(LoxValue::Bool(result));
                }
                OpCode::Greater => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let lt = a.less(&b)?;
                    let gt = !lt && !a.equal(&b);
                    self.push(LoxValue::Bool(gt));
                }
                OpCode::Print => {
                    let value = self.pop()?;
                    writeln!(self.writer, "{value}")
                        .map_err(|e| ProgramError::Runtime(e.to_string()))?;
                }
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::DefineGlobal => {
                    self.define_global(ip, CONST_SIZE)?;
                    ip += CONST_SIZE;
                }
                OpCode::DefineGlobalLong => {
                    self.define_global(ip, CONST_LONG_SIZE)?;
                    ip += CONST_LONG_SIZE;
                }
                OpCode::GetGlobal => {
                    self.get_global(ip, CONST_SIZE)?;
                    ip += CONST_SIZE;
                }
                OpCode::GetGlobalLong => {
                    self.get_global(ip, CONST_LONG_SIZE)?;
                    ip += CONST_LONG_SIZE;
                }
                OpCode::SetGlobal => {
                    self.set_global(ip, CONST_SIZE)?;
                    ip += CONST_SIZE;
                }
                OpCode::SetGlobalLong => {
                    self.set_global(ip, CONST_LONG_SIZE)?;
                    ip += CONST_LONG_SIZE;
                }
                OpCode::GetLocal => {
                    let slots_offset = self.frame().slots_offset;
                    let frame_offset = self.chunk().read_byte(ip);
                    let val = self.stack[slots_offset + frame_offset as usize - 1].clone();
                    self.push(val);
                    ip += 1;
                }
                OpCode::SetLocal => {
                    let slots_offset = self.frame().slots_offset;
                    let frame_offset = self.chunk().read_byte(ip);
                    let value = self.peek(0)?;
                    self.stack[slots_offset + frame_offset as usize - 1] = value.clone();
                    ip += 1;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.chunk().read_short(ip);
                    let top = self.peek(0)?;
                    let falsey = !top.is_truthy();
                    ip += 2;
                    if falsey {
                        ip += offset;
                    }
                }
                OpCode::Jump => {
                    let offset = self.chunk().read_short(ip);
                    ip += 2;
                    ip += offset;
                }
                OpCode::Loop => {
                    let offset = self.chunk().read_short(ip);
                    ip += 2;
                    ip -= offset;
                }
                OpCode::Call => {
                    let args_count = self.chunk().read_byte(ip);
                    let func = self.peek(args_count as usize)?;
                    self.call_value(func.clone(), args_count as usize)?;
                    ip += 1;
                }
                OpCode::Closure => {
                    let function_value = self.chunk().read_constant(ip, 1);
                    let LoxValue::Function(function) = function_value else {
                        let line = self.chunk().line(ip - 1);
                        return Err(ProgramError::ExpectedFunction(line));
                    };
                    let upvalues_count = function.upvalue_count;
                    ip += 1;

                    let mut closure = Closure::new(function);
                    for _ in 0..upvalues_count {
                        let is_local = self.chunk().read_byte(ip);
                        let index = self.chunk().read_byte(ip + 1);
                        ip += 2;
                        let upvalue = if is_local == 1 {
                            let slots_offset = self.frame().slots_offset;
                            self.capture_upvalue(slots_offset + index as usize - 1)
                        } else {
                            self.frame().closure.upvalues[index as usize].clone()
                        };
                        closure.upvalues.push(upvalue);
                    }

                    let val = LoxValue::Closure(closure);
                    self.push(val);
                }
                OpCode::GetUpvalue => {
                    let slot = self.chunk().read_byte(ip);
                    let upvalue = self.frame().closure.upvalues[slot as usize].clone();
                    let lox_value = match &*upvalue.borrow() {
                        Upvalue::Open(location) => self.stack[*location].clone(),
                        Upvalue::Closed(value) => value.clone(),
                    };
                    self.push(lox_value);

                    ip += 1;
                }
                OpCode::SetUpvalue => {
                    let slot = self.chunk().read_byte(ip);
                    let val = self.peek(0)?;
                    let val = val.clone();
                    let upvalue = self.frame().closure.upvalues[slot as usize].clone();
                    match &mut *upvalue.borrow_mut() {
                        Upvalue::Open(location) => self.stack[*location] = val,
                        Upvalue::Closed(value) => *value = val,
                    }
                    ip += 1;
                }
                OpCode::CloseUpvalue => {
                    let location = self.stack.len() - 1; // old location upvalues point to
                    self.close_upvalues(location);
                    self.pop()?;
                }
                OpCode::Class => {
                    let class_name = self.chunk().read_constant(ip, CONST_SIZE);
                    let class_name = class_name.try_str()?;
                    let class = Class::new(class_name.clone());
                    let class = LoxValue::Class(class);
                    self.push(class);
                    ip += 1;
                }
                OpCode::GetProperty => {
                    let property = self.chunk().read_constant(ip, CONST_SIZE);
                    let property = property.try_str()?;
                    let instance = self.peek(0)?;
                    let field_value = if let LoxValue::Instance(instance) = instance {
                        instance.fields.get(property).cloned()
                    } else {
                        let line = self.chunk().line(ip - 1);
                        return Err(ProgramError::ExpectedInstance(line));
                    };
                    if let Some(val) = field_value {
                        self.pop()?; // instance
                        self.push(val);
                    }

                    ip += 1;
                }
                OpCode::SetProperty => {
                    let property = self.chunk().read_constant(ip, CONST_SIZE);
                    let property_name = property.try_str()?;
                    let property_value = self.pop()?;

                    let instance = self.peek_mut(0)?;

                    if let LoxValue::Instance(instance) = instance {
                        instance
                            .fields
                            .insert(property_name.clone(), property_value.clone());
                        self.pop()?; // instance
                        self.push(property_value);
                    } else {
                        let line = self.chunk().line(ip - 1);
                        return Err(ProgramError::ExpectedInstance(line));
                    };
                    ip += 1;
                }
            }
        }
        Ok(())
    }

    #[inline]
    fn close_upvalues(&mut self, location: usize) {
        let value = &self.stack[location];
        for upval in &self.open_upvalues {
            if upval.borrow().is_open_with_index(location) {
                upval.replace(Upvalue::Closed(value.clone()));
            }
        }

        self.open_upvalues.retain(|u| u.borrow().is_open());
    }

    #[inline]
    fn capture_upvalue(&mut self, location: usize) -> Rc<RefCell<Upvalue>> {
        if let Some(upval) = self
            .open_upvalues
            .iter()
            .rev()
            .find(|upval| upval.borrow().is_open_with_index(location))
        {
            upval.clone()
        } else {
            let upval = Rc::new(RefCell::new(Upvalue::Open(location)));
            self.open_upvalues.push(upval.clone());
            upval
        }
    }

    #[inline]
    fn call_value(&mut self, callee: LoxValue, args_count: usize) -> Result<(), ProgramError> {
        match callee {
            LoxValue::Closure(closure) => self.call(closure, args_count),
            LoxValue::Class(class) => self.call_class(class, args_count),
            LoxValue::Native(func) => self.call_native(&func, args_count),
            _ => Err(ProgramError::InvalidCallable),
        }
    }

    #[inline]
    fn call(&mut self, closure: Closure, args_count: usize) -> Result<(), ProgramError> {
        if closure.function.arity != args_count {
            return Err(ProgramError::InvalidFunctionArgsCount(
                closure.function.arity,
                args_count,
            ));
        }
        self.frame_count += 1;
        self.frame().slots_offset = self.stack.len() - args_count;
        self.frame().closure = closure;
        self.run()
    }

    #[inline]
    fn call_native(
        &mut self,
        func: &NativeFunction,
        args_count: usize,
    ) -> Result<(), ProgramError> {
        let mut args = Vec::new();
        for _ in 0..args_count {
            args.push(self.pop()?); // pop args
        }
        args.reverse();
        let args = args;
        self.pop()?; // native function value

        let result = (func.func)(&args)?;

        self.push(result);
        Ok(())
    }

    #[inline]
    fn call_class(&mut self, class: Class, args_count: usize) -> Result<(), ProgramError> {
        let instance = Instance::new(class);
        let instance = LoxValue::Instance(instance);
        let stack_size = self.stack.len();
        self.stack[stack_size - args_count - 1] = instance;
        Ok(())
    }

    #[inline]
    fn set_global(&mut self, offset: usize, constant_size: usize) -> Result<(), ProgramError> {
        let chunk = self.chunk();
        let val = chunk.ref_constant(offset, constant_size);

        let name = val.try_str()?;
        if !self.globals.contains_key(name) {
            let line = chunk.line(offset);
            return Err(ProgramError::UndefinedGlobal(line, name.clone()));
        }
        let value = self.peek(0)?;
        let value = value.clone();
        let name = name.clone();
        drop(chunk);
        self.globals.insert(name, value);
        Ok(())
    }

    #[inline]
    fn get_global(&mut self, offset: usize, constant_size: usize) -> Result<(), ProgramError> {
        let chunk = self.chunk();
        let val = chunk.ref_constant(offset, constant_size);
        let name = val.try_str()?;
        let Some(val) = self.globals.get(name) else {
            let line = chunk.line(offset);
            return Err(ProgramError::UndefinedGlobal(line, name.clone()));
        };
        let val = val.clone();
        drop(chunk);
        self.push(val);
        Ok(())
    }

    #[inline]
    fn define_global(&mut self, offset: usize, constant_size: usize) -> Result<(), ProgramError> {
        let chunk = self.chunk();
        let val = chunk.ref_constant(offset, constant_size);
        let name = val.try_str()?;
        let name = name.clone();
        drop(chunk);
        let value = self.peek(0)?;
        let value = value.clone();
        self.globals.insert(name, value);
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
    #[test_case("print 4 / 0;", "NaN")]
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
    #[test_case(r#"fun outer() {
  var a = 1;
  var b = 2;
  fun middle() {
    var c = 3;
    var d = 4;
    fun inner() {
      print a + c + b + d;
    }
    inner();
  }
  middle();
}

outer();"#, "10" ; "closure2")]
    #[test_case("fun outer() { var x = 10; fun inner() { x = 20; } inner(); print x; } outer();", "20" ; "assign in closure")]
    #[test_case("class Foo { } print Foo;", "Foo" ; "class print")]
    #[test_case("class Foo { } print Foo();", "Foo instance" ; "class instance print")]
    #[test_case("class Foo { } var foo = Foo(); print foo.value = 10;", "10" ; "instance field simple test")]
    #[test_case("class Pair { } var pair = Pair(); pair.first = 1; pair.second = 2; print pair.first + pair.second;", "3" ; "instance field usage test")]
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
