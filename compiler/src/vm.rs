#![allow(clippy::missing_errors_doc)]

use crate::chunk::Chunk;
use crate::value::{Class, Closure, Instance, Upvalue};
use crate::{RuntimeError, builtin};
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
        self.call_function(closure, 0)
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
    fn pop(&mut self) -> Result<LoxValue, RuntimeError> {
        self.stack.pop().ok_or(RuntimeError::InstructionsStackEmpty)
    }

    #[inline]
    fn pop_stack_n_times(&mut self, num_to_pop: usize) -> Result<(), RuntimeError> {
        for _ in 0..num_to_pop {
            self.pop()?;
        }
        Ok(())
    }

    #[inline]
    fn peek(&self, distance: usize) -> Result<&LoxValue, RuntimeError> {
        if self.stack.len() < distance + 1 {
            Err(RuntimeError::NotEnoughStackCapacity(
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

    fn run(&mut self) -> Result<(), RuntimeError> {
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
            ip += 1; // shift opcode offset itself
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
                        .map_err(|e| RuntimeError::Common(e.to_string()))?;
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
                        return Err(RuntimeError::ExpectedFunction(line));
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
                    let class = LoxValue::Class(Rc::new(RefCell::new(class)));
                    self.push(class);
                    ip += 1;
                }
                OpCode::GetProperty => {
                    let property = self.chunk().read_constant(ip, CONST_SIZE);
                    let property = property.try_str()?;
                    let instance = self.peek(0)?;
                    let value = if let LoxValue::Instance(instance) = instance {
                        Self::get_member(property, instance)
                    } else {
                        let line = self.chunk().line(ip - 1);
                        return Err(RuntimeError::ExpectedInstance(line));
                    };
                    if let Some(val) = value {
                        self.pop()?; // instance
                        self.push(val);
                    } else {
                        let line = self.chunk().line(ip - 1);
                        return Err(RuntimeError::UndefinedMethodOrProperty(
                            line,
                            property.clone(),
                        ));
                    }

                    ip += 1;
                }
                OpCode::SetProperty => {
                    let property = self.chunk().read_constant(ip, CONST_SIZE);
                    let property_name = property.try_str()?;
                    let instance = self.peek(1)?;
                    let property_value = self.peek(0)?;

                    if let LoxValue::Instance(instance) = instance {
                        instance
                            .borrow_mut()
                            .fields
                            .insert(property_name.to_owned(), property_value.clone());
                        let value = self.pop()?;
                        self.pop()?;
                        self.push(value);
                    } else {
                        let line = self.chunk().line(ip - 1);
                        return Err(RuntimeError::ExpectedInstance(line));
                    }
                    ip += 1;
                }
                OpCode::Method => {
                    let method_name = self.chunk().read_constant(ip, CONST_SIZE);
                    let method_name = method_name.try_str()?;
                    self.define_method(method_name, ip)?;
                    ip += 1;
                }
                OpCode::Invoke => {
                    let method_name = self.chunk().read_constant(ip, CONST_SIZE);
                    let method_name = method_name.try_str()?;
                    let argc = self.chunk().read_byte(ip + 1);
                    self.invoke(ip, method_name, argc)?;

                    ip += 2;
                }
                OpCode::Inherit => {
                    let super_class = self.peek(1)?;
                    let super_class = super_class.try_class()?;
                    let sub_class = self.peek(0)?;
                    let sub_class = sub_class.try_class()?;
                    for (name, method) in super_class.borrow().methods.iter() {
                        sub_class
                            .borrow_mut()
                            .methods
                            .insert(name.clone(), method.clone());
                    }

                    self.pop()?;
                }
                OpCode::GetSuper => {
                    let name = self.chunk().read_constant(ip, CONST_SIZE);
                    let name = name.try_str()?;
                    let super_class = self.pop()?;
                    let super_class = super_class.try_class()?;

                    let line = self.chunk().line(ip - 1);
                    let instance = self.pop()?;
                    let instance = instance.try_instance(line)?;
                    let super_class = super_class.borrow();
                    let method = super_class.methods.get(name).ok_or(
                        RuntimeError::UndefinedMethodOrProperty(line, name.to_owned()),
                    )?;
                    let bound = LoxValue::Bound(instance.clone(), Box::new(method.clone()));
                    self.push(bound);
                    ip += 1;
                }
                OpCode::SuperInvoke => {
                    let method_name = self.chunk().read_constant(ip, CONST_SIZE);
                    let method_name = method_name.try_str()?;
                    let argc = self.chunk().read_byte(ip + 1);
                    let super_class = self.pop()?;
                    let super_class = super_class.try_class()?;
                    let super_class = super_class.borrow();
                    let line = self.chunk().line(ip - 1);
                    let method = super_class.methods.get(method_name).ok_or(
                        RuntimeError::UndefinedMethodOrProperty(line, method_name.to_owned()),
                    )?;
                    self.call_value(method.clone(), argc as usize)?;

                    ip += 2;
                }
            }
        }
        Ok(())
    }

    #[inline]
    fn invoke(&mut self, ip: usize, method_name: &String, argc: u8) -> Result<(), RuntimeError> {
        let receiver = self.peek(argc as usize)?;
        match receiver.clone() {
            LoxValue::Instance(inst) => self.invoke_method(ip, method_name, argc, inst),
            LoxValue::Bound(inst, _) => self.invoke_method(ip, method_name, argc, inst),
            _ => {
                let line = self.chunk().line(ip - 1);
                Err(RuntimeError::ExpectedInstance(line))
            }
        }
    }

    #[inline]
    fn invoke_method(
        &mut self,
        ip: usize,
        method_name: &String,
        argc: u8,
        inst: Rc<RefCell<Instance>>,
    ) -> Result<(), RuntimeError> {
        let instance = inst.borrow();
        let class = instance.class.borrow();
        let method = class.methods.get(method_name);
        let field = instance.fields.get(method_name);
        if let Some(method) = method {
            self.call_value(method.clone(), argc as usize)?;
        } else if let Some(field) = field {
            let len = self.stack.len();
            self.stack[len - argc as usize - 1] = field.clone();
            self.call_value(field.clone(), argc as usize)?;
        } else {
            let line = self.chunk().line(ip - 1);
            return Err(RuntimeError::UndefinedMethodOrProperty(
                line,
                method_name.clone(),
            ));
        };
        Ok(())
    }

    #[inline]
    fn get_member(property: &String, instance: &Rc<RefCell<Instance>>) -> Option<LoxValue> {
        if let Some(field) = instance.borrow().fields.get(property) {
            Some(field.clone())
        } else {
            let inst = instance.borrow();
            let class = inst.class.borrow();
            let method = class.methods.get(property);
            method.map(|val| LoxValue::Bound(instance.clone(), Box::new(val.clone())))
        }
    }

    #[inline]
    fn define_method(&mut self, name: &str, ip: usize) -> Result<(), RuntimeError> {
        let method_closure = self.pop()?;
        let class = self.peek(0)?;
        if let LoxValue::Class(class) = class {
            class
                .borrow_mut()
                .methods
                .insert(name.to_owned(), method_closure);
        } else {
            let line = self.chunk().line(ip - 1);
            return Err(RuntimeError::ExpectedInstance(line));
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
    fn call_value(&mut self, callee: LoxValue, args_count: usize) -> Result<(), RuntimeError> {
        match callee {
            LoxValue::Closure(closure) => self.call_function(closure, args_count),
            LoxValue::Class(class) => self.call_class(class, args_count),
            LoxValue::Bound(receiver, method) => self.call_method(receiver, *method, args_count),
            LoxValue::Native(func) => self.call_native(&func, args_count),
            _ => Err(RuntimeError::InvalidCallable),
        }
    }

    #[inline]
    fn call_function(&mut self, closure: Closure, args_count: usize) -> Result<(), RuntimeError> {
        if closure.function.arity != args_count {
            return Err(RuntimeError::InvalidFunctionArgsCount(
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
    ) -> Result<(), RuntimeError> {
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
    fn call_class(
        &mut self,
        class: Rc<RefCell<Class>>,
        args_count: usize,
    ) -> Result<(), RuntimeError> {
        let instance = Instance::new(class.clone());
        let receiver = Rc::new(RefCell::new(instance));
        let instance = LoxValue::Instance(receiver.clone());
        let stack_size = self.stack.len();

        self.stack[stack_size - args_count - 1] = instance;
        if let Some(init) = class.borrow().methods.get(scanner::INIT) {
            self.call_method(receiver, init.clone(), args_count)?;
        } else if args_count > 0 {
            return Err(RuntimeError::InvalidFunctionArgsCount(0, args_count));
        }
        Ok(())
    }

    #[inline]
    fn call_method(
        &mut self,
        receiver: Rc<RefCell<Instance>>,
        method: LoxValue,
        args_count: usize,
    ) -> Result<(), RuntimeError> {
        let stack_size = self.stack.len();
        self.stack[stack_size - args_count - 1] = LoxValue::Instance(receiver);
        self.call_value(method, args_count)
    }

    #[inline]
    fn set_global(&mut self, offset: usize, constant_size: usize) -> Result<(), RuntimeError> {
        let chunk = self.chunk();
        let val = chunk.ref_constant(offset, constant_size);

        let name = val.try_str()?;
        if !self.globals.contains_key(name) {
            let line = chunk.line(offset);
            return Err(RuntimeError::UndefinedGlobal(line, name.clone()));
        }
        let value = self.peek(0)?;
        let value = value.clone();
        let name = name.clone();
        drop(chunk);
        self.globals.insert(name, value);
        Ok(())
    }

    #[inline]
    fn get_global(&mut self, offset: usize, constant_size: usize) -> Result<(), RuntimeError> {
        let chunk = self.chunk();
        let val = chunk.ref_constant(offset, constant_size);
        let name = val.try_str()?;
        let Some(val) = self.globals.get(name) else {
            let line = chunk.line(offset);
            return Err(RuntimeError::UndefinedGlobal(line, name.clone()));
        };
        let val = val.clone();
        drop(chunk);
        self.push(val);
        Ok(())
    }

    #[inline]
    fn define_global(&mut self, offset: usize, constant_size: usize) -> Result<(), RuntimeError> {
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
    #[test_case("class Bagel { method() { print 10;} } var b = Bagel(); b.method();", "10" ; "call class method")]
    #[test_case("class Bagel { method() { print 10;} } Bagel().method();", "10" ; "call class method without temp instance")]
    #[test_case("class Bagel{} var b = Bagel(); b.field = 1; print b.field;", "1" ; "get/set class field")]
    #[test_case("class Bagel { method() { print 10;} } Bagel().method();", "10" ; "call class method without instance in var")]
    #[test_case("class Bagel { method() { print 10;} } var b = Bagel().method; b();", "10" ; "call class method from assigned var")]
    #[test_case("class Class { init() { print 10; } method() { print 20; } } var c = Class(); c.method();", "10\n20" ; "class constructor without fields setting")]
    #[test_case("class Class { init(x) { print x; } method() { print 20; } } var c = Class(10); c.method();", "10\n20" ; "class constructor without fields setting pass parameters")]
    #[test_case("class Class { init() { this.some = 10; } method() { print this.some; } } var c = Class(); c.method();", "10" ; "class constructor")]
    #[test_case("class Class { init(x) { this.some = x; } method() { print this.some; } } var c = Class(10); c.method();", "10" ; "class constructor with arg")]
    #[test_case("class Class { init(x) { this.some = x; } method() { print this.some; } } Class(10).method();", "10" ; "class constructor with arg without temp local")]
    #[test_case("class Class { init(x) { this.some = x; } method() { print this.some; } } var c = Class(0); c.init(10); c.method();", "10" ; "class constructor with arg and invoking ctor directly")]
    #[test_case("class Class { init(x) { this.some = x; } method() { print this.some; } } var c = Class(0).init(10); c.method();", "10" ; "class constructor with arg and invoking ctor directly from instance")]
    #[test_case("class Oops { init() { fun f() { print 10; } this.field = f; } } var oops = Oops(); oops.field();", "10" ; "call on field")]
    #[test_case("class A { af() { print 10; }} class B < A { bf() { print 5; } } print B().af();", "10" ; "Call inherited method")]
    #[test_case("class A { af() { print 10; }} class B < A { bf() { print 5; } } B().bf();", "5" ; "Call own method with inherited present")]
    #[test_case("class A { af() { print 10; }} class B < A { bf() { this.af(); } } print B().bf();", "10" ; "Call inherited method inside other")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { super.method(); }} class C < B {} C().test();", "A" ; "Call super method inside grandchild class")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { super.method(); }} class C < B {} var c =C(); c.test();", "A" ; "Call super method inside grandchild class var variant")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { super.method(); }} B().test();", "A" ; "Call super method when shadowed defined in class")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { this.method(); }} B().test();", "B" ; "Call this method when shadowed defined in class")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { this.method(); }} class C < B {} C().test();", "B" ; "Call super method when shadowed defined in class and call shadowed")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { this.method(); }} class C < B {} var c = C(); c.test();", "B" ; "Call super method when shadowed defined in class and call shadowed var variant")]
    #[test_case("class A { init(param) { this.field = param; } test() { print this.field; } } class B < A {} var b = B(10); b.test();", "10" ; "Call superclass with parameter init subclass without parameter init")]
    #[test_case("class A { init(x) { this.f1 = x; } test() { return this.f1; } } class B < A { init(x, y) { this.f1 = x; this.f2 = y; } sum() { return this.test() + this.f1 + this.f2; } } var b = B(10, 20); print b.sum();", "40" ; "Call superclass with less init parameters then subclass")]
    #[test_case("class Foo{ init(arg) { print 1; } } fun init() { print 0; } init();", "0" ; "Plain function with init name")]
    #[test_case("class Foo { foo(arg) { this.arg1 = arg; } fooPrint() { print this.arg1; } } class Bar < Foo { bar(arg) { this.arg1 = arg; } barPrint() { print this.arg1; } } var b = Bar(); b.bar(1); b.fooPrint(); b.barPrint();", "1\n1" ; "Sets fields from base class")]
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
