#![allow(clippy::missing_errors_doc)]

use std::fmt;
use std::hash::BuildHasherDefault;
use std::rc::Rc;

use fnv::FnvHashMap;
use miette::LabeledSpan;
use num_traits::FromPrimitive;

use crate::object::{ObjId, ObjType, ObjectStore, string_chars, string_key};
use crate::value::LoxValue;
use crate::{RuntimeError, builtin};
use crate::{
    chunk::{Chunk, OpCode},
    compile::Parser,
};

const FRAMES_MAX: usize = 64;
const CONST_SIZE: usize = 1;
const CONST_LONG_SIZE: usize = 3;
const STACK_MAX: usize = FRAMES_MAX * 256;

struct CallFrame {
    closure: ObjId,
    ip: usize,
    slots: usize,
    chunk: Rc<Chunk>,
}

pub struct VirtualMachine<W: std::io::Write> {
    stack: [LoxValue; STACK_MAX],
    stack_top: usize,
    objects: ObjectStore,
    globals: FnvHashMap<ObjId, LoxValue>,
    frames: [CallFrame; FRAMES_MAX],
    frame_count: usize,
    open_upvalues: Option<ObjId>,
    init_string: ObjId,
    writer: W,
    line: usize,
}

struct FormattedValue<'a> {
    store: &'a ObjectStore,
    value: LoxValue,
}

impl fmt::Display for FormattedValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.store.format(self.value, f)
    }
}

impl<W: std::io::Write> VirtualMachine<W> {
    #[must_use]
    pub fn new(writer: W) -> Self {
        Self {
            stack: [LoxValue::NIL; STACK_MAX],
            stack_top: 0,
            objects: ObjectStore::new(),
            globals: FnvHashMap::with_capacity_and_hasher(32, BuildHasherDefault::default()),
            frames: std::array::from_fn(|_| CallFrame {
                closure: 0,
                ip: 0,
                slots: 0,
                chunk: Rc::new(Chunk::new()),
            }),
            frame_count: 0,
            open_upvalues: None,
            init_string: 0,
            writer,
            line: 0,
        }
    }

    pub fn init(&mut self) -> Result<(), RuntimeError> {
        for i in 0..self.stack_top {
            self.objects.release(self.stack[i]);
        }
        self.stack_top = 0;
        self.frame_count = 0;
        self.open_upvalues = None;
        for value in self.globals.values() {
            self.objects.release(*value);
        }
        self.globals.clear();
        self.init_string = self
            .objects
            .intern_string(scanner::INIT)?
            .obj_id_unchecked();
        self.add_global("clock", 0, builtin::clock)?;
        self.add_global("sqrt", 1, builtin::sqrt)?;
        self.add_global("min", 2, builtin::min)?;
        self.add_global("max", 2, builtin::max)?;
        Ok(())
    }

    pub fn interpret(&mut self, content: &str, printcode: bool) -> crate::Result<()> {
        let function = {
            let mut parser = Parser::new(content, printcode, &mut self.objects);
            parser.compile()?
        };
        let function_val = self
            .objects
            .alloc_function(function)
            .map_err(|e| miette::miette!("{e}"))?;
        let function_id = function_val
            .try_function(&self.objects)
            .map_err(|e| miette::miette!("{e}"))?;
        let closure_val = self
            .objects
            .alloc_closure(function_id)
            .map_err(|e| miette::miette!("{e}"))?;
        self.push(closure_val);
        self.call_function(closure_val, 0)
            .map_err(|e| miette::miette!("{e}"))?;
        self.run().map_err(|e| {
            let mut stack_trace = Vec::with_capacity(self.frame_count);
            for i in 0..self.frame_count {
                if let Ok(frame_name) = self.format_frame(i) {
                    stack_trace.push(frame_name);
                }
            }
            stack_trace.reverse();
            miette::miette!(
                labels = vec![LabeledSpan::at(
                    scanner::Lexer::new(content).line_span(self.line),
                    format!("{e}. Stack trace:\n{}", stack_trace.join("\n"))
                )],
                "Runtime error"
            )
        })
    }

    fn format_frame(&self, index: usize) -> Result<String, RuntimeError> {
        let closure_id = self.frames[index].closure;
        let function_id = self.objects.closure(closure_id)?.function;
        let name = self
            .objects
            .string(self.objects.function(function_id)?.name)?
            .chars
            .clone();
        let start = self.objects.function(function_id)?.chunk.first_line();
        Ok(format!(" at {name}:{start}"))
    }

    fn add_global(
        &mut self,
        name: &str,
        arity: usize,
        func: fn(&[LoxValue]) -> crate::Result<LoxValue, RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let name_val = self.objects.intern_string(name)?;
        let key = name_val.obj_id_unchecked();
        let native = self.objects.alloc_native(name, arity, func)?;
        self.objects.retain(native);
        self.globals.insert(key, native);
        Ok(())
    }

    #[inline]
    fn push(&mut self, value: LoxValue) {
        self.objects.retain(value);
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    #[inline]
    fn pop(&mut self) -> Result<LoxValue, RuntimeError> {
        if self.stack_top == 0 {
            return Err(RuntimeError::InstructionsStackEmpty);
        }
        self.stack_top -= 1;
        let value = self.stack[self.stack_top];
        self.objects.release(value);
        Ok(value)
    }

    #[inline]
    fn set_stack(&mut self, index: usize, value: LoxValue) {
        if self.stack[index] == value {
            return;
        }
        self.objects.release(self.stack[index]);
        self.objects.retain(value);
        self.stack[index] = value;
    }

    #[inline]
    fn release_stack_range(&mut self, start: usize, end: usize) {
        for index in start..end {
            self.objects.release(self.stack[index]);
        }
    }

    #[inline]
    fn peek(&self, distance: usize) -> Result<LoxValue, RuntimeError> {
        if self.stack_top < distance + 1 {
            Err(RuntimeError::NotEnoughStackCapacity(
                distance,
                self.stack_top,
            ))
        } else {
            Ok(self.stack[self.stack_top - 1 - distance])
        }
    }

    #[inline]
    fn read_u16(code: &[u8], offset: usize) -> usize {
        (code[offset + 1] as usize) << 8 | code[offset] as usize
    }

    #[inline]
    fn read_u24(code: &[u8], offset: usize) -> usize {
        (code[offset + 2] as usize) << 16 | (code[offset + 1] as usize) << 8 | code[offset] as usize
    }

    #[inline]
    fn constant_index(code: &[u8], offset: usize, size: usize) -> Result<usize, RuntimeError> {
        match size {
            CONST_SIZE => Ok(code[offset] as usize),
            CONST_LONG_SIZE => Ok(Self::read_u24(code, offset)),
            _ => Err(RuntimeError::InvalidInstruction(offset)),
        }
    }

    #[inline]
    fn runtime_error(&mut self, line: usize, err: RuntimeError) -> Result<(), RuntimeError> {
        self.line = line;
        Err(err)
    }

    fn write_value(&mut self, value: LoxValue) -> Result<(), RuntimeError> {
        let formatted = FormattedValue {
            store: &self.objects,
            value,
        };
        writeln!(self.writer, "{}", format_args!("{formatted}"))
            .map_err(|e| RuntimeError::Common(e.to_string()))
    }

    fn value_to_string(&self, value: LoxValue) -> String {
        format!(
            "{}",
            format_args!(
                "{}",
                FormattedValue {
                    store: &self.objects,
                    value,
                }
            )
        )
    }

    fn run(&mut self) -> Result<(), RuntimeError> {
        while self.frame_count > 0 {
            let frame_index = self.frame_count - 1;
            let mut ip = self.frames[frame_index].ip;
            let slots = self.frames[frame_index].slots;
            let closure_id = self.frames[frame_index].closure;
            let chunk = Rc::clone(&self.frames[frame_index].chunk);
            let bytecode = chunk.code.as_slice();
            let constants = chunk.constants.as_slice();

            if ip >= bytecode.len() {
                return Ok(());
            }

            let instruction_ip = ip;
            self.line = chunk.line(instruction_ip);
            let Some(opcode) = OpCode::from_u8(bytecode[ip]) else {
                return self
                    .runtime_error(self.line, RuntimeError::InvalidInstruction(instruction_ip));
            };
            ip += 1;

            #[cfg(feature = "disassembly")]
            {
                print!("          ");
                for i in 0..self.stack_top {
                    print!(
                        "[ {} ]",
                        FormattedValue {
                            store: &self.objects,
                            value: self.stack[i],
                        }
                    );
                }
                println!();
                let closure_id = self.frames[frame_index].closure;
                let function_id = self.objects.closure(closure_id)?.function;
                let disasm_chunk = self.objects.function(function_id)?.chunk.clone();
                disasm_chunk.disassembly_instruction(instruction_ip, &self.objects);
            }

            match opcode {
                OpCode::Constant => {
                    let ix = Self::constant_index(bytecode, ip, CONST_SIZE)?;
                    ip += CONST_SIZE;
                    self.push(constants[ix]);
                }
                OpCode::ConstantLong => {
                    let ix = Self::constant_index(bytecode, ip, CONST_LONG_SIZE)?;
                    ip += CONST_LONG_SIZE;
                    self.push(constants[ix]);
                }
                OpCode::Return => {
                    let ret_slot = self.stack_top - 1;
                    let value = self.stack[ret_slot];
                    self.stack_top = ret_slot;
                    let returns_slot0 =
                        slots > 0 && value == self.stack[slots.saturating_sub(1)];

                    if slots > 0 {
                        self.close_upvalue_at(slots - 1)?;
                    }
                    self.close_upvalues(slots)?;

                    self.frame_count -= 1;

                    let release_from = if returns_slot0 {
                        slots
                    } else {
                        slots.saturating_sub(1)
                    };
                    self.release_stack_range(release_from, ret_slot);
                    self.objects.release(value);

                    if returns_slot0 {
                        self.stack_top = if slots > 0 { slots } else { 1 };
                    } else {
                        self.stack_top = slots.saturating_sub(1);
                        self.push(value);
                    }

                    if self.frame_count == 0 {
                        return Ok(());
                    }
                    continue;
                }
                OpCode::Negate => {
                    let value = self.pop()?;
                    let value = value.try_num()?;
                    self.push(LoxValue::number(-value));
                }
                OpCode::Add => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    if let Ok(l_id) = a.try_str(&self.objects) {
                        let l = string_chars(&self.objects, l_id)?;
                        let r = if b.is_nil() {
                            String::new()
                        } else {
                            self.value_to_string(b)
                        };
                        let result = self.objects.intern_string(l.to_owned() + &r)?;
                        self.push(result);
                    } else if let Ok(r_id) = b.try_str(&self.objects) {
                        let r = string_chars(&self.objects, r_id)?;
                        let l = if a.is_nil() {
                            String::new()
                        } else {
                            self.value_to_string(a)
                        };
                        let result = self.objects.intern_string(l.to_owned() + r)?;
                        self.push(result);
                    } else {
                        let l = a.try_num()?;
                        let r = b.try_num()?;
                        self.push(LoxValue::number(l + r));
                    }
                }
                OpCode::Subtract => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(LoxValue::number(a.try_num()? - b.try_num()?));
                }
                OpCode::Multiply => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(LoxValue::number(a.try_num()? * b.try_num()?));
                }
                OpCode::Divide => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let a = a.try_num()?;
                    let b = b.try_num()?;
                    if b == 0.0 {
                        self.push(LoxValue::number(f64::NAN));
                    } else {
                        self.push(LoxValue::number(a / b));
                    }
                }
                OpCode::Nil => self.push(LoxValue::NIL),
                OpCode::True => self.push(LoxValue::TRUE),
                OpCode::False => self.push(LoxValue::FALSE),
                OpCode::Not => {
                    let value = self.pop()?;
                    self.push(LoxValue::bool_val(value.is_falsey()));
                }
                OpCode::Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(LoxValue::bool_val(a.equal(b)));
                }
                OpCode::Less => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(LoxValue::bool_val(a.less(b, &self.objects)?));
                }
                OpCode::Greater => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let lt = a.less(b, &self.objects)?;
                    let gt = !lt && !a.equal(b);
                    self.push(LoxValue::bool_val(gt));
                }
                OpCode::Print => {
                    let value = self.peek(0)?;
                    self.write_value(value)?;
                    self.pop()?;
                }
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::DefineGlobal => {
                    let ix = Self::constant_index(bytecode, ip, CONST_SIZE)?;
                    ip += CONST_SIZE;
                    self.define_global(constants[ix])?;
                }
                OpCode::DefineGlobalLong => {
                    let ix = Self::constant_index(bytecode, ip, CONST_LONG_SIZE)?;
                    ip += CONST_LONG_SIZE;
                    self.define_global(constants[ix])?;
                }
                OpCode::GetGlobal => {
                    let ix = Self::constant_index(bytecode, ip, CONST_SIZE)?;
                    ip += CONST_SIZE;
                    self.get_global(constants[ix])?;
                }
                OpCode::GetGlobalLong => {
                    let ix = Self::constant_index(bytecode, ip, CONST_LONG_SIZE)?;
                    ip += CONST_LONG_SIZE;
                    self.get_global(constants[ix])?;
                }
                OpCode::SetGlobal => {
                    let ix = Self::constant_index(bytecode, ip, CONST_SIZE)?;
                    ip += CONST_SIZE;
                    self.set_global(constants[ix])?;
                }
                OpCode::SetGlobalLong => {
                    let ix = Self::constant_index(bytecode, ip, CONST_LONG_SIZE)?;
                    ip += CONST_LONG_SIZE;
                    self.set_global(constants[ix])?;
                }
                OpCode::GetLocal => {
                    let frame_offset = bytecode[ip] as usize;
                    ip += 1;
                    let local_index = slots + frame_offset - 1;
                    self.push(self.stack[local_index]);
                }
                OpCode::SetLocal => {
                    let frame_offset = bytecode[ip] as usize;
                    ip += 1;
                    let local_index = slots + frame_offset - 1;
                    let value = self.stack[self.stack_top - 1];
                    self.set_stack(local_index, value);
                }
                OpCode::JumpIfFalse => {
                    let offset = Self::read_u16(bytecode, ip);
                    ip += 2;
                    if self.peek(0)?.is_falsey() {
                        ip += offset;
                    }
                }
                OpCode::Jump => {
                    let offset = Self::read_u16(bytecode, ip);
                    ip += 2;
                    ip += offset;
                }
                OpCode::Loop => {
                    let offset = Self::read_u16(bytecode, ip);
                    ip += 2;
                    ip -= offset;
                }
                OpCode::Call => {
                    let args_count = bytecode[ip] as usize;
                    ip += 1;
                    self.frames[frame_index].ip = ip;
                    self.call_value_at(args_count)?;
                    continue;
                }
                OpCode::Invoke => {
                    let method_ix = Self::constant_index(bytecode, ip, CONST_SIZE)?;
                    let method_name = constants[method_ix];
                    let argc = bytecode[ip + 1];
                    ip += 2;
                    self.frames[frame_index].ip = ip;
                    self.invoke(method_name, argc)?;
                    continue;
                }
                OpCode::Closure => {
                    let function_ix = Self::constant_index(bytecode, ip, CONST_SIZE)?;
                    ip += CONST_SIZE;
                    let function_value = constants[function_ix];
                    let function_id = function_value.try_function(&self.objects)?;
                    let closure_val = self.objects.alloc_closure(function_id)?;
                    let new_closure_id = closure_val.try_closure(&self.objects)?;
                    let upvalues_count = self.objects.function(function_id)?.upvalue_count;
                    let mut upvalues = Vec::with_capacity(upvalues_count);

                    for _ in 0..upvalues_count {
                        let is_local = bytecode[ip];
                        let index = bytecode[ip + 1];
                        ip += 2;
                        let upvalue = if is_local == 1 {
                            self.capture_upvalue(slots + index as usize - 1)?
                        } else {
                            self.objects.closure(closure_id)?.upvalues[index as usize]
                        };
                        upvalues.push(upvalue);
                    }

                    self.objects.closure_mut(new_closure_id)?.upvalues = upvalues;
                    self.push(closure_val);
                }
                OpCode::GetUpvalue => {
                    let slot = bytecode[ip] as usize;
                    ip += 1;
                    let upvalue_id = self.objects.closure(closure_id)?.upvalues[slot];
                    let upvalue = self.objects.upvalue(upvalue_id)?;
                    let lox_value = match upvalue.location {
                        Some(location) => self.stack[location],
                        None => upvalue.closed,
                    };
                    self.push(lox_value);
                }
                OpCode::SetUpvalue => {
                    let slot = bytecode[ip] as usize;
                    ip += 1;
                    let val = self.stack[self.stack_top - 1];
                    let upvalue_id = self.objects.closure(closure_id)?.upvalues[slot];
                    let location = self.objects.upvalue(upvalue_id)?.location;
                    if let Some(location) = location {
                        self.set_stack(location, val);
                    } else {
                        let old_closed = {
                            let upvalue = self.objects.upvalue_mut(upvalue_id)?;
                            let old = upvalue.closed;
                            upvalue.closed = val;
                            old
                        };
                        self.objects.release(old_closed);
                        self.objects.retain(val);
                    }
                }
                OpCode::CloseUpvalue => {
                    let location = self.stack_top - 1;
                    self.close_upvalues(location)?;
                    self.pop()?;
                }
                OpCode::Class => {
                    let class_ix = Self::constant_index(bytecode, ip, CONST_SIZE)?;
                    ip += CONST_SIZE;
                    let class_name = constants[class_ix];
                    let class = self.objects.alloc_class(class_name)?;
                    self.push(class);
                }
                OpCode::GetProperty => {
                    let prop_ix = Self::constant_index(bytecode, ip, CONST_SIZE)?;
                    ip += CONST_SIZE;
                    let property_id = constants[prop_ix].try_str(&self.objects)?;
                    let instance_id = self.peek(0)?.try_instance(&self.objects)?;
                    if let Some(val) =
                        Self::get_member(instance_id, property_id, &mut self.objects)?
                    {
                        self.pop()?;
                        self.push(val);
                    } else {
                        let name = string_chars(&self.objects, property_id)?.to_owned();
                        return self.runtime_error(
                            self.line,
                            RuntimeError::UndefinedMethodOrProperty(name),
                        );
                    }
                }
                OpCode::SetProperty => {
                    let prop_ix = Self::constant_index(bytecode, ip, CONST_SIZE)?;
                    ip += CONST_SIZE;
                    let property_id =
                        constants[prop_ix].try_str(&self.objects).inspect_err(|_| {
                            self.line = chunk.line(instruction_ip);
                        })?;
                    self.objects.retain(self.stack[self.stack_top - 1]);
                    let property_value = self.pop()?;
                    let instance_id = self.pop()?.try_instance(&self.objects)?;
                    let old = {
                        let instance = self.objects.instance_mut(instance_id)?;
                        instance.fields.insert(property_id, property_value)
                    };
                    if let Some(old) = old {
                        if old != property_value {
                            self.objects.release(old);
                        }
                    }
                    self.push(property_value);
                }
                OpCode::Method => {
                    let method_ix = Self::constant_index(bytecode, ip, CONST_SIZE)?;
                    ip += CONST_SIZE;
                    self.define_method(constants[method_ix])?;
                }
                OpCode::Inherit => {
                    let super_class_id = self.peek(1)?.try_class(&self.objects)?;
                    let sub_class_id = self.peek(0)?.try_class(&self.objects)?;
                    let super_methods = self.objects.class(super_class_id)?.methods.clone();
                    for (name, method) in super_methods {
                        if !self
                            .objects
                            .class(sub_class_id)?
                            .methods
                            .contains_key(&name)
                        {
                            self.objects.retain(method);
                            self.objects
                                .class_mut(sub_class_id)?
                                .methods
                                .insert(name, method);
                        }
                    }
                    self.pop()?;
                }
                OpCode::GetSuper => {
                    let name_ix = Self::constant_index(bytecode, ip, CONST_SIZE)?;
                    ip += CONST_SIZE;
                    let name_id = constants[name_ix].try_str(&self.objects).inspect_err(|_| {
                        self.line = chunk.line(instruction_ip);
                    })?;
                    let super_class_id = self.pop()?.try_class(&self.objects)?;
                    let instance_id = self.pop()?.try_instance(&self.objects)?;
                    let Some(method) = self.objects.class(super_class_id)?.methods.get(&name_id)
                    else {
                        return Err(RuntimeError::UndefinedMethodOrProperty(
                            string_chars(&self.objects, name_id)?.to_owned(),
                        ));
                    };
                    let method_closure_id = method.try_closure(&self.objects)?;
                    let bound = self.objects.alloc_bound_method(
                        LoxValue::from_obj(instance_id, ObjType::Instance),
                        method_closure_id,
                    )?;
                    self.push(bound);
                }
                OpCode::SuperInvoke => {
                    let method_ix = Self::constant_index(bytecode, ip, CONST_SIZE)?;
                    let method_name = constants[method_ix];
                    let argc = bytecode[ip + 1];
                    ip += 2;
                    self.frames[frame_index].ip = ip;
                    let err_line = chunk.line(instruction_ip);
                    let name_id = string_key(&self.objects, method_name).ok_or_else(|| {
                        self.line = err_line;
                        RuntimeError::ExpectedString(method_name)
                    })?;
                    let super_class_id = self.pop()?.try_class(&self.objects)?;
                    let Some(method) = self.objects.class(super_class_id)?.methods.get(&name_id)
                    else {
                        return Err(RuntimeError::UndefinedMethodOrProperty(
                            string_chars(&self.objects, name_id)?.to_owned(),
                        ));
                    };
                    self.call_value(*method, argc as usize)?;
                    continue;
                }
            }
            self.frames[frame_index].ip = ip;
        }
        Ok(())
    }

    #[inline]
    fn invoke(&mut self, method_name: LoxValue, argc: u8) -> Result<(), RuntimeError> {
        let method_key = string_key(&self.objects, method_name)
            .ok_or(RuntimeError::ExpectedString(method_name))?;
        let receiver = self.peek(argc as usize)?;
        let instance_id = receiver.try_instance(&self.objects)?;
        let (is_field, callable) = {
            let instance = self.objects.instance(instance_id)?;
            if let Some(field) = instance.fields.get(&method_key) {
                (true, *field)
            } else {
                let class_id = instance.class;
                let Some(callable) = self
                    .objects
                    .class(class_id)?
                    .methods
                    .get(&method_key)
                    .copied()
                else {
                    return Err(RuntimeError::UndefinedMethodOrProperty(
                        string_chars(&self.objects, method_key)?.to_owned(),
                    ));
                };
                (false, callable)
            }
        };
        if is_field {
            self.set_stack(self.stack_top - argc as usize - 1, callable);
        }
        self.call_value(callable, argc as usize)
    }

    #[inline]
    fn get_member(
        instance_id: ObjId,
        property_id: ObjId,
        store: &mut ObjectStore,
    ) -> Result<Option<LoxValue>, RuntimeError> {
        let class_id = {
            let instance = store.instance(instance_id)?;
            if let Some(field) = instance.fields.get(&property_id) {
                return Ok(Some(*field));
            }
            instance.class
        };
        let Some(method) = store.class(class_id)?.methods.get(&property_id).copied() else {
            return Ok(None);
        };
        let method_closure_id = method.try_closure(store)?;
        Ok(Some(store.alloc_bound_method(
            LoxValue::from_obj(instance_id, ObjType::Instance),
            method_closure_id,
        )?))
    }

    #[inline]
    fn define_method(&mut self, name: LoxValue) -> Result<(), RuntimeError> {
        let method_key =
            string_key(&self.objects, name).ok_or(RuntimeError::ExpectedString(name))?;
        let method_closure = self.pop()?;
        let class_id = self.peek(0)?.try_class(&self.objects)?;
        let class = self.objects.class_mut(class_id)?;
        if let Some(old) = class.methods.insert(method_key, method_closure) {
            self.objects.release(old);
        }
        self.objects.retain(method_closure);
        Ok(())
    }

    #[inline]
    fn close_upvalues(&mut self, from_slot: usize) -> Result<(), RuntimeError> {
        while let Some(upvalue_id) = self.open_upvalues {
            let location = match self.objects.upvalue(upvalue_id)?.location {
                Some(location) if location >= from_slot => location,
                _ => break,
            };
            let next = self.objects.upvalue(upvalue_id)?.next;
            let closed_value = self.stack[location];
            let old_closed = {
                let upvalue = self.objects.upvalue_mut(upvalue_id)?;
                let old = upvalue.closed;
                upvalue.closed = closed_value;
                upvalue.location = None;
                old
            };
            self.objects.release(old_closed);
            self.objects.retain(closed_value);
            self.open_upvalues = next;
        }
        Ok(())
    }

    #[inline]
    fn close_upvalue_at(&mut self, location: usize) -> Result<(), RuntimeError> {
        let mut prev: Option<ObjId> = None;
        let mut current = self.open_upvalues;

        while let Some(upvalue_id) = current {
            let upvalue = self.objects.upvalue(upvalue_id)?;
            if upvalue.location == Some(location) {
                let next = upvalue.next;
                let closed_value = self.stack[location];
                let old_closed = {
                    let upvalue = self.objects.upvalue_mut(upvalue_id)?;
                    let old = upvalue.closed;
                    upvalue.closed = closed_value;
                    upvalue.location = None;
                    old
                };
                self.objects.release(old_closed);
                self.objects.retain(closed_value);
                if let Some(prev_id) = prev {
                    self.objects.upvalue_mut(prev_id)?.next = next;
                } else {
                    self.open_upvalues = next;
                }
                return Ok(());
            }
            prev = Some(upvalue_id);
            current = upvalue.next;
        }
        Ok(())
    }

    #[inline]
    fn capture_upvalue(&mut self, location: usize) -> Result<ObjId, RuntimeError> {
        let mut prev: Option<ObjId> = None;
        let mut current = self.open_upvalues;

        while let Some(upvalue_id) = current {
            let upvalue = self.objects.upvalue(upvalue_id)?;
            match upvalue.location {
                Some(loc) if loc > location => {
                    prev = Some(upvalue_id);
                    current = upvalue.next;
                }
                Some(loc) if loc == location => return Ok(upvalue_id),
                _ => break,
            }
        }

        let created = self.objects.alloc_upvalue(location)?;
        {
            let created_upvalue = self.objects.upvalue_mut(created)?;
            created_upvalue.next = current;
        }

        if let Some(prev_id) = prev {
            self.objects.upvalue_mut(prev_id)?.next = Some(created);
        } else {
            self.open_upvalues = Some(created);
        }

        Ok(created)
    }

    #[inline]
    fn call_value_at(&mut self, args_count: usize) -> Result<(), RuntimeError> {
        let callee = self.stack[self.stack_top - args_count - 1];
        self.call_value(callee, args_count)
    }

    #[inline]
    fn call_value(&mut self, callee: LoxValue, args_count: usize) -> Result<(), RuntimeError> {
        match callee.obj_type() {
            Some(ObjType::Closure) => self.call_function(callee, args_count),
            Some(ObjType::Class) => self.call_class(callee.obj_id_unchecked(), args_count),
            Some(ObjType::BoundMethod) => {
                let bound_id = callee.obj_id_unchecked();
                let bound = self.objects.bound_method(bound_id)?;
                let receiver = bound.receiver;
                let method = bound.method;
                self.call_method(receiver, method, args_count)
            }
            Some(ObjType::Native) => self.call_native(callee.obj_id_unchecked(), args_count),
            _ => Err(RuntimeError::InvalidCallable(callee)),
        }
    }

    #[inline]
    fn call_function(
        &mut self,
        closure_val: LoxValue,
        args_count: usize,
    ) -> Result<(), RuntimeError> {
        let closure_id = closure_val.try_closure(&self.objects)?;
        let function_id = self.objects.closure(closure_id)?.function;
        let function = self.objects.function(function_id)?;
        if function.arity != args_count {
            return Err(RuntimeError::InvalidFunctionArgsCount(
                function.arity,
                args_count,
            ));
        }
        if self.frame_count == FRAMES_MAX - 1 {
            return Err(RuntimeError::StackOverflow);
        }
        let frame = &mut self.frames[self.frame_count];
        frame.slots = self.stack_top - args_count;
        frame.closure = closure_id;
        frame.ip = 0;
        frame.chunk = Rc::clone(&function.chunk);
        self.frame_count += 1;
        Ok(())
    }

    #[inline]
    fn call_native(&mut self, native_id: ObjId, args_count: usize) -> Result<(), RuntimeError> {
        let native = self.objects.native(native_id)?;
        if native.arity != args_count {
            return Err(RuntimeError::InvalidFunctionArgsCount(
                native.arity,
                args_count,
            ));
        }
        let args_start = self.stack_top - args_count;
        let result = (native.func)(&self.stack[args_start..self.stack_top])?;
        self.release_stack_range(args_start - 1, self.stack_top);
        self.stack_top = args_start - 1;
        self.push(result);
        Ok(())
    }

    #[inline]
    fn call_class(&mut self, class_id: ObjId, args_count: usize) -> Result<(), RuntimeError> {
        let instance = self.objects.alloc_instance(class_id)?;
        self.set_stack(self.stack_top - args_count - 1, instance);

        if let Some(init) = self.objects.class(class_id)?.methods.get(&self.init_string) {
            self.call_value(*init, args_count)
        } else if args_count > 0 {
            Err(RuntimeError::InvalidFunctionArgsCount(0, args_count))
        } else {
            Ok(())
        }
    }

    #[inline]
    fn call_method(
        &mut self,
        receiver: LoxValue,
        method: ObjId,
        args_count: usize,
    ) -> Result<(), RuntimeError> {
        self.set_stack(self.stack_top - args_count - 1, receiver);
        self.call_function(LoxValue::from_obj(method, ObjType::Closure), args_count)
    }

    #[inline]
    fn set_global(&mut self, name: LoxValue) -> Result<(), RuntimeError> {
        let key = string_key(&self.objects, name).ok_or(RuntimeError::ExpectedString(name))?;
        if !self.globals.contains_key(&key) {
            return Err(RuntimeError::UndefinedGlobal(
                string_chars(&self.objects, key)?.to_owned(),
            ));
        }
        let value = self.stack[self.stack_top - 1];
        if let Some(old) = self.globals.insert(key, value) {
            self.objects.release(old);
        }
        self.objects.retain(value);
        Ok(())
    }

    #[inline]
    fn get_global(&mut self, name: LoxValue) -> Result<(), RuntimeError> {
        let key = string_key(&self.objects, name).ok_or(RuntimeError::ExpectedString(name))?;
        let Some(val) = self.globals.get(&key) else {
            return Err(RuntimeError::UndefinedGlobal(
                string_chars(&self.objects, key)?.to_owned(),
            ));
        };
        self.push(*val);
        Ok(())
    }

    #[inline]
    fn define_global(&mut self, name: LoxValue) -> Result<(), RuntimeError> {
        let key = string_key(&self.objects, name).ok_or(RuntimeError::ExpectedString(name))?;
        self.stack_top -= 1;
        let value = self.stack[self.stack_top];
        if let Some(old) = self.globals.insert(key, value) {
            self.objects.release(old);
        }
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
    #[test_case("print !1;", "false" ; "not number")]
    #[test_case("print !\"s\";", "false" ; "not string")]
    #[test_case("class Foo{} print !Foo;", "false" ; "not class")]
    #[test_case("fun foo() {} print !foo;", "false" ; "not function")]
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
    #[test_case("fun foo() { print 10; } foo();", "10" ; "simple call no args")]
    #[test_case("fun foo(v) { print v; } foo(10);", "10" ; "simple call one arg")]
    #[test_case("fun sum(a1, a2) { print a1 + a2; } sum(1, 2);", "3" ; "simple call two args")]
    #[test_case("fun foo(x) { return x + 1; } print foo(1);", "2" ; "function with return")]
    #[test_case("fun fib(n) { if (n < 2) return n; return fib(n - 1) + fib(n - 2); } print fib(8);", "21" ; "fibonacci")]
    #[test_case("fun foo(n) { if (n < 2) return n; return 10; } print foo(1);", "1" ; "conditional return success")]
    #[test_case("fun foo(n) { if (n < 2) return n; return 10; } print foo(5);", "10" ; "conditional return fail")]
    #[test_case("print clock() - clock();", "0" ; "simple clock call")]
    #[test_case("print sqrt(9);", "3" ; "use sqrt call")]
    #[test_case("print min(1, 2);", "1" ; "use min call")]
    #[test_case("print max(1, 2);", "2" ; "use max call")]
    #[test_case("fun foo() { var i = 1; fun bar(x) { return i + x; } return bar; } print foo()(2);", "3" ; "closure")]
    #[test_case(
        "fun make() { var a = \"A\"; var b = \"B\"; fun read() { print a; print b; } return read; } var f = make(); f();",
        "A\nB" ; "closure return captures two locals"
    )]
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
    #[test_case("class Class { init(x) { this.some = x; } method(y) { this.some = y; print this.some; } } var c = Class(10); c.method(20);", "20" ; "set field in method")]
    #[test_case("class Class { init(x) { this.some = x; } method() { print this.some; } } Class(10).method();", "10" ; "class constructor with arg without temp local")]
    #[test_case("class Class { init(x) { this.some = x; } method() { print this.some; } } var c = Class(0); c.init(10); c.method();", "10" ; "class constructor with arg and invoking ctor directly")]
    #[test_case("class Class { init(x) { this.some = x; } method() { print this.some; } } var c = Class(0).init(10); c.method();", "10" ; "class constructor with arg and invoking ctor directly from instance")]
    #[test_case("class Oops { init() { fun f() { print 10; } this.field = f; } } var oops = Oops(); oops.field();", "10" ; "call on field")]
    #[test_case("class Foo { method(a) { print \"method\"; print a; } other(a) { print \"other\"; print a; } } var foo = Foo(); var method = foo.method; foo.method = foo.other; foo.method(1); method(2);", "other\n1\nmethod\n2" ; "field shadows bound method on invoke")]
    #[test_case("class A { af() { print 10; }} class B < A { bf() { print 5; } } B().af();", "10" ; "Call inherited method")]
    #[test_case("class A { af() { print 10; }} class B < A { bf() { print 5; } } B().bf();", "5" ; "Call own method with inherited present")]
    #[test_case("class A { af() { print 10; }} class B < A { bf() { this.af(); } } B().bf();", "10" ; "Call inherited method inside other")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { super.method(); }} class C < B {} C().test();", "A" ; "Call super method inside grandchild class")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { super.method(); }} class C < B {} var c =C(); c.test();", "A" ; "Call super method inside grandchild class var variant")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { super.method(); }} B().test();", "A" ; "Call super method when shadowed defined in class")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { this.method(); }} B().test();", "B" ; "Call this method when shadowed defined in class")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { this.method(); }} class C < B {} C().test();", "B" ; "Call super method when shadowed defined in class and call shadowed")]
    #[test_case("class A { method() { print \"A\"; }} class B < A {  method() { print \"B\";  } test() { this.method(); }} class C < B {} var c = C(); c.test();", "B" ; "Call super method when shadowed defined in class and call shadowed var variant")]
    #[test_case("class A { init(param) { this.field = param; } test() { print this.field; } } class B < A {} var b = B(10); b.test();", "10" ; "Call superclass with parameter init subclass without parameter init")]
    #[test_case("class A { init(x) { this.f1 = x; } test() { return this.f1; } } class B < A { init(x, y) { this.f1 = x; this.f2 = y; } sum() { return this.test() + this.f1 + this.f2; } } var b = B(10, 20); print b.sum();", "40" ; "Call superclass with less init parameters then subclass")]
    #[test_case("class Foo{ init(arg) { print 1; } } fun init() { print 0; } init();", "0" ; "Plain function with init name")]
    #[test_case("fun init() { return \"bar\"; } print init();", "bar" ; "init return value")]
    #[test_case("class Foo { foo(arg) { this.arg1 = arg; } fooPrint() { print this.arg1; } } class Bar < Foo { bar(arg) { this.arg1 = arg; } barPrint() { print this.arg1; } } var b = Bar(); b.bar(1); b.fooPrint(); b.barPrint();", "1\n1" ; "Sets fields from base class")]
    #[test_case(r#"
var f1;
var f2;
var f3;

for (var i = 1; i < 4; i = i + 1) {
  var j = i;
  fun f() {
    print i;
    print j;
  }

  if (j == 1) f1 = f;
  else if (j == 2) f2 = f;
  else f3 = f;
}

f1(); // expect: 4
      // expect: 4
      // expect: 1
f2(); // expect: 4
      // expect: 2
f3(); // expect: 4
      // expect: 3
"#, "4\n1\n4\n2\n4\n3" ; "closure in body")]
    #[test_case(r#"
class Foo {
  getClosure() {
    fun f() {
      fun g() {
        fun h() {
          return this.toString();
        }
        return h;
      }
      return g;
    }
    return f;
  }

  toString() { return "Foo"; }
}

var closure = Foo().getClosure();
print closure()()(); // expect: Foo
"#, "Foo" ; "this nested closure")]
    #[test_case(r#"
var f;

fun f1() {
  var a = "a";
  fun f2() {
    var b = "b";
    fun f3() {
      var c = "c";
      fun f4() {
        print a;
        print b;
        print c;
      }
      f = f4;
    }
    f3();
  }
  f2();
}
f1();

f();
"#, "a\nb\nc" ; "closure nested closure")]
    #[test_case(r#"
class Foo {
  getClosure() {
    fun closure() {
      return this.toString();
    }
    return closure;
  }

  toString() { return "Foo"; }
}

var closure = Foo().getClosure();
print closure(); // expect: Foo
"#, "Foo" ; "this closure")]
    #[test_case(r#"
class Base {
  toString() { return "Base"; }
}

class Derived < Base {
  getClosure() {
    fun closure() {
      return super.toString();
    }
    return closure;
  }

  toString() { return "Derived"; }
}

var closure = Derived().getClosure();
print closure(); // expect: Base
"#, "Base" ; "super closure")]
    #[test_case(r#"
// Single-expression body.
for (var c = 0; c < 3;) print c = c + 1;
// expect: 1
// expect: 2
// expect: 3

// Block body.
for (var a = 0; a < 3; a = a + 1) {
  print a;
}
// expect: 0
// expect: 1
// expect: 2

// No clauses.
fun foo() {
  for (;;) return "done";
}
print foo(); // expect: done

// No variable.
var i = 0;
for (; i < 2; i = i + 1) print i;
// expect: 0
// expect: 1

// No condition.
fun bar() {
  for (var i = 0;; i = i + 1) {
    print i;
    if (i >= 2) return;
  }
}
bar();
// expect: 0
// expect: 1
// expect: 2

// No increment.
for (var i = 0; i < 2;) {
  print i;
  i = i + 1;
}
// expect: 0
// expect: 1

// Statement bodies.
for (; false;) if (true) 1; else 2;
for (; false;) while (true) 1;
for (; false;) for (;;) 1;
"#, "1\n2\n3\n0\n1\n2\ndone\n0\n1\n0\n1\n2\n0\n1" ; "syntax")]
    #[test_case(r#"
class A { m() { print this.x; } }
var a = A(); a.x = 1;
var b = A(); b.x = 2;
a.m();
"#, "1" ; "this test")]
    fn vm_positive_tests(input: &str, expected: &str) {
        // Arrange
        let mut stdout = Vec::new();
        let mut vm = VirtualMachine::new(&mut stdout);
        vm.init().unwrap();

        // Act
        let actual = vm.interpret(input, true);

        // Assert
        if actual.is_err() {
            println!("{input}");
            println!("{actual:?}");
        }
        assert!(actual.is_ok());
        let actual = String::from_utf8(stdout).unwrap();
        assert_eq!(actual.trim_end_matches('\n'), expected);
    }
}
