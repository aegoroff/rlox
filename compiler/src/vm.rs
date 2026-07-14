#![allow(clippy::missing_errors_doc)]

use std::fmt;
use std::hash::BuildHasherDefault;
use std::rc::Rc;

use fnv::FnvHashMap;
use miette::LabeledSpan;

use crate::object::{ObjId, ObjType, ObjectStore, string_chars};
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
const OPCODE_MAX: u8 = OpCode::Method as u8;

struct CallFrame {
    closure: ObjId,
    ip: usize,
    slots: usize,
    chunk: Rc<Chunk>,
}

/// Dispatch cursor: tracks the active frame without cloning its chunk.
///
/// # Safety invariants
/// - `code` / `constants` / `lines` point into the `Rc` buffers owned by
///   `frames[index].chunk`, which stays alive for the lifetime of that frame.
/// - Chunk contents are immutable after compilation, so these pointers remain
///   valid while the frame is active.
struct FrameCursor {
    index: usize,
    ip: usize,
    slots: usize,
    closure: ObjId,
    code: *const u8,
    code_len: usize,
    constants: *const LoxValue,
    lines: *const usize,
}

impl FrameCursor {
    #[inline]
    fn active<W: std::io::Write>(vm: &VirtualMachine<W>) -> Self {
        let index = vm.frame_count - 1;
        let frame = &vm.frames[index];
        Self {
            index,
            ip: frame.ip,
            slots: frame.slots,
            closure: frame.closure,
            code: frame.chunk.code.as_ptr(),
            code_len: frame.chunk.code.len(),
            constants: frame.chunk.constants.as_ptr(),
            lines: frame.chunk.lines.as_ptr(),
        }
    }

    #[inline(always)]
    unsafe fn read_byte(&self, offset: usize) -> u8 {
        debug_assert!(offset < self.code_len);
        // SAFETY: caller must ensure `offset < code_len`.
        unsafe { *self.code.add(offset) }
    }

    #[inline(always)]
    unsafe fn read_u16(&self, offset: usize) -> usize {
        debug_assert!(offset + 1 < self.code_len);
        // SAFETY: caller must ensure `offset + 1 < code_len`.
        unsafe {
            let lo = usize::from(*self.code.add(offset));
            let hi = usize::from(*self.code.add(offset + 1));
            (hi << 8) | lo
        }
    }

    #[inline(always)]
    unsafe fn read_u24(&self, offset: usize) -> usize {
        debug_assert!(offset + 2 < self.code_len);
        // SAFETY: caller must ensure `offset + 2 < code_len`.
        unsafe {
            let b0 = usize::from(*self.code.add(offset));
            let b1 = usize::from(*self.code.add(offset + 1));
            let b2 = usize::from(*self.code.add(offset + 2));
            (b2 << 16) | (b1 << 8) | b0
        }
    }

    #[inline(always)]
    unsafe fn read_constant(&self, index: usize) -> LoxValue {
        // SAFETY: constant indices come from compiler-emitted operands.
        unsafe { *self.constants.add(index) }
    }

    #[inline(always)]
    unsafe fn line(&self, offset: usize) -> usize {
        debug_assert!(offset < self.code_len);
        // SAFETY: `lines` has the same length as `code`.
        unsafe { *self.lines.add(offset) }
    }
}

/// Decode a dense `#[repr(u8)]` opcode without going through `FromPrimitive`.
#[inline(always)]
fn decode_opcode(byte: u8) -> Option<OpCode> {
    if byte <= OPCODE_MAX {
        // SAFETY: OpCode is `#[repr(u8)]` with contiguous values `0..=OPCODE_MAX`.
        Some(unsafe { std::mem::transmute::<u8, OpCode>(byte) })
    } else {
        None
    }
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
        let (function, line_starts) = {
            let mut parser = Parser::new(content, printcode, &mut self.objects);
            let function = parser.compile()?;
            let line_starts = parser.copy_line_starts();
            (function, line_starts)
        };
        let function_val = self
            .objects
            .alloc_function(function)
            .map_err(|e| miette::miette!("{e}"))?;
        let function_id = function_val
            .try_function()
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
                    scanner::Lexer::line_span_in(&line_starts, self.line),
                    format!("{e} Stack trace:\n{}", stack_trace.join("\n"))
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
        let line = if index + 1 == self.frame_count {
            self.line
        } else {
            self.objects.function(function_id)?.chunk.first_line()
        };
        Ok(format!(" at {name}:{line}"))
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

    #[inline(always)]
    fn push(&mut self, value: LoxValue) {
        self.push_raw(value);
        self.objects.retain(value);
    }

    /// Push without retain. Only for values that are never refcounted
    /// (numbers, bools, nil, strings, closures, …) or when the caller
    /// handles retain separately.
    #[inline(always)]
    fn push_raw(&mut self, value: LoxValue) {
        debug_assert!(self.stack_top < STACK_MAX);
        // SAFETY: compiler-enforced frame/slot limits keep `stack_top < STACK_MAX`.
        unsafe {
            *self.stack.get_unchecked_mut(self.stack_top) = value;
        }
        self.stack_top += 1;
    }

    #[inline(always)]
    fn pop(&mut self) -> Result<LoxValue, RuntimeError> {
        if self.stack_top == 0 {
            return Err(RuntimeError::InstructionsStackEmpty);
        }
        Ok(self.pop_unchecked())
    }

    #[inline(always)]
    fn pop_unchecked(&mut self) -> LoxValue {
        let value = self.pop_raw();
        self.objects.release(value);
        value
    }

    /// Pop without release. Caller must release if the value is refcounted.
    #[inline(always)]
    fn pop_raw(&mut self) -> LoxValue {
        debug_assert!(self.stack_top > 0);
        self.stack_top -= 1;
        // SAFETY: caller ensures the stack is non-empty (bytecode stack discipline).
        unsafe { *self.stack.get_unchecked(self.stack_top) }
    }

    #[inline(always)]
    fn set_stack(&mut self, index: usize, value: LoxValue) {
        // SAFETY: `index` is a live stack slot within the current frame window.
        let slot = unsafe { self.stack.get_unchecked_mut(index) };
        if *slot == value {
            return;
        }
        self.objects.release(*slot);
        self.objects.retain(value);
        *slot = value;
    }

    #[inline(always)]
    fn release_stack_range(&mut self, start: usize, end: usize) {
        for index in start..end {
            // SAFETY: `start..end` is a live range of stack slots being discarded.
            let value = unsafe { *self.stack.get_unchecked(index) };
            self.objects.release(value);
        }
    }

    #[inline(always)]
    fn peek(&self, distance: usize) -> Result<LoxValue, RuntimeError> {
        if self.stack_top < distance + 1 {
            Err(RuntimeError::NotEnoughStackCapacity(
                distance,
                self.stack_top,
            ))
        } else {
            Ok(self.peek_unchecked(distance))
        }
    }

    #[inline(always)]
    fn peek_unchecked(&self, distance: usize) -> LoxValue {
        debug_assert!(self.stack_top > distance);
        // SAFETY: caller ensures `stack_top > distance`.
        unsafe { *self.stack.get_unchecked(self.stack_top - 1 - distance) }
    }

    #[inline(always)]
    fn stack_get(&self, index: usize) -> LoxValue {
        // SAFETY: `index` is a live stack slot.
        unsafe { *self.stack.get_unchecked(index) }
    }

    #[inline]
    fn runtime_error_at(
        &mut self,
        cursor: &FrameCursor,
        ip: usize,
        err: RuntimeError,
    ) -> Result<(), RuntimeError> {
        // SAFETY: `ip` is an offset into this frame's code / line table.
        self.line = unsafe { cursor.line(ip) };
        self.frames[cursor.index].ip = ip;
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

    #[allow(clippy::too_many_lines)]
    fn run(&mut self) -> Result<(), RuntimeError> {
        if self.frame_count == 0 {
            return Ok(());
        }

        let mut cursor = FrameCursor::active(self);

        'run_insns: loop {
            let mut ip = cursor.ip;

            if ip >= cursor.code_len {
                return Ok(());
            }

            let instruction_ip = ip;
            // SAFETY: `ip < code_len`.
            let byte = unsafe { cursor.read_byte(ip) };
            let Some(opcode) = decode_opcode(byte) else {
                return self.runtime_error_at(
                    &cursor,
                    instruction_ip,
                    RuntimeError::InvalidInstruction(instruction_ip),
                );
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
                let function_id = self.objects.closure(cursor.closure)?.function;
                let disasm_chunk = self.objects.function(function_id)?.chunk.clone();
                disasm_chunk.disassembly_instruction(instruction_ip, &self.objects);
            }

            match opcode {
                OpCode::Constant => {
                    // SAFETY: operand byte is within the frame code.
                    let ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    // SAFETY: constant index emitted by the compiler.
                    let constant = unsafe { cursor.read_constant(ix) };
                    self.push_raw(constant);
                    self.objects.retain(constant);
                }
                OpCode::ConstantLong => {
                    // SAFETY: 3-byte operand within the frame code.
                    let ix = unsafe { cursor.read_u24(ip) };
                    ip += CONST_LONG_SIZE;
                    let constant = unsafe { cursor.read_constant(ix) };
                    self.push_raw(constant);
                    self.objects.retain(constant);
                }
                OpCode::Return => {
                    let ret_slot = self.stack_top - 1;
                    let value = self.stack_get(ret_slot);
                    self.stack_top = ret_slot;
                    let returns_slot0 =
                        cursor.slots > 0 && value == self.stack_get(cursor.slots.saturating_sub(1));

                    if cursor.slots > 0 {
                        self.close_upvalue_at(cursor.slots - 1)?;
                    }
                    self.close_upvalues(cursor.slots)?;

                    self.frame_count -= 1;

                    let release_from = if returns_slot0 {
                        cursor.slots
                    } else {
                        cursor.slots.saturating_sub(1)
                    };
                    self.release_stack_range(release_from, ret_slot);
                    self.objects.release(value);

                    if returns_slot0 {
                        self.stack_top = if cursor.slots > 0 { cursor.slots } else { 1 };
                    } else {
                        self.stack_top = cursor.slots.saturating_sub(1);
                        self.push(value);
                    }

                    if self.frame_count == 0 {
                        return Ok(());
                    }
                    cursor = FrameCursor::active(self);
                    continue 'run_insns;
                }
                OpCode::Negate => {
                    let value = self.pop_raw();
                    if value.is_number() {
                        self.push_raw(LoxValue::number(-value.as_number()));
                    } else {
                        self.objects.release(value);
                        return self.runtime_error_at(
                            &cursor,
                            instruction_ip,
                            RuntimeError::ExpectedNumber(value),
                        );
                    }
                }
                OpCode::Add => {
                    let b = self.pop_raw();
                    let a = self.pop_raw();
                    if a.is_number() && b.is_number() {
                        self.push_raw(LoxValue::number(a.as_number() + b.as_number()));
                    } else if let (Ok(l_id), Ok(r_id)) = (a.try_str(), b.try_str()) {
                        let l = string_chars(&self.objects, l_id)?;
                        let r = string_chars(&self.objects, r_id)?;
                        let result = self.objects.intern_string(l.to_owned() + r)?;
                        self.push_raw(result);
                    } else {
                        self.objects.release(a);
                        self.objects.release(b);
                        return self.runtime_error_at(
                            &cursor,
                            instruction_ip,
                            RuntimeError::OperandsMustBeNumbersOrStrings,
                        );
                    }
                }
                OpCode::Subtract => {
                    let b = self.pop_raw();
                    let a = self.pop_raw();
                    if a.is_number() && b.is_number() {
                        self.push_raw(LoxValue::number(a.as_number() - b.as_number()));
                    } else {
                        self.objects.release(a);
                        self.objects.release(b);
                        return self.runtime_error_at(
                            &cursor,
                            instruction_ip,
                            RuntimeError::OperandsMustBeNumbers(a, b),
                        );
                    }
                }
                OpCode::Multiply => {
                    let b = self.pop_raw();
                    let a = self.pop_raw();
                    if a.is_number() && b.is_number() {
                        self.push_raw(LoxValue::number(a.as_number() * b.as_number()));
                    } else {
                        self.objects.release(a);
                        self.objects.release(b);
                        return self.runtime_error_at(
                            &cursor,
                            instruction_ip,
                            RuntimeError::OperandsMustBeNumbers(a, b),
                        );
                    }
                }
                OpCode::Divide => {
                    let b = self.pop_raw();
                    let a = self.pop_raw();
                    if a.is_number() && b.is_number() {
                        let a = a.as_number();
                        let b = b.as_number();
                        if b == 0.0 {
                            self.push_raw(LoxValue::number(f64::NAN));
                        } else {
                            self.push_raw(LoxValue::number(a / b));
                        }
                    } else {
                        self.objects.release(a);
                        self.objects.release(b);
                        return self.runtime_error_at(
                            &cursor,
                            instruction_ip,
                            RuntimeError::OperandsMustBeNumbers(a, b),
                        );
                    }
                }
                OpCode::Nil => self.push_raw(LoxValue::NIL),
                OpCode::True => self.push_raw(LoxValue::TRUE),
                OpCode::False => self.push_raw(LoxValue::FALSE),
                OpCode::Not => {
                    let value = self.pop_raw();
                    let result = LoxValue::bool_val(value.is_falsey());
                    self.objects.release(value);
                    self.push_raw(result);
                }
                OpCode::Equal => {
                    let b = self.pop_raw();
                    let a = self.pop_raw();
                    let result = LoxValue::bool_val(a.equal(b));
                    self.objects.release(a);
                    self.objects.release(b);
                    self.push_raw(result);
                }
                OpCode::Less => {
                    let b = self.pop_raw();
                    let a = self.pop_raw();
                    if a.is_number() && b.is_number() {
                        self.push_raw(LoxValue::bool_val(a.as_number() < b.as_number()));
                    } else {
                        let cmp = a.less(b, &self.objects);
                        self.objects.release(a);
                        self.objects.release(b);
                        self.push_raw(LoxValue::bool_val(cmp?));
                    }
                }
                OpCode::Greater => {
                    let b = self.pop_raw();
                    let a = self.pop_raw();
                    if a.is_number() && b.is_number() {
                        self.push_raw(LoxValue::bool_val(a.as_number() > b.as_number()));
                    } else {
                        let lt = a.less(b, &self.objects);
                        let eq = a.equal(b);
                        self.objects.release(a);
                        self.objects.release(b);
                        let lt = lt?;
                        self.push_raw(LoxValue::bool_val(!lt && !eq));
                    }
                }
                OpCode::Print => {
                    let value = self.peek_unchecked(0);
                    self.write_value(value)?;
                    self.pop_unchecked();
                }
                OpCode::Pop => {
                    self.pop_unchecked();
                }
                OpCode::DefineGlobal => {
                    let ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    self.define_global(unsafe { cursor.read_constant(ix) })?;
                }
                OpCode::DefineGlobalLong => {
                    let ix = unsafe { cursor.read_u24(ip) };
                    ip += CONST_LONG_SIZE;
                    self.define_global(unsafe { cursor.read_constant(ix) })?;
                }
                OpCode::GetGlobal => {
                    let ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    self.get_global(unsafe { cursor.read_constant(ix) })?;
                }
                OpCode::GetGlobalLong => {
                    let ix = unsafe { cursor.read_u24(ip) };
                    ip += CONST_LONG_SIZE;
                    self.get_global(unsafe { cursor.read_constant(ix) })?;
                }
                OpCode::SetGlobal => {
                    let ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    self.set_global(unsafe { cursor.read_constant(ix) })?;
                }
                OpCode::SetGlobalLong => {
                    let ix = unsafe { cursor.read_u24(ip) };
                    ip += CONST_LONG_SIZE;
                    self.set_global(unsafe { cursor.read_constant(ix) })?;
                }
                OpCode::GetLocal => {
                    let frame_offset = unsafe { cursor.read_byte(ip) } as usize;
                    ip += 1;
                    let local_index = cursor.slots + frame_offset - 1;
                    let value = self.stack_get(local_index);
                    self.push_raw(value);
                    self.objects.retain(value);
                }
                OpCode::SetLocal => {
                    let frame_offset = unsafe { cursor.read_byte(ip) } as usize;
                    ip += 1;
                    let local_index = cursor.slots + frame_offset - 1;
                    let value = self.peek_unchecked(0);
                    self.set_stack(local_index, value);
                }
                OpCode::JumpIfFalse => {
                    let offset = unsafe { cursor.read_u16(ip) };
                    ip += 2;
                    if self.peek_unchecked(0).is_falsey() {
                        ip += offset;
                    }
                }
                OpCode::Jump => {
                    let offset = unsafe { cursor.read_u16(ip) };
                    ip += 2;
                    ip += offset;
                }
                OpCode::Loop => {
                    let offset = unsafe { cursor.read_u16(ip) };
                    ip += 2;
                    ip -= offset;
                }
                OpCode::Call => {
                    let args_count = unsafe { cursor.read_byte(ip) } as usize;
                    ip += 1;
                    self.frames[cursor.index].ip = ip;
                    let prev_frame_count = self.frame_count;
                    self.call_value_at(args_count)?;
                    if self.frame_count == prev_frame_count {
                        cursor.ip = ip;
                    } else {
                        cursor = FrameCursor::active(self);
                    }
                    continue 'run_insns;
                }
                OpCode::Invoke => {
                    let method_ix = unsafe { cursor.read_byte(ip) } as usize;
                    let method_name = unsafe { cursor.read_constant(method_ix) };
                    let argc = unsafe { cursor.read_byte(ip + 1) };
                    ip += 2;
                    self.frames[cursor.index].ip = ip;
                    let prev_frame_count = self.frame_count;
                    self.invoke(method_name, argc)?;
                    if self.frame_count == prev_frame_count {
                        cursor.ip = ip;
                    } else {
                        cursor = FrameCursor::active(self);
                    }
                    continue 'run_insns;
                }
                OpCode::Closure => {
                    let const_ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    let function_value = unsafe { cursor.read_constant(const_ix) };
                    let func_id = function_value.try_function()?;
                    let closure_val = self.objects.alloc_closure(func_id)?;
                    let new_closure_id = closure_val.try_closure()?;
                    let upvalues_count = self.objects.function(func_id)?.upvalue_count;
                    let mut upvalues = Vec::with_capacity(upvalues_count);

                    for _ in 0..upvalues_count {
                        let is_local = unsafe { cursor.read_byte(ip) };
                        let index = unsafe { cursor.read_byte(ip + 1) };
                        ip += 2;
                        let upvalue = if is_local == 1 {
                            self.capture_upvalue(cursor.slots + index as usize - 1)?
                        } else {
                            self.objects.closure(cursor.closure)?.upvalues[index as usize]
                        };
                        upvalues.push(upvalue);
                    }

                    self.objects.closure_mut(new_closure_id)?.upvalues = upvalues;
                    self.push(closure_val);
                }
                OpCode::GetUpvalue => {
                    let slot = unsafe { cursor.read_byte(ip) } as usize;
                    ip += 1;
                    let upvalue_id = self.objects.closure(cursor.closure)?.upvalues[slot];
                    let upvalue = self.objects.upvalue(upvalue_id)?;
                    let lox_value = match upvalue.location {
                        Some(location) => self.stack_get(location),
                        None => upvalue.closed,
                    };
                    self.push(lox_value);
                }
                OpCode::SetUpvalue => {
                    let slot = unsafe { cursor.read_byte(ip) } as usize;
                    ip += 1;
                    let val = self.peek_unchecked(0);
                    let upvalue_id = self.objects.closure(cursor.closure)?.upvalues[slot];
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
                    self.pop_unchecked();
                }
                OpCode::Class => {
                    let class_ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    let class_name = unsafe { cursor.read_constant(class_ix) };
                    let class = self.objects.alloc_class(class_name)?;
                    self.push(class);
                }
                OpCode::GetProperty => {
                    let prop_ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    let property_id = unsafe { cursor.read_constant(prop_ix) }.try_str()?;
                    let instance_id = self.peek_unchecked(0).try_instance()?;
                    if let Some(val) =
                        Self::get_member(instance_id, property_id, &mut self.objects)?
                    {
                        self.pop_unchecked();
                        self.push(val);
                    } else {
                        let name = string_chars(&self.objects, property_id)?.to_owned();
                        return self.runtime_error_at(
                            &cursor,
                            instruction_ip,
                            RuntimeError::UndefinedMethodOrProperty(name),
                        );
                    }
                }
                OpCode::SetProperty => {
                    let prop_ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    let property_id = match unsafe { cursor.read_constant(prop_ix) }.try_str() {
                        Ok(id) => id,
                        Err(err) => {
                            return self.runtime_error_at(&cursor, instruction_ip, err);
                        }
                    };
                    self.objects.retain(self.peek_unchecked(0));
                    let property_value = self.pop_unchecked();
                    let instance_id = self.pop_unchecked().try_instance_field()?;
                    let old = {
                        let instance = self.objects.instance_mut(instance_id)?;
                        instance.fields.insert(property_id, property_value)
                    };
                    if let Some(old) = old
                        && old != property_value
                    {
                        self.objects.release(old);
                    }
                    self.push(property_value);
                }
                OpCode::Method => {
                    let method_ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    self.define_method(unsafe { cursor.read_constant(method_ix) })?;
                }
                OpCode::Inherit => {
                    let Ok(super_class_id) = self.peek_unchecked(1).try_class() else {
                        return self.runtime_error_at(
                            &cursor,
                            instruction_ip,
                            RuntimeError::SuperclassMustBeClass,
                        );
                    };
                    let sub_class_id = self.peek_unchecked(0).try_class()?;
                    let super_methods = self.objects.class(super_class_id)?.methods.clone();
                    for (name, method) in super_methods {
                        if !self.objects.class(sub_class_id)?.methods.contains_key(name) {
                            self.objects.retain(method);
                            self.objects
                                .class_mut(sub_class_id)?
                                .methods
                                .insert(name, method);
                        }
                    }
                    self.pop_unchecked();
                }
                OpCode::GetSuper => {
                    let const_ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    let name_id = match unsafe { cursor.read_constant(const_ix) }.try_str() {
                        Ok(id) => id,
                        Err(err) => {
                            return self.runtime_error_at(&cursor, instruction_ip, err);
                        }
                    };
                    let super_class_id = self.pop_unchecked().try_class()?;
                    let instance_id = self.pop_unchecked().try_instance()?;
                    let Some(method) = self.objects.class(super_class_id)?.methods.get(name_id)
                    else {
                        return self.runtime_error_at(
                            &cursor,
                            instruction_ip,
                            RuntimeError::UndefinedMethodOrProperty(
                                string_chars(&self.objects, name_id)?.to_owned(),
                            ),
                        );
                    };
                    let method_closure_id = method.try_closure()?;
                    let bound = self.objects.alloc_bound_method(
                        LoxValue::from_obj(instance_id, ObjType::Instance),
                        method_closure_id,
                    )?;
                    self.push(bound);
                }
                OpCode::SuperInvoke => {
                    let method_ix = unsafe { cursor.read_byte(ip) } as usize;
                    let method_name = unsafe { cursor.read_constant(method_ix) };
                    let argc = unsafe { cursor.read_byte(ip + 1) };
                    ip += 2;
                    self.frames[cursor.index].ip = ip;
                    let prev_frame_count = self.frame_count;
                    let name_id = method_name.try_str()?;
                    let super_class_id = self.pop_unchecked().try_class()?;
                    let Some(method) = self.objects.class(super_class_id)?.methods.get(name_id)
                    else {
                        return self.runtime_error_at(
                            &cursor,
                            instruction_ip,
                            RuntimeError::UndefinedMethodOrProperty(
                                string_chars(&self.objects, name_id)?.to_owned(),
                            ),
                        );
                    };
                    self.call_value(*method, argc as usize)?;
                    if self.frame_count == prev_frame_count {
                        cursor.ip = ip;
                    } else {
                        cursor = FrameCursor::active(self);
                    }
                    continue 'run_insns;
                }
            }
            // Flush IP only into the cursor; frames[i].ip is updated on call/return/error.
            cursor.ip = ip;
        }
    }

    #[inline]
    fn invoke(&mut self, method_name: LoxValue, argc: u8) -> Result<(), RuntimeError> {
        let method_key = method_name.try_str()?;
        let receiver = self.peek(argc as usize)?;
        let instance_id = receiver.try_instance()?;
        let (is_field, callable) = {
            let instance = self.objects.instance(instance_id)?;
            if let Some(field) = instance.fields.get(method_key) {
                (true, *field)
            } else {
                let class_id = instance.class;
                let Some(callable) = self
                    .objects
                    .class(class_id)?
                    .methods
                    .get(method_key)
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
            if let Some(field) = instance.fields.get(property_id) {
                return Ok(Some(*field));
            }
            instance.class
        };
        let Some(method) = store.class(class_id)?.methods.get(property_id).copied() else {
            return Ok(None);
        };
        let method_closure_id = method.try_closure()?;
        Ok(Some(store.alloc_bound_method(
            LoxValue::from_obj(instance_id, ObjType::Instance),
            method_closure_id,
        )?))
    }

    #[inline]
    fn define_method(&mut self, name: LoxValue) -> Result<(), RuntimeError> {
        let method_key = name.try_str()?;
        let method_closure = self.pop()?;
        let class_id = self.peek(0)?.try_class()?;
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
            let closed_value = self.stack_get(location);
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
                let closed_value = self.stack_get(location);
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
        let callee = self.stack_get(self.stack_top - args_count - 1);
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
        let closure_id = closure_val.try_closure()?;
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

        if let Some(init) = self.objects.class(class_id)?.methods.get(self.init_string) {
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
        let key = name.try_str()?;
        if !self.globals.contains_key(&key) {
            return Err(RuntimeError::UndefinedGlobal(
                string_chars(&self.objects, key)?.to_owned(),
            ));
        }
        let value = self.peek_unchecked(0);
        if let Some(old) = self.globals.insert(key, value) {
            self.objects.release(old);
        }
        self.objects.retain(value);
        Ok(())
    }

    #[inline]
    fn get_global(&mut self, name: LoxValue) -> Result<(), RuntimeError> {
        let key = name.try_str()?;
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
        let key = name.try_str()?;
        self.stack_top -= 1;
        let value = self.stack_get(self.stack_top);
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
    #[test_case("print nil <= false;", "false" ; "nil is not less than or equal to false")]
    #[test_case("print nil < false;", "false" ; "nil lrs less")]
    #[test_case("print nil == false;", "false" ; "nil is not equal to false")]
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
