#![allow(clippy::missing_errors_doc)]

use std::fmt;

use miette::LabeledSpan;

use crate::obj_map::ObjMap;
use crate::object::{HeapObject, ObjId, ObjType, ObjectStore, string_chars};
use crate::value::LoxValue;
use crate::{RuntimeError, builtin};
use crate::{chunk::OpCode, compile::Parser};

const FRAMES_MAX: usize = 64;
const CONST_SIZE: usize = 1;
const CONST_LONG_SIZE: usize = 3;
const STACK_MAX: usize = FRAMES_MAX * 256;
const OPCODE_MAX: u8 = OpCode::Method as u8;

/// Call frame with raw pointers into the callee's immutable bytecode.
///
/// # Safety invariants
/// - `code` / `constants` / `lines` point into `ObjFunction.chunk` buffers.
/// - Functions and their chunks are never freed while the VM runs (only
///   instances / bound methods are refcounted), so these pointers stay valid
///   for the lifetime of any frame that references the function via `closure`.
struct CallFrame {
    closure: ObjId,
    ip: usize,
    slots: usize,
    code: *const u8,
    code_len: usize,
    constants: *const LoxValue,
    lines: *const usize,
}

/// Dispatch cursor: the active frame's hot fields, kept in locals so the
/// dispatch loop holds them in registers. Cold fields (`closure`, `lines`)
/// are read from `frames[index]` when needed.
///
/// # Safety invariants
/// - Pointers are copies of the active [`CallFrame`] pointers (see above).
/// - Chunk contents are immutable after compilation.
struct FrameCursor {
    index: usize,
    slots: usize,
    code: *const u8,
    constants: *const LoxValue,
}

impl FrameCursor {
    /// Cursor for the top frame and the `ip` to resume it at.
    #[inline(always)]
    fn active<W: std::io::Write>(vm: &VirtualMachine<W>) -> (Self, usize) {
        let index = vm.frame_count - 1;
        let frame = &vm.frames[index];
        let cursor = Self {
            index,
            slots: frame.slots,
            code: frame.code,
            constants: frame.constants,
        };
        (cursor, frame.ip)
    }

    #[inline(always)]
    unsafe fn read_byte(&self, offset: usize) -> u8 {
        // SAFETY: caller must ensure `offset < code_len`.
        unsafe { *self.code.add(offset) }
    }

    #[inline(always)]
    unsafe fn read_u16(&self, offset: usize) -> usize {
        // SAFETY: caller must ensure `offset + 1 < code_len`.
        unsafe {
            let lo = usize::from(*self.code.add(offset));
            let hi = usize::from(*self.code.add(offset + 1));
            (hi << 8) | lo
        }
    }

    #[inline(always)]
    unsafe fn read_u24(&self, offset: usize) -> usize {
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
}

/// Value-stack cursor kept in the dispatch loop so the top stays in a register.
///
/// `VirtualMachine::stack_top` is written back only when a handler reads the
/// stack through `&mut self` (calls, globals, allocations, errors).
///
/// # Safety invariants
/// - `ptr` addresses `VirtualMachine::stack`, a fixed array that does not move.
/// - Bytecode stack discipline keeps every index below `STACK_MAX`.
/// - A slot is not read through `ptr` while a `&mut` to that same slot is live.
struct StackCursor {
    ptr: *mut LoxValue,
    top: usize,
}

impl StackCursor {
    #[inline(always)]
    unsafe fn peek(&self, distance: usize) -> LoxValue {
        debug_assert!(self.top > distance);
        // SAFETY: caller ensures `top > distance` and `top <= STACK_MAX`.
        unsafe { *self.ptr.add(self.top - 1 - distance) }
    }

    #[inline(always)]
    unsafe fn get(&self, index: usize) -> LoxValue {
        debug_assert!(index < STACK_MAX);
        // SAFETY: caller ensures `index` is a live slot.
        unsafe { *self.ptr.add(index) }
    }

    #[inline(always)]
    unsafe fn push(&mut self, value: LoxValue) {
        debug_assert!(self.top < STACK_MAX);
        // SAFETY: compiler-enforced frame limits keep `top < STACK_MAX`.
        unsafe { *self.ptr.add(self.top) = value };
        self.top += 1;
    }

    #[inline(always)]
    unsafe fn pop(&mut self) -> LoxValue {
        debug_assert!(self.top > 0);
        self.top -= 1;
        // SAFETY: caller ensures the stack is non-empty.
        unsafe { *self.ptr.add(self.top) }
    }

    /// Overwrite the top of the stack without changing its height.
    #[inline(always)]
    unsafe fn replace_tos(&mut self, value: LoxValue) {
        debug_assert!(self.top > 0);
        // SAFETY: caller ensures the stack is non-empty.
        unsafe { *self.ptr.add(self.top - 1) = value };
    }

    /// Drop the right-hand operand and store `value` as the new top.
    #[inline(always)]
    unsafe fn pop_and_replace(&mut self, value: LoxValue) {
        debug_assert!(self.top >= 2);
        self.top -= 1;
        // SAFETY: caller ensures at least two slots are live.
        unsafe { *self.ptr.add(self.top - 1) = value };
    }

    #[inline(always)]
    unsafe fn set(&mut self, index: usize, value: LoxValue) {
        debug_assert!(index < STACK_MAX);
        // SAFETY: caller ensures `index` is a live slot.
        unsafe { *self.ptr.add(index) = value };
    }
}

/// Decode a dense `#[repr(u8)]` opcode without a range check.
///
/// # Safety
/// `byte` must sit at an instruction start. The compiler writes those only
/// through [`crate::chunk::Chunk::write_code`], so they are valid opcodes.
#[inline(always)]
unsafe fn decode_opcode(byte: u8) -> OpCode {
    debug_assert!(byte <= OPCODE_MAX);
    // SAFETY: OpCode is `#[repr(u8)]` with contiguous values `0..=OPCODE_MAX`.
    unsafe { std::mem::transmute::<u8, OpCode>(byte) }
}

pub struct VirtualMachine<W: std::io::Write> {
    stack: [LoxValue; STACK_MAX],
    stack_top: usize,
    objects: ObjectStore,
    globals: ObjMap,
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
            globals: ObjMap::new(),
            frames: std::array::from_fn(|_| CallFrame {
                closure: 0,
                ip: 0,
                slots: 0,
                code: std::ptr::null(),
                code_len: 0,
                constants: std::ptr::null(),
                lines: std::ptr::null(),
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
            .alloc_closure(function_id, Box::default())
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
        // Retain before release: `value` may only be kept alive by `*slot`
        // (e.g. bound-method callee whose receiver replaces it).
        self.objects.retain(value);
        self.objects.release(*slot);
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
        let frame = &mut self.frames[cursor.index];
        frame.ip = ip;
        debug_assert!(ip < frame.code_len);
        // SAFETY: `ip` is an offset into this frame's code, and `lines` has
        // one entry per code byte.
        self.line = unsafe { *frame.lines.add(ip) };
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

        let (mut cursor, mut ip) = FrameCursor::active(self);
        // SAFETY: `stack` is a fixed array and does not move for the VM lifetime.
        // `stack.top` is the live height. `self.stack_top` is stale until a handler
        // syncs it before reading the stack through `&mut self`.
        let mut stack = StackCursor {
            ptr: self.stack.as_mut_ptr(),
            top: self.stack_top,
        };

        loop {
            let instruction_ip = ip;
            // SAFETY: every function body ends with `OP_RETURN`, and jumps only
            // target instruction starts, so `ip` never runs past the code.
            let opcode = unsafe { decode_opcode(cursor.read_byte(ip)) };
            ip += 1;

            #[cfg(feature = "disassembly")]
            {
                self.stack_top = stack.top;
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
                let function_id = self
                    .objects
                    .closure(self.frames[cursor.index].closure)?
                    .function;
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
                    // SAFETY: a constant push stays within the frame's stack window.
                    unsafe { stack.push(constant) };
                    if constant.is_refcounted() {
                        self.objects.retain(constant);
                    }
                }
                OpCode::ConstantLong => {
                    // SAFETY: 3-byte operand within the frame code.
                    let ix = unsafe { cursor.read_u24(ip) };
                    ip += CONST_LONG_SIZE;
                    let constant = unsafe { cursor.read_constant(ix) };
                    // SAFETY: a constant push stays within the frame's stack window.
                    unsafe { stack.push(constant) };
                    if constant.is_refcounted() {
                        self.objects.retain(constant);
                    }
                }
                OpCode::Return => {
                    // SAFETY: a function body leaves its return value on top.
                    let value = unsafe { stack.pop() };
                    // The callee (or receiver) slot sits right below the frame's locals.
                    let base = cursor.slots - 1;
                    self.stack_top = stack.top;
                    if self.open_upvalues.is_some() {
                        self.close_upvalues(base)?;
                    }
                    self.release_stack_range(base, stack.top);
                    // The popped reference to `value` moves into the callee slot,
                    // so it needs neither a retain nor a release.
                    // SAFETY: `base` is below the popped top.
                    unsafe { stack.set(base, value) };
                    stack.top = cursor.slots;
                    self.stack_top = stack.top;

                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        return Ok(());
                    }
                    (cursor, ip) = FrameCursor::active(self);
                }
                OpCode::Negate => {
                    // SAFETY: `Negate` consumes one stack operand.
                    let value = unsafe { stack.peek(0) };
                    if value.is_number() {
                        unsafe { stack.replace_tos(LoxValue::number(-value.as_number())) };
                    } else {
                        self.stack_top = stack.top;
                        let value = self.pop_raw();
                        self.objects.release(value);
                        return self.runtime_error_at(
                            &cursor,
                            instruction_ip,
                            RuntimeError::ExpectedNumber(value),
                        );
                    }
                }
                OpCode::Add => {
                    // SAFETY: `Add` reads the top two stack slots.
                    let b = unsafe { stack.peek(0) };
                    let a = unsafe { stack.peek(1) };
                    if a.is_number() && b.is_number() {
                        unsafe {
                            stack.pop_and_replace(LoxValue::number(a.as_number() + b.as_number()));
                        }
                    } else {
                        self.stack_top = stack.top;
                        let b = self.pop_raw();
                        let a = self.pop_raw();
                        if let (Ok(l_id), Ok(r_id)) = (a.try_str(), b.try_str()) {
                            let l = string_chars(&self.objects, l_id)?;
                            let r = string_chars(&self.objects, r_id)?;
                            let result = self.objects.intern_string(l.to_owned() + r)?;
                            self.push_raw(result);
                            stack.top = self.stack_top;
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
                }
                OpCode::Subtract | OpCode::Multiply | OpCode::Divide => {
                    // SAFETY: arithmetic reads the top two stack slots.
                    let b = unsafe { stack.peek(0) };
                    let a = unsafe { stack.peek(1) };
                    if a.is_number() && b.is_number() {
                        let left = a.as_number();
                        let right = b.as_number();
                        let result = match opcode {
                            OpCode::Subtract => left - right,
                            OpCode::Multiply => left * right,
                            OpCode::Divide => {
                                if right == 0.0 {
                                    f64::NAN
                                } else {
                                    left / right
                                }
                            }
                            _ => unreachable!("opcode is subtract, multiply, or divide"),
                        };
                        unsafe { stack.pop_and_replace(LoxValue::number(result)) };
                    } else {
                        self.stack_top = stack.top;
                        let b = self.pop_raw();
                        let a = self.pop_raw();
                        self.objects.release(a);
                        self.objects.release(b);
                        return self.runtime_error_at(
                            &cursor,
                            instruction_ip,
                            RuntimeError::OperandsMustBeNumbers(a, b),
                        );
                    }
                }
                OpCode::Nil => unsafe { stack.push(LoxValue::NIL) },
                OpCode::True => unsafe { stack.push(LoxValue::TRUE) },
                OpCode::False => unsafe { stack.push(LoxValue::FALSE) },
                OpCode::Not => {
                    // SAFETY: `Not` consumes one stack operand.
                    let value = unsafe { stack.peek(0) };
                    let result = LoxValue::bool_val(value.is_falsey());
                    if value.is_refcounted() {
                        self.stack_top = stack.top;
                        let value = self.pop_raw();
                        self.objects.release(value);
                        self.push_raw(result);
                        stack.top = self.stack_top;
                    } else {
                        unsafe { stack.replace_tos(result) };
                    }
                }
                OpCode::Equal => {
                    // SAFETY: `Equal` reads the top two stack slots.
                    let b = unsafe { stack.peek(0) };
                    let a = unsafe { stack.peek(1) };
                    let result = LoxValue::bool_val(a.equal(b));
                    if a.is_refcounted() || b.is_refcounted() {
                        self.stack_top = stack.top;
                        let b = self.pop_raw();
                        let a = self.pop_raw();
                        self.objects.release(a);
                        self.objects.release(b);
                        self.push_raw(result);
                        stack.top = self.stack_top;
                    } else {
                        unsafe { stack.pop_and_replace(result) };
                    }
                }
                OpCode::Less => {
                    // SAFETY: `Less` reads the top two stack slots.
                    let b = unsafe { stack.peek(0) };
                    let a = unsafe { stack.peek(1) };
                    if a.is_number() && b.is_number() {
                        unsafe {
                            stack
                                .pop_and_replace(LoxValue::bool_val(a.as_number() < b.as_number()));
                        }
                    } else {
                        self.stack_top = stack.top;
                        let b = self.pop_raw();
                        let a = self.pop_raw();
                        let cmp = a.less(b, &self.objects);
                        self.objects.release(a);
                        self.objects.release(b);
                        self.push_raw(LoxValue::bool_val(cmp?));
                        stack.top = self.stack_top;
                    }
                }
                OpCode::Greater => {
                    // SAFETY: `Greater` reads the top two stack slots.
                    let b = unsafe { stack.peek(0) };
                    let a = unsafe { stack.peek(1) };
                    if a.is_number() && b.is_number() {
                        unsafe {
                            stack
                                .pop_and_replace(LoxValue::bool_val(a.as_number() > b.as_number()));
                        }
                    } else {
                        self.stack_top = stack.top;
                        let b = self.pop_raw();
                        let a = self.pop_raw();
                        let lt = a.less(b, &self.objects);
                        let eq = a.equal(b);
                        self.objects.release(a);
                        self.objects.release(b);
                        let lt = lt?;
                        self.push_raw(LoxValue::bool_val(!lt && !eq));
                        stack.top = self.stack_top;
                    }
                }
                OpCode::Print => {
                    self.stack_top = stack.top;
                    let value = self.peek_unchecked(0);
                    self.write_value(value)?;
                    self.pop_unchecked();
                    stack.top = self.stack_top;
                }
                OpCode::Pop => {
                    // SAFETY: `Pop` discards the top slot.
                    let value = unsafe { stack.peek(0) };
                    if value.is_refcounted() {
                        self.stack_top = stack.top;
                        self.pop_unchecked();
                        stack.top = self.stack_top;
                    } else {
                        unsafe { stack.pop() };
                    }
                }
                OpCode::DefineGlobal => {
                    self.stack_top = stack.top;
                    let ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    self.define_global(unsafe { cursor.read_constant(ix) })?;
                    stack.top = self.stack_top;
                }
                OpCode::DefineGlobalLong => {
                    self.stack_top = stack.top;
                    let ix = unsafe { cursor.read_u24(ip) };
                    ip += CONST_LONG_SIZE;
                    self.define_global(unsafe { cursor.read_constant(ix) })?;
                    stack.top = self.stack_top;
                }
                OpCode::GetGlobal => {
                    let ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    let name = unsafe { cursor.read_constant(ix) };
                    if let Some(value) = self.defined_global(name) {
                        // SAFETY: a global load pushes one slot within the frame window.
                        unsafe { stack.push(value) };
                        self.objects.retain(value);
                    } else {
                        self.stack_top = stack.top;
                        self.get_global(name)?;
                        stack.top = self.stack_top;
                    }
                }
                OpCode::GetGlobalLong => {
                    let ix = unsafe { cursor.read_u24(ip) };
                    ip += CONST_LONG_SIZE;
                    let name = unsafe { cursor.read_constant(ix) };
                    if let Some(value) = self.defined_global(name) {
                        // SAFETY: a global load pushes one slot within the frame window.
                        unsafe { stack.push(value) };
                        self.objects.retain(value);
                    } else {
                        self.stack_top = stack.top;
                        self.get_global(name)?;
                        stack.top = self.stack_top;
                    }
                }
                OpCode::SetGlobal => {
                    self.stack_top = stack.top;
                    let ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    self.set_global(unsafe { cursor.read_constant(ix) })?;
                    stack.top = self.stack_top;
                }
                OpCode::SetGlobalLong => {
                    self.stack_top = stack.top;
                    let ix = unsafe { cursor.read_u24(ip) };
                    ip += CONST_LONG_SIZE;
                    self.set_global(unsafe { cursor.read_constant(ix) })?;
                    stack.top = self.stack_top;
                }
                OpCode::GetLocal => {
                    let frame_offset = unsafe { cursor.read_byte(ip) } as usize;
                    ip += 1;
                    let local_index = cursor.slots + frame_offset - 1;
                    // SAFETY: local slots lie inside the caller's frame window.
                    let value = unsafe { stack.get(local_index) };
                    unsafe { stack.push(value) };
                    if value.is_refcounted() {
                        self.objects.retain(value);
                    }
                }
                OpCode::SetLocal => {
                    let frame_offset = unsafe { cursor.read_byte(ip) } as usize;
                    ip += 1;
                    let local_index = cursor.slots + frame_offset - 1;
                    // SAFETY: `SetLocal` reads the top slot and writes a frame slot.
                    let value = unsafe { stack.peek(0) };
                    let current = unsafe { stack.get(local_index) };
                    if value.is_refcounted() || current.is_refcounted() {
                        self.stack_top = stack.top;
                        self.set_stack(local_index, value);
                    } else if current != value {
                        unsafe { stack.set(local_index, value) };
                    }
                }
                OpCode::JumpIfFalse => {
                    let offset = unsafe { cursor.read_u16(ip) };
                    ip += 2;
                    // SAFETY: the condition is the top stack slot.
                    if unsafe { stack.peek(0) }.is_falsey() {
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
                    // SAFETY: the callee sits under the arguments.
                    let callee = unsafe { stack.peek(args_count) };
                    if let Some(next) = self.call_closure_fast(callee, args_count, stack.top) {
                        cursor = next;
                        ip = 0;
                        continue;
                    }
                    self.stack_top = stack.top;
                    let prev_frame_count = self.frame_count;
                    self.call_value(callee, args_count)?;
                    stack.top = self.stack_top;
                    if self.frame_count != prev_frame_count {
                        (cursor, ip) = FrameCursor::active(self);
                    }
                }
                OpCode::Invoke => {
                    let method_ix = unsafe { cursor.read_byte(ip) } as usize;
                    let method_name = unsafe { cursor.read_constant(method_ix) };
                    let argc = unsafe { cursor.read_byte(ip + 1) };
                    ip += 2;
                    self.frames[cursor.index].ip = ip;
                    // SAFETY: the receiver sits under the arguments.
                    let receiver = unsafe { stack.peek(argc as usize) };
                    if let Some(next) = self
                        .find_class_method(receiver, method_name)
                        .and_then(|method| self.call_closure_fast(method, argc as usize, stack.top))
                    {
                        cursor = next;
                        ip = 0;
                        continue;
                    }
                    self.stack_top = stack.top;
                    let prev_frame_count = self.frame_count;
                    self.invoke(method_name, argc)?;
                    stack.top = self.stack_top;
                    if self.frame_count != prev_frame_count {
                        (cursor, ip) = FrameCursor::active(self);
                    }
                }
                OpCode::Closure => {
                    self.stack_top = stack.top;
                    ip = self.op_closure(&cursor, ip)?;
                    stack.top = self.stack_top;
                }
                OpCode::GetUpvalue => {
                    let slot = unsafe { cursor.read_byte(ip) } as usize;
                    ip += 1;
                    let upvalue_id = self
                        .objects
                        .closure(self.frames[cursor.index].closure)?
                        .upvalues[slot];
                    let upvalue = self.objects.upvalue(upvalue_id)?;
                    let lox_value = if upvalue.location.is_null() {
                        upvalue.closed
                    } else {
                        // SAFETY: an open upvalue points at a live stack slot.
                        unsafe { *upvalue.location }
                    };
                    // SAFETY: pushing a loaded upvalue stays inside the frame window.
                    unsafe { stack.push(lox_value) };
                    if lox_value.is_refcounted() {
                        self.objects.retain(lox_value);
                    }
                }
                OpCode::SetUpvalue => {
                    let slot = unsafe { cursor.read_byte(ip) } as usize;
                    ip += 1;
                    // SAFETY: the assigned value is the top stack slot.
                    let val = unsafe { stack.peek(0) };
                    let upvalue_id = self
                        .objects
                        .closure(self.frames[cursor.index].closure)?
                        .upvalues[slot];
                    let location = self.objects.upvalue(upvalue_id)?.location;
                    if location.is_null() {
                        self.stack_top = stack.top;
                        let old_closed = {
                            let upvalue = self.objects.upvalue_mut(upvalue_id)?;
                            let old = upvalue.closed;
                            upvalue.closed = val;
                            old
                        };
                        self.objects.release(old_closed);
                        self.objects.retain(val);
                    } else {
                        // SAFETY: an open upvalue points at a live stack slot.
                        let current = unsafe { *location };
                        if val.is_refcounted() || current.is_refcounted() {
                            if current != val {
                                self.objects.retain(val);
                                self.objects.release(current);
                                unsafe { *location = val };
                            }
                        } else if current != val {
                            unsafe { *location = val };
                        }
                    }
                }
                OpCode::CloseUpvalue => {
                    self.stack_top = stack.top;
                    let location = self.stack_top - 1;
                    self.close_upvalues(location)?;
                    self.pop_unchecked();
                    stack.top = self.stack_top;
                }
                OpCode::Class => {
                    self.stack_top = stack.top;
                    let class_ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    let class_name = unsafe { cursor.read_constant(class_ix) };
                    let class = self.objects.alloc_class(class_name)?;
                    self.push(class);
                    stack.top = self.stack_top;
                }
                OpCode::GetProperty => {
                    let prop_ix = unsafe { cursor.read_byte(ip) } as usize;
                    // SAFETY: the receiver is the top stack slot.
                    let receiver = unsafe { stack.peek(0) };
                    let name = unsafe { cursor.read_constant(prop_ix) };
                    if let Some(value) = self.instance_field(receiver, name) {
                        ip += CONST_SIZE;
                        // Retain before release: dropping a temporary receiver
                        // also drops its fields.
                        self.objects.retain(value);
                        unsafe { stack.replace_tos(value) };
                        self.objects.release(receiver);
                        continue;
                    }
                    self.stack_top = stack.top;
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
                    stack.top = self.stack_top;
                }
                OpCode::SetProperty => {
                    let prop_ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    // SAFETY: `SetProperty` reads the value and the receiver under it.
                    let value = unsafe { stack.peek(0) };
                    let receiver = unsafe { stack.peek(1) };
                    let name = unsafe { cursor.read_constant(prop_ix) };
                    if self.set_instance_field(receiver, name, value) {
                        // The value's stack reference moves down onto the receiver slot.
                        unsafe { stack.pop_and_replace(value) };
                        self.objects.release(receiver);
                        continue;
                    }
                    self.stack_top = stack.top;
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
                    stack.top = self.stack_top;
                }
                OpCode::Method => {
                    self.stack_top = stack.top;
                    let method_ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    self.define_method(unsafe { cursor.read_constant(method_ix) })?;
                    stack.top = self.stack_top;
                }
                OpCode::Inherit => {
                    self.stack_top = stack.top;
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
                    stack.top = self.stack_top;
                }
                OpCode::GetSuper => {
                    self.stack_top = stack.top;
                    let const_ix = unsafe { cursor.read_byte(ip) } as usize;
                    ip += CONST_SIZE;
                    let name_id = match unsafe { cursor.read_constant(const_ix) }.try_str() {
                        Ok(id) => id,
                        Err(err) => {
                            return self.runtime_error_at(&cursor, instruction_ip, err);
                        }
                    };
                    let super_class_id = self.pop_unchecked().try_class()?;
                    // Peek then pop (like GetProperty) so the instance stays live
                    // while `alloc_bound_method` retains it as the receiver.
                    let instance_id = self.peek_unchecked(0).try_instance()?;
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
                    self.pop_unchecked();
                    self.push(bound);
                    stack.top = self.stack_top;
                }
                OpCode::SuperInvoke => {
                    self.stack_top = stack.top;
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
                    stack.top = self.stack_top;
                    if self.frame_count != prev_frame_count {
                        (cursor, ip) = FrameCursor::active(self);
                    }
                }
            }
        }
    }

    /// `OP_CLOSURE` is cold and bulky. Keeping it out of `run` leaves the
    /// dispatch loop small enough to stay in the instruction cache.
    #[inline(never)]
    fn op_closure(&mut self, cursor: &FrameCursor, mut ip: usize) -> Result<usize, RuntimeError> {
        let const_ix = unsafe { cursor.read_byte(ip) } as usize;
        ip += CONST_SIZE;
        let function_value = unsafe { cursor.read_constant(const_ix) };
        let func_id = function_value.try_function()?;
        let upvalues_count = self.objects.function(func_id)?.upvalue_count;
        let mut upvalues = Vec::with_capacity(upvalues_count);

        for _ in 0..upvalues_count {
            let is_local = unsafe { cursor.read_byte(ip) };
            let index = unsafe { cursor.read_byte(ip + 1) };
            ip += 2;
            let upvalue = if is_local == 1 {
                self.capture_upvalue(cursor.slots + index as usize - 1)?
            } else {
                self.objects
                    .closure(self.frames[cursor.index].closure)?
                    .upvalues[index as usize]
            };
            upvalues.push(upvalue);
        }

        let closure_val = self
            .objects
            .alloc_closure(func_id, upvalues.into_boxed_slice())?;
        self.push(closure_val);
        Ok(ip)
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

    /// Happy path lookup for `OP_INVOKE`: the class method `name` of an
    /// instance `receiver` whose fields do not shadow it. `None` sends the
    /// call to [`Self::invoke`], which also reports the errors.
    #[inline(always)]
    fn find_class_method(&self, receiver: LoxValue, name: LoxValue) -> Option<LoxValue> {
        if !receiver.is_obj_type(ObjType::Instance) || !name.is_obj_type(ObjType::String) {
            return None;
        }
        let key = name.obj_id_unchecked();
        // SAFETY: an instance value on the stack is a live object of this store.
        let HeapObject::Instance(instance) =
            (unsafe { self.objects.get_unchecked(receiver.obj_id_unchecked()) })
        else {
            return None;
        };
        if instance.fields.contains_key(key) {
            return None;
        }
        // SAFETY: an instance's class id comes from this store; classes are never freed.
        let HeapObject::Class(class) = (unsafe { self.objects.get_unchecked(instance.class) })
        else {
            return None;
        };
        class.methods.get(key).copied()
    }

    /// Happy path for `OP_GET_PROPERTY`: the field `name` of an instance
    /// `receiver`. `None` covers methods and every error case.
    #[inline(always)]
    fn instance_field(&self, receiver: LoxValue, name: LoxValue) -> Option<LoxValue> {
        if !receiver.is_obj_type(ObjType::Instance) || !name.is_obj_type(ObjType::String) {
            return None;
        }
        // SAFETY: an instance value on the stack is a live object of this store.
        let HeapObject::Instance(instance) =
            (unsafe { self.objects.get_unchecked(receiver.obj_id_unchecked()) })
        else {
            return None;
        };
        instance.fields.get(name.obj_id_unchecked()).copied()
    }

    /// Happy path for `OP_SET_PROPERTY`: stores `value` in the field `name`
    /// of an instance `receiver`, retaining it for the field. Returns `false`
    /// without touching anything when `receiver` is not an instance.
    #[inline(never)]
    fn set_instance_field(&mut self, receiver: LoxValue, name: LoxValue, value: LoxValue) -> bool {
        if !receiver.is_obj_type(ObjType::Instance) || !name.is_obj_type(ObjType::String) {
            return false;
        }
        // SAFETY: an instance value on the stack is a live object of this store.
        let HeapObject::Instance(instance) =
            (unsafe { self.objects.get_unchecked_mut(receiver.obj_id_unchecked()) })
        else {
            return false;
        };
        let key = name.obj_id_unchecked();
        let old = match instance.fields.get_mut(key) {
            Some(slot) => Some(std::mem::replace(slot, value)),
            None => instance.fields.insert(key, value),
        };
        self.objects.retain(value);
        if let Some(old) = old {
            self.objects.release(old);
        }
        true
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

    #[inline(always)]
    fn stack_ptr(&mut self, index: usize) -> *mut LoxValue {
        debug_assert!(index < STACK_MAX);
        // SAFETY: `index` is a stack slot. The array does not move.
        unsafe { self.stack.as_mut_ptr().add(index) }
    }

    #[inline]
    fn close_upvalues(&mut self, from_slot: usize) -> Result<(), RuntimeError> {
        let from_ptr = self.stack_ptr(from_slot) as usize;
        while let Some(upvalue_id) = self.open_upvalues {
            let location = self.objects.upvalue(upvalue_id)?.location;
            if location.is_null() || (location as usize) < from_ptr {
                break;
            }
            let next = self.objects.upvalue(upvalue_id)?.next;
            // SAFETY: `location` is an open upvalue pointing at a live stack slot.
            let closed_value = unsafe { *location };
            let old_closed = {
                let upvalue = self.objects.upvalue_mut(upvalue_id)?;
                let old = upvalue.closed;
                upvalue.closed = closed_value;
                upvalue.location = std::ptr::null_mut();
                old
            };
            self.objects.release(old_closed);
            self.objects.retain(closed_value);
            self.open_upvalues = next;
        }
        Ok(())
    }

    #[inline]
    fn capture_upvalue(&mut self, location: usize) -> Result<ObjId, RuntimeError> {
        let slot = self.stack_ptr(location);
        let slot_addr = slot as usize;
        let mut prev: Option<ObjId> = None;
        let mut current = self.open_upvalues;

        while let Some(upvalue_id) = current {
            let upvalue = self.objects.upvalue(upvalue_id)?;
            let loc = upvalue.location;
            if !loc.is_null() && (loc as usize) > slot_addr {
                prev = Some(upvalue_id);
                current = upvalue.next;
            } else if loc == slot {
                return Ok(upvalue_id);
            } else {
                break;
            }
        }

        let created = self.objects.alloc_upvalue(slot)?;
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

    /// Happy path for `OP_CALL` / `OP_INVOKE` of a closure: pushes its frame
    /// and returns the cursor to continue in it. Returns `None` without
    /// touching frames when the callee is not a closure, the arity does not
    /// match, or the frame stack is full; the caller reports that through
    /// the slow path.
    ///
    /// `stack_top` is the live stack height, including the callee and
    /// arguments; `self.stack_top` may be stale.
    #[inline(always)]
    fn call_closure_fast(
        &mut self,
        callee: LoxValue,
        args_count: usize,
        stack_top: usize,
    ) -> Option<FrameCursor> {
        if !callee.is_obj_type(ObjType::Closure) {
            return None;
        }
        let closure_id = callee.obj_id_unchecked();
        // SAFETY: a closure value's id was allocated by this store.
        let HeapObject::Closure(closure) = (unsafe { self.objects.get_unchecked(closure_id) })
        else {
            return None;
        };
        if closure.arity as usize != args_count || self.frame_count == FRAMES_MAX - 1 {
            return None;
        }
        let chunk = &closure.chunk;
        let index = self.frame_count;
        let slots = stack_top - args_count;
        let frame = &mut self.frames[index];
        frame.slots = slots;
        frame.closure = closure_id;
        frame.ip = 0;
        frame.code = chunk.code.as_ptr();
        frame.code_len = chunk.code.len();
        frame.constants = chunk.constants.as_ptr();
        frame.lines = chunk.lines.as_ptr();
        self.frame_count += 1;
        Some(FrameCursor {
            index,
            slots,
            code: frame.code,
            constants: frame.constants,
        })
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
        // Capture chunk pointers before taking a mutable borrow of `frames`.
        // SAFETY: chunk buffers outlive the VM; see CallFrame invariants.
        let code = function.chunk.code.as_ptr();
        let code_len = function.chunk.code.len();
        let constants = function.chunk.constants.as_ptr();
        let lines = function.chunk.lines.as_ptr();
        let frame = &mut self.frames[self.frame_count];
        frame.slots = self.stack_top - args_count;
        frame.closure = closure_id;
        frame.ip = 0;
        frame.code = code;
        frame.code_len = code_len;
        frame.constants = constants;
        frame.lines = lines;
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
        let value = self.peek_unchecked(0);
        let Some(slot) = self.globals.get_mut(key) else {
            return Err(RuntimeError::UndefinedGlobal(
                string_chars(&self.objects, key)?.to_owned(),
            ));
        };
        let old = std::mem::replace(slot, value);
        self.objects.retain(value);
        self.objects.release(old);
        Ok(())
    }

    /// Happy path for `OP_GET_GLOBAL`. `None` sends the load to
    /// [`Self::get_global`], which reports an undefined variable.
    #[inline(never)]
    fn defined_global(&self, name: LoxValue) -> Option<LoxValue> {
        if !name.is_obj_type(ObjType::String) {
            return None;
        }
        self.globals.get(name.obj_id_unchecked()).copied()
    }

    #[cold]
    fn get_global(&mut self, name: LoxValue) -> Result<(), RuntimeError> {
        let key = name.try_str()?;
        let Some(val) = self.globals.get(key) else {
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

    /// Discarding / calling temporary bound methods must free them (and their
    /// receivers). The previous retain model left BM refcount at 1 forever.
    #[test]
    fn temporary_bound_method_is_collected() {
        // Arrange
        let mut stdout = Vec::new();
        let mut vm = VirtualMachine::new(&mut stdout);
        vm.init().unwrap();
        let script = r#"
class A {
  init() { this.x = 1; }
  m() { return this.x; }
}
for (var i = 0; i < 50; i = i + 1) {
  A().m;
  print A().m();
}
"#;

        // Act
        vm.interpret(script, false).unwrap();

        // Assert
        assert_eq!(vm.objects.count_live(ObjType::BoundMethod), 0);
        // Only immortal / still-rooted objects should remain; no A() leftovers.
        assert_eq!(vm.objects.count_live(ObjType::Instance), 0);
        assert_eq!(std::str::from_utf8(&stdout).unwrap(), &"1\n".repeat(50));
    }

    /// Instance stored in a field must outlive the temporary that created it.
    #[test]
    fn instance_field_keeps_nested_instance_alive() {
        // Arrange
        let mut stdout = Vec::new();
        let mut vm = VirtualMachine::new(&mut stdout);
        vm.init().unwrap();
        let script = r#"
class Box { init(n) { this.n = n; } }
class Holder {}
var h = Holder();
{
  var b = Box(7);
  h.f = b;
}
print h.f.n;
"#;

        // Act
        vm.interpret(script, false).unwrap();

        // Assert
        assert_eq!(vm.objects.count_live(ObjType::Instance), 2); // h and h.f
        assert_eq!(std::str::from_utf8(&stdout).unwrap(), "7\n");
    }

    /// Bound method must keep its receiver alive after the defining local dies.
    #[test]
    fn stored_bound_method_keeps_receiver_alive() {
        // Arrange
        let mut stdout = Vec::new();
        let mut vm = VirtualMachine::new(&mut stdout);
        vm.init().unwrap();
        let script = r#"
class A {
  init(n) { this.n = n; }
  m() { print this.n; }
}
var method;
{
  var a = A(99);
  method = a.m;
}
method();
"#;

        // Act
        vm.interpret(script, false).unwrap();

        // Assert
        assert_eq!(vm.objects.count_live(ObjType::BoundMethod), 1);
        assert_eq!(vm.objects.count_live(ObjType::Instance), 1);
        assert_eq!(std::str::from_utf8(&stdout).unwrap(), "99\n");
    }

    /// Field-held bound method invoked via Call / GetProperty must use `this`.
    #[test]
    fn field_bound_method_call_uses_receiver() {
        // Arrange
        let mut stdout = Vec::new();
        let mut vm = VirtualMachine::new(&mut stdout);
        vm.init().unwrap();
        let script = r#"
class A {
  init(n) { this.n = n; }
  m() { print this.n; }
}
var a = A(42);
a.f = a.m;
a.f();
var b = a.f;
b();
"#;

        // Act
        vm.interpret(script, false).unwrap();

        // Assert
        assert_eq!(std::str::from_utf8(&stdout).unwrap(), "42\n42\n");
    }

    /// Free-list reuse after collecting temporaries must not corrupt later instances.
    #[test]
    fn free_list_reuse_after_temporary_instances() {
        // Arrange
        let mut stdout = Vec::new();
        let mut vm = VirtualMachine::new(&mut stdout);
        vm.init().unwrap();
        let script = r#"
class Box { init(n) { this.n = n; } }
class Holder {}
var h = Holder();
for (var i = 0; i < 100; i = i + 1) {
  {
    var b = Box(i);
    h.f = b;
  }
}
print h.f.n;
"#;

        // Act
        vm.interpret(script, false).unwrap();

        // Assert
        assert!(vm.objects.free_list_len() > 0);
        assert_eq!(vm.objects.count_live(ObjType::Instance), 2);
        assert_eq!(std::str::from_utf8(&stdout).unwrap(), "99\n");
    }

    /// `super.method` as a value must bind `this` and free temporaries correctly.
    #[test]
    fn super_bound_method_returned_from_method_keeps_receiver() {
        // Arrange
        let mut stdout = Vec::new();
        let mut vm = VirtualMachine::new(&mut stdout);
        vm.init().unwrap();
        let script = r#"
class A {
  method(arg) { print "A.method(" + arg + ")"; }
}
class B < A {
  getClosure() { return super.method; }
  method(arg) { print "B.method(" + arg + ")"; }
}
var closure = B().getClosure();
closure("arg");
"#;

        // Act
        let result = vm.interpret(script, false);

        // Assert
        assert!(result.is_ok());
        assert_eq!(std::str::from_utf8(&stdout).unwrap(), "A.method(arg)\n");
    }

    #[test]
    fn method_returning_this_keeps_instance_alive() {
        // Arrange
        let mut stdout = Vec::new();
        let mut vm = VirtualMachine::new(&mut stdout);
        vm.init().unwrap();
        let script = r#"
class Counter {
  init() { this.count = 0; }
  bump() { this.count = this.count + 1; return this; }
}
print Counter().bump().bump().count;
"#;

        // Act
        vm.interpret(script, false).unwrap();

        // Assert
        assert_eq!(vm.objects.count_live(ObjType::Instance), 0);
        assert_eq!(std::str::from_utf8(&stdout).unwrap(), "2\n");
    }

    #[test]
    fn get_super_bound_method_then_call() {
        // Arrange
        let mut stdout = Vec::new();
        let mut vm = VirtualMachine::new(&mut stdout);
        vm.init().unwrap();
        let script = r#"
class A {
  method() { print this.value; }
}
class B < A {
  method() {
    var m = super.method;
    m();
  }
}
var b = B();
b.value = 3;
b.method();
"#;

        // Act
        vm.interpret(script, false).unwrap();

        // Assert
        assert_eq!(vm.objects.count_live(ObjType::BoundMethod), 0);
        assert_eq!(std::str::from_utf8(&stdout).unwrap(), "3\n");
    }
}
