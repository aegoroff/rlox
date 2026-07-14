#![allow(clippy::missing_errors_doc)]

use std::fmt;
use std::rc::Rc;

use num_derive::FromPrimitive;

use fnv::FnvHashMap;

use crate::{
    RuntimeError,
    chunk::Chunk,
    obj_map::ObjMap,
    value::{Function, LoxValue},
};

pub type ObjId = u32;

#[derive(Copy, Clone, Debug, PartialEq, Eq, FromPrimitive)]
#[repr(u8)]
pub enum ObjType {
    String = 0,
    Function = 1,
    Native = 2,
    Closure = 3,
    Upvalue = 4,
    Class = 5,
    Instance = 6,
    BoundMethod = 7,
}

pub struct ObjString {
    pub chars: String,
    pub hash: u32,
}

pub struct ObjFunction {
    pub arity: usize,
    pub upvalue_count: usize,
    pub chunk: Rc<Chunk>,
    pub name: ObjId,
}

pub struct ObjNative {
    pub arity: usize,
    pub name: ObjId,
    pub func: fn(&[LoxValue]) -> crate::Result<LoxValue, RuntimeError>,
}

pub struct ObjUpvalue {
    pub location: Option<usize>,
    pub closed: LoxValue,
    pub next: Option<ObjId>,
}

pub struct ObjClosure {
    pub function: ObjId,
    pub upvalues: Vec<ObjId>,
}

pub struct ObjClass {
    pub name: ObjId,
    pub methods: ObjMap,
}

pub struct ObjInstance {
    pub class: ObjId,
    pub fields: ObjMap,
}

pub struct ObjBoundMethod {
    pub receiver: LoxValue,
    pub method: ObjId,
}

pub enum HeapObject {
    /// Slot recycled by the free list; not a live object.
    Free,
    String(ObjString),
    Function(ObjFunction),
    Native(ObjNative),
    Closure(ObjClosure),
    Upvalue(ObjUpvalue),
    Class(ObjClass),
    Instance(ObjInstance),
    BoundMethod(ObjBoundMethod),
}

pub struct ObjectStore {
    /// Dense object table. Freed slots hold [`HeapObject::Free`] and their
    /// ids sit on `free_list` for reuse.
    objects: Vec<HeapObject>,
    ref_counts: Vec<u32>,
    free_list: Vec<ObjId>,
    strings: FnvHashMap<(u32, usize), ObjId>,
}

impl Default for ObjectStore {
    fn default() -> Self {
        Self::new()
    }
}

impl ObjectStore {
    #[must_use]
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            ref_counts: Vec::new(),
            free_list: Vec::new(),
            strings: FnvHashMap::default(),
        }
    }

    fn alloc_slot(&mut self, object: HeapObject) -> Result<ObjId, RuntimeError> {
        if let Some(id) = self.free_list.pop() {
            let index = id as usize;
            // SAFETY: free-list ids are always in-range table indices.
            unsafe {
                *self.objects.get_unchecked_mut(index) = object;
                *self.ref_counts.get_unchecked_mut(index) = 0;
            }
            return Ok(id);
        }
        let id = u32::try_from(self.objects.len()).map_err(|_| RuntimeError::TooManyObjects)?;
        // Leave `u32::MAX` / `u32::MAX - 1` free for ObjMap empty/tombstone sentinels.
        if id >= u32::MAX - 1 {
            return Err(RuntimeError::TooManyObjects);
        }
        self.objects.push(object);
        self.ref_counts.push(0);
        Ok(id)
    }

    /// # Safety
    /// `id` must be a slot allocated by this store (`id as usize < objects.len()`).
    #[inline(always)]
    unsafe fn get_unchecked(&self, id: ObjId) -> &HeapObject {
        unsafe { self.objects.get_unchecked(id as usize) }
    }

    /// # Safety
    /// `id` must be a slot allocated by this store (`id as usize < objects.len()`).
    #[inline(always)]
    unsafe fn get_unchecked_mut(&mut self, id: ObjId) -> &mut HeapObject {
        unsafe { self.objects.get_unchecked_mut(id as usize) }
    }

    #[inline(always)]
    pub fn retain(&mut self, value: LoxValue) {
        if value.is_refcounted() {
            let index = value.obj_id_unchecked() as usize;
            // SAFETY: obj ids are allocated from this store; index is in range.
            unsafe {
                *self.ref_counts.get_unchecked_mut(index) += 1;
            }
        }
    }

    #[inline(always)]
    pub fn release(&mut self, value: LoxValue) {
        if !value.is_refcounted() {
            return;
        }
        let id = value.obj_id_unchecked();
        let index = id as usize;
        // SAFETY: obj ids are allocated from this store; index is in range.
        let count = unsafe { self.ref_counts.get_unchecked_mut(index) };
        if *count == 0 {
            return;
        }
        *count -= 1;
        if *count == 0 {
            self.free_object(id);
        }
    }

    fn free_object(&mut self, id: ObjId) {
        // SAFETY: `id` came from a live refcounted object in this store.
        let object = unsafe { std::mem::replace(self.get_unchecked_mut(id), HeapObject::Free) };
        match object {
            HeapObject::Free => return,
            HeapObject::Instance(instance) => {
                for value in instance.fields.values() {
                    self.release(*value);
                }
            }
            HeapObject::BoundMethod(bound) => {
                self.release(bound.receiver);
            }
            HeapObject::String(_)
            | HeapObject::Function(_)
            | HeapObject::Native(_)
            | HeapObject::Class(_)
            | HeapObject::Closure(_)
            | HeapObject::Upvalue(_) => {}
        }
        self.free_list.push(id);
    }

    fn push_object(&mut self, object: HeapObject) -> Result<ObjId, RuntimeError> {
        self.alloc_slot(object)
    }

    #[inline(always)]
    pub fn try_get(&self, id: ObjId) -> Result<&HeapObject, RuntimeError> {
        let index = id as usize;
        if index >= self.objects.len() {
            return Err(RuntimeError::ObjectUnavailable(id));
        }
        // SAFETY: bounds checked above.
        match unsafe { self.get_unchecked(id) } {
            HeapObject::Free => Err(RuntimeError::ObjectUnavailable(id)),
            object => Ok(object),
        }
    }

    #[inline(always)]
    pub fn try_get_mut(&mut self, id: ObjId) -> Result<&mut HeapObject, RuntimeError> {
        let index = id as usize;
        if index >= self.objects.len() {
            return Err(RuntimeError::ObjectUnavailable(id));
        }
        // SAFETY: bounds checked above.
        match unsafe { self.get_unchecked_mut(id) } {
            HeapObject::Free => Err(RuntimeError::ObjectUnavailable(id)),
            object => Ok(object),
        }
    }

    pub fn intern_string(&mut self, text: impl Into<String>) -> Result<LoxValue, RuntimeError> {
        let chars = text.into();
        let hash = hash_string(&chars);
        let key = (hash, chars.len());
        if let Some(&existing_id) = self.strings.get(&key)
            && let Ok(existing) = self.string(existing_id)
            && existing.chars == chars
        {
            return Ok(LoxValue::from_obj(existing_id, ObjType::String));
        }

        let id = self.push_object(HeapObject::String(ObjString { chars, hash }))?;
        self.strings.insert(key, id);
        Ok(LoxValue::from_obj(id, ObjType::String))
    }

    pub fn alloc_function(&mut self, function: Function) -> Result<LoxValue, RuntimeError> {
        let name = self.intern_string(function.name)?;
        let name_id = name.obj_id_unchecked();
        for constant in function.chunk.constants.iter() {
            self.retain(*constant);
        }
        let id = self.push_object(HeapObject::Function(ObjFunction {
            arity: function.arity,
            upvalue_count: function.upvalue_count,
            chunk: Rc::new(function.chunk),
            name: name_id,
        }))?;
        Ok(LoxValue::from_obj(id, ObjType::Function))
    }

    pub fn alloc_native(
        &mut self,
        name: &str,
        arity: usize,
        func: fn(&[LoxValue]) -> crate::Result<LoxValue, RuntimeError>,
    ) -> Result<LoxValue, RuntimeError> {
        let name_val = self.intern_string(name.to_string())?;
        let name_id = name_val.obj_id_unchecked();
        let id = self.push_object(HeapObject::Native(ObjNative {
            arity,
            name: name_id,
            func,
        }))?;
        Ok(LoxValue::from_obj(id, ObjType::Native))
    }

    pub fn alloc_closure(&mut self, function: ObjId) -> Result<LoxValue, RuntimeError> {
        let upvalue_count = self.function(function)?.upvalue_count;
        let id = self.push_object(HeapObject::Closure(ObjClosure {
            function,
            upvalues: Vec::with_capacity(upvalue_count),
        }))?;
        Ok(LoxValue::from_obj(id, ObjType::Closure))
    }

    pub fn alloc_upvalue(&mut self, location: usize) -> Result<ObjId, RuntimeError> {
        self.push_object(HeapObject::Upvalue(ObjUpvalue {
            location: Some(location),
            closed: LoxValue::NIL,
            next: None,
        }))
    }

    pub fn alloc_class(&mut self, name: LoxValue) -> Result<LoxValue, RuntimeError> {
        let name_id = name.try_str()?;
        let id = self.push_object(HeapObject::Class(ObjClass {
            name: name_id,
            methods: ObjMap::new(),
        }))?;
        Ok(LoxValue::from_obj(id, ObjType::Class))
    }

    pub fn alloc_instance(&mut self, class: ObjId) -> Result<LoxValue, RuntimeError> {
        let id = self.push_object(HeapObject::Instance(ObjInstance {
            class,
            fields: ObjMap::new(),
        }))?;
        Ok(LoxValue::from_obj(id, ObjType::Instance))
    }

    pub fn alloc_bound_method(
        &mut self,
        receiver: LoxValue,
        method: ObjId,
    ) -> Result<LoxValue, RuntimeError> {
        let id = self.push_object(HeapObject::BoundMethod(ObjBoundMethod { receiver, method }))?;
        self.retain(receiver);
        let val = LoxValue::from_obj(id, ObjType::BoundMethod);
        self.retain(val);
        Ok(val)
    }

    /// Typed accessors for ids known to come from this store (VM hot path).
    /// Invalid / freed ids still return `ObjectUnavailable` / type errors.
    #[inline(always)]
    fn expect_live(&self, id: ObjId) -> Result<&HeapObject, RuntimeError> {
        debug_assert!((id as usize) < self.objects.len());
        // SAFETY: allocated ObjIds are always in range of `objects`.
        match unsafe { self.get_unchecked(id) } {
            HeapObject::Free => Err(RuntimeError::ObjectUnavailable(id)),
            object => Ok(object),
        }
    }

    #[inline(always)]
    fn expect_live_mut(&mut self, id: ObjId) -> Result<&mut HeapObject, RuntimeError> {
        debug_assert!((id as usize) < self.objects.len());
        // SAFETY: allocated ObjIds are always in range of `objects`.
        match unsafe { self.get_unchecked_mut(id) } {
            HeapObject::Free => Err(RuntimeError::ObjectUnavailable(id)),
            object => Ok(object),
        }
    }

    #[inline(always)]
    pub fn string(&self, id: ObjId) -> Result<&ObjString, RuntimeError> {
        match self.expect_live(id)? {
            HeapObject::String(string) => Ok(string),
            _ => Err(RuntimeError::UnexpectedObjectType(id, ObjType::String)),
        }
    }

    #[inline(always)]
    pub fn function(&self, id: ObjId) -> Result<&ObjFunction, RuntimeError> {
        match self.expect_live(id)? {
            HeapObject::Function(function) => Ok(function),
            _ => Err(RuntimeError::UnexpectedObjectType(id, ObjType::Function)),
        }
    }

    #[inline(always)]
    pub fn native(&self, id: ObjId) -> Result<&ObjNative, RuntimeError> {
        match self.expect_live(id)? {
            HeapObject::Native(native) => Ok(native),
            _ => Err(RuntimeError::UnexpectedObjectType(id, ObjType::Native)),
        }
    }

    #[inline(always)]
    pub fn closure(&self, id: ObjId) -> Result<&ObjClosure, RuntimeError> {
        match self.expect_live(id)? {
            HeapObject::Closure(closure) => Ok(closure),
            _ => Err(RuntimeError::UnexpectedObjectType(id, ObjType::Closure)),
        }
    }

    #[inline(always)]
    pub fn closure_mut(&mut self, id: ObjId) -> Result<&mut ObjClosure, RuntimeError> {
        match self.expect_live_mut(id)? {
            HeapObject::Closure(closure) => Ok(closure),
            _ => Err(RuntimeError::UnexpectedObjectType(id, ObjType::Closure)),
        }
    }

    #[inline(always)]
    pub fn upvalue(&self, id: ObjId) -> Result<&ObjUpvalue, RuntimeError> {
        match self.expect_live(id)? {
            HeapObject::Upvalue(upvalue) => Ok(upvalue),
            _ => Err(RuntimeError::UnexpectedObjectType(id, ObjType::Upvalue)),
        }
    }

    #[inline(always)]
    pub fn upvalue_mut(&mut self, id: ObjId) -> Result<&mut ObjUpvalue, RuntimeError> {
        match self.expect_live_mut(id)? {
            HeapObject::Upvalue(upvalue) => Ok(upvalue),
            _ => Err(RuntimeError::UnexpectedObjectType(id, ObjType::Upvalue)),
        }
    }

    #[inline(always)]
    pub fn class(&self, id: ObjId) -> Result<&ObjClass, RuntimeError> {
        match self.expect_live(id)? {
            HeapObject::Class(class) => Ok(class),
            _ => Err(RuntimeError::UnexpectedObjectType(id, ObjType::Class)),
        }
    }

    #[inline(always)]
    pub fn class_mut(&mut self, id: ObjId) -> Result<&mut ObjClass, RuntimeError> {
        match self.expect_live_mut(id)? {
            HeapObject::Class(class) => Ok(class),
            _ => Err(RuntimeError::UnexpectedObjectType(id, ObjType::Class)),
        }
    }

    #[inline(always)]
    pub fn instance(&self, id: ObjId) -> Result<&ObjInstance, RuntimeError> {
        match self.expect_live(id)? {
            HeapObject::Instance(instance) => Ok(instance),
            _ => Err(RuntimeError::UnexpectedObjectType(id, ObjType::Instance)),
        }
    }

    #[inline(always)]
    pub fn instance_mut(&mut self, id: ObjId) -> Result<&mut ObjInstance, RuntimeError> {
        match self.expect_live_mut(id)? {
            HeapObject::Instance(instance) => Ok(instance),
            _ => Err(RuntimeError::UnexpectedObjectType(id, ObjType::Instance)),
        }
    }

    #[inline(always)]
    pub fn bound_method(&self, id: ObjId) -> Result<&ObjBoundMethod, RuntimeError> {
        match self.expect_live(id)? {
            HeapObject::BoundMethod(bound) => Ok(bound),
            _ => Err(RuntimeError::UnexpectedObjectType(id, ObjType::BoundMethod)),
        }
    }

    pub fn format(&self, value: LoxValue, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if value.is_number() {
            let number = value.as_number();
            if number.is_nan() {
                write!(f, "NaN")
            } else {
                write!(f, "{number}")
            }
        } else if value.is_bool() {
            write!(f, "{}", value.as_bool())
        } else if value.is_nil() {
            write!(f, "nil")
        } else if value.is_obj() {
            self.format_object(value.obj_id_unchecked(), f)
        } else {
            write!(f, "unknown")
        }
    }

    fn format_object(&self, id: ObjId, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Ok(object) = self.try_get(id) else {
            return write!(f, "<freed>");
        };
        match object {
            HeapObject::Free => write!(f, "<freed>"),
            HeapObject::String(string) => write!(f, "{}", string.chars),
            HeapObject::Function(function) => match self.string(function.name) {
                Ok(name) => write!(f, "<fn {}>", name.chars),
                Err(_) => write!(f, "<fn ?>"),
            },
            HeapObject::Native(_) => write!(f, "<native fn>"),
            HeapObject::Closure(closure) => match self
                .function(closure.function)
                .and_then(|function| self.string(function.name))
            {
                Ok(name) => write!(f, "<fn {}>", name.chars),
                Err(_) => write!(f, "<fn ?>"),
            },
            HeapObject::Class(class) => match self.string(class.name) {
                Ok(name) => write!(f, "{}", name.chars),
                Err(_) => write!(f, "<class>"),
            },
            HeapObject::Instance(instance) => match self
                .class(instance.class)
                .and_then(|class| self.string(class.name))
            {
                Ok(name) => write!(f, "{} instance", name.chars),
                Err(_) => write!(f, "<instance>"),
            },
            HeapObject::BoundMethod(bound) => match self.closure(bound.method).and_then(|closure| {
                self.function(closure.function)
                    .and_then(|function| self.string(function.name))
            }) {
                Ok(name) => write!(f, "<fn {}>", name.chars),
                Err(_) => write!(f, "<fn ?>"),
            },
            HeapObject::Upvalue(_) => write!(f, "upvalue"),
        }
    }
}

#[must_use]
pub fn hash_string(text: &str) -> u32 {
    let mut hash: u32 = 2_166_136_261;
    for byte in text.bytes() {
        hash ^= u32::from(byte);
        hash = hash.wrapping_mul(16_777_619);
    }
    hash
}

#[must_use]
pub fn obj_type_in(store: &ObjectStore, value: LoxValue) -> Option<ObjType> {
    let id = value.obj_id()?;
    let object = store.try_get(id).ok()?;
    Some(match object {
        HeapObject::Free => return None,
        HeapObject::String(_) => ObjType::String,
        HeapObject::Function(_) => ObjType::Function,
        HeapObject::Native(_) => ObjType::Native,
        HeapObject::Closure(_) => ObjType::Closure,
        HeapObject::Upvalue(_) => ObjType::Upvalue,
        HeapObject::Class(_) => ObjType::Class,
        HeapObject::Instance(_) => ObjType::Instance,
        HeapObject::BoundMethod(_) => ObjType::BoundMethod,
    })
}

pub fn string_chars(store: &ObjectStore, id: ObjId) -> Result<&str, RuntimeError> {
    Ok(&store.string(id)?.chars)
}
