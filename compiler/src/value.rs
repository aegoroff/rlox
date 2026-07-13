#![allow(clippy::missing_errors_doc)]

use std::fmt::{self, Display};

use num_traits::FromPrimitive;

use crate::{
    RuntimeError,
    chunk::Chunk,
    object::{
        ObjId, ObjType, ObjectStore, string_chars, try_bound_method_id, try_class_id,
        try_closure_id, try_function_id, try_instance_id, try_native_id, try_string_id,
    },
};

const SIGN_BIT: u64 = 0x8000_0000_0000_0000;
const QNAN: u64 = 0x7ffc_0000_0000_0000;
const TAG_NIL: u64 = 1;
const TAG_FALSE: u64 = 2;
const TAG_TRUE: u64 = 3;
const OBJ_TYPE_SHIFT: u64 = 32;
const OBJ_ID_MASK: u64 = (1u64 << OBJ_TYPE_SHIFT) - 1;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[repr(transparent)]
pub struct LoxValue(u64);

impl LoxValue {
    pub const NIL: Self = Self(QNAN | TAG_NIL);
    pub const FALSE: Self = Self(QNAN | TAG_FALSE);
    pub const TRUE: Self = Self(QNAN | TAG_TRUE);

    #[inline]
    #[must_use]
    pub fn number(value: f64) -> Self {
        Self(value.to_bits())
    }

    #[inline]
    #[must_use]
    pub fn from_obj(id: ObjId, ty: ObjType) -> Self {
        Self(SIGN_BIT | QNAN | (u64::from(ty as u8) << OBJ_TYPE_SHIFT) | u64::from(id))
    }

    #[inline]
    #[must_use]
    pub fn obj_id(self) -> Option<ObjId> {
        if self.is_obj() {
            Some(self.obj_id_unchecked())
        } else {
            None
        }
    }

    #[inline]
    #[must_use]
    pub fn obj_id_unchecked(self) -> ObjId {
        debug_assert!(self.is_obj());
        (self.0 & OBJ_ID_MASK) as ObjId
    }

    #[inline]
    #[must_use]
    pub fn obj_type(self) -> Option<ObjType> {
        if !self.is_obj() {
            return None;
        }
        ObjType::from_u8(((self.0 >> OBJ_TYPE_SHIFT) & 0xFF) as u8)
    }

    #[inline]
    #[must_use]
    pub fn is_number(self) -> bool {
        (self.0 & QNAN) != QNAN
    }

    #[inline]
    #[must_use]
    pub fn is_nil(self) -> bool {
        self.0 == Self::NIL.0
    }

    #[inline]
    #[must_use]
    pub fn is_bool(self) -> bool {
        (self.0 | 1) == Self::TRUE.0
    }

    #[inline]
    #[must_use]
    pub fn is_obj(self) -> bool {
        (self.0 & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT)
    }

    #[inline]
    #[must_use]
    pub fn is_refcounted(self) -> bool {
        self.is_obj()
            && matches!(
                self.obj_type(),
                Some(ObjType::Instance | ObjType::BoundMethod)
            )
    }

    #[inline]
    #[must_use]
    pub fn as_number(self) -> f64 {
        f64::from_bits(self.0)
    }

    #[inline]
    #[must_use]
    pub fn as_bool(self) -> bool {
        self.0 == Self::TRUE.0
    }

    #[inline]
    #[must_use]
    pub fn bool_val(value: bool) -> Self {
        if value { Self::TRUE } else { Self::FALSE }
    }

    #[inline]
    #[must_use]
    pub fn equal(self, other: Self) -> bool {
        if self.is_number() && other.is_number() {
            return (self.as_number() - other.as_number()).abs() < 0.00001;
        }
        if (self.is_bool() || self.is_nil()) && (other.is_bool() || other.is_nil()) {
            let left = self.is_bool() && self.as_bool();
            let right = other.is_bool() && other.as_bool();
            return left == right;
        }
        self.0 == other.0
    }

    pub fn less(self, other: Self, store: &ObjectStore) -> Result<bool, RuntimeError> {
        if self.is_number() && other.is_number() {
            return Ok(self.as_number() < other.as_number());
        }
        if (self.is_bool() || self.is_nil()) && (other.is_bool() || other.is_nil()) {
            let left = self.is_bool() && self.as_bool();
            let right = other.is_bool() && other.as_bool();
            return Ok(!left & right);
        }
        if let (Ok(left_id), Ok(right_id)) =
            (try_string_id(store, self), try_string_id(store, other))
        {
            return Ok(string_chars(store, left_id)? < string_chars(store, right_id)?);
        }
        Err(RuntimeError::OperandsMustBeNumbers(self, other))
    }

    pub fn try_num(self) -> Result<f64, RuntimeError> {
        if self.is_number() {
            Ok(self.as_number())
        } else {
            Err(RuntimeError::ExpectedNumber(self))
        }
    }

    pub fn try_str(self, store: &ObjectStore) -> Result<ObjId, RuntimeError> {
        try_string_id(store, self)
    }

    pub fn try_bool(self) -> Result<bool, RuntimeError> {
        if self.is_bool() {
            Ok(self.as_bool())
        } else if self.is_nil() {
            Ok(false)
        } else {
            Err(RuntimeError::ExpectedBool(self))
        }
    }

    pub fn try_class(self, store: &ObjectStore) -> Result<ObjId, RuntimeError> {
        try_class_id(store, self)
    }

    pub fn try_instance(self, store: &ObjectStore) -> Result<ObjId, RuntimeError> {
        try_instance_id(store, self)
    }

    pub fn try_function(self, store: &ObjectStore) -> Result<ObjId, RuntimeError> {
        try_function_id(store, self)
    }

    pub fn try_closure(self, store: &ObjectStore) -> Result<ObjId, RuntimeError> {
        try_closure_id(store, self)
    }

    pub fn try_native(self, store: &ObjectStore) -> Result<ObjId, RuntimeError> {
        try_native_id(store, self)
    }

    pub fn try_bound_method(self, store: &ObjectStore) -> Result<ObjId, RuntimeError> {
        try_bound_method_id(store, self)
    }

    #[must_use]
    pub fn is_falsey(self) -> bool {
        self.is_nil() || (self.is_bool() && !self.as_bool())
    }
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_number() {
            let number = self.as_number();
            if number.is_nan() {
                write!(f, "NaN")
            } else {
                write!(f, "{number}")
            }
        } else if self.is_bool() {
            write!(f, "{}", self.as_bool())
        } else if self.is_nil() {
            write!(f, "nil")
        } else if self.is_obj() {
            write!(f, "<object>")
        } else {
            write!(f, "unknown")
        }
    }
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
    pub upvalue_count: usize,
}

impl Function {
    #[must_use]
    pub fn new(name: &str) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: name.to_owned(),
            upvalue_count: 0,
        }
    }

    pub fn disassembly(&self, store: &ObjectStore) {
        self.chunk.disassembly(self.name.as_str(), store);
    }

    #[must_use]
    pub fn start(&self) -> usize {
        self.chunk.first_line()
    }
}
