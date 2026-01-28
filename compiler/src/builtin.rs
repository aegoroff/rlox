use crate::{RuntimeError, value::LoxValue};
use std::time::{SystemTime, UNIX_EPOCH};

#[inline]
pub fn clock(_: &[LoxValue]) -> crate::Result<LoxValue, RuntimeError> {
    let start = SystemTime::now();
    let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap_or_default();
    let millis = since_the_epoch.as_millis();
    Ok(LoxValue::Number(millis as f64 * 0.001))
}

#[inline]
pub fn sqrt(args: &[LoxValue]) -> crate::Result<LoxValue, RuntimeError> {
    if let LoxValue::Number(num) = args[0] {
        Ok(LoxValue::Number(num.sqrt()))
    } else {
        Err(RuntimeError::ExpectedNumber(args[0].clone()))
    }
}

#[inline]
pub fn min(args: &[LoxValue]) -> crate::Result<LoxValue, RuntimeError> {
    if let LoxValue::Number(a) = args[0] {
        if let LoxValue::Number(b) = args[1] {
            Ok(LoxValue::Number(a.min(b)))
        } else {
            Err(RuntimeError::ExpectedNumber(args[1].clone()))
        }
    } else {
        Err(RuntimeError::ExpectedNumber(args[0].clone()))
    }
}

#[inline]
pub fn max(args: &[LoxValue]) -> crate::Result<LoxValue, RuntimeError> {
    if let LoxValue::Number(a) = args[0] {
        if let LoxValue::Number(b) = args[1] {
            Ok(LoxValue::Number(a.max(b)))
        } else {
            Err(RuntimeError::ExpectedNumber(args[1].clone()))
        }
    } else {
        Err(RuntimeError::ExpectedNumber(args[0].clone()))
    }
}
