use crate::{RuntimeError, value::LoxValue};

#[inline]
pub fn clock(_: &[LoxValue]) -> crate::Result<LoxValue, RuntimeError> {
    let millis = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis();
    Ok(LoxValue::number(millis as f64 * 0.001))
}

#[inline]
pub fn sqrt(args: &[LoxValue]) -> crate::Result<LoxValue, RuntimeError> {
    Ok(LoxValue::number(args[0].try_num()?.sqrt()))
}

#[inline]
pub fn min(args: &[LoxValue]) -> crate::Result<LoxValue, RuntimeError> {
    Ok(LoxValue::number(args[0].try_num()?.min(args[1].try_num()?)))
}

#[inline]
pub fn max(args: &[LoxValue]) -> crate::Result<LoxValue, RuntimeError> {
    Ok(LoxValue::number(args[0].try_num()?.max(args[1].try_num()?)))
}
