use crate::error::LResult;
use crate::value::{list, Value};

/// Get the current package version
pub fn prim_package_version(_args: &[Value]) -> LResult<Value> {
    Ok(Value::string("0.3.0"))
}

/// Get package information
pub fn prim_package_info(_args: &[Value]) -> LResult<Value> {
    Ok(list(vec![
        Value::string("Elle"),
        Value::string("0.3.0"),
        Value::string("A Lisp interpreter with bytecode compilation"),
    ]))
}
