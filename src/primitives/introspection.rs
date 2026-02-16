//! Exception introspection primitives (Phase 8)
//! Provides functions for handlers to query exception details
use crate::error::{LError, LResult};
use crate::value::Value;

/// Get the exception ID from a Condition
pub fn prim_exception_id(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err("exception-id expects 1 argument".to_string().into());
    }

    if let Some(cond) = args[0].as_condition() {
        Ok(Value::int(cond.exception_id as i64))
    } else {
        Err("exception-id expects a Condition".to_string().into())
    }
}

/// Get a field value from a Condition by field ID
pub fn prim_condition_field(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err("condition-field expects 2 arguments (condition field-id)"
            .to_string()
            .into());
    }

    let field_id = (args[1]
        .as_int()
        .ok_or_else(|| LError::from("field-id must be an integer"))?) as u32;
    if let Some(cond) = args[0].as_condition() {
        match cond.fields.get(&field_id) {
            Some(val) => {
                // Convert old Value to new Value
                let new_value = crate::compiler::cps::primitives::old_value_to_new(val);
                Ok(new_value)
            }
            None => Ok(Value::NIL),
        }
    } else {
        Err("condition-field expects a Condition as first argument"
            .to_string()
            .into())
    }
}

/// Check if a Condition matches a given exception type ID
pub fn prim_condition_matches_type(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err("condition-matches-type expects 2 arguments"
            .to_string()
            .into());
    }

    let exception_type_id = (args[1]
        .as_int()
        .ok_or_else(|| LError::from("exception-type-id must be an integer"))?)
        as u32;
    if let Some(cond) = args[0].as_condition() {
        use crate::vm::is_exception_subclass;
        Ok(Value::bool(is_exception_subclass(
            cond.exception_id,
            exception_type_id,
        )))
    } else {
        Err(
            "condition-matches-type expects a Condition as first argument"
                .to_string()
                .into(),
        )
    }
}

/// Get the backtrace from a Condition
pub fn prim_condition_backtrace(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err("condition-backtrace expects 1 argument".to_string().into());
    }

    if let Some(cond) = args[0].as_condition() {
        match &cond.backtrace {
            Some(bt) => Ok(Value::string(bt.as_str())),
            None => Ok(Value::NIL),
        }
    } else {
        Err("condition-backtrace expects a Condition".to_string().into())
    }
}
