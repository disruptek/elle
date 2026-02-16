//! Exception handling primitives
use crate::error::{LError, LResult};
use crate::value::{Condition, Value};

/// Throw an exception
pub fn prim_throw(args: &[Value]) -> LResult<Value> {
    if args.is_empty() {
        return Err(LError::from("throw requires at least 1 argument"));
    }

    if let Some(msg) = args[0].as_string() {
        Err(LError::from(msg.to_string()))
    } else if let Some(cond) = args[0].as_condition() {
        if let Some(msg) = cond.message() {
            Err(LError::from(msg.to_string()))
        } else {
            Err(LError::from(format!("Condition(id={})", cond.exception_id)))
        }
    } else {
        Err(LError::from(format!(
            "throw requires a string or condition, got {}",
            args[0].type_name()
        )))
    }
}

/// Create an exception
pub fn prim_exception(args: &[Value]) -> LResult<Value> {
    if args.is_empty() {
        return Err(LError::from("exception requires at least 1 argument"));
    }

    if let Some(msg) = args[0].as_string() {
        let cond = if args.len() > 1 {
            Condition::generic_with_data(msg.to_string(), args[1])
        } else {
            Condition::generic(msg.to_string())
        };
        use crate::value::heap::{alloc, HeapObject};
        // Convert new Condition to old Condition
        let mut old_cond = crate::value_old::Condition::new(cond.exception_id);
        for (field_id, value) in cond.fields {
            let old_value = crate::primitives::coroutines::new_value_to_old(value);
            old_cond.set_field(field_id, old_value);
        }
        if let Some(bt) = cond.backtrace {
            old_cond.backtrace = Some(bt);
        }
        if let Some(loc) = cond.location {
            old_cond.location = Some(loc);
        }
        Ok(alloc(HeapObject::Condition(old_cond)))
    } else {
        Err(LError::from(
            "exception requires a string as first argument",
        ))
    }
}

/// Get the message from an exception
pub fn prim_exception_message(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::from(
            "exception-message requires exactly 1 argument",
        ));
    }

    if let Some(cond) = args[0].as_condition() {
        if let Some(msg) = cond.message() {
            Ok(Value::string(msg))
        } else {
            Ok(Value::NIL)
        }
    } else {
        Err(LError::from(format!(
            "exception-message requires a condition, got {}",
            args[0].type_name()
        )))
    }
}

/// Get the data from an exception
pub fn prim_exception_data(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::from("exception-data requires exactly 1 argument"));
    }

    if let Some(cond) = args[0].as_condition() {
        match cond.data() {
            Some(data) => {
                // Convert old Value to new Value
                let new_value = crate::compiler::cps::primitives::old_value_to_new(data);
                Ok(new_value)
            }
            None => Ok(Value::NIL),
        }
    } else {
        Err(LError::from(format!(
            "exception-data requires a condition, got {}",
            args[0].type_name()
        )))
    }
}
