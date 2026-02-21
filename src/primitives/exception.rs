//! Exception handling primitives
use crate::value::fiber::{SignalBits, SIG_ERROR, SIG_OK};
use crate::value::{Condition, Value};

/// Throw an exception
pub fn prim_throw(args: &[Value]) -> (SignalBits, Value) {
    if args.is_empty() {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(
                "throw: expected at least 1 argument, got 0".to_string(),
            )),
        );
    }

    if let Some(msg) = args[0].as_string() {
        (
            SIG_ERROR,
            Value::condition(Condition::error(msg.to_string())),
        )
    } else if let Some(cond) = args[0].as_condition() {
        // Re-throw the condition - clone it
        (SIG_ERROR, Value::condition(cond.clone()))
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(format!(
                "throw: expected string or condition, got {}",
                args[0].type_name()
            ))),
        )
    }
}

/// Create an exception
pub fn prim_exception(args: &[Value]) -> (SignalBits, Value) {
    if args.is_empty() {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(
                "exception: expected at least 1 argument, got 0".to_string(),
            )),
        );
    }

    if let Some(msg) = args[0].as_string() {
        let cond = if args.len() > 1 {
            Condition::generic_with_data(msg.to_string(), args[1])
        } else {
            Condition::generic(msg.to_string())
        };
        use crate::value::heap::{alloc, HeapObject};
        // Store the Condition directly (no conversion needed)
        (SIG_OK, alloc(HeapObject::Condition(cond)))
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(
                "exception: expected string as first argument".to_string(),
            )),
        )
    }
}

/// Get the message from an exception
pub fn prim_exception_message(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "exception-message: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    if let Some(cond) = args[0].as_condition() {
        // message() returns &str directly (always present)
        (SIG_OK, Value::string(cond.message()))
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(format!(
                "exception-message: expected condition, got {}",
                args[0].type_name()
            ))),
        )
    }
}

/// Get the data from an exception
pub fn prim_exception_data(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "exception-data: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    if let Some(cond) = args[0].as_condition() {
        match cond.data() {
            Some(data) => (SIG_OK, *data),
            None => (SIG_OK, Value::NIL),
        }
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(format!(
                "exception-data: expected condition, got {}",
                args[0].type_name()
            ))),
        )
    }
}
