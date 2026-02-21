//! Exception introspection primitives (Phase 8)
//! Provides functions for handlers to query exception details
use crate::value::fiber::{SignalBits, SIG_ERROR, SIG_OK};
use crate::value::{Condition, Value};

/// Get the exception ID from a Condition
pub fn prim_exception_id(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "exception-id: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    if let Some(cond) = args[0].as_condition() {
        (SIG_OK, Value::int(cond.exception_id as i64))
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(
                "exception-id: expected a Condition".to_string(),
            )),
        )
    }
}

/// Get a field value from a Condition by field ID
pub fn prim_condition_field(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 2 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "condition-field: expected 2 arguments, got {}",
                args.len()
            ))),
        );
    }

    let field_id = match args[1].as_int() {
        Some(id) => id as u32,
        None => {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(
                    "condition-field: field-id must be an integer".to_string(),
                )),
            );
        }
    };

    if let Some(cond) = args[0].as_condition() {
        match cond.fields.get(&field_id) {
            Some(val) => (SIG_OK, *val),
            None => (SIG_OK, Value::NIL),
        }
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(
                "condition-field: expected a Condition as first argument".to_string(),
            )),
        )
    }
}

/// Check if a Condition matches a given exception type ID
pub fn prim_condition_matches_type(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 2 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "condition-matches-type: expected 2 arguments, got {}",
                args.len()
            ))),
        );
    }

    let exception_type_id = match args[1].as_int() {
        Some(id) => id as u32,
        None => {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(
                    "condition-matches-type: exception-type-id must be an integer".to_string(),
                )),
            );
        }
    };

    if let Some(cond) = args[0].as_condition() {
        use crate::vm::is_exception_subclass;
        (
            SIG_OK,
            Value::bool(is_exception_subclass(cond.exception_id, exception_type_id)),
        )
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(
                "condition-matches-type: expected a Condition as first argument".to_string(),
            )),
        )
    }
}

/// Get the backtrace from a Condition
pub fn prim_condition_backtrace(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "condition-backtrace: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    if let Some(cond) = args[0].as_condition() {
        match &cond.backtrace {
            Some(bt) => (SIG_OK, Value::string(bt.as_str())),
            None => (SIG_OK, Value::NIL),
        }
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(
                "condition-backtrace: expected a Condition".to_string(),
            )),
        )
    }
}
