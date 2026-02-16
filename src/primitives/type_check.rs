//! Type checking primitives
use crate::error::{LError, LResult};
use crate::ffi::primitives::context::get_symbol_table;
use crate::value::Value;

/// Check if value is nil
pub fn prim_is_nil(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    Ok(Value::bool(args[0].is_nil()))
}

/// Check if value is a pair (cons cell)
pub fn prim_is_pair(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    Ok(Value::bool(args[0].as_cons().is_some()))
}

/// Check if value is a list (nil or cons cell)
pub fn prim_is_list(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    Ok(Value::bool(
        args[0].is_nil() || args[0].is_empty_list() || args[0].as_cons().is_some(),
    ))
}

/// Check if value is a number
pub fn prim_is_number(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    Ok(Value::bool(args[0].is_number()))
}

/// Check if value is a symbol
pub fn prim_is_symbol(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    Ok(Value::bool(args[0].is_symbol()))
}

/// Check if value is a string
pub fn prim_is_string(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    Ok(Value::bool(args[0].as_string().is_some()))
}

/// Check if value is a boolean
pub fn prim_is_boolean(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    Ok(Value::bool(args[0].is_bool()))
}

/// Check if value is a keyword
pub fn prim_is_keyword(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    Ok(Value::bool(args[0].is_keyword()))
}

/// Check if value is a keyword

/// Get the type name of a value as a keyword
pub fn prim_type_of(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    let type_name = args[0].type_name();

    // Try to get the symbol table from thread-local context
    // Safety: The symbol table pointer is set in main() and cleared only at exit,
    // so it's valid during program execution.
    unsafe {
        if let Some(symbols_ptr) = get_symbol_table() {
            let keyword_id = (*symbols_ptr).intern(type_name);
            Ok(Value::keyword(keyword_id.0))
        } else {
            // Fallback to string if no symbol table in context
            Ok(Value::string(type_name.to_string()))
        }
    }
}
