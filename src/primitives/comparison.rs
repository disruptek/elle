//! Comparison primitives
use crate::value::Value;

/// Equality comparison
pub fn prim_eq(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("= requires exactly 2 arguments".to_string());
    }
    Ok(Value::Bool(args[0] == args[1]))
}

/// Less than comparison
pub fn prim_lt(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("< requires exactly 2 arguments".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) < *b)),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(*a < (*b as f64))),
        _ => Err("< requires numbers".to_string()),
    }
}

/// Greater than comparison
pub fn prim_gt(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("> requires exactly 2 arguments".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) > *b)),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(*a > (*b as f64))),
        _ => Err("> requires numbers".to_string()),
    }
}

/// Less than or equal comparison
pub fn prim_le(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("<= requires exactly 2 arguments".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) <= *b)),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(*a <= (*b as f64))),
        _ => Err("<= requires numbers".to_string()),
    }
}

/// Greater than or equal comparison
pub fn prim_ge(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err(">= requires exactly 2 arguments".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) >= *b)),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(*a >= (*b as f64))),
        _ => Err(">= requires numbers".to_string()),
    }
}
