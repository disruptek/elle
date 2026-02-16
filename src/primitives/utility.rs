//! Utility primitives (mod, remainder, even?, odd?)
use crate::error::{LError, LResult};
use crate::value::Value;

/// Modulo operation (result has same sign as divisor)
pub fn prim_mod(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err(LError::from("mod requires exactly 2 arguments"));
    }

    match (args[0].as_int(), args[1].as_int()) {
        (Some(a), Some(b)) => {
            if b == 0 {
                return Err(LError::division_by_zero());
            }
            // Lisp mod: result has same sign as divisor
            let rem = a % b;
            if rem == 0 {
                Ok(Value::int(0))
            } else if (rem > 0) != (b > 0) {
                Ok(Value::int(rem + b))
            } else {
                Ok(Value::int(rem))
            }
        }
        _ => Err(LError::type_mismatch("integer", args[0].type_name())),
    }
}

/// Remainder operation (result has same sign as dividend)
pub fn prim_remainder(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err(LError::from("remainder requires exactly 2 arguments"));
    }

    match (args[0].as_int(), args[1].as_int()) {
        (Some(a), Some(b)) => {
            if b == 0 {
                return Err(LError::division_by_zero());
            }
            let rem = a % b;
            // Adjust remainder to have same sign as dividend
            if (rem > 0 && b < 0) || (rem < 0 && b > 0) {
                Ok(Value::int(rem + b))
            } else {
                Ok(Value::int(rem))
            }
        }
        _ => Err(LError::type_mismatch("integer", args[0].type_name())),
    }
}

/// Check if number is even
pub fn prim_even(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    match args[0].as_int() {
        Some(n) => Ok(Value::bool(n % 2 == 0)),
        _ => Err(LError::type_mismatch("integer", args[0].type_name())),
    }
}

/// Check if number is odd
pub fn prim_odd(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    match args[0].as_int() {
        Some(n) => Ok(Value::bool(n % 2 != 0)),
        _ => Err(LError::type_mismatch("integer", args[0].type_name())),
    }
}
