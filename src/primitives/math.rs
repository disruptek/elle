use crate::error::{LError, LResult};
use crate::value::Value;
use std::f64::consts::{E, PI};

pub fn prim_sqrt(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    match args[0].as_int() {
        Some(n) => Ok(Value::float((n as f64).sqrt())),
        None => match args[0].as_float() {
            Some(f) => Ok(Value::float(f.sqrt())),
            None => Err(LError::type_mismatch("number", args[0].type_name())),
        },
    }
}

pub fn prim_sin(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    match args[0].as_int() {
        Some(n) => Ok(Value::float((n as f64).sin())),
        None => match args[0].as_float() {
            Some(f) => Ok(Value::float(f.sin())),
            None => Err(LError::type_mismatch("number", args[0].type_name())),
        },
    }
}

pub fn prim_cos(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    match args[0].as_int() {
        Some(n) => Ok(Value::float((n as f64).cos())),
        None => match args[0].as_float() {
            Some(f) => Ok(Value::float(f.cos())),
            None => Err(LError::type_mismatch("number", args[0].type_name())),
        },
    }
}

pub fn prim_tan(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    match args[0].as_int() {
        Some(n) => Ok(Value::float((n as f64).tan())),
        None => match args[0].as_float() {
            Some(f) => Ok(Value::float(f.tan())),
            None => Err(LError::type_mismatch("number", args[0].type_name())),
        },
    }
}

pub fn prim_log(args: &[Value]) -> LResult<Value> {
    if args.is_empty() || args.len() > 2 {
        return Err(LError::arity_range(1, 2, args.len()));
    }

    let value = match args[0].as_int() {
        Some(n) => n as f64,
        None => match args[0].as_float() {
            Some(f) => f,
            None => return Err(LError::type_mismatch("number", args[0].type_name())),
        },
    };

    if args.len() == 1 {
        // Natural logarithm
        Ok(Value::float(value.ln()))
    } else {
        // Logarithm with specified base
        let base = match args[1].as_int() {
            Some(n) => n as f64,
            None => match args[1].as_float() {
                Some(f) => f,
                None => return Err(LError::type_mismatch("number", args[1].type_name())),
            },
        };
        Ok(Value::float(value.log(base)))
    }
}

pub fn prim_exp(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    match args[0].as_int() {
        Some(n) => Ok(Value::float((n as f64).exp())),
        None => match args[0].as_float() {
            Some(f) => Ok(Value::float(f.exp())),
            None => Err(LError::type_mismatch("number", args[0].type_name())),
        },
    }
}

pub fn prim_pow(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err(LError::arity_mismatch(2, args.len()));
    }

    match (args[0].as_int(), args[1].as_int()) {
        (Some(a), Some(b)) => {
            if b < 0 {
                Ok(Value::float((a as f64).powf(b as f64)))
            } else {
                Ok(Value::int(a.pow(b as u32)))
            }
        }
        _ => match (args[0].as_float(), args[1].as_float()) {
            (Some(a), Some(b)) => Ok(Value::float(a.powf(b))),
            _ => Err(LError::type_mismatch("number", args[0].type_name())),
        },
    }
}

pub fn prim_floor(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    match args[0].as_int() {
        Some(n) => Ok(Value::int(n)),
        None => match args[0].as_float() {
            Some(f) => Ok(Value::int(f.floor() as i64)),
            None => Err(LError::type_mismatch("number", args[0].type_name())),
        },
    }
}

pub fn prim_ceil(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    match args[0].as_int() {
        Some(n) => Ok(Value::int(n)),
        None => match args[0].as_float() {
            Some(f) => Ok(Value::int(f.ceil() as i64)),
            None => Err(LError::type_mismatch("number", args[0].type_name())),
        },
    }
}

pub fn prim_round(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    match args[0].as_int() {
        Some(n) => Ok(Value::int(n)),
        None => match args[0].as_float() {
            Some(f) => Ok(Value::int(f.round() as i64)),
            None => Err(LError::type_mismatch("number", args[0].type_name())),
        },
    }
}

pub fn prim_pi(_args: &[Value]) -> LResult<Value> {
    Ok(Value::float(PI))
}

pub fn prim_e(_args: &[Value]) -> LResult<Value> {
    Ok(Value::float(E))
}
