use crate::value::Value;
use std::f64::consts::{E, PI};

pub fn prim_sqrt(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("sqrt requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Float((*n as f64).sqrt())),
        Value::Float(f) => Ok(Value::Float(f.sqrt())),
        _ => Err("Type error: sqrt requires a number".to_string()),
    }
}

pub fn prim_sin(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("sin requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Float((*n as f64).sin())),
        Value::Float(f) => Ok(Value::Float(f.sin())),
        _ => Err("Type error: sin requires a number".to_string()),
    }
}

pub fn prim_cos(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("cos requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Float((*n as f64).cos())),
        Value::Float(f) => Ok(Value::Float(f.cos())),
        _ => Err("Type error: cos requires a number".to_string()),
    }
}

pub fn prim_tan(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("tan requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Float((*n as f64).tan())),
        Value::Float(f) => Ok(Value::Float(f.tan())),
        _ => Err("Type error: tan requires a number".to_string()),
    }
}

pub fn prim_log(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("log requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Float((*n as f64).ln())),
        Value::Float(f) => Ok(Value::Float(f.ln())),
        _ => Err("Type error: log requires a number".to_string()),
    }
}

pub fn prim_exp(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("exp requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Float((*n as f64).exp())),
        Value::Float(f) => Ok(Value::Float(f.exp())),
        _ => Err("Type error: exp requires a number".to_string()),
    }
}

pub fn prim_pow(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("pow requires exactly 2 arguments".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => {
            if *b < 0 {
                Ok(Value::Float((*a as f64).powf(*b as f64)))
            } else {
                Ok(Value::Int(a.pow(*b as u32)))
            }
        }
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.powf(*b))),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).powf(*b))),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.powf(*b as f64))),
        _ => Err("Type error: pow requires numbers".to_string()),
    }
}

pub fn prim_floor(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("floor requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.floor() as i64)),
        _ => Err("Type error: floor requires a number".to_string()),
    }
}

pub fn prim_ceil(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("ceil requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.ceil() as i64)),
        _ => Err("Type error: ceil requires a number".to_string()),
    }
}

pub fn prim_round(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("round requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.round() as i64)),
        _ => Err("Type error: round requires a number".to_string()),
    }
}

pub fn prim_pi(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Float(PI))
}

pub fn prim_e(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Float(E))
}
