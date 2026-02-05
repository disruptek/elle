//! String manipulation primitives
use crate::value::Value;
use std::rc::Rc;

/// Get the length of a string
pub fn prim_string_length(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("string-length requires exactly 1 argument".to_string());
    }
    match &args[0] {
        Value::String(s) => Ok(Value::Int(s.len() as i64)),
        _ => Err("string-length requires a string".to_string()),
    }
}

/// Append multiple strings
pub fn prim_string_append(args: &[Value]) -> Result<Value, String> {
    let mut result = String::new();
    for arg in args {
        match arg {
            Value::String(s) => result.push_str(s),
            _ => return Err("string-append requires all arguments to be strings".to_string()),
        }
    }
    Ok(Value::String(Rc::from(result)))
}

/// Convert string to uppercase
pub fn prim_string_upcase(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("string-upcase requires exactly 1 argument".to_string());
    }
    match &args[0] {
        Value::String(s) => Ok(Value::String(Rc::from(s.to_uppercase()))),
        _ => Err("string-upcase requires a string".to_string()),
    }
}

/// Convert string to lowercase
pub fn prim_string_downcase(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("string-downcase requires exactly 1 argument".to_string());
    }
    match &args[0] {
        Value::String(s) => Ok(Value::String(Rc::from(s.to_lowercase()))),
        _ => Err("string-downcase requires a string".to_string()),
    }
}

/// Get a substring
pub fn prim_substring(args: &[Value]) -> Result<Value, String> {
    if args.len() < 2 || args.len() > 3 {
        return Err("substring requires 2 or 3 arguments (string, start [, end])".to_string());
    }

    let s = match &args[0] {
        Value::String(s) => s.as_ref(),
        _ => return Err("substring requires a string as first argument".to_string()),
    };

    let start = args[1].as_int()? as usize;

    let end = if args.len() == 3 {
        args[2].as_int()? as usize
    } else {
        s.len()
    };

    if start > s.len() || end > s.len() || start > end {
        return Err("substring indices out of range".to_string());
    }

    Ok(Value::String(Rc::from(&s[start..end])))
}

/// Find the index of a character
pub fn prim_string_index(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("string-index requires exactly 2 arguments (string, char)".to_string());
    }

    let haystack = match &args[0] {
        Value::String(s) => s.as_ref(),
        _ => return Err("string-index requires a string as first argument".to_string()),
    };

    let needle = match &args[1] {
        Value::String(s) => {
            if s.len() != 1 {
                return Err(
                    "string-index requires a single character as second argument".to_string(),
                );
            }
            s.chars().next().unwrap()
        }
        _ => return Err("string-index requires a string as second argument".to_string()),
    };

    match haystack.find(needle) {
        Some(idx) => Ok(Value::Int(idx as i64)),
        None => Ok(Value::Nil),
    }
}

/// Get a character at an index
pub fn prim_char_at(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("char-at requires exactly 2 arguments (string, index)".to_string());
    }

    let s = match &args[0] {
        Value::String(s) => s.as_ref(),
        _ => return Err("char-at requires a string as first argument".to_string()),
    };

    let index = args[1].as_int()? as usize;

    if index >= s.len() {
        return Err("Index out of bounds".to_string());
    }

    let ch = s.chars().nth(index).ok_or("Index out of bounds")?;
    Ok(Value::String(Rc::from(ch.to_string())))
}

/// Convert to integer
pub fn prim_to_int(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("int requires exactly 1 argument".to_string());
    }
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(*f as i64)),
        Value::String(s) => s
            .parse::<i64>()
            .map(Value::Int)
            .map_err(|_| "Cannot parse string as integer".to_string()),
        _ => Err("Cannot convert to integer".to_string()),
    }
}

/// Convert to float
pub fn prim_to_float(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("float requires exactly 1 argument".to_string());
    }
    match &args[0] {
        Value::Int(n) => Ok(Value::Float(*n as f64)),
        Value::Float(f) => Ok(Value::Float(*f)),
        Value::String(s) => s
            .parse::<f64>()
            .map(Value::Float)
            .map_err(|_| "Cannot parse string as float".to_string()),
        _ => Err("Cannot convert to float".to_string()),
    }
}

/// Convert to string
pub fn prim_to_string(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("string requires exactly 1 argument".to_string());
    }
    Ok(Value::String(Rc::from(args[0].to_string())))
}
