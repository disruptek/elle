use crate::value::Value;

pub fn prim_add(args: &[Value]) -> Result<Value, String> {
    let mut result = 0i64;
    let mut is_float = false;
    let mut float_result = 0.0;

    for arg in args {
        match arg {
            Value::Int(n) => {
                if is_float {
                    float_result += *n as f64;
                } else {
                    result += n;
                }
            }
            Value::Float(f) => {
                if !is_float {
                    is_float = true;
                    float_result = result as f64;
                }
                float_result += f;
            }
            _ => return Err("Type error: + requires numbers".to_string()),
        }
    }

    if is_float {
        Ok(Value::Float(float_result))
    } else {
        Ok(Value::Int(result))
    }
}

pub fn prim_sub(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("- requires at least 1 argument".to_string());
    }

    if args.len() == 1 {
        return match &args[0] {
            Value::Int(n) => Ok(Value::Int(-n)),
            Value::Float(f) => Ok(Value::Float(-f)),
            _ => Err("Type error: - requires numbers".to_string()),
        };
    }

    let mut is_float = false;
    let (result, float_result) = match &args[0] {
        Value::Int(n) => (*n, 0.0),
        Value::Float(f) => {
            is_float = true;
            (0, *f)
        }
        _ => return Err("Type error: - requires numbers".to_string()),
    };

    let mut current_result = result;
    let mut current_float = float_result;

    for arg in &args[1..] {
        match arg {
            Value::Int(n) => {
                if is_float {
                    current_float -= *n as f64;
                } else {
                    current_result -= n;
                }
            }
            Value::Float(f) => {
                if !is_float {
                    is_float = true;
                    current_float = current_result as f64;
                }
                current_float -= f;
            }
            _ => return Err("Type error: - requires numbers".to_string()),
        }
    }

    if is_float {
        Ok(Value::Float(current_float))
    } else {
        Ok(Value::Int(current_result))
    }
}

pub fn prim_mul(args: &[Value]) -> Result<Value, String> {
    let mut result = 1i64;
    let mut is_float = false;
    let mut float_result = 1.0;

    for arg in args {
        match arg {
            Value::Int(n) => {
                if is_float {
                    float_result *= *n as f64;
                } else {
                    result *= n;
                }
            }
            Value::Float(f) => {
                if !is_float {
                    is_float = true;
                    float_result = result as f64;
                }
                float_result *= f;
            }
            _ => return Err("Type error: * requires numbers".to_string()),
        }
    }

    if is_float {
        Ok(Value::Float(float_result))
    } else {
        Ok(Value::Int(result))
    }
}

pub fn prim_div(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("/ requires at least 1 argument".to_string());
    }

    if args.len() == 1 {
        // Reciprocal
        return match &args[0] {
            Value::Int(n) => {
                if *n == 0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Float(1.0 / (*n as f64)))
                }
            }
            Value::Float(f) => Ok(Value::Float(1.0 / f)),
            _ => Err("Type error: / requires numbers".to_string()),
        };
    }

    let mut is_float = false;
    let (result, float_result) = match &args[0] {
        Value::Int(n) => (*n, 0.0),
        Value::Float(f) => {
            is_float = true;
            (0, *f)
        }
        _ => Err("Type error: / requires numbers".to_string())?,
    };

    let mut result = result;
    let mut float_result = float_result;

    for arg in &args[1..] {
        match arg {
            Value::Int(n) => {
                if *n == 0 {
                    return Err("Division by zero".to_string());
                }
                if is_float {
                    float_result /= *n as f64;
                } else {
                    result /= n;
                }
            }
            Value::Float(f) => {
                if !is_float {
                    is_float = true;
                    float_result = result as f64;
                }
                float_result /= f;
            }
            _ => return Err("Type error: / requires numbers".to_string()),
        }
    }

    if is_float {
        Ok(Value::Float(float_result))
    } else {
        Ok(Value::Int(result))
    }
}

pub fn prim_mod(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("mod requires exactly 2 arguments".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => {
            if *b == 0 {
                return Err("Modulo by zero".to_string());
            }
            Ok(Value::Int(a % b))
        }
        _ => Err("Type error: mod requires integers".to_string()),
    }
}

pub fn prim_remainder(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("remainder requires exactly 2 arguments".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => {
            if *b == 0 {
                return Err("Remainder by zero".to_string());
            }
            Ok(Value::Int(a.rem_euclid(*b)))
        }
        _ => Err("Type error: remainder requires integers".to_string()),
    }
}

pub fn prim_abs(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("abs requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        _ => Err("Type error: abs requires a number".to_string()),
    }
}

pub fn prim_min(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("min requires at least 1 argument".to_string());
    }

    let mut min = &args[0];
    for arg in &args[1..] {
        let cmp = match (min, arg) {
            (Value::Int(a), Value::Int(b)) => a > b,
            (Value::Int(a), Value::Float(b)) => *a as f64 > *b,
            (Value::Float(a), Value::Int(b)) => *a > *b as f64,
            (Value::Float(a), Value::Float(b)) => a > b,
            _ => return Err("Type error: min requires numbers".to_string()),
        };
        if cmp {
            min = arg;
        }
    }
    Ok(min.clone())
}

pub fn prim_max(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("max requires at least 1 argument".to_string());
    }

    let mut max = &args[0];
    for arg in &args[1..] {
        let cmp = match (max, arg) {
            (Value::Int(a), Value::Int(b)) => a < b,
            (Value::Int(a), Value::Float(b)) => (*a as f64) < *b,
            (Value::Float(a), Value::Int(b)) => *a < *b as f64,
            (Value::Float(a), Value::Float(b)) => a < b,
            _ => return Err("Type error: max requires numbers".to_string()),
        };
        if cmp {
            max = arg;
        }
    }
    Ok(max.clone())
}

pub fn prim_even(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("even? requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Bool(n % 2 == 0)),
        _ => Err("Type error: even? requires an integer".to_string()),
    }
}

pub fn prim_odd(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("odd? requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Bool(n % 2 != 0)),
        _ => Err("Type error: odd? requires an integer".to_string()),
    }
}
