use crate::symbol::SymbolTable;
use crate::value::{cons, list, Value};
use crate::vm::VM;
use std::rc::Rc;

pub fn register_primitives(vm: &mut VM, symbols: &mut SymbolTable) {
    // Arithmetic
    register_fn(vm, symbols, "+", prim_add);
    register_fn(vm, symbols, "-", prim_sub);
    register_fn(vm, symbols, "*", prim_mul);
    register_fn(vm, symbols, "/", prim_div);

    // Comparisons
    register_fn(vm, symbols, "=", prim_eq);
    register_fn(vm, symbols, "<", prim_lt);
    register_fn(vm, symbols, ">", prim_gt);
    register_fn(vm, symbols, "<=", prim_le);
    register_fn(vm, symbols, ">=", prim_ge);

    // List operations
    register_fn(vm, symbols, "cons", prim_cons);
    register_fn(vm, symbols, "first", prim_first);
    register_fn(vm, symbols, "rest", prim_rest);
    register_fn(vm, symbols, "list", prim_list);

    // Type predicates
    register_fn(vm, symbols, "nil?", prim_is_nil);
    register_fn(vm, symbols, "pair?", prim_is_pair);
    register_fn(vm, symbols, "number?", prim_is_number);
    register_fn(vm, symbols, "symbol?", prim_is_symbol);
    register_fn(vm, symbols, "string?", prim_is_string);

    // Logic
    register_fn(vm, symbols, "not", prim_not);

    // Display
    register_fn(vm, symbols, "display", prim_display);
    register_fn(vm, symbols, "newline", prim_newline);

    // Additional list operations
    register_fn(vm, symbols, "length", prim_length);
    register_fn(vm, symbols, "append", prim_append);
    register_fn(vm, symbols, "reverse", prim_reverse);

    // Type conversions
    register_fn(vm, symbols, "int", prim_to_int);
    register_fn(vm, symbols, "float", prim_to_float);
    register_fn(vm, symbols, "string", prim_to_string);

    // Min/Max
    register_fn(vm, symbols, "min", prim_min);
    register_fn(vm, symbols, "max", prim_max);

    // Absolute value
    register_fn(vm, symbols, "abs", prim_abs);

    // String operations
    register_fn(vm, symbols, "string-length", prim_string_length);
    register_fn(vm, symbols, "string-append", prim_string_append);
    register_fn(vm, symbols, "string-upcase", prim_string_upcase);
    register_fn(vm, symbols, "string-downcase", prim_string_downcase);
    register_fn(vm, symbols, "substring", prim_substring);
    register_fn(vm, symbols, "string-index", prim_string_index);
    register_fn(vm, symbols, "char-at", prim_char_at);

    // List utilities
    register_fn(vm, symbols, "nth", prim_nth);
    register_fn(vm, symbols, "last", prim_last);
    register_fn(vm, symbols, "take", prim_take);
    register_fn(vm, symbols, "drop", prim_drop);

    // Vector operations
    register_fn(vm, symbols, "vector", prim_vector);
    register_fn(vm, symbols, "vector-length", prim_vector_length);
    register_fn(vm, symbols, "vector-ref", prim_vector_ref);
    register_fn(vm, symbols, "vector-set!", prim_vector_set);

    // Type info
    register_fn(vm, symbols, "type", prim_type);

    // Math functions
    register_fn(vm, symbols, "sqrt", prim_sqrt);
    register_fn(vm, symbols, "sin", prim_sin);
    register_fn(vm, symbols, "cos", prim_cos);
    register_fn(vm, symbols, "tan", prim_tan);
    register_fn(vm, symbols, "log", prim_log);
    register_fn(vm, symbols, "exp", prim_exp);
    register_fn(vm, symbols, "pow", prim_pow);
    register_fn(vm, symbols, "floor", prim_floor);
    register_fn(vm, symbols, "ceil", prim_ceil);
    register_fn(vm, symbols, "round", prim_round);

    // Math constants
    register_fn(vm, symbols, "pi", prim_pi);
    register_fn(vm, symbols, "e", prim_e);

    // Additional utilities
    register_fn(vm, symbols, "mod", prim_mod);
    register_fn(vm, symbols, "remainder", prim_remainder);
    register_fn(vm, symbols, "even?", prim_even);
    register_fn(vm, symbols, "odd?", prim_odd);
}

fn register_fn(
    vm: &mut VM,
    symbols: &mut SymbolTable,
    name: &str,
    func: fn(&[Value]) -> Result<Value, String>,
) {
    let sym_id = symbols.intern(name);
    vm.set_global(sym_id.0, Value::NativeFn(func));
}

// Arithmetic primitives
fn prim_add(args: &[Value]) -> Result<Value, String> {
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

fn prim_sub(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("- requires at least 1 argument".to_string());
    }

    if args.len() == 1 {
        // Negate
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

    let mut result = result;
    let mut float_result = float_result;

    for arg in &args[1..] {
        match arg {
            Value::Int(n) => {
                if is_float {
                    float_result -= *n as f64;
                } else {
                    result -= n;
                }
            }
            Value::Float(f) => {
                if !is_float {
                    is_float = true;
                    float_result = result as f64;
                }
                float_result -= f;
            }
            _ => return Err("Type error: - requires numbers".to_string()),
        }
    }

    if is_float {
        Ok(Value::Float(float_result))
    } else {
        Ok(Value::Int(result))
    }
}

fn prim_mul(args: &[Value]) -> Result<Value, String> {
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

fn prim_div(args: &[Value]) -> Result<Value, String> {
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
        _ => return Err("Type error: / requires numbers".to_string()),
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

// Comparison primitives
fn prim_eq(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("= requires exactly 2 arguments".to_string());
    }
    Ok(Value::Bool(args[0] == args[1]))
}

fn prim_lt(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("< requires exactly 2 arguments".to_string());
    }
    let result = match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => a < b,
        (Value::Float(a), Value::Float(b)) => a < b,
        (Value::Int(a), Value::Float(b)) => (*a as f64) < *b,
        (Value::Float(a), Value::Int(b)) => *a < (*b as f64),
        _ => return Err("Type error: < requires numbers".to_string()),
    };
    Ok(Value::Bool(result))
}

fn prim_gt(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("> requires exactly 2 arguments".to_string());
    }
    let result = match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => a > b,
        (Value::Float(a), Value::Float(b)) => a > b,
        (Value::Int(a), Value::Float(b)) => (*a as f64) > *b,
        (Value::Float(a), Value::Int(b)) => *a > (*b as f64),
        _ => return Err("Type error: > requires numbers".to_string()),
    };
    Ok(Value::Bool(result))
}

fn prim_le(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("<= requires exactly 2 arguments".to_string());
    }
    let result = match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => a <= b,
        (Value::Float(a), Value::Float(b)) => a <= b,
        (Value::Int(a), Value::Float(b)) => (*a as f64) <= *b,
        (Value::Float(a), Value::Int(b)) => *a <= (*b as f64),
        _ => return Err("Type error: <= requires numbers".to_string()),
    };
    Ok(Value::Bool(result))
}

fn prim_ge(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err(">= requires exactly 2 arguments".to_string());
    }
    let result = match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => a >= b,
        (Value::Float(a), Value::Float(b)) => a >= b,
        (Value::Int(a), Value::Float(b)) => (*a as f64) >= *b,
        (Value::Float(a), Value::Int(b)) => *a >= (*b as f64),
        _ => return Err("Type error: >= requires numbers".to_string()),
    };
    Ok(Value::Bool(result))
}

// List primitives
fn prim_cons(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("cons requires exactly 2 arguments".to_string());
    }
    Ok(cons(args[0].clone(), args[1].clone()))
}

fn prim_first(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("first requires exactly 1 argument".to_string());
    }
    let cons = args[0].as_cons()?;
    Ok(cons.first.clone())
}

fn prim_rest(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("rest requires exactly 1 argument".to_string());
    }
    let cons = args[0].as_cons()?;
    Ok(cons.rest.clone())
}

fn prim_list(args: &[Value]) -> Result<Value, String> {
    Ok(list(args.to_vec()))
}

// Type predicates
fn prim_is_nil(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("nil? requires exactly 1 argument".to_string());
    }
    Ok(Value::Bool(args[0].is_nil()))
}

fn prim_is_pair(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("pair? requires exactly 1 argument".to_string());
    }
    Ok(Value::Bool(matches!(args[0], Value::Cons(_))))
}

fn prim_is_number(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("number? requires exactly 1 argument".to_string());
    }
    Ok(Value::Bool(matches!(
        args[0],
        Value::Int(_) | Value::Float(_)
    )))
}

fn prim_is_symbol(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("symbol? requires exactly 1 argument".to_string());
    }
    Ok(Value::Bool(matches!(args[0], Value::Symbol(_))))
}

fn prim_is_string(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("string? requires exactly 1 argument".to_string());
    }
    Ok(Value::Bool(matches!(args[0], Value::String(_))))
}

// Logic primitives
fn prim_not(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("not requires exactly 1 argument".to_string());
    }
    Ok(Value::Bool(!args[0].is_truthy()))
}

// Display primitives
fn prim_display(args: &[Value]) -> Result<Value, String> {
    for arg in args {
        print!("{}", arg);
    }
    Ok(Value::Nil)
}

fn prim_newline(_args: &[Value]) -> Result<Value, String> {
    println!();
    Ok(Value::Nil)
}

// Additional list operations
fn prim_length(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("length requires exactly 1 argument".to_string());
    }
    let vec = args[0].list_to_vec()?;
    Ok(Value::Int(vec.len() as i64))
}

fn prim_append(args: &[Value]) -> Result<Value, String> {
    let mut result = Vec::new();
    for arg in args {
        let vec = arg.list_to_vec()?;
        result.extend(vec);
    }
    Ok(list(result))
}

fn prim_reverse(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("reverse requires exactly 1 argument".to_string());
    }
    let mut vec = args[0].list_to_vec()?;
    vec.reverse();
    Ok(list(vec))
}

// Type conversions
fn prim_to_int(args: &[Value]) -> Result<Value, String> {
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

fn prim_to_float(args: &[Value]) -> Result<Value, String> {
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

fn prim_to_string(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("string requires exactly 1 argument".to_string());
    }
    Ok(Value::String(Rc::from(args[0].to_string())))
}

// Min/Max
fn prim_min(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("min requires at least 1 argument".to_string());
    }

    let mut result = match &args[0] {
        Value::Int(n) => *n as f64,
        Value::Float(f) => *f,
        _ => return Err("min requires numbers".to_string()),
    };
    let mut is_float = matches!(args[0], Value::Float(_));

    for arg in &args[1..] {
        match arg {
            Value::Int(n) => {
                let val = *n as f64;
                if val < result {
                    result = val;
                }
            }
            Value::Float(f) => {
                if !is_float {
                    is_float = true;
                }
                if *f < result {
                    result = *f;
                }
            }
            _ => return Err("min requires numbers".to_string()),
        }
    }

    if is_float {
        Ok(Value::Float(result))
    } else {
        Ok(Value::Int(result as i64))
    }
}

fn prim_max(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("max requires at least 1 argument".to_string());
    }

    let mut result = match &args[0] {
        Value::Int(n) => *n as f64,
        Value::Float(f) => *f,
        _ => return Err("max requires numbers".to_string()),
    };
    let mut is_float = matches!(args[0], Value::Float(_));

    for arg in &args[1..] {
        match arg {
            Value::Int(n) => {
                let val = *n as f64;
                if val > result {
                    result = val;
                }
            }
            Value::Float(f) => {
                if !is_float {
                    is_float = true;
                }
                if *f > result {
                    result = *f;
                }
            }
            _ => return Err("max requires numbers".to_string()),
        }
    }

    if is_float {
        Ok(Value::Float(result))
    } else {
        Ok(Value::Int(result as i64))
    }
}

fn prim_abs(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("abs requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        _ => Err("abs requires a number".to_string()),
    }
}

// String operations
fn prim_string_length(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("string-length requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::String(s) => Ok(Value::Int(s.len() as i64)),
        _ => Err("string-length requires a string".to_string()),
    }
}

fn prim_string_append(args: &[Value]) -> Result<Value, String> {
    let mut result = String::new();
    for arg in args {
        match arg {
            Value::String(s) => result.push_str(s),
            _ => return Err("string-append requires strings".to_string()),
        }
    }
    Ok(Value::String(Rc::from(result)))
}

fn prim_string_upcase(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("string-upcase requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::String(s) => Ok(Value::String(Rc::from(s.to_uppercase()))),
        _ => Err("string-upcase requires a string".to_string()),
    }
}

fn prim_string_downcase(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("string-downcase requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::String(s) => Ok(Value::String(Rc::from(s.to_lowercase()))),
        _ => Err("string-downcase requires a string".to_string()),
    }
}

fn prim_substring(args: &[Value]) -> Result<Value, String> {
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

fn prim_string_index(args: &[Value]) -> Result<Value, String> {
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

fn prim_char_at(args: &[Value]) -> Result<Value, String> {
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

// List utilities
fn prim_nth(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("nth requires exactly 2 arguments (index, list)".to_string());
    }

    let index = args[0].as_int()? as usize;
    let vec = args[1].list_to_vec()?;

    vec.get(index)
        .cloned()
        .ok_or("Index out of bounds".to_string())
}

fn prim_last(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("last requires exactly 1 argument".to_string());
    }

    let vec = args[0].list_to_vec()?;
    vec.last()
        .cloned()
        .ok_or("Cannot get last of empty list".to_string())
}

fn prim_take(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("take requires exactly 2 arguments (count, list)".to_string());
    }

    let count = args[0].as_int()? as usize;
    let vec = args[1].list_to_vec()?;

    let taken: Vec<Value> = vec.into_iter().take(count).collect();
    Ok(list(taken))
}

fn prim_drop(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("drop requires exactly 2 arguments (count, list)".to_string());
    }

    let count = args[0].as_int()? as usize;
    let vec = args[1].list_to_vec()?;

    let dropped: Vec<Value> = vec.into_iter().skip(count).collect();
    Ok(list(dropped))
}

// Type info
fn prim_type(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("type requires exactly 1 argument".to_string());
    }

    let type_name = match &args[0] {
        Value::Nil => "nil",
        Value::Bool(_) => "boolean",
        Value::Int(_) => "integer",
        Value::Float(_) => "float",
        Value::Symbol(_) => "symbol",
        Value::String(_) => "string",
        Value::Cons(_) => "pair",
        Value::Vector(_) => "vector",
        Value::Closure(_) => "closure",
        Value::NativeFn(_) => "native-function",
        Value::LibHandle(_) => "library-handle",
    };

    Ok(Value::String(Rc::from(type_name)))
}

// Math functions
fn prim_sqrt(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("sqrt requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Float((*n as f64).sqrt())),
        Value::Float(f) => Ok(Value::Float(f.sqrt())),
        _ => Err("sqrt requires a number".to_string()),
    }
}

fn prim_sin(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("sin requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Float((*n as f64).sin())),
        Value::Float(f) => Ok(Value::Float(f.sin())),
        _ => Err("sin requires a number".to_string()),
    }
}

fn prim_cos(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("cos requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Float((*n as f64).cos())),
        Value::Float(f) => Ok(Value::Float(f.cos())),
        _ => Err("cos requires a number".to_string()),
    }
}

fn prim_tan(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("tan requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Float((*n as f64).tan())),
        Value::Float(f) => Ok(Value::Float(f.tan())),
        _ => Err("tan requires a number".to_string()),
    }
}

fn prim_log(args: &[Value]) -> Result<Value, String> {
    if args.len() < 1 || args.len() > 2 {
        return Err("log requires 1 or 2 arguments".to_string());
    }

    let value = match &args[0] {
        Value::Int(n) => *n as f64,
        Value::Float(f) => *f,
        _ => return Err("log requires numbers".to_string()),
    };

    if args.len() == 1 {
        // Natural logarithm
        Ok(Value::Float(value.ln()))
    } else {
        // Logarithm with specified base
        let base = match &args[1] {
            Value::Int(n) => *n as f64,
            Value::Float(f) => *f,
            _ => return Err("log requires numbers".to_string()),
        };
        Ok(Value::Float(value.log(base)))
    }
}

fn prim_exp(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("exp requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Float((*n as f64).exp())),
        Value::Float(f) => Ok(Value::Float(f.exp())),
        _ => Err("exp requires a number".to_string()),
    }
}

fn prim_pow(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("pow requires exactly 2 arguments".to_string());
    }

    let base = match &args[0] {
        Value::Int(n) => *n as f64,
        Value::Float(f) => *f,
        _ => return Err("pow requires numbers".to_string()),
    };

    let exp = match &args[1] {
        Value::Int(n) => *n as f64,
        Value::Float(f) => *f,
        _ => return Err("pow requires numbers".to_string()),
    };

    let result = base.powf(exp);

    // Return int if both args are ints and result is whole number
    if matches!(&args[0], Value::Int(_))
        && matches!(&args[1], Value::Int(_))
        && result.fract() == 0.0
        && result.is_finite()
    {
        Ok(Value::Int(result as i64))
    } else {
        Ok(Value::Float(result))
    }
}

fn prim_floor(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("floor requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.floor() as i64)),
        _ => Err("floor requires a number".to_string()),
    }
}

fn prim_ceil(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("ceil requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.ceil() as i64)),
        _ => Err("ceil requires a number".to_string()),
    }
}

fn prim_round(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("round requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.round() as i64)),
        _ => Err("round requires a number".to_string()),
    }
}

// Vector operations
fn prim_vector(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Vector(Rc::new(args.to_vec())))
}

fn prim_vector_length(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("vector-length requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Vector(v) => Ok(Value::Int(v.len() as i64)),
        _ => Err("vector-length requires a vector".to_string()),
    }
}

fn prim_vector_ref(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("vector-ref requires exactly 2 arguments (vector, index)".to_string());
    }

    let vec = args[0].as_vector()?;
    let index = args[1].as_int()? as usize;

    vec.get(index)
        .cloned()
        .ok_or("Vector index out of bounds".to_string())
}

fn prim_vector_set(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("vector-set! requires exactly 3 arguments (vector, index, value)".to_string());
    }

    let mut vec = args[0].as_vector()?.as_ref().clone();
    let index = args[1].as_int()? as usize;
    let value = args[2].clone();

    if index >= vec.len() {
        return Err("Vector index out of bounds".to_string());
    }

    vec[index] = value;
    Ok(Value::Vector(Rc::new(vec)))
}

// Math constants
fn prim_pi(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Float(std::f64::consts::PI))
}

fn prim_e(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Float(std::f64::consts::E))
}

// Modulo and remainder
fn prim_mod(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("mod requires exactly 2 arguments".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => {
            if *b == 0 {
                return Err("Division by zero".to_string());
            }
            // Lisp mod: result has same sign as divisor
            let rem = a % b;
            if rem == 0 {
                Ok(Value::Int(0))
            } else if (rem > 0) != (*b > 0) {
                Ok(Value::Int(rem + b))
            } else {
                Ok(Value::Int(rem))
            }
        }
        _ => Err("mod requires integers".to_string()),
    }
}

fn prim_remainder(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("remainder requires exactly 2 arguments".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => {
            if *b == 0 {
                return Err("Division by zero".to_string());
            }
            let rem = a % b;
            // Adjust remainder to have same sign as dividend
            if (rem > 0 && *b < 0) || (rem < 0 && *b > 0) {
                Ok(Value::Int(rem + b))
            } else {
                Ok(Value::Int(rem))
            }
        }
        _ => Err("remainder requires integers".to_string()),
    }
}

// Predicates
fn prim_even(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("even? requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Bool(n % 2 == 0)),
        _ => Err("even? requires an integer".to_string()),
    }
}

fn prim_odd(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("odd? requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Bool(n % 2 != 0)),
        _ => Err("odd? requires an integer".to_string()),
    }
}
