use crate::symbol::SymbolTable;
use crate::value::{cons, list, Value};
use crate::vm::VM;

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
