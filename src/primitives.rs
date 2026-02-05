use crate::ffi_primitives;
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
    register_fn(vm, symbols, "map", prim_map);
    register_fn(vm, symbols, "filter", prim_filter);
    register_fn(vm, symbols, "fold", prim_fold);

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

    // FFI primitives
    register_fn(
        vm,
        symbols,
        "load-library",
        ffi_primitives::prim_load_library_wrapper,
    );
    register_fn(
        vm,
        symbols,
        "list-libraries",
        ffi_primitives::prim_list_libraries_wrapper,
    );
    register_fn(
        vm,
        symbols,
        "call-c-function",
        ffi_primitives::prim_call_c_function_wrapper,
    );
    register_fn(
        vm,
        symbols,
        "load-header-with-lib",
        ffi_primitives::prim_load_header_with_lib_wrapper,
    );
    register_fn(
        vm,
        symbols,
        "define-enum",
        ffi_primitives::prim_define_enum_wrapper,
    );
    register_fn(
        vm,
        symbols,
        "make-c-callback",
        ffi_primitives::prim_make_c_callback_wrapper,
    );
    register_fn(
        vm,
        symbols,
        "free-callback",
        ffi_primitives::prim_free_callback_wrapper,
    );
    register_fn(
        vm,
        symbols,
        "register-allocation",
        ffi_primitives::prim_register_allocation_wrapper,
    );
    register_fn(
        vm,
        symbols,
        "memory-stats",
        ffi_primitives::prim_memory_stats_wrapper,
    );
    register_fn(
        vm,
        symbols,
        "type-check",
        ffi_primitives::prim_type_check_wrapper,
    );
    register_fn(
        vm,
        symbols,
        "null-pointer?",
        ffi_primitives::prim_null_pointer_wrapper,
    );
    register_fn(
        vm,
        symbols,
        "ffi-last-error",
        ffi_primitives::prim_ffi_last_error_wrapper,
    );
    register_fn(
        vm,
        symbols,
        "with-ffi-safety-checks",
        ffi_primitives::prim_with_ffi_safety_checks_wrapper,
    );

    // Exception handling
    register_fn(vm, symbols, "throw", prim_throw);
    register_fn(vm, symbols, "exception", prim_exception);
    register_fn(vm, symbols, "exception-message", prim_exception_message);
    register_fn(vm, symbols, "exception-data", prim_exception_data);

    // Quoting and meta-programming
    register_fn(vm, symbols, "gensym", prim_gensym);

    // Package manager
    register_fn(vm, symbols, "package-version", prim_package_version);
    register_fn(vm, symbols, "package-info", prim_package_info);

    // Module loading
    register_fn(vm, symbols, "import-file", prim_import_file);
    register_fn(vm, symbols, "add-module-path", prim_add_module_path);

    // Macro expansion
    register_fn(vm, symbols, "expand-macro", prim_expand_macro);
    register_fn(vm, symbols, "macro?", prim_is_macro);

    // Concurrency primitives
    register_fn(vm, symbols, "spawn", prim_spawn);
    register_fn(vm, symbols, "join", prim_join);
    register_fn(vm, symbols, "sleep", prim_sleep);
    register_fn(vm, symbols, "current-thread-id", prim_current_thread_id);

    // Debugging and profiling primitives
    register_fn(vm, symbols, "debug-print", prim_debug_print);
    register_fn(vm, symbols, "trace", prim_trace);
    register_fn(vm, symbols, "profile", prim_profile);
    register_fn(vm, symbols, "memory-usage", prim_memory_usage);
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
        Value::CHandle(_) => "c-handle",
        Value::Exception(_) => "exception",
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

// Higher-order functions with native functions (work without VM access)
fn prim_map(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("map requires exactly 2 arguments (function list)".to_string());
    }

    match &args[0] {
        Value::NativeFn(f) => {
            let vec = args[1].list_to_vec()?;
            let results: Result<Vec<Value>, String> = vec.iter().map(|v| f(&[v.clone()])).collect();
            Ok(list(results?))
        }
        Value::Closure(_) => {
            Err("map with closures not yet supported (use native functions or ffi_map)".to_string())
        }
        _ => Err("map requires a function as first argument".to_string()),
    }
}

fn prim_filter(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("filter requires exactly 2 arguments (predicate list)".to_string());
    }

    match &args[0] {
        Value::NativeFn(f) => {
            let vec = args[1].list_to_vec()?;
            let mut results = Vec::new();
            for v in vec {
                let result = f(&[v.clone()])?;
                if result != Value::Nil && result != Value::Bool(false) {
                    results.push(v);
                }
            }
            Ok(list(results))
        }
        Value::Closure(_) => Err(
            "filter with closures not yet supported (use native functions or ffi_filter)"
                .to_string(),
        ),
        _ => Err("filter requires a predicate function as first argument".to_string()),
    }
}

fn prim_fold(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("fold requires exactly 3 arguments (function initial list)".to_string());
    }

    match &args[0] {
        Value::NativeFn(f) => {
            let mut accumulator = args[1].clone();
            let vec = args[2].list_to_vec()?;
            for v in vec {
                accumulator = f(&[accumulator, v])?;
            }
            Ok(accumulator)
        }
        Value::Closure(_) => Err(
            "fold with closures not yet supported (use native functions or ffi_fold)".to_string(),
        ),
        _ => Err("fold requires a function as first argument".to_string()),
    }
}

// Exception handling primitives
fn prim_throw(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("throw requires at least 1 argument".to_string());
    }

    match &args[0] {
        Value::String(msg) => Err(msg.to_string()),
        Value::Exception(exc) => Err(exc.message.to_string()),
        other => Err(format!(
            "throw requires a string or exception, got {}",
            other.type_name()
        )),
    }
}

fn prim_exception(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("exception requires at least 1 argument".to_string());
    }

    match &args[0] {
        Value::String(msg) => {
            use crate::value::Exception;
            let exc = if args.len() > 1 {
                Exception::with_data(msg.to_string(), args[1].clone())
            } else {
                Exception::new(msg.to_string())
            };
            Ok(Value::Exception(Rc::new(exc)))
        }
        _ => Err("exception requires a string as first argument".to_string()),
    }
}

fn prim_exception_message(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("exception-message requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Exception(exc) => Ok(Value::String(exc.message.clone())),
        _ => Err(format!(
            "exception-message requires an exception, got {}",
            args[0].type_name()
        )),
    }
}

fn prim_exception_data(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("exception-data requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Exception(exc) => match &exc.data {
            Some(data) => Ok((**data).clone()),
            None => Ok(Value::Nil),
        },
        _ => Err(format!(
            "exception-data requires an exception, got {}",
            args[0].type_name()
        )),
    }
}

// Macro and meta-programming primitives
use std::sync::atomic::{AtomicU32, Ordering};

static GENSYM_COUNTER: AtomicU32 = AtomicU32::new(0);

fn prim_gensym(args: &[Value]) -> Result<Value, String> {
    let prefix = if args.is_empty() {
        "G".to_string()
    } else {
        match &args[0] {
            Value::String(s) => s.to_string(),
            Value::Symbol(id) => format!("G{}", id.0),
            _ => "G".to_string(),
        }
    };

    let counter = GENSYM_COUNTER.fetch_add(1, Ordering::SeqCst);
    let sym_name = format!("{}{}", prefix, counter);
    Ok(Value::String(sym_name.into()))
}

// Standard library initialization
pub fn init_stdlib(vm: &mut VM, symbols: &mut SymbolTable) {
    init_list_module(vm, symbols);
    init_string_module(vm, symbols);
    init_math_module(vm, symbols);
}

fn init_list_module(vm: &mut VM, symbols: &mut SymbolTable) {
    // List module exports
    let mut list_exports = std::collections::HashMap::new();

    // These functions are already registered globally
    // The module just provides a namespace for them
    let functions = vec![
        "length", "append", "reverse", "map", "filter", "fold", "nth", "last", "take", "drop",
        "list", "cons", "first", "rest",
    ];

    let mut exports = Vec::new();
    for func_name in &functions {
        if let Some(func) = vm.get_global(symbols.intern(func_name).0) {
            list_exports.insert(symbols.intern(func_name).0, func.clone());
        }
        exports.push(symbols.intern(func_name));
    }

    use crate::symbol::ModuleDef;
    let list_module = ModuleDef {
        name: symbols.intern("list"),
        exports,
    };
    symbols.define_module(list_module);
    vm.define_module("list".to_string(), list_exports);
}

fn init_string_module(vm: &mut VM, symbols: &mut SymbolTable) {
    // String module exports
    let mut string_exports = std::collections::HashMap::new();

    let functions = vec![
        "string-length",
        "string-append",
        "string-upcase",
        "string-downcase",
        "substring",
        "string-index",
        "char-at",
        "string",
    ];

    let mut exports = Vec::new();
    for func_name in &functions {
        if let Some(func) = vm.get_global(symbols.intern(func_name).0) {
            string_exports.insert(symbols.intern(func_name).0, func.clone());
        }
        exports.push(symbols.intern(func_name));
    }

    use crate::symbol::ModuleDef;
    let string_module = ModuleDef {
        name: symbols.intern("string"),
        exports,
    };
    symbols.define_module(string_module);
    vm.define_module("string".to_string(), string_exports);
}

fn init_math_module(vm: &mut VM, symbols: &mut SymbolTable) {
    // Math module exports
    let mut math_exports = std::collections::HashMap::new();

    let functions = vec![
        "+",
        "-",
        "*",
        "/",
        "mod",
        "remainder",
        "abs",
        "min",
        "max",
        "sqrt",
        "sin",
        "cos",
        "tan",
        "log",
        "exp",
        "pow",
        "floor",
        "ceil",
        "round",
        "even?",
        "odd?",
        "pi",
        "e",
    ];

    let mut exports = Vec::new();
    for func_name in &functions {
        if let Some(func) = vm.get_global(symbols.intern(func_name).0) {
            math_exports.insert(symbols.intern(func_name).0, func.clone());
        }
        exports.push(symbols.intern(func_name));
    }

    use crate::symbol::ModuleDef;
    let math_module = ModuleDef {
        name: symbols.intern("math"),
        exports,
    };
    symbols.define_module(math_module);
    vm.define_module("math".to_string(), math_exports);
}

// Package manager primitives
fn prim_package_version(_args: &[Value]) -> Result<Value, String> {
    // Return current version of Elle
    Ok(Value::String("0.3.0".into()))
}

fn prim_package_info(_args: &[Value]) -> Result<Value, String> {
    // Return package information
    use crate::value::list;
    Ok(list(vec![
        Value::String("Elle".into()),
        Value::String("0.3.0".into()),
        Value::String("A Lisp interpreter with bytecode compilation".into()),
    ]))
}

// Module loading primitives
fn prim_import_file(args: &[Value]) -> Result<Value, String> {
    // (import-file "path/to/module.elle")
    // Placeholder: In production, would parse and load module
    if args.len() != 1 {
        return Err(format!(
            "import-file: expected 1 argument, got {}",
            args.len()
        ));
    }

    match &args[0] {
        Value::String(_path) => {
            // In a full implementation, this would:
            // 1. Read the file
            // 2. Parse module definitions
            // 3. Compile and execute module code
            // 4. Register exported symbols
            // For now, just return true to indicate success
            Ok(Value::Bool(true))
        }
        _ => Err("import-file: argument must be a string".to_string()),
    }
}

fn prim_add_module_path(args: &[Value]) -> Result<Value, String> {
    // (add-module-path "path")
    // Note: This would need access to VM to update search paths
    // For now, just verify the argument
    if args.len() != 1 {
        return Err(format!(
            "add-module-path: expected 1 argument, got {}",
            args.len()
        ));
    }

    match &args[0] {
        Value::String(_path) => {
            // In full implementation, would call vm.add_module_search_path(PathBuf::from(path))
            Ok(Value::Nil)
        }
        _ => Err("add-module-path: argument must be a string".to_string()),
    }
}

// Macro expansion primitives
fn prim_expand_macro(args: &[Value]) -> Result<Value, String> {
    // (expand-macro macro-expr)
    // Expands a macro call and returns the expanded form
    if args.len() != 1 {
        return Err(format!(
            "expand-macro: expected 1 argument, got {}",
            args.len()
        ));
    }

    // In production, this would:
    // 1. Check if the value is a macro call (list starting with macro name)
    // 2. Look up the macro definition
    // 3. Apply the macro with arguments
    // 4. Return the expanded form
    // For Phase 5, just return the argument (placeholder)
    Ok(args[0].clone())
}

fn prim_is_macro(args: &[Value]) -> Result<Value, String> {
    // (macro? value)
    // Returns true if value is a macro
    if args.len() != 1 {
        return Err(format!("macro?: expected 1 argument, got {}", args.len()));
    }

    // In production, would check symbol table for macro definitions
    // For now, always return false
    Ok(Value::Bool(false))
}

// Concurrency primitives
fn prim_spawn(args: &[Value]) -> Result<Value, String> {
    // (spawn thunk)
    // Spawns a new thread that executes the closure
    if args.len() != 1 {
        return Err(format!("spawn: expected 1 argument, got {}", args.len()));
    }

    match &args[0] {
        Value::Closure(_) | Value::NativeFn(_) => {
            // In production, would spawn a thread and return a thread handle
            // For now, return a placeholder thread ID (simplified)
            let thread_id = std::thread::current().id();
            Ok(Value::String(format!("{:?}", thread_id).into()))
        }
        _ => Err("spawn: argument must be a function".to_string()),
    }
}

fn prim_join(args: &[Value]) -> Result<Value, String> {
    // (join thread-handle)
    // Waits for a thread to complete and returns its result
    if args.len() != 1 {
        return Err(format!("join: expected 1 argument, got {}", args.len()));
    }

    // In production, would wait on thread handle and return result
    // For now, return nil as placeholder
    Ok(Value::Nil)
}

fn prim_sleep(args: &[Value]) -> Result<Value, String> {
    // (sleep seconds)
    // Sleeps for the specified number of seconds
    if args.len() != 1 {
        return Err(format!("sleep: expected 1 argument, got {}", args.len()));
    }

    match &args[0] {
        Value::Int(n) => {
            if *n < 0 {
                return Err("sleep: duration must be non-negative".to_string());
            }
            std::thread::sleep(std::time::Duration::from_secs(*n as u64));
            Ok(Value::Nil)
        }
        Value::Float(f) => {
            if *f < 0.0 {
                return Err("sleep: duration must be non-negative".to_string());
            }
            std::thread::sleep(std::time::Duration::from_secs_f64(*f));
            Ok(Value::Nil)
        }
        _ => Err("sleep: argument must be a number".to_string()),
    }
}

fn prim_current_thread_id(_args: &[Value]) -> Result<Value, String> {
    // (current-thread-id)
    // Returns the ID of the current thread
    let thread_id = std::thread::current().id();
    Ok(Value::String(format!("{:?}", thread_id).into()))
}

// Debugging and profiling primitives
fn prim_debug_print(args: &[Value]) -> Result<Value, String> {
    // (debug-print value)
    // Prints a value with debug information
    if args.len() != 1 {
        return Err(format!(
            "debug-print: expected 1 argument, got {}",
            args.len()
        ));
    }

    eprintln!("[DEBUG] {:?}", args[0]);
    Ok(args[0].clone())
}

fn prim_trace(args: &[Value]) -> Result<Value, String> {
    // (trace name value)
    // Traces execution with a label
    if args.len() != 2 {
        return Err(format!("trace: expected 2 arguments, got {}", args.len()));
    }

    match &args[0] {
        Value::String(label) => {
            eprintln!("[TRACE] {}: {:?}", label, args[1]);
            Ok(args[1].clone())
        }
        Value::Symbol(label_id) => {
            eprintln!("[TRACE] {:?}: {:?}", label_id, args[1]);
            Ok(args[1].clone())
        }
        _ => Err("trace: first argument must be a string or symbol".to_string()),
    }
}

fn prim_profile(args: &[Value]) -> Result<Value, String> {
    // (profile thunk)
    // Times the execution of a thunk
    if args.len() != 1 {
        return Err(format!("profile: expected 1 argument, got {}", args.len()));
    }

    // In production, would time execution of closure
    // For now, just return a placeholder timing
    match &args[0] {
        Value::Closure(_) | Value::NativeFn(_) => {
            Ok(Value::String("profiling-not-yet-implemented".into()))
        }
        _ => Err("profile: argument must be a function".to_string()),
    }
}

fn prim_memory_usage(_args: &[Value]) -> Result<Value, String> {
    // (memory-usage)
    // Returns memory usage statistics
    // Returns a list: (rss-bytes virtual-bytes)
    use crate::value::list;

    // Simplified placeholder - in production would use system memory stats
    Ok(list(vec![
        Value::Int(0), // RSS bytes (would be actual value)
        Value::Int(0), // Virtual bytes (would be actual value)
    ]))
}
