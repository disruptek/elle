//! Higher-order function primitives (map, filter, fold)
use crate::value::{list, Value};

/// Apply a function to each element of a list
pub fn prim_map(args: &[Value]) -> Result<Value, String> {
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

/// Filter a list using a predicate function
pub fn prim_filter(args: &[Value]) -> Result<Value, String> {
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

/// Fold (reduce) a list with an accumulator
pub fn prim_fold(args: &[Value]) -> Result<Value, String> {
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
