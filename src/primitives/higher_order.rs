//! Higher-order function primitives (map, filter, fold)
use crate::error::{LError, LResult};
use crate::value::{list, Value};

/// Apply a function to each element of a list
pub fn prim_map(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err("map requires exactly 2 arguments (function list)"
            .to_string()
            .into());
    }

    if let Some(f) = args[0].as_native_fn() {
        let vec = args[1].list_to_vec()?;
        let results: Result<Vec<Value>, LError> =
            vec.iter().map(|v| f(std::slice::from_ref(v))).collect();
        Ok(list(results?))
    } else if args[0].is_closure() {
        Err(
            "map with closures not yet supported (use native functions or ffi_map)"
                .to_string()
                .into(),
        )
    } else {
        Err("map requires a function as first argument"
            .to_string()
            .into())
    }
}

/// Filter a list using a predicate function
pub fn prim_filter(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err("filter requires exactly 2 arguments (predicate list)"
            .to_string()
            .into());
    }

    if let Some(f) = args[0].as_native_fn() {
        let vec = args[1].list_to_vec()?;
        let mut results = Vec::new();
        for v in vec {
            let result = f(std::slice::from_ref(&v))?;
            if !result.is_nil() && result != Value::FALSE {
                results.push(v);
            }
        }
        Ok(list(results))
    } else if args[0].is_closure() {
        Err(
            "filter with closures not yet supported (use native functions or ffi_filter)"
                .to_string()
                .into(),
        )
    } else {
        Err("filter requires a predicate function as first argument"
            .to_string()
            .into())
    }
}

/// Fold (reduce) a list with an accumulator
pub fn prim_fold(args: &[Value]) -> LResult<Value> {
    if args.len() != 3 {
        return Err("fold requires exactly 3 arguments (function initial list)"
            .to_string()
            .into());
    }

    if let Some(f) = args[0].as_native_fn() {
        let mut accumulator = args[1];
        let vec = args[2].list_to_vec()?;
        for v in vec {
            accumulator = f(&[accumulator, v])?;
        }
        Ok(accumulator)
    } else if args[0].is_closure() {
        Err(
            "fold with closures not yet supported (use native functions or ffi_fold)"
                .to_string()
                .into(),
        )
    } else {
        Err("fold requires a function as first argument"
            .to_string()
            .into())
    }
}
