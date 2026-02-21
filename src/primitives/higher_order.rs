//! Higher-order function primitives (map, filter, fold)
use crate::value::fiber::{SignalBits, SIG_ERROR, SIG_OK};
use crate::value::{list, Condition, Value};

/// Apply a function to each element of a list
pub fn prim_map(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 2 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(
                "map: expected 2 arguments, got ".to_string() + &args.len().to_string(),
            )),
        );
    }

    if let Some(f) = args[0].as_native_fn() {
        let vec = match args[1].list_to_vec() {
            Ok(v) => v,
            Err(e) => {
                return (
                    SIG_ERROR,
                    Value::condition(Condition::type_error(format!("map: {}", e))),
                );
            }
        };

        let mut results = Vec::new();
        for v in vec {
            let (sig, val) = f(std::slice::from_ref(&v));
            if sig != SIG_OK {
                return (sig, val);
            }
            results.push(val);
        }
        (SIG_OK, list(results))
    } else if args[0].is_closure() {
        (
            SIG_ERROR,
            Value::condition(Condition::error(
                "map with closures not yet supported (use native functions or ffi_map)".to_string(),
            )),
        )
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(
                "map: first argument must be a function".to_string(),
            )),
        )
    }
}

/// Filter a list using a predicate function
pub fn prim_filter(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 2 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(
                "filter: expected 2 arguments, got ".to_string() + &args.len().to_string(),
            )),
        );
    }

    if let Some(f) = args[0].as_native_fn() {
        let vec = match args[1].list_to_vec() {
            Ok(v) => v,
            Err(e) => {
                return (
                    SIG_ERROR,
                    Value::condition(Condition::type_error(format!("filter: {}", e))),
                );
            }
        };

        let mut results = Vec::new();
        for v in vec {
            let (sig, result) = f(std::slice::from_ref(&v));
            if sig != SIG_OK {
                return (sig, result);
            }
            if !result.is_nil() && result != Value::FALSE {
                results.push(v);
            }
        }
        (SIG_OK, list(results))
    } else if args[0].is_closure() {
        (
            SIG_ERROR,
            Value::condition(Condition::error(
                "filter with closures not yet supported (use native functions or ffi_filter)"
                    .to_string(),
            )),
        )
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(
                "filter: first argument must be a predicate function".to_string(),
            )),
        )
    }
}

/// Fold (reduce) a list with an accumulator
pub fn prim_fold(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 3 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(
                "fold: expected 3 arguments, got ".to_string() + &args.len().to_string(),
            )),
        );
    }

    if let Some(f) = args[0].as_native_fn() {
        let mut accumulator = args[1];
        let vec = match args[2].list_to_vec() {
            Ok(v) => v,
            Err(e) => {
                return (
                    SIG_ERROR,
                    Value::condition(Condition::type_error(format!("fold: {}", e))),
                );
            }
        };

        for v in vec {
            let (sig, result) = f(&[accumulator, v]);
            if sig != SIG_OK {
                return (sig, result);
            }
            accumulator = result;
        }
        (SIG_OK, accumulator)
    } else if args[0].is_closure() {
        (
            SIG_ERROR,
            Value::condition(Condition::error(
                "fold with closures not yet supported (use native functions or ffi_fold)"
                    .to_string(),
            )),
        )
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(
                "fold: first argument must be a function".to_string(),
            )),
        )
    }
}
