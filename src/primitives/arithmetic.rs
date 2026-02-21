use crate::arithmetic;
use crate::value::fiber::{SignalBits, SIG_ERROR, SIG_OK};
use crate::value::{Condition, Value};

/// Variadic addition: (+ 1 2 3) -> 6, (+) -> 0
pub fn prim_add(args: &[Value]) -> (SignalBits, Value) {
    // Check that all args are numbers first
    for arg in args {
        if !arg.is_number() {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(format!(
                    "+: expected number, got {}",
                    arg.type_name()
                ))),
            );
        }
    }

    if args.is_empty() {
        return (SIG_OK, Value::int(0)); // Identity element for addition
    }

    let mut result = args[0];
    for arg in &args[1..] {
        match arithmetic::add_values(&result, arg) {
            Ok(val) => result = val,
            Err(e) => return (SIG_ERROR, Value::condition(Condition::error(e))),
        }
    }
    (SIG_OK, result)
}

/// Variadic subtraction: (- 10 3 2) -> 5, (- 5) -> -5
pub fn prim_sub(args: &[Value]) -> (SignalBits, Value) {
    if args.is_empty() {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(
                "-: expected at least 1 argument, got 0".to_string(),
            )),
        );
    }

    if args.len() == 1 {
        return match arithmetic::negate_value(&args[0]) {
            Ok(val) => (SIG_OK, val),
            Err(e) => (SIG_ERROR, Value::condition(Condition::error(e))),
        };
    }

    let mut result = args[0];
    for arg in &args[1..] {
        match arithmetic::sub_values(&result, arg) {
            Ok(val) => result = val,
            Err(e) => return (SIG_ERROR, Value::condition(Condition::error(e))),
        }
    }
    (SIG_OK, result)
}

/// Variadic multiplication: (* 2 3 4) -> 24, (*) -> 1
pub fn prim_mul(args: &[Value]) -> (SignalBits, Value) {
    // Check that all args are numbers first
    for arg in args {
        if !arg.is_number() {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(format!(
                    "*: expected number, got {}",
                    arg.type_name()
                ))),
            );
        }
    }

    if args.is_empty() {
        return (SIG_OK, Value::int(1)); // Identity element for multiplication
    }

    let mut result = args[0];
    for arg in &args[1..] {
        match arithmetic::mul_values(&result, arg) {
            Ok(val) => result = val,
            Err(e) => return (SIG_ERROR, Value::condition(Condition::error(e))),
        }
    }
    (SIG_OK, result)
}

/// Variadic division: (/ 24 2 3) -> 4, (/ 5) -> 1/5
pub fn prim_div(args: &[Value]) -> (SignalBits, Value) {
    if args.is_empty() {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(
                "/: expected at least 1 argument, got 0".to_string(),
            )),
        );
    }

    if args.len() == 1 {
        return match arithmetic::reciprocal_value(&args[0]) {
            Ok(val) => (SIG_OK, val),
            Err(e) => (SIG_ERROR, Value::condition(Condition::error(e))),
        };
    }

    let mut result = args[0];
    for arg in &args[1..] {
        match arithmetic::div_values(&result, arg) {
            Ok(val) => result = val,
            Err(e) => return (SIG_ERROR, Value::condition(Condition::error(e))),
        }
    }
    (SIG_OK, result)
}

pub fn prim_mod(args: &[Value]) -> (SignalBits, Value) {
    // Euclidean modulo: result always has same sign as divisor (b)
    // Example: (mod -17 5) => 3 (because -17 = -4*5 + 3)
    if args.len() != 2 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "mod: expected 2 arguments, got {}",
                args.len()
            ))),
        );
    }
    match arithmetic::mod_values(&args[0], &args[1]) {
        Ok(val) => (SIG_OK, val),
        Err(e) => (SIG_ERROR, Value::condition(Condition::error(e))),
    }
}

pub fn prim_rem(args: &[Value]) -> (SignalBits, Value) {
    // Truncated division remainder: result has same sign as dividend (a)
    // Example: (rem -17 5) => -2 (because -17 = -3*5 + -2)
    if args.len() != 2 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "rem: expected 2 arguments, got {}",
                args.len()
            ))),
        );
    }
    match arithmetic::remainder_values(&args[0], &args[1]) {
        Ok(val) => (SIG_OK, val),
        Err(e) => (SIG_ERROR, Value::condition(Condition::error(e))),
    }
}

pub fn prim_abs(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "abs: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }
    match arithmetic::abs_value(&args[0]) {
        Ok(val) => (SIG_OK, val),
        Err(e) => (SIG_ERROR, Value::condition(Condition::error(e))),
    }
}

pub fn prim_min(args: &[Value]) -> (SignalBits, Value) {
    if args.is_empty() {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(
                "min: expected at least 1 argument, got 0".to_string(),
            )),
        );
    }

    let mut min = args[0];
    for arg in &args[1..] {
        // Check if arg is a number
        if !arg.is_number() {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(format!(
                    "min: expected number, got {}",
                    arg.type_name()
                ))),
            );
        }
        min = arithmetic::min_values(&min, arg);
    }
    (SIG_OK, min)
}

pub fn prim_max(args: &[Value]) -> (SignalBits, Value) {
    if args.is_empty() {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(
                "max: expected at least 1 argument, got 0".to_string(),
            )),
        );
    }

    let mut max = args[0];
    for arg in &args[1..] {
        // Check if arg is a number
        if !arg.is_number() {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(format!(
                    "max: expected number, got {}",
                    arg.type_name()
                ))),
            );
        }
        max = arithmetic::max_values(&max, arg);
    }
    (SIG_OK, max)
}

pub fn prim_even(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "even?: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    match args[0].as_int() {
        Some(n) => (SIG_OK, Value::bool(n % 2 == 0)),
        _ => (
            SIG_ERROR,
            Value::condition(Condition::type_error(format!(
                "even?: expected integer, got {}",
                args[0].type_name()
            ))),
        ),
    }
}

pub fn prim_odd(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "odd?: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    match args[0].as_int() {
        Some(n) => (SIG_OK, Value::bool(n % 2 != 0)),
        _ => (
            SIG_ERROR,
            Value::condition(Condition::type_error(format!(
                "odd?: expected integer, got {}",
                args[0].type_name()
            ))),
        ),
    }
}

pub fn prim_div_vm(args: &[Value]) -> (SignalBits, Value) {
    if args.is_empty() {
        let cond = Condition::arity_error("/: expected at least 1 argument, got 0");
        return (SIG_ERROR, Value::condition(cond));
    }

    if args.len() == 1 {
        return match arithmetic::reciprocal_value(&args[0]) {
            Ok(val) => (SIG_OK, val),
            Err(msg) => {
                let cond = Condition::type_error(msg);
                (SIG_ERROR, Value::condition(cond))
            }
        };
    }

    let mut result = args[0];
    for arg in &args[1..] {
        // Check for division by zero
        let is_zero = match (result.as_int(), arg.as_int()) {
            (Some(_), Some(y)) => y == 0,
            _ => match (result.as_float(), arg.as_float()) {
                (Some(_), Some(y)) => y == 0.0,
                _ => match (result.as_int(), arg.as_float()) {
                    (Some(_), Some(y)) => y == 0.0,
                    _ => match (result.as_float(), arg.as_int()) {
                        (Some(_), Some(y)) => y == 0,
                        _ => false,
                    },
                },
            },
        };

        if is_zero {
            // Create a division-by-zero Condition
            let cond = crate::value::Condition::division_by_zero("division by zero")
                .with_field(0, result) // dividend
                .with_field(1, *arg); // divisor
            return (SIG_ERROR, Value::condition(cond));
        }

        match arithmetic::div_values(&result, arg) {
            Ok(val) => result = val,
            Err(msg) => {
                let cond = Condition::type_error(msg);
                return (SIG_ERROR, Value::condition(cond));
            }
        }
    }
    (SIG_OK, result)
}
