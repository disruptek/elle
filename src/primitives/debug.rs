use crate::value::{list, Value};

/// Prints a value with debug information
/// (debug-print value)
pub fn prim_debug_print(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err(format!(
            "debug-print: expected 1 argument, got {}",
            args.len()
        ));
    }

    eprintln!("[DEBUG] {:?}", args[0]);
    Ok(args[0].clone())
}

/// Traces execution with a label
/// (trace name value)
pub fn prim_trace(args: &[Value]) -> Result<Value, String> {
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

/// Times the execution of a thunk
/// (profile thunk)
pub fn prim_profile(args: &[Value]) -> Result<Value, String> {
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

/// Returns memory usage statistics
/// (memory-usage)
/// Returns a list: (rss-bytes virtual-bytes)
pub fn prim_memory_usage(_args: &[Value]) -> Result<Value, String> {
    // Simplified placeholder - in production would use system memory stats
    Ok(list(vec![
        Value::Int(0), // RSS bytes (would be actual value)
        Value::Int(0), // Virtual bytes (would be actual value)
    ]))
}
