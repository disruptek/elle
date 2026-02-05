use crate::value::Value;

/// Spawns a new thread that executes the closure
/// (spawn thunk)
pub fn prim_spawn(args: &[Value]) -> Result<Value, String> {
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

/// Waits for a thread to complete and returns its result
/// (join thread-handle)
pub fn prim_join(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err(format!("join: expected 1 argument, got {}", args.len()));
    }

    // In production, would wait on thread handle and return result
    // For now, return nil as placeholder
    Ok(Value::Nil)
}

/// Sleeps for the specified number of seconds
/// (sleep seconds)
pub fn prim_sleep(args: &[Value]) -> Result<Value, String> {
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

/// Returns the ID of the current thread
/// (current-thread-id)
pub fn prim_current_thread_id(_args: &[Value]) -> Result<Value, String> {
    let thread_id = std::thread::current().id();
    Ok(Value::String(format!("{:?}", thread_id).into()))
}
