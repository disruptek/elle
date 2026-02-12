//! System-level primitives
use crate::value::Value;
use std::process;

/// Exit the process with an optional exit code
/// (exit [code])
/// If no code is provided, defaults to 0 (success)
/// The code must be an integer (Int or Float)
pub fn prim_exit(args: &[Value]) -> Result<Value, String> {
    let exit_code = if args.is_empty() {
        0
    } else if args.len() == 1 {
        match &args[0] {
            Value::Int(code) => *code as i32,
            Value::Float(code) => *code as i32,
            _ => {
                return Err(format!(
                    "exit: code must be a number, got {}",
                    args[0].type_name()
                ))
            }
        }
    } else {
        return Err(format!(
            "exit: expected 0 or 1 argument, got {}",
            args.len()
        ));
    };

    process::exit(exit_code);
}
