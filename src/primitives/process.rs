//! Process-related primitives
use crate::error::LResult;

use crate::value::Value;

/// Exit the process with an optional exit code
///
/// (exit)       ; exits with code 0
/// (exit 0)     ; exits with code 0
/// (exit 1)     ; exits with code 1
/// (exit 42)    ; exits with code 42
pub fn prim_exit(args: &[Value]) -> LResult<Value> {
    let code = if args.is_empty() {
        0
    } else if args.len() == 1 {
        if let Some(n) = args[0].as_int() {
            if !(0..=255).contains(&n) {
                return Err(format!("exit code must be between 0 and 255, got {}", n).into());
            }
            n as i32
        } else {
            return Err(format!(
                "exit requires an integer argument, got {}",
                args[0].type_name()
            )
            .into());
        }
    } else {
        return Err(format!("exit requires 0 or 1 arguments, got {}", args.len()).into());
    };

    std::process::exit(code);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exit_too_many_args() {
        let result = prim_exit(&[Value::int(0), Value::int(1)]);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("0 or 1 arguments"));
    }

    #[test]
    fn test_exit_wrong_type() {
        let result = prim_exit(&[Value::bool(true)]);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("integer argument"));
    }

    #[test]
    fn test_exit_negative() {
        let result = prim_exit(&[Value::int(-1)]);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("between 0 and 255"));
    }

    #[test]
    fn test_exit_too_large() {
        let result = prim_exit(&[Value::int(256)]);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("between 0 and 255"));
    }
}
