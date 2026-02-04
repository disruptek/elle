//! C function calling via dynamic invocation.
//!
//! Phase 1 provides basic function pointer calling for simple types.
//! Phase 2+ will add libffi for complex calling conventions.

use super::types::{CType, FunctionSignature};
use crate::value::Value;
use std::mem;

/// Wrapper around a C function ready to be called.
#[derive(Clone)]
pub struct FunctionCall {
    /// The function signature
    pub signature: FunctionSignature,
    /// Raw function pointer from library
    pub func_ptr: *const std::ffi::c_void,
}

impl FunctionCall {
    /// Create a new function call wrapper.
    ///
    /// # Arguments
    /// * `signature` - Function signature with types
    /// * `func_ptr` - Raw function pointer from library
    ///
    /// # Returns
    /// * `Ok(wrapper)` - Ready to call
    /// * `Err(message)` - If signature is invalid
    pub fn new(
        signature: FunctionSignature,
        func_ptr: *const std::ffi::c_void,
    ) -> Result<Self, String> {
        if func_ptr.is_null() {
            return Err("Function pointer is null".to_string());
        }

        Ok(FunctionCall {
            signature,
            func_ptr,
        })
    }

    /// Call the C function with Elle values as arguments.
    ///
    /// # Arguments
    /// * `args` - Arguments as Elle values
    ///
    /// # Returns
    /// * `Ok(result)` - Return value as Elle value
    /// * `Err(message)` - If argument count/types mismatch or call fails
    ///
    /// # Note
    /// Phase 1 only supports simple integer and floating-point functions.
    /// This is a simplified implementation without libffi support.
    /// Phase 2+ will add proper calling convention support via libffi.
    pub fn call(&self, args: &[Value]) -> Result<Value, String> {
        // Type check argument count
        if args.len() != self.signature.args.len() {
            return Err(format!(
                "Function '{}' expects {} arguments, got {}",
                self.signature.name,
                self.signature.args.len(),
                args.len()
            ));
        }

        // Phase 1: Only support simple integer/float functions with <=6 args (x86-64 ABI)
        // This covers functions like strlen, abs, sin, cos, etc.

        // For now, return an error indicating FFI calling not yet implemented
        // Phase 2 will implement actual function calling
        Err("FFI function calling requires libffi (Phase 2+)".to_string())
    }
}

/// Marshal Elle value to C representation.
pub fn marshal_value(value: &Value, ctype: CType) -> Result<i64, String> {
    match ctype {
        CType::Bool => match value {
            Value::Bool(b) => Ok(if *b { 1 } else { 0 }),
            Value::Int(n) => Ok(if *n != 0 { 1 } else { 0 }),
            _ => Err(format!("Cannot convert {:?} to bool", value)),
        },
        CType::Char | CType::SChar | CType::UChar => match value {
            Value::Int(n) => Ok(*n as i64),
            _ => Err(format!("Cannot convert {:?} to char", value)),
        },
        CType::Short | CType::UShort => match value {
            Value::Int(n) => Ok(*n as i64),
            _ => Err(format!("Cannot convert {:?} to short", value)),
        },
        CType::Int | CType::UInt => match value {
            Value::Int(n) => Ok(*n as i64),
            _ => Err(format!("Cannot convert {:?} to int", value)),
        },
        CType::Long | CType::ULong | CType::LongLong | CType::ULongLong => match value {
            Value::Int(n) => Ok(*n),
            _ => Err(format!("Cannot convert {:?} to long", value)),
        },
        CType::Float | CType::Double => match value {
            Value::Float(f) => {
                // Encode float as i64 bits (for later retrieval)
                let bits = f.to_bits() as i64;
                Ok(bits)
            }
            Value::Int(n) => {
                let f = *n as f64;
                let bits = f.to_bits() as i64;
                Ok(bits)
            }
            _ => Err(format!("Cannot convert {:?} to float", value)),
        },
        CType::Void => Err("Cannot pass void as argument".to_string()),
    }
}

/// Unmarshal C result to Elle value.
pub fn unmarshal_value(result: i64, return_type: CType) -> Result<Value, String> {
    match return_type {
        CType::Void => Ok(Value::Nil),
        CType::Bool => Ok(Value::Bool((result & 1) != 0)),
        CType::Char | CType::SChar | CType::UChar => Ok(Value::Int(result as i8 as i64)),
        CType::Short | CType::UShort => Ok(Value::Int(result as i16 as i64)),
        CType::Int | CType::UInt => Ok(Value::Int(result as i32 as i64)),
        CType::Long | CType::ULong | CType::LongLong | CType::ULongLong => Ok(Value::Int(result)),
        CType::Float | CType::Double => {
            let f = f64::from_bits(result as u64);
            Ok(Value::Float(f))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_marshal_int() {
        let val = Value::Int(42);
        let result = marshal_value(&val, CType::Int).unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_marshal_bool() {
        let val = Value::Bool(true);
        let result = marshal_value(&val, CType::Bool).unwrap();
        assert_eq!(result, 1);

        let val = Value::Bool(false);
        let result = marshal_value(&val, CType::Bool).unwrap();
        assert_eq!(result, 0);
    }

    #[test]
    fn test_unmarshal_int() {
        let result = unmarshal_value(42, CType::Int).unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_unmarshal_bool() {
        let result = unmarshal_value(1, CType::Bool).unwrap();
        assert_eq!(result, Value::Bool(true));

        let result = unmarshal_value(0, CType::Bool).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_float_round_trip() {
        let original = 3.14159f64;
        let bits = original.to_bits() as i64;
        let recovered = f64::from_bits(bits as u64);
        assert_eq!(original, recovered);
    }
}
