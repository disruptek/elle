//! Value marshaling between Elle and C types.
//!
//! This module handles conversion of Elle values to C representations
//! and vice versa, supporting all basic types, pointers, structs, and arrays.

use super::types::CType;
use crate::value::{CHandle, Value};
use std::ffi::c_void;
use std::sync::atomic::{AtomicU32, Ordering};

/// A value in C representation - raw bytes that can be passed to C functions.
#[derive(Debug, Clone)]
pub enum CValue {
    /// 64-bit integer (covers all scalar integer types on x86-64)
    Int(i64),
    /// 64-bit float (stored as f64)
    Float(f64),
    /// Opaque pointer to C data
    Pointer(*const c_void),
    /// Raw struct bytes
    Struct(Vec<u8>),
}

impl CValue {
    /// Get the raw bytes for this value (for libffi calling).
    pub fn as_raw(&self) -> Vec<u8> {
        match self {
            CValue::Int(n) => n.to_le_bytes().to_vec(),
            CValue::Float(f) => f.to_le_bytes().to_vec(),
            CValue::Pointer(p) => (*p as u64).to_le_bytes().to_vec(),
            CValue::Struct(bytes) => bytes.clone(),
        }
    }
}

/// Marshals Elle values to C representations.
pub struct Marshal;

impl Marshal {
    /// Convert an Elle value to a C representation.
    pub fn elle_to_c(value: &Value, ctype: &CType) -> Result<CValue, String> {
        match ctype {
            CType::Bool => match value {
                Value::Bool(b) => Ok(CValue::Int(if *b { 1 } else { 0 })),
                Value::Int(n) => Ok(CValue::Int(if *n != 0 { 1 } else { 0 })),
                Value::Nil => Ok(CValue::Int(0)),
                _ => Err(format!("Cannot convert {:?} to bool", value)),
            },
            CType::Char | CType::SChar | CType::UChar => match value {
                Value::Int(n) => Ok(CValue::Int(*n as i8 as i64)),
                _ => Err(format!("Cannot convert {:?} to char", value)),
            },
            CType::Short | CType::UShort => match value {
                Value::Int(n) => Ok(CValue::Int(*n as i16 as i64)),
                _ => Err(format!("Cannot convert {:?} to short", value)),
            },
            CType::Int | CType::UInt => match value {
                Value::Int(n) => Ok(CValue::Int(*n as i32 as i64)),
                _ => Err(format!("Cannot convert {:?} to int", value)),
            },
            CType::Long | CType::ULong | CType::LongLong | CType::ULongLong => match value {
                Value::Int(n) => Ok(CValue::Int(*n)),
                _ => Err(format!("Cannot convert {:?} to long", value)),
            },
            CType::Float => match value {
                Value::Float(f) => Ok(CValue::Float(*f)),
                Value::Int(n) => Ok(CValue::Float(*n as f64)),
                _ => Err(format!("Cannot convert {:?} to float", value)),
            },
            CType::Double => match value {
                Value::Float(f) => Ok(CValue::Float(*f)),
                Value::Int(n) => Ok(CValue::Float(*n as f64)),
                _ => Err(format!("Cannot convert {:?} to double", value)),
            },
            CType::Pointer(_) => match value {
                Value::CHandle(handle) => Ok(CValue::Pointer(handle.ptr)),
                Value::Nil => Ok(CValue::Pointer(std::ptr::null())),
                _ => Err(format!("Cannot convert {:?} to pointer", value)),
            },
            CType::Enum(_) => match value {
                Value::Int(n) => Ok(CValue::Int(*n)),
                _ => Err(format!("Cannot convert {:?} to enum", value)),
            },
            CType::Void => Err("Cannot marshal void as argument".to_string()),
            CType::Struct(_) => Err("Struct marshaling not yet implemented".to_string()),
            CType::Array(_, _) => Err("Array marshaling not yet implemented".to_string()),
        }
    }

    /// Convert a C value back to an Elle value.
    pub fn c_to_elle(cvalue: &CValue, ctype: &CType) -> Result<Value, String> {
        match ctype {
            CType::Void => Ok(Value::Nil),
            CType::Bool => match cvalue {
                CValue::Int(n) => Ok(Value::Bool(*n != 0)),
                _ => Err("Type mismatch in unmarshal".to_string()),
            },
            CType::Char | CType::SChar | CType::UChar => match cvalue {
                CValue::Int(n) => Ok(Value::Int(*n as i8 as i64)),
                _ => Err("Type mismatch in unmarshal".to_string()),
            },
            CType::Short | CType::UShort => match cvalue {
                CValue::Int(n) => Ok(Value::Int(*n as i16 as i64)),
                _ => Err("Type mismatch in unmarshal".to_string()),
            },
            CType::Int | CType::UInt => match cvalue {
                CValue::Int(n) => Ok(Value::Int(*n as i32 as i64)),
                _ => Err("Type mismatch in unmarshal".to_string()),
            },
            CType::Long | CType::ULong | CType::LongLong | CType::ULongLong => match cvalue {
                CValue::Int(n) => Ok(Value::Int(*n)),
                _ => Err("Type mismatch in unmarshal".to_string()),
            },
            CType::Float => match cvalue {
                CValue::Float(f) => Ok(Value::Float(*f as f32 as f64)),
                _ => Err("Type mismatch in unmarshal".to_string()),
            },
            CType::Double => match cvalue {
                CValue::Float(f) => Ok(Value::Float(*f)),
                _ => Err("Type mismatch in unmarshal".to_string()),
            },
            CType::Pointer(_) => match cvalue {
                CValue::Pointer(p) => {
                    if p.is_null() {
                        Ok(Value::Nil)
                    } else {
                        // Generate a unique ID for this handle
                        static HANDLE_ID: AtomicU32 = AtomicU32::new(0);
                        let id = HANDLE_ID.fetch_add(1, Ordering::SeqCst);
                        Ok(Value::CHandle(CHandle::new(*p, id)))
                    }
                }
                _ => Err("Type mismatch in unmarshal".to_string()),
            },
            CType::Enum(_) => match cvalue {
                CValue::Int(n) => Ok(Value::Int(*n)),
                _ => Err("Type mismatch in unmarshal".to_string()),
            },
            CType::Struct(_) => Err("Struct unmarshaling not yet implemented".to_string()),
            CType::Array(_, _) => Err("Array unmarshaling not yet implemented".to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_marshal_int() {
        let val = Value::Int(42);
        let cval = Marshal::elle_to_c(&val, &CType::Int).unwrap();
        match cval {
            CValue::Int(n) => assert_eq!(n, 42),
            _ => panic!("Wrong type"),
        }
    }

    #[test]
    fn test_marshal_bool_true() {
        let val = Value::Bool(true);
        let cval = Marshal::elle_to_c(&val, &CType::Bool).unwrap();
        match cval {
            CValue::Int(n) => assert_eq!(n, 1),
            _ => panic!("Wrong type"),
        }
    }

    #[test]
    fn test_marshal_bool_false() {
        let val = Value::Bool(false);
        let cval = Marshal::elle_to_c(&val, &CType::Bool).unwrap();
        match cval {
            CValue::Int(n) => assert_eq!(n, 0),
            _ => panic!("Wrong type"),
        }
    }

    #[test]
    fn test_marshal_float() {
        let val = Value::Float(3.14);
        let cval = Marshal::elle_to_c(&val, &CType::Float).unwrap();
        match cval {
            CValue::Float(f) => assert!((f - 3.14).abs() < 0.01),
            _ => panic!("Wrong type"),
        }
    }

    #[test]
    fn test_unmarshal_int() {
        let cval = CValue::Int(42);
        let val = Marshal::c_to_elle(&cval, &CType::Int).unwrap();
        assert_eq!(val, Value::Int(42));
    }

    #[test]
    fn test_unmarshal_bool() {
        let cval = CValue::Int(1);
        let val = Marshal::c_to_elle(&cval, &CType::Bool).unwrap();
        assert_eq!(val, Value::Bool(true));

        let cval = CValue::Int(0);
        let val = Marshal::c_to_elle(&cval, &CType::Bool).unwrap();
        assert_eq!(val, Value::Bool(false));
    }

    #[test]
    fn test_unmarshal_float() {
        let cval = CValue::Float(2.71828);
        let val = Marshal::c_to_elle(&cval, &CType::Double).unwrap();
        match val {
            Value::Float(f) => assert!((f - 2.71828).abs() < 0.0001),
            _ => panic!("Wrong type"),
        }
    }

    #[test]
    fn test_marshal_enum() {
        use super::super::types::EnumId;
        let val = Value::Int(5);
        let enum_type = CType::Enum(EnumId::new(1));
        let cval = Marshal::elle_to_c(&val, &enum_type).unwrap();
        match cval {
            CValue::Int(n) => assert_eq!(n, 5),
            _ => panic!("Wrong type"),
        }
    }

    #[test]
    fn test_unmarshal_enum() {
        use super::super::types::EnumId;
        let cval = CValue::Int(10);
        let enum_type = CType::Enum(EnumId::new(1));
        let val = Marshal::c_to_elle(&cval, &enum_type).unwrap();
        assert_eq!(val, Value::Int(10));
    }
}
