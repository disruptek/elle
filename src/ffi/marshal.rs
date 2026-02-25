//! Marshalling between Elle Values and C-typed data for libffi calls.
//!
//! Two concerns:
//! - **Argument marshalling**: Elle `Value` -> C-typed storage -> `libffi::middle::Arg`
//! - **Return conversion**: C return value -> Elle `Value`

use crate::error::{LError, LResult};
use crate::ffi::types::TypeDesc;
use crate::value::Value;
use libffi::middle::Type;
use std::ffi::{c_void, CString};

/// Convert a `TypeDesc` to the corresponding `libffi::middle::Type`.
pub fn to_libffi_type(desc: &TypeDesc) -> Type {
    match desc {
        TypeDesc::Void => Type::void(),
        TypeDesc::Bool => Type::c_int(),
        TypeDesc::I8 => Type::i8(),
        TypeDesc::U8 => Type::u8(),
        TypeDesc::I16 => Type::i16(),
        TypeDesc::U16 => Type::u16(),
        TypeDesc::I32 => Type::i32(),
        TypeDesc::U32 => Type::u32(),
        TypeDesc::I64 => Type::i64(),
        TypeDesc::U64 => Type::u64(),
        TypeDesc::Float => Type::f32(),
        TypeDesc::Double => Type::f64(),
        TypeDesc::Int => Type::c_int(),
        TypeDesc::UInt => Type::c_uint(),
        TypeDesc::Long => Type::c_long(),
        TypeDesc::ULong => Type::c_ulong(),
        TypeDesc::Char => Type::i8(),
        TypeDesc::UChar => Type::u8(),
        TypeDesc::Short => Type::c_short(),
        TypeDesc::UShort => Type::c_ushort(),
        TypeDesc::Size => Type::usize(),
        TypeDesc::SSize => Type::isize(),
        TypeDesc::Ptr | TypeDesc::Str => Type::pointer(),
        TypeDesc::Struct(desc) => {
            let fields: Vec<Type> = desc.fields.iter().map(to_libffi_type).collect();
            Type::structure(fields)
        }
        TypeDesc::Array(elem, count) => {
            let elem_type = to_libffi_type(elem);
            let fields: Vec<Type> = (0..*count).map(|_| elem_type.clone()).collect();
            Type::structure(fields)
        }
    }
}

// ── Argument marshalling ────────────────────────────────────────────

/// Holds C-typed data for an FFI argument.
///
/// Must live as long as the `libffi::middle::Arg` references it.
/// Created from an Elle `Value` and a `TypeDesc`, then passed to
/// `ffi_call` via `as_arg()`.
pub struct MarshalledArg {
    storage: ArgStorage,
}

#[allow(dead_code)]
enum ArgStorage {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    F32(f32),
    F64(f64),
    Ptr(*const c_void),
    /// Owned CString for `:string` type. The `*const c_char` is the
    /// pointer that libffi reads through (it's a `char*` argument).
    /// The CString must outlive the Arg.
    Str(CString, *const std::ffi::c_char),
}

impl MarshalledArg {
    /// Create from an Elle Value and a type descriptor.
    pub fn new(value: &Value, desc: &TypeDesc) -> LResult<Self> {
        let storage = match desc {
            TypeDesc::Void => {
                return Err(LError::ffi_type_error(
                    "void",
                    "void is not valid for arguments",
                ));
            }

            TypeDesc::Bool => ArgStorage::I32(if value.is_truthy() { 1 } else { 0 }),

            TypeDesc::I8 => {
                let n = extract_int(value, "i8")?;
                range_check(n, i8::MIN as i64, i8::MAX as i64, "i8")?;
                ArgStorage::I8(n as i8)
            }
            TypeDesc::U8 | TypeDesc::UChar => {
                let n = extract_int(value, desc_name(desc))?;
                range_check(n, u8::MIN as i64, u8::MAX as i64, desc_name(desc))?;
                ArgStorage::U8(n as u8)
            }
            TypeDesc::I16 => {
                let n = extract_int(value, "i16")?;
                range_check(n, i16::MIN as i64, i16::MAX as i64, "i16")?;
                ArgStorage::I16(n as i16)
            }
            TypeDesc::U16 => {
                let n = extract_int(value, "u16")?;
                range_check(n, u16::MIN as i64, u16::MAX as i64, "u16")?;
                ArgStorage::U16(n as u16)
            }
            TypeDesc::I32 => {
                let n = extract_int(value, "i32")?;
                range_check(n, i32::MIN as i64, i32::MAX as i64, "i32")?;
                ArgStorage::I32(n as i32)
            }
            TypeDesc::U32 => {
                let n = extract_int(value, "u32")?;
                range_check(n, u32::MIN as i64, u32::MAX as i64, "u32")?;
                ArgStorage::U32(n as u32)
            }
            TypeDesc::I64 => {
                let n = extract_int(value, "i64")?;
                ArgStorage::I64(n)
            }
            TypeDesc::U64 => {
                let n = extract_int(value, "u64")?;
                ArgStorage::U64(n as u64)
            }
            TypeDesc::Int => {
                let n = extract_int(value, "int")?;
                range_check(
                    n,
                    std::ffi::c_int::MIN as i64,
                    std::ffi::c_int::MAX as i64,
                    "int",
                )?;
                ArgStorage::I32(n as i32)
            }
            TypeDesc::UInt => {
                let n = extract_int(value, "uint")?;
                range_check(n, 0, std::ffi::c_uint::MAX as i64, "uint")?;
                ArgStorage::U32(n as u32)
            }
            TypeDesc::Long => {
                let n = extract_int(value, "long")?;
                ArgStorage::I64(n as std::ffi::c_long as i64)
            }
            TypeDesc::ULong => {
                let n = extract_int(value, "ulong")?;
                ArgStorage::U64(n as std::ffi::c_ulong as u64)
            }
            TypeDesc::Char => {
                let n = extract_int(value, "char")?;
                range_check(n, i8::MIN as i64, i8::MAX as i64, "char")?;
                ArgStorage::I8(n as i8)
            }
            TypeDesc::Short => {
                let n = extract_int(value, "short")?;
                range_check(
                    n,
                    std::ffi::c_short::MIN as i64,
                    std::ffi::c_short::MAX as i64,
                    "short",
                )?;
                ArgStorage::I16(n as i16)
            }
            TypeDesc::UShort => {
                let n = extract_int(value, "ushort")?;
                range_check(n, 0, std::ffi::c_ushort::MAX as i64, "ushort")?;
                ArgStorage::U16(n as u16)
            }
            TypeDesc::Size => {
                let n = extract_int(value, "size")?;
                ArgStorage::U64(n as usize as u64)
            }
            TypeDesc::SSize => {
                let n = extract_int(value, "ssize")?;
                ArgStorage::I64(n as isize as i64)
            }

            TypeDesc::Float => {
                let f = value
                    .as_float()
                    .or_else(|| value.as_int().map(|i| i as f64))
                    .ok_or_else(|| {
                        LError::ffi_type_error(
                            "float",
                            format!("expected number, got {}", value.type_name()),
                        )
                    })?;
                ArgStorage::F32(f as f32)
            }
            TypeDesc::Double => {
                let f = value
                    .as_float()
                    .or_else(|| value.as_int().map(|i| i as f64))
                    .ok_or_else(|| {
                        LError::ffi_type_error(
                            "double",
                            format!("expected number, got {}", value.type_name()),
                        )
                    })?;
                ArgStorage::F64(f)
            }

            TypeDesc::Ptr => {
                if value.is_nil() {
                    ArgStorage::Ptr(std::ptr::null())
                } else if let Some(addr) = value.as_pointer() {
                    ArgStorage::Ptr(addr as *const c_void)
                } else {
                    return Err(LError::ffi_type_error(
                        "ptr",
                        format!("expected pointer or nil, got {}", value.type_name()),
                    ));
                }
            }

            TypeDesc::Str => {
                let s = value.as_string().ok_or_else(|| {
                    LError::ffi_type_error(
                        "string",
                        format!("expected string, got {}", value.type_name()),
                    )
                })?;
                let cstring = CString::new(s)
                    .map_err(|_| LError::ffi_type_error("string", "contains interior null byte"))?;
                let ptr = cstring.as_ptr();
                ArgStorage::Str(cstring, ptr)
            }

            TypeDesc::Struct(_) | TypeDesc::Array(_, _) => {
                return Err(LError::ffi_error(
                    "marshal",
                    "struct/array arguments not yet supported",
                ));
            }
        };
        Ok(MarshalledArg { storage })
    }

    /// Get a libffi Arg referencing this storage.
    pub fn as_arg(&self) -> libffi::middle::Arg<'_> {
        match &self.storage {
            ArgStorage::I8(v) => libffi::middle::arg(v),
            ArgStorage::U8(v) => libffi::middle::arg(v),
            ArgStorage::I16(v) => libffi::middle::arg(v),
            ArgStorage::U16(v) => libffi::middle::arg(v),
            ArgStorage::I32(v) => libffi::middle::arg(v),
            ArgStorage::U32(v) => libffi::middle::arg(v),
            ArgStorage::I64(v) => libffi::middle::arg(v),
            ArgStorage::U64(v) => libffi::middle::arg(v),
            ArgStorage::F32(v) => libffi::middle::arg(v),
            ArgStorage::F64(v) => libffi::middle::arg(v),
            ArgStorage::Ptr(v) => libffi::middle::arg(v),
            ArgStorage::Str(_, ptr) => libffi::middle::arg(ptr),
        }
    }
}

// ── Helpers ─────────────────────────────────────────────────────────

fn extract_int(value: &Value, type_name: &str) -> LResult<i64> {
    value.as_int().ok_or_else(|| {
        LError::ffi_type_error(
            type_name,
            format!("expected integer, got {}", value.type_name()),
        )
    })
}

fn range_check(n: i64, min: i64, max: i64, type_name: &str) -> LResult<()> {
    if n < min || n > max {
        Err(LError::ffi_type_error(
            type_name,
            format!("value {} out of range [{}, {}]", n, min, max),
        ))
    } else {
        Ok(())
    }
}

fn desc_name(desc: &TypeDesc) -> &'static str {
    match desc {
        TypeDesc::UChar => "uchar",
        TypeDesc::U8 => "u8",
        _ => "unknown",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ffi::types::StructDesc;

    #[test]
    fn test_to_libffi_type_primitives() {
        // Smoke test: these should not panic
        to_libffi_type(&TypeDesc::Void);
        to_libffi_type(&TypeDesc::Bool);
        to_libffi_type(&TypeDesc::I8);
        to_libffi_type(&TypeDesc::U8);
        to_libffi_type(&TypeDesc::I16);
        to_libffi_type(&TypeDesc::U16);
        to_libffi_type(&TypeDesc::I32);
        to_libffi_type(&TypeDesc::U32);
        to_libffi_type(&TypeDesc::I64);
        to_libffi_type(&TypeDesc::U64);
        to_libffi_type(&TypeDesc::Float);
        to_libffi_type(&TypeDesc::Double);
        to_libffi_type(&TypeDesc::Int);
        to_libffi_type(&TypeDesc::UInt);
        to_libffi_type(&TypeDesc::Long);
        to_libffi_type(&TypeDesc::ULong);
        to_libffi_type(&TypeDesc::Char);
        to_libffi_type(&TypeDesc::UChar);
        to_libffi_type(&TypeDesc::Short);
        to_libffi_type(&TypeDesc::UShort);
        to_libffi_type(&TypeDesc::Size);
        to_libffi_type(&TypeDesc::SSize);
        to_libffi_type(&TypeDesc::Ptr);
        to_libffi_type(&TypeDesc::Str);
    }

    #[test]
    fn test_to_libffi_type_struct() {
        let desc = TypeDesc::Struct(StructDesc {
            fields: vec![TypeDesc::I32, TypeDesc::Double],
        });
        to_libffi_type(&desc);
    }

    #[test]
    fn test_to_libffi_type_array() {
        let desc = TypeDesc::Array(Box::new(TypeDesc::I32), 4);
        to_libffi_type(&desc);
    }

    #[test]
    fn test_marshal_int_types() {
        let val = Value::int(42);
        assert!(MarshalledArg::new(&val, &TypeDesc::I8).is_ok());
        assert!(MarshalledArg::new(&val, &TypeDesc::U8).is_ok());
        assert!(MarshalledArg::new(&val, &TypeDesc::I16).is_ok());
        assert!(MarshalledArg::new(&val, &TypeDesc::U16).is_ok());
        assert!(MarshalledArg::new(&val, &TypeDesc::I32).is_ok());
        assert!(MarshalledArg::new(&val, &TypeDesc::U32).is_ok());
        assert!(MarshalledArg::new(&val, &TypeDesc::I64).is_ok());
        assert!(MarshalledArg::new(&val, &TypeDesc::U64).is_ok());
        assert!(MarshalledArg::new(&val, &TypeDesc::Int).is_ok());
        assert!(MarshalledArg::new(&val, &TypeDesc::UInt).is_ok());
    }

    #[test]
    fn test_marshal_int_range_error() {
        let val = Value::int(256);
        assert!(MarshalledArg::new(&val, &TypeDesc::I8).is_err());
        assert!(MarshalledArg::new(&val, &TypeDesc::U8).is_err());

        let neg = Value::int(-1);
        assert!(MarshalledArg::new(&neg, &TypeDesc::U8).is_err());
        assert!(MarshalledArg::new(&neg, &TypeDesc::U16).is_err());
        assert!(MarshalledArg::new(&neg, &TypeDesc::U32).is_err());
    }

    #[test]
    fn test_marshal_float() {
        let val = Value::float(2.5);
        assert!(MarshalledArg::new(&val, &TypeDesc::Float).is_ok());
        assert!(MarshalledArg::new(&val, &TypeDesc::Double).is_ok());
    }

    #[test]
    fn test_marshal_int_as_float() {
        let val = Value::int(42);
        assert!(MarshalledArg::new(&val, &TypeDesc::Float).is_ok());
        assert!(MarshalledArg::new(&val, &TypeDesc::Double).is_ok());
    }

    #[test]
    fn test_marshal_bool() {
        let t = Value::bool(true);
        let f = Value::bool(false);
        assert!(MarshalledArg::new(&t, &TypeDesc::Bool).is_ok());
        assert!(MarshalledArg::new(&f, &TypeDesc::Bool).is_ok());
    }

    #[test]
    fn test_marshal_ptr_nil() {
        let nil = Value::NIL;
        assert!(MarshalledArg::new(&nil, &TypeDesc::Ptr).is_ok());
    }

    #[test]
    fn test_marshal_ptr_value() {
        let ptr = Value::pointer(0x1234);
        assert!(MarshalledArg::new(&ptr, &TypeDesc::Ptr).is_ok());
    }

    #[test]
    fn test_marshal_ptr_type_error() {
        let val = Value::int(42);
        assert!(MarshalledArg::new(&val, &TypeDesc::Ptr).is_err());
    }

    #[test]
    fn test_marshal_string() {
        let val = Value::string("hello");
        assert!(MarshalledArg::new(&val, &TypeDesc::Str).is_ok());
    }

    #[test]
    fn test_marshal_string_interior_null() {
        let val = Value::string("hel\0lo");
        assert!(MarshalledArg::new(&val, &TypeDesc::Str).is_err());
    }

    #[test]
    fn test_marshal_void_error() {
        let val = Value::NIL;
        assert!(MarshalledArg::new(&val, &TypeDesc::Void).is_err());
    }

    #[test]
    fn test_marshal_struct_not_supported() {
        let val = Value::int(1);
        let desc = TypeDesc::Struct(StructDesc {
            fields: vec![TypeDesc::I32],
        });
        assert!(MarshalledArg::new(&val, &desc).is_err());
    }

    #[test]
    fn test_as_arg_does_not_panic() {
        let val = Value::int(42);
        let m = MarshalledArg::new(&val, &TypeDesc::I32).unwrap();
        let _ = m.as_arg();

        let fval = Value::float(1.5);
        let m2 = MarshalledArg::new(&fval, &TypeDesc::Double).unwrap();
        let _ = m2.as_arg();

        let sval = Value::string("test");
        let m3 = MarshalledArg::new(&sval, &TypeDesc::Str).unwrap();
        let _ = m3.as_arg();
    }
}
