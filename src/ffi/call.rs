//! FFI call dispatch via libffi.
//!
//! Wraps `libffi::middle::Cif` to perform actual C function calls,
//! converting between Elle `Value` and C types.

use crate::error::{LError, LResult};
use crate::ffi::marshal::{to_libffi_type, MarshalledArg};
use crate::ffi::types::{Signature, TypeDesc};
use crate::value::Value;
use libffi::middle::{Cif, CodePtr, Type};
use std::ffi::c_void;

/// Prepare a libffi CIF from our Signature.
pub fn prepare_cif(sig: &Signature) -> Cif {
    let arg_types: Vec<Type> = sig.args.iter().map(to_libffi_type).collect();
    let ret_type = to_libffi_type(&sig.ret);
    Cif::new(arg_types, ret_type)
}

/// Call a C function through libffi.
///
/// # Safety
/// The function pointer must be valid and match the signature.
/// Arguments must match the expected C types.
pub unsafe fn ffi_call(fn_ptr: *const c_void, args: &[Value], sig: &Signature) -> LResult<Value> {
    if args.len() != sig.args.len() {
        return Err(LError::ffi_error(
            "call",
            format!("expected {} arguments, got {}", sig.args.len(), args.len()),
        ));
    }

    let cif = prepare_cif(sig);
    let code_ptr = CodePtr(fn_ptr as *mut c_void);

    let marshalled: Vec<MarshalledArg> = args
        .iter()
        .zip(sig.args.iter())
        .map(|(val, desc)| MarshalledArg::new(val, desc))
        .collect::<LResult<Vec<_>>>()?;

    let ffi_args: Vec<libffi::middle::Arg> = marshalled.iter().map(|m| m.as_arg()).collect();

    match &sig.ret {
        TypeDesc::Void => {
            cif.call::<()>(code_ptr, &ffi_args);
            Ok(Value::NIL)
        }
        TypeDesc::Bool => {
            let r: std::ffi::c_int = cif.call(code_ptr, &ffi_args);
            Ok(Value::bool(r != 0))
        }
        TypeDesc::I8 | TypeDesc::Char => {
            let r: i8 = cif.call(code_ptr, &ffi_args);
            Ok(Value::int(r as i64))
        }
        TypeDesc::U8 | TypeDesc::UChar => {
            let r: u8 = cif.call(code_ptr, &ffi_args);
            Ok(Value::int(r as i64))
        }
        TypeDesc::I16 | TypeDesc::Short => {
            let r: i16 = cif.call(code_ptr, &ffi_args);
            Ok(Value::int(r as i64))
        }
        TypeDesc::U16 | TypeDesc::UShort => {
            let r: u16 = cif.call(code_ptr, &ffi_args);
            Ok(Value::int(r as i64))
        }
        TypeDesc::I32 | TypeDesc::Int => {
            let r: std::ffi::c_int = cif.call(code_ptr, &ffi_args);
            Ok(Value::int(r as i64))
        }
        TypeDesc::U32 | TypeDesc::UInt => {
            let r: std::ffi::c_uint = cif.call(code_ptr, &ffi_args);
            Ok(Value::int(r as i64))
        }
        TypeDesc::I64 | TypeDesc::Long | TypeDesc::SSize => {
            let r: i64 = cif.call(code_ptr, &ffi_args);
            Ok(Value::int(r))
        }
        TypeDesc::U64 | TypeDesc::ULong | TypeDesc::Size => {
            let r: u64 = cif.call(code_ptr, &ffi_args);
            Ok(Value::int(r as i64))
        }
        TypeDesc::Float => {
            let r: f32 = cif.call(code_ptr, &ffi_args);
            Ok(Value::float(r as f64))
        }
        TypeDesc::Double => {
            let r: f64 = cif.call(code_ptr, &ffi_args);
            Ok(Value::float(r))
        }
        TypeDesc::Ptr | TypeDesc::Str => {
            let r: *const c_void = cif.call(code_ptr, &ffi_args);
            Ok(Value::pointer(r as usize))
        }
        TypeDesc::Struct(_) | TypeDesc::Array(_, _) => Err(LError::ffi_error(
            "call",
            "struct/array return types not yet supported",
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ffi::types::{CallingConvention, Signature};

    #[test]
    fn test_prepare_cif() {
        let sig = Signature {
            convention: CallingConvention::Default,
            ret: TypeDesc::I32,
            args: vec![TypeDesc::I32],
        };
        let _cif = prepare_cif(&sig);
    }

    #[test]
    fn test_prepare_cif_no_args() {
        let sig = Signature {
            convention: CallingConvention::Default,
            ret: TypeDesc::Void,
            args: vec![],
        };
        let _cif = prepare_cif(&sig);
    }

    #[test]
    fn test_arity_check() {
        let sig = Signature {
            convention: CallingConvention::Default,
            ret: TypeDesc::I32,
            args: vec![TypeDesc::I32],
        };
        // Wrong number of args
        let result = unsafe { ffi_call(std::ptr::null(), &[], &sig) };
        assert!(result.is_err());
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_call_abs() {
        extern "C" {
            fn abs(n: std::ffi::c_int) -> std::ffi::c_int;
        }
        let sig = Signature {
            convention: CallingConvention::Default,
            ret: TypeDesc::Int,
            args: vec![TypeDesc::Int],
        };
        let result = unsafe { ffi_call(abs as *const c_void, &[Value::int(-42)], &sig) };
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_int(), Some(42));
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_call_strlen() {
        extern "C" {
            fn strlen(s: *const std::ffi::c_char) -> usize;
        }
        let sig = Signature {
            convention: CallingConvention::Default,
            ret: TypeDesc::Size,
            args: vec![TypeDesc::Str],
        };
        let hello = Value::string("hello");
        let result = unsafe { ffi_call(strlen as *const c_void, &[hello], &sig) };
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_int(), Some(5));
    }
}
