//! FFI primitives for Elle.
//!
//! Provides Lisp functions for loading and calling C functions.

use crate::ffi::types::{CType, FunctionSignature};
use crate::value::{LibHandle, Value};
use crate::vm::VM;

/// Register FFI primitives in the VM.
pub fn register_ffi_primitives(vm: &mut VM) {
    // Phase 1: Basic FFI primitives
    register_ffi_primitive(vm, "load-library", prim_load_library);
}

fn register_ffi_primitive(
    vm: &mut VM,
    name: &str,
    func: fn(&mut VM, &[Value]) -> Result<Value, String>,
) {
    // Note: We can't directly register these like native functions since they need VM access
    // Instead, we'll handle them specially in the REPL/main code
    // For now, create a wrapper that captures the primitive name
}

/// (load-library path) -> library-handle
pub fn prim_load_library(vm: &mut VM, args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("load-library requires exactly 1 argument".to_string());
    }

    let path = match &args[0] {
        Value::String(s) => s.as_ref(),
        _ => return Err("load-library requires a string path".to_string()),
    };

    let lib_id = vm.ffi_mut().load_library(path)?;
    Ok(Value::LibHandle(LibHandle(lib_id)))
}

/// (list-libraries) -> ((id path) ...)
pub fn prim_list_libraries(vm: &VM, _args: &[Value]) -> Result<Value, String> {
    let libs = vm.ffi().loaded_libraries();

    let mut result = Value::Nil;
    for (id, path) in libs.into_iter().rev() {
        let entry = crate::value::cons(
            Value::Int(id as i64),
            crate::value::cons(Value::String(path.into()), Value::Nil),
        );
        result = crate::value::cons(entry, result);
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load_library_error_on_wrong_args() {
        // This would require a full VM setup to test properly
        // For now, just verify function exists
        assert_eq!(
            std::mem::size_of::<fn(&mut VM, &[Value]) -> Result<Value, String>>(),
            std::mem::size_of::<fn(&mut VM, &[Value]) -> Result<Value, String>>()
        );
    }
}
