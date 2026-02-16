//! Library loading and management primitives.

use crate::error::{LError, LResult};
use crate::value::Value;
use crate::vm::VM;

/// (load-library path) -> library-handle
pub fn prim_load_library(vm: &mut VM, args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("load-library requires exactly 1 argument".into());
    }

    let path = args[0]
        .as_string()
        .ok_or("load-library requires a string path")?;

    let lib_id = vm.ffi_mut().load_library(path)?;
    Ok(Value::int(lib_id as i64))
}

/// (list-libraries) -> ((id path) ...)
pub fn prim_list_libraries(vm: &VM, _args: &[Value]) -> Result<Value, String> {
    let libs = vm.ffi().loaded_libraries();

    let mut result = Value::NIL;
    for (id, path) in libs.into_iter().rev() {
        let entry = crate::value::cons(
            Value::int(id as i64),
            crate::value::cons(Value::string(path), Value::EMPTY_LIST),
        );
        result = crate::value::cons(entry, result);
    }

    Ok(result)
}

pub fn prim_load_library_wrapper(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err("load-library requires exactly 1 argument"
            .to_string()
            .into());
    }

    let path = args[0]
        .as_string()
        .ok_or("load-library requires a string path")?;

    // Get VM context
    let vm_ptr = super::context::get_vm_context().ok_or("FFI not initialized".to_string())?;
    unsafe {
        let vm = &mut *vm_ptr;
        let lib_id = vm.ffi_mut().load_library(path).map_err(LError::from)?;
        Ok(Value::int(lib_id as i64))
    }
}

pub fn prim_list_libraries_wrapper(_args: &[Value]) -> LResult<Value> {
    let vm_ptr = super::context::get_vm_context().ok_or("FFI not initialized".to_string())?;
    unsafe {
        let vm = &*vm_ptr;
        let libs = vm.ffi().loaded_libraries();
        let mut result = Value::NIL;
        for (id, path) in libs.into_iter().rev() {
            let entry = crate::value::cons(
                Value::int(id as i64),
                crate::value::cons(Value::string(path), Value::EMPTY_LIST),
            );
            result = crate::value::cons(entry, result);
        }
        Ok(result)
    }
}
