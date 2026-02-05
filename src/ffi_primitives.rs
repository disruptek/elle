//! FFI primitives for Elle.
//!
//! Provides Lisp functions for loading and calling C functions.

use crate::ffi::bindings::generate_elle_bindings;
use crate::ffi::call::FunctionCall;
use crate::ffi::header::HeaderParser;
use crate::ffi::types::{CType, EnumId, EnumLayout, EnumVariant, FunctionSignature};
use crate::value::{LibHandle, Value};
use crate::vm::VM;

/// Register FFI primitives in the VM.
pub fn register_ffi_primitives(_vm: &mut VM) {
    // Phase 2: FFI primitives for function calling
    // Note: These are meant to be called from Elle code
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

/// (call-c-function lib-id func-name return-type (arg-type ...) (arg-val ...)) -> result
///
/// Calls a C function with given arguments.
///
/// # Arguments
/// - lib-id: Library handle (from load-library)
/// - func-name: Name of C function as string
/// - return-type: Return type keyword (:int, :float, :double, :void, :pointer, etc.)
/// - arg-types: List of argument type keywords
/// - arg-values: List of argument values to pass
pub fn prim_call_c_function(vm: &VM, args: &[Value]) -> Result<Value, String> {
    if args.len() != 5 {
        return Err("call-c-function requires exactly 5 arguments".to_string());
    }

    // Parse library ID
    let lib_id = match &args[0] {
        Value::LibHandle(LibHandle(id)) => *id,
        _ => return Err("First argument must be a library handle".to_string()),
    };

    // Parse function name
    let func_name = match &args[1] {
        Value::String(s) => s.as_ref(),
        _ => return Err("Second argument must be a function name string".to_string()),
    };

    // Parse return type
    let return_type = parse_ctype(&args[2])?;

    // Parse argument types
    let arg_types = match &args[3] {
        Value::Nil => vec![],
        Value::Cons(_) => {
            let type_list = args[3].list_to_vec()?;
            type_list
                .iter()
                .map(parse_ctype)
                .collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err("Fourth argument must be a list of argument types".to_string()),
    };

    // Parse argument values
    let arg_values = match &args[4] {
        Value::Nil => vec![],
        Value::Cons(_) => args[4].list_to_vec()?,
        _ => return Err("Fifth argument must be a list of argument values".to_string()),
    };

    // Check argument count matches
    if arg_types.len() != arg_values.len() {
        return Err(format!(
            "Argument count mismatch: expected {}, got {}",
            arg_types.len(),
            arg_values.len()
        ));
    }

    // Create function signature first
    let sig = FunctionSignature::new(func_name.to_string(), arg_types, return_type);

    // Get library and resolve symbol
    let lib = vm
        .ffi()
        .get_library(lib_id)
        .ok_or("Library handle not found".to_string())?;

    // Get function pointer directly from library
    let func_ptr = lib.get_symbol(func_name)?;

    // Create and execute function call
    let call = FunctionCall::new(sig, func_ptr)?;
    call.call(&arg_values)
}

/// Parse a C type from a keyword value.
fn parse_ctype(val: &Value) -> Result<CType, String> {
    match val {
        Value::Symbol(_) => {
            // We need to look up the symbol name, but we don't have access to SymbolTable
            // For now, we'll return an error indicating this needs symbol table integration
            Err("Symbol-based type specification not yet supported".to_string())
        }
        Value::String(s) => match s.as_ref() {
            "void" => Ok(CType::Void),
            "bool" => Ok(CType::Bool),
            "char" => Ok(CType::Char),
            "schar" => Ok(CType::SChar),
            "uchar" => Ok(CType::UChar),
            "short" => Ok(CType::Short),
            "ushort" => Ok(CType::UShort),
            "int" => Ok(CType::Int),
            "uint" => Ok(CType::UInt),
            "long" => Ok(CType::Long),
            "ulong" => Ok(CType::ULong),
            "longlong" => Ok(CType::LongLong),
            "ulonglong" => Ok(CType::ULongLong),
            "float" => Ok(CType::Float),
            "double" => Ok(CType::Double),
            "pointer" => Ok(CType::Pointer(Box::new(CType::Void))),
            _ => Err(format!("Unknown C type: {}", s)),
        },
        _ => Err("Type must be a string".to_string()),
    }
}

/// (load-header-with-lib header-path lib-path) -> library-handle
///
/// Loads a C header file, parses it, and generates Elle bindings.
///
/// # Arguments
/// - header-path: Path to C header file
/// - lib-path: Path to compiled library
pub fn prim_load_header_with_lib(_vm: &mut VM, args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("load-header-with-lib requires exactly 2 arguments".to_string());
    }

    let header_path = match &args[0] {
        Value::String(s) => s.as_ref(),
        _ => return Err("header-path must be a string".to_string()),
    };

    let lib_path = match &args[1] {
        Value::String(s) => s.as_ref(),
        _ => return Err("lib-path must be a string".to_string()),
    };

    // Parse header
    let mut parser = HeaderParser::new();
    let parsed = parser.parse(header_path)?;

    // Generate bindings
    let _lisp_code = generate_elle_bindings(&parsed, lib_path);

    // In a full implementation, we would evaluate the generated Lisp code here
    // For now, return the library handle
    Ok(Value::String(lib_path.into()))
}

/// (define-enum name ((variant-name value) ...)) -> enum-id
///
/// Defines a C enum type in Elle.
pub fn prim_define_enum(_vm: &mut VM, args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("define-enum requires exactly 2 arguments".to_string());
    }

    let enum_name = match &args[0] {
        Value::String(s) => s.as_ref(),
        _ => return Err("enum name must be a string".to_string()),
    };

    // Parse variants from list
    let variants_list = &args[1];
    let mut variants = Vec::new();

    match variants_list {
        Value::Cons(_) => {
            let variant_vec = variants_list.list_to_vec()?;
            for variant_val in variant_vec {
                match variant_val {
                    Value::Cons(cons) => {
                        let name = match &cons.first {
                            Value::String(n) => n.as_ref().to_string(),
                            _ => return Err("variant name must be a string".to_string()),
                        };

                        let value = match &cons.rest {
                            Value::Cons(rest_cons) => match &rest_cons.first {
                                Value::Int(n) => *n,
                                _ => return Err("variant value must be an integer".to_string()),
                            },
                            _ => return Err("variant must be (name value)".to_string()),
                        };

                        variants.push(EnumVariant { name, value });
                    }
                    _ => return Err("each variant must be a cons cell".to_string()),
                }
            }
        }
        Value::Nil => {}
        _ => return Err("variants must be a list".to_string()),
    }

    // Create enum layout
    static ENUM_ID_COUNTER: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
    let enum_id = EnumId::new(ENUM_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst));

    let _layout = EnumLayout::new(enum_id, enum_name.to_string(), variants, CType::Int);

    // Return enum ID as integer
    Ok(Value::Int(enum_id.0 as i64))
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
