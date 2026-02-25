//! FFI primitive functions for Elle.
//!
//! Provides the Elle-facing API for calling C functions:
//! library loading, symbol lookup, signature creation,
//! function calls, memory management, and typed memory access.

use crate::effects::Effect;
use crate::ffi::types::{CallingConvention, Signature, TypeDesc};
use crate::primitives::def::PrimitiveDef;
use crate::value::fiber::{SignalBits, SIG_ERROR, SIG_OK};
use crate::value::types::Arity;
use crate::value::{error_val, Value};

// ── Library loading ─────────────────────────────────────────────────

pub fn prim_ffi_native(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            error_val("arity-error", "ffi/native: expected 1 argument"),
        );
    }
    let vm_ptr = match crate::context::get_vm_context() {
        Some(ptr) => ptr,
        None => {
            return (
                SIG_ERROR,
                error_val("ffi-error", "ffi/native: no VM context"),
            )
        }
    };
    let vm = unsafe { &mut *vm_ptr };

    // nil → load self process (dlopen(NULL))
    if args[0].is_nil() {
        return match vm.ffi_mut().load_self() {
            Ok(id) => (SIG_OK, Value::lib_handle(id)),
            Err(e) => (
                SIG_ERROR,
                error_val("ffi-error", format!("ffi/native: {}", e)),
            ),
        };
    }

    let path = match args[0].as_string() {
        Some(s) => s,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!(
                        "ffi/native: expected string or nil, got {}",
                        args[0].type_name()
                    ),
                ),
            )
        }
    };
    match vm.ffi_mut().load_library(path) {
        Ok(id) => (SIG_OK, Value::lib_handle(id)),
        Err(e) => (
            SIG_ERROR,
            error_val("ffi-error", format!("ffi/native: {}", e)),
        ),
    }
}

// ── Symbol lookup ───────────────────────────────────────────────────

pub fn prim_ffi_lookup(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 2 {
        return (
            SIG_ERROR,
            error_val("arity-error", "ffi/lookup: expected 2 arguments"),
        );
    }
    let lib_id = match args[0].as_lib_handle() {
        Some(id) => id,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!(
                        "ffi/lookup: expected library handle, got {}",
                        args[0].type_name()
                    ),
                ),
            )
        }
    };
    let sym_name = match args[1].as_string() {
        Some(s) => s,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!("ffi/lookup: expected string, got {}", args[1].type_name()),
                ),
            )
        }
    };
    let vm_ptr = match crate::context::get_vm_context() {
        Some(ptr) => ptr,
        None => {
            return (
                SIG_ERROR,
                error_val("ffi-error", "ffi/lookup: no VM context"),
            )
        }
    };
    let vm = unsafe { &*vm_ptr };
    let lib = match vm.ffi().get_library(lib_id) {
        Some(lib) => lib,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "ffi-error",
                    format!("ffi/lookup: library {} not loaded", lib_id),
                ),
            )
        }
    };
    match lib.get_symbol(sym_name) {
        Ok(ptr) => (SIG_OK, Value::pointer(ptr as usize)),
        Err(e) => (
            SIG_ERROR,
            error_val("ffi-error", format!("ffi/lookup: {}", e)),
        ),
    }
}

// ── Signature creation ──────────────────────────────────────────────

pub fn prim_ffi_signature(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 2 {
        return (
            SIG_ERROR,
            error_val("arity-error", "ffi/signature: expected 2 arguments"),
        );
    }
    let ret_name = match args[0].as_keyword_name() {
        Some(name) => name,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!(
                        "ffi/signature: expected keyword for return type, got {}",
                        args[0].type_name()
                    ),
                ),
            )
        }
    };
    let ret = match TypeDesc::from_keyword(ret_name) {
        Some(t) => t,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "ffi-error",
                    format!("ffi/signature: unknown type :{}", ret_name),
                ),
            )
        }
    };

    // Parse argument types from array or list
    let arg_vals = if let Some(arr) = args[1].as_array() {
        arr.borrow().clone()
    } else {
        match args[1].list_to_vec() {
            Ok(v) => v,
            Err(_) => {
                return (
                    SIG_ERROR,
                    error_val(
                        "type-error",
                        format!(
                            "ffi/signature: expected array or list for arg types, got {}",
                            args[1].type_name()
                        ),
                    ),
                )
            }
        }
    };

    let mut arg_types = Vec::with_capacity(arg_vals.len());
    for val in &arg_vals {
        let name = match val.as_keyword_name() {
            Some(n) => n,
            None => {
                return (
                    SIG_ERROR,
                    error_val(
                        "type-error",
                        format!(
                            "ffi/signature: expected keyword in arg types, got {}",
                            val.type_name()
                        ),
                    ),
                )
            }
        };
        match TypeDesc::from_keyword(name) {
            Some(t) => arg_types.push(t),
            None => {
                return (
                    SIG_ERROR,
                    error_val(
                        "ffi-error",
                        format!("ffi/signature: unknown type :{}", name),
                    ),
                )
            }
        }
    }

    let sig = Signature {
        convention: CallingConvention::Default,
        ret,
        args: arg_types,
    };
    (SIG_OK, Value::ffi_signature(sig))
}

// ── Function call ───────────────────────────────────────────────────

pub fn prim_ffi_call(args: &[Value]) -> (SignalBits, Value) {
    if args.len() < 2 {
        return (
            SIG_ERROR,
            error_val("arity-error", "ffi/call: expected at least 2 arguments"),
        );
    }
    if args[0].is_nil() {
        return (
            SIG_ERROR,
            error_val("type-error", "ffi/call: function pointer is nil"),
        );
    }
    let fn_addr = match args[0].as_pointer() {
        Some(addr) => addr,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!("ffi/call: expected pointer, got {}", args[0].type_name()),
                ),
            )
        }
    };

    let sig = match args[1].as_ffi_signature() {
        Some(s) => s.clone(),
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!("ffi/call: expected signature, got {}", args[1].type_name()),
                ),
            )
        }
    };

    let call_args = &args[2..];

    match unsafe { crate::ffi::call::ffi_call(fn_addr as *const std::ffi::c_void, call_args, &sig) }
    {
        Ok(val) => (SIG_OK, val),
        Err(e) => (
            SIG_ERROR,
            error_val("ffi-error", format!("ffi/call: {}", e)),
        ),
    }
}

// ── Type introspection ──────────────────────────────────────────────

pub fn prim_ffi_size(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            error_val("arity-error", "ffi/size: expected 1 argument"),
        );
    }
    let name = match args[0].as_keyword_name() {
        Some(n) => n,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!("ffi/size: expected keyword, got {}", args[0].type_name()),
                ),
            )
        }
    };
    let desc = match TypeDesc::from_keyword(name) {
        Some(t) => t,
        None => {
            return (
                SIG_ERROR,
                error_val("ffi-error", format!("ffi/size: unknown type :{}", name)),
            )
        }
    };
    match desc.size() {
        Some(s) => (SIG_OK, Value::int(s as i64)),
        None => (SIG_OK, Value::NIL),
    }
}

pub fn prim_ffi_align(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            error_val("arity-error", "ffi/align: expected 1 argument"),
        );
    }
    let name = match args[0].as_keyword_name() {
        Some(n) => n,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!("ffi/align: expected keyword, got {}", args[0].type_name()),
                ),
            )
        }
    };
    let desc = match TypeDesc::from_keyword(name) {
        Some(t) => t,
        None => {
            return (
                SIG_ERROR,
                error_val("ffi-error", format!("ffi/align: unknown type :{}", name)),
            )
        }
    };
    match desc.align() {
        Some(a) => (SIG_OK, Value::int(a as i64)),
        None => (SIG_OK, Value::NIL),
    }
}

// ── Memory management ───────────────────────────────────────────────

pub fn prim_ffi_malloc(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            error_val("arity-error", "ffi/malloc: expected 1 argument"),
        );
    }
    let size = match args[0].as_int() {
        Some(n) if n > 0 => n as usize,
        Some(_) => {
            return (
                SIG_ERROR,
                error_val("argument-error", "ffi/malloc: size must be positive"),
            )
        }
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!("ffi/malloc: expected integer, got {}", args[0].type_name()),
                ),
            )
        }
    };
    let ptr = unsafe { libc::malloc(size) };
    if ptr.is_null() {
        (
            SIG_ERROR,
            error_val("ffi-error", "ffi/malloc: allocation failed"),
        )
    } else {
        (SIG_OK, Value::pointer(ptr as usize))
    }
}

pub fn prim_ffi_free(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            error_val("arity-error", "ffi/free: expected 1 argument"),
        );
    }
    if args[0].is_nil() {
        return (SIG_OK, Value::NIL); // free(NULL) is a no-op
    }
    let addr = match args[0].as_pointer() {
        Some(a) => a,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!("ffi/free: expected pointer, got {}", args[0].type_name()),
                ),
            )
        }
    };
    unsafe { libc::free(addr as *mut libc::c_void) };
    (SIG_OK, Value::NIL)
}

// ── Typed memory access ─────────────────────────────────────────────

pub fn prim_ffi_read(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 2 {
        return (
            SIG_ERROR,
            error_val("arity-error", "ffi/read: expected 2 arguments"),
        );
    }
    if args[0].is_nil() {
        return (
            SIG_ERROR,
            error_val("argument-error", "ffi/read: cannot read from null pointer"),
        );
    }
    let addr = match args[0].as_pointer() {
        Some(a) => a,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!("ffi/read: expected pointer, got {}", args[0].type_name()),
                ),
            )
        }
    };
    let type_name = match args[1].as_keyword_name() {
        Some(n) => n,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!("ffi/read: expected keyword, got {}", args[1].type_name()),
                ),
            )
        }
    };
    let desc = match TypeDesc::from_keyword(type_name) {
        Some(t) => t,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "ffi-error",
                    format!("ffi/read: unknown type :{}", type_name),
                ),
            )
        }
    };
    let ptr = addr as *const u8;
    unsafe {
        let val = match desc {
            TypeDesc::I8 | TypeDesc::Char => Value::int(*(ptr as *const i8) as i64),
            TypeDesc::U8 | TypeDesc::UChar => Value::int(*ptr as i64),
            TypeDesc::I16 | TypeDesc::Short => Value::int(*(ptr as *const i16) as i64),
            TypeDesc::U16 | TypeDesc::UShort => Value::int(*(ptr as *const u16) as i64),
            TypeDesc::I32 | TypeDesc::Int => Value::int(*(ptr as *const i32) as i64),
            TypeDesc::U32 | TypeDesc::UInt => Value::int(*(ptr as *const u32) as i64),
            TypeDesc::I64 | TypeDesc::Long | TypeDesc::SSize => Value::int(*(ptr as *const i64)),
            TypeDesc::U64 | TypeDesc::ULong | TypeDesc::Size => {
                Value::int(*(ptr as *const u64) as i64)
            }
            TypeDesc::Float => Value::float(*(ptr as *const f32) as f64),
            TypeDesc::Double => Value::float(*(ptr as *const f64)),
            TypeDesc::Bool => Value::bool(*(ptr as *const std::ffi::c_int) != 0),
            TypeDesc::Ptr => Value::pointer(*(ptr as *const usize)),
            TypeDesc::Str => {
                let cptr = *(ptr as *const *const std::ffi::c_char);
                if cptr.is_null() {
                    Value::NIL
                } else {
                    let cstr = std::ffi::CStr::from_ptr(cptr);
                    match cstr.to_str() {
                        Ok(s) => Value::string(s),
                        Err(_) => {
                            return (
                                SIG_ERROR,
                                error_val("ffi-error", "ffi/read: string is not valid UTF-8"),
                            )
                        }
                    }
                }
            }
            TypeDesc::Void => {
                return (
                    SIG_ERROR,
                    error_val("ffi-error", "ffi/read: cannot read void"),
                )
            }
            TypeDesc::Struct(_) | TypeDesc::Array(_, _) => {
                return (
                    SIG_ERROR,
                    error_val("ffi-error", "ffi/read: struct/array not yet supported"),
                )
            }
        };
        (SIG_OK, val)
    }
}

pub fn prim_ffi_write(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 3 {
        return (
            SIG_ERROR,
            error_val("arity-error", "ffi/write: expected 3 arguments"),
        );
    }
    if args[0].is_nil() {
        return (
            SIG_ERROR,
            error_val("argument-error", "ffi/write: cannot write to null pointer"),
        );
    }
    let addr = match args[0].as_pointer() {
        Some(a) => a,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!("ffi/write: expected pointer, got {}", args[0].type_name()),
                ),
            )
        }
    };
    let type_name = match args[1].as_keyword_name() {
        Some(n) => n,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "type-error",
                    format!("ffi/write: expected keyword, got {}", args[1].type_name()),
                ),
            )
        }
    };
    let desc = match TypeDesc::from_keyword(type_name) {
        Some(t) => t,
        None => {
            return (
                SIG_ERROR,
                error_val(
                    "ffi-error",
                    format!("ffi/write: unknown type :{}", type_name),
                ),
            )
        }
    };

    let ptr = addr as *mut u8;
    let value = &args[2];

    unsafe {
        match desc {
            TypeDesc::I8 | TypeDesc::Char => {
                let n = match value.as_int() {
                    Some(n) => n as i8,
                    None => {
                        return (
                            SIG_ERROR,
                            error_val("type-error", "ffi/write: expected integer"),
                        )
                    }
                };
                *(ptr as *mut i8) = n;
            }
            TypeDesc::U8 | TypeDesc::UChar => {
                let n = match value.as_int() {
                    Some(n) => n as u8,
                    None => {
                        return (
                            SIG_ERROR,
                            error_val("type-error", "ffi/write: expected integer"),
                        )
                    }
                };
                *ptr = n;
            }
            TypeDesc::I16 | TypeDesc::Short => {
                let n = match value.as_int() {
                    Some(n) => n as i16,
                    None => {
                        return (
                            SIG_ERROR,
                            error_val("type-error", "ffi/write: expected integer"),
                        )
                    }
                };
                *(ptr as *mut i16) = n;
            }
            TypeDesc::U16 | TypeDesc::UShort => {
                let n = match value.as_int() {
                    Some(n) => n as u16,
                    None => {
                        return (
                            SIG_ERROR,
                            error_val("type-error", "ffi/write: expected integer"),
                        )
                    }
                };
                *(ptr as *mut u16) = n;
            }
            TypeDesc::I32 | TypeDesc::Int => {
                let n = match value.as_int() {
                    Some(n) => n as i32,
                    None => {
                        return (
                            SIG_ERROR,
                            error_val("type-error", "ffi/write: expected integer"),
                        )
                    }
                };
                *(ptr as *mut i32) = n;
            }
            TypeDesc::U32 | TypeDesc::UInt => {
                let n = match value.as_int() {
                    Some(n) => n as u32,
                    None => {
                        return (
                            SIG_ERROR,
                            error_val("type-error", "ffi/write: expected integer"),
                        )
                    }
                };
                *(ptr as *mut u32) = n;
            }
            TypeDesc::I64 | TypeDesc::Long | TypeDesc::SSize => {
                let n = match value.as_int() {
                    Some(n) => n,
                    None => {
                        return (
                            SIG_ERROR,
                            error_val("type-error", "ffi/write: expected integer"),
                        )
                    }
                };
                *(ptr as *mut i64) = n;
            }
            TypeDesc::U64 | TypeDesc::ULong | TypeDesc::Size => {
                let n = match value.as_int() {
                    Some(n) => n as u64,
                    None => {
                        return (
                            SIG_ERROR,
                            error_val("type-error", "ffi/write: expected integer"),
                        )
                    }
                };
                *(ptr as *mut u64) = n;
            }
            TypeDesc::Float => {
                let f = match value
                    .as_float()
                    .or_else(|| value.as_int().map(|i| i as f64))
                {
                    Some(f) => f,
                    None => {
                        return (
                            SIG_ERROR,
                            error_val("type-error", "ffi/write: expected number"),
                        )
                    }
                };
                *(ptr as *mut f32) = f as f32;
            }
            TypeDesc::Double => {
                let f = match value
                    .as_float()
                    .or_else(|| value.as_int().map(|i| i as f64))
                {
                    Some(f) => f,
                    None => {
                        return (
                            SIG_ERROR,
                            error_val("type-error", "ffi/write: expected number"),
                        )
                    }
                };
                *(ptr as *mut f64) = f;
            }
            TypeDesc::Bool => {
                *(ptr as *mut std::ffi::c_int) = if value.is_truthy() { 1 } else { 0 };
            }
            TypeDesc::Ptr => {
                let p = if value.is_nil() {
                    0usize
                } else if let Some(a) = value.as_pointer() {
                    a
                } else {
                    return (
                        SIG_ERROR,
                        error_val("type-error", "ffi/write: expected pointer"),
                    );
                };
                *(ptr as *mut usize) = p;
            }
            TypeDesc::Void => {
                return (
                    SIG_ERROR,
                    error_val("ffi-error", "ffi/write: cannot write void"),
                )
            }
            TypeDesc::Str => {
                return (
                    SIG_ERROR,
                    error_val("ffi-error", "ffi/write: use ptr type for writing pointers"),
                )
            }
            TypeDesc::Struct(_) | TypeDesc::Array(_, _) => {
                return (
                    SIG_ERROR,
                    error_val("ffi-error", "ffi/write: struct/array not yet supported"),
                )
            }
        }
    }
    (SIG_OK, Value::NIL)
}

// ── PRIMITIVES table ────────────────────────────────────────────────

pub const PRIMITIVES: &[PrimitiveDef] = &[
    PrimitiveDef {
        name: "ffi/native",
        func: prim_ffi_native,
        effect: Effect::ffi_raises(),
        arity: Arity::Exact(1),
        doc: "Load a shared library. Pass nil for the current process.",
        params: &["path"],
        category: "ffi",
        example: "(ffi/native \"libm.so.6\")",
        aliases: &[],
    },
    PrimitiveDef {
        name: "ffi/lookup",
        func: prim_ffi_lookup,
        effect: Effect::ffi_raises(),
        arity: Arity::Exact(2),
        doc: "Look up a symbol in a loaded library.",
        params: &["lib", "name"],
        category: "ffi",
        example: "(ffi/lookup lib \"strlen\")",
        aliases: &[],
    },
    PrimitiveDef {
        name: "ffi/signature",
        func: prim_ffi_signature,
        effect: Effect::raises(),
        arity: Arity::Exact(2),
        doc: "Create a reified function signature.",
        params: &["return-type", "arg-types"],
        category: "ffi",
        example: "(ffi/signature :double [:double])",
        aliases: &[],
    },
    PrimitiveDef {
        name: "ffi/call",
        func: prim_ffi_call,
        effect: Effect::ffi_raises(),
        arity: Arity::AtLeast(2),
        doc: "Call a C function through libffi.",
        params: &["fn-ptr", "sig"],
        category: "ffi",
        example: "(ffi/call sqrt-ptr sig 2.0)",
        aliases: &[],
    },
    PrimitiveDef {
        name: "ffi/size",
        func: prim_ffi_size,
        effect: Effect::raises(),
        arity: Arity::Exact(1),
        doc: "Get the size of a C type in bytes.",
        params: &["type"],
        category: "ffi",
        example: "(ffi/size :i32) ;=> 4",
        aliases: &[],
    },
    PrimitiveDef {
        name: "ffi/align",
        func: prim_ffi_align,
        effect: Effect::raises(),
        arity: Arity::Exact(1),
        doc: "Get the alignment of a C type in bytes.",
        params: &["type"],
        category: "ffi",
        example: "(ffi/align :double) ;=> 8",
        aliases: &[],
    },
    PrimitiveDef {
        name: "ffi/malloc",
        func: prim_ffi_malloc,
        effect: Effect::ffi_raises(),
        arity: Arity::Exact(1),
        doc: "Allocate C memory.",
        params: &["size"],
        category: "ffi",
        example: "(ffi/malloc 100)",
        aliases: &[],
    },
    PrimitiveDef {
        name: "ffi/free",
        func: prim_ffi_free,
        effect: Effect::ffi_raises(),
        arity: Arity::Exact(1),
        doc: "Free C memory.",
        params: &["ptr"],
        category: "ffi",
        example: "(ffi/free ptr)",
        aliases: &[],
    },
    PrimitiveDef {
        name: "ffi/read",
        func: prim_ffi_read,
        effect: Effect::ffi_raises(),
        arity: Arity::Exact(2),
        doc: "Read a typed value from C memory.",
        params: &["ptr", "type"],
        category: "ffi",
        example: "(ffi/read ptr :i32)",
        aliases: &[],
    },
    PrimitiveDef {
        name: "ffi/write",
        func: prim_ffi_write,
        effect: Effect::ffi_raises(),
        arity: Arity::Exact(3),
        doc: "Write a typed value to C memory.",
        params: &["ptr", "type", "value"],
        category: "ffi",
        example: "(ffi/write ptr :i32 42)",
        aliases: &[],
    },
];

// ── Tests ───────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ffi_size() {
        let result = prim_ffi_size(&[Value::keyword("i32")]);
        assert_eq!(result.0, SIG_OK);
        assert_eq!(result.1.as_int(), Some(4));
    }

    #[test]
    fn test_ffi_size_void() {
        let result = prim_ffi_size(&[Value::keyword("void")]);
        assert_eq!(result.0, SIG_OK);
        assert!(result.1.is_nil());
    }

    #[test]
    fn test_ffi_size_unknown_type() {
        let result = prim_ffi_size(&[Value::keyword("nonsense")]);
        assert_eq!(result.0, SIG_ERROR);
    }

    #[test]
    fn test_ffi_align() {
        let result = prim_ffi_align(&[Value::keyword("double")]);
        assert_eq!(result.0, SIG_OK);
        assert_eq!(result.1.as_int(), Some(8));
    }

    #[test]
    fn test_ffi_signature() {
        let result = prim_ffi_signature(&[
            Value::keyword("double"),
            Value::array(vec![Value::keyword("double")]),
        ]);
        assert_eq!(result.0, SIG_OK);
        assert!(result.1.as_ffi_signature().is_some());
    }

    #[test]
    fn test_ffi_signature_unknown_ret() {
        let result = prim_ffi_signature(&[Value::keyword("bad"), Value::array(vec![])]);
        assert_eq!(result.0, SIG_ERROR);
    }

    #[test]
    fn test_ffi_malloc_free() {
        let result = prim_ffi_malloc(&[Value::int(100)]);
        assert_eq!(result.0, SIG_OK);
        assert!(result.1.as_pointer().is_some());
        let ptr_val = result.1;

        let free_result = prim_ffi_free(&[ptr_val]);
        assert_eq!(free_result.0, SIG_OK);
    }

    #[test]
    fn test_ffi_free_nil() {
        let result = prim_ffi_free(&[Value::NIL]);
        assert_eq!(result.0, SIG_OK);
    }

    #[test]
    fn test_ffi_malloc_zero() {
        let result = prim_ffi_malloc(&[Value::int(0)]);
        assert_eq!(result.0, SIG_ERROR);
    }

    #[test]
    fn test_ffi_read_write_i32() {
        let alloc_result = prim_ffi_malloc(&[Value::int(4)]);
        assert_eq!(alloc_result.0, SIG_OK);
        let ptr = alloc_result.1;

        let write_result = prim_ffi_write(&[ptr, Value::keyword("i32"), Value::int(42)]);
        assert_eq!(write_result.0, SIG_OK);

        let read_result = prim_ffi_read(&[ptr, Value::keyword("i32")]);
        assert_eq!(read_result.0, SIG_OK);
        assert_eq!(read_result.1.as_int(), Some(42));

        prim_ffi_free(&[ptr]);
    }

    #[test]
    fn test_ffi_read_write_double() {
        let alloc_result = prim_ffi_malloc(&[Value::int(8)]);
        assert_eq!(alloc_result.0, SIG_OK);
        let ptr = alloc_result.1;

        let write_result = prim_ffi_write(&[ptr, Value::keyword("double"), Value::float(1.234)]);
        assert_eq!(write_result.0, SIG_OK);

        let read_result = prim_ffi_read(&[ptr, Value::keyword("double")]);
        assert_eq!(read_result.0, SIG_OK);
        assert_eq!(read_result.1.as_float(), Some(1.234));

        prim_ffi_free(&[ptr]);
    }

    #[test]
    fn test_ffi_read_null_error() {
        let result = prim_ffi_read(&[Value::NIL, Value::keyword("i32")]);
        assert_eq!(result.0, SIG_ERROR);
    }

    #[test]
    fn test_ffi_call_arity_error() {
        let result = prim_ffi_call(&[]);
        assert_eq!(result.0, SIG_ERROR);
    }

    #[test]
    fn test_ffi_native_wrong_type() {
        let result = prim_ffi_native(&[Value::int(42)]);
        assert_eq!(result.0, SIG_ERROR);
    }
}
