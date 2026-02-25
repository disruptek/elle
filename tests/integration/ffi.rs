// End-to-end FFI integration tests.
// These test the full pipeline: Elle source → compiler → VM → libffi → C.

use crate::common::eval_source;
use elle::Value;

// ── Type introspection ──────────────────────────────────────────────

#[test]
fn test_ffi_size_i32() {
    assert_eq!(eval_source("(ffi/size :i32)").unwrap(), Value::int(4));
}

#[test]
fn test_ffi_size_double() {
    assert_eq!(eval_source("(ffi/size :double)").unwrap(), Value::int(8));
}

#[test]
fn test_ffi_size_ptr() {
    assert_eq!(eval_source("(ffi/size :ptr)").unwrap(), Value::int(8));
}

#[test]
fn test_ffi_size_void() {
    assert_eq!(eval_source("(ffi/size :void)").unwrap(), Value::NIL);
}

#[test]
fn test_ffi_align_double() {
    assert_eq!(eval_source("(ffi/align :double)").unwrap(), Value::int(8));
}

// ── Signature creation ──────────────────────────────────────────────

#[test]
fn test_ffi_signature_creation() {
    // Signature is an opaque value — just check it's not an error
    let result = eval_source("(ffi/signature :int [:int])");
    assert!(result.is_ok());
}

#[test]
fn test_ffi_signature_void_no_args() {
    let result = eval_source("(ffi/signature :void [])");
    assert!(result.is_ok());
}

#[test]
fn test_ffi_signature_bad_type() {
    let result = eval_source("(ffi/signature :bad [:int])");
    assert!(result.is_err());
}

// ── Memory management ───────────────────────────────────────────────

#[test]
fn test_ffi_malloc_free() {
    let result = eval_source(
        "
        (def ptr (ffi/malloc 64))
        (ffi/free ptr)
        :ok
    ",
    );
    assert_eq!(result.unwrap(), Value::keyword("ok"));
}

#[test]
fn test_ffi_read_write_roundtrip() {
    let result = eval_source(
        "
        (def ptr (ffi/malloc 4))
        (ffi/write ptr :i32 42)
        (def val (ffi/read ptr :i32))
        (ffi/free ptr)
        val
    ",
    );
    assert_eq!(result.unwrap(), Value::int(42));
}

#[test]
fn test_ffi_read_write_double() {
    let result = eval_source(
        "
        (def ptr (ffi/malloc 8))
        (ffi/write ptr :double 1.234)
        (def val (ffi/read ptr :double))
        (ffi/free ptr)
        val
    ",
    );
    assert_eq!(result.unwrap(), Value::float(1.234));
}

#[test]
fn test_ffi_read_null_error() {
    let result = eval_source("(ffi/read nil :i32)");
    assert!(result.is_err());
}

#[test]
fn test_ffi_malloc_negative_error() {
    let result = eval_source("(ffi/malloc -1)");
    assert!(result.is_err());
}

// ── Library loading and calling ─────────────────────────────────────

#[cfg(target_os = "linux")]
#[test]
fn test_ffi_call_abs() {
    // abs() is in libc
    let result = eval_source(
        r#"
        (def libc (ffi/native "/lib64/libc.so.6"))
        (def abs-ptr (ffi/lookup libc "abs"))
        (def abs-sig (ffi/signature :int [:int]))
        (ffi/call abs-ptr abs-sig -42)
    "#,
    );
    match &result {
        Ok(v) => assert_eq!(*v, Value::int(42)),
        Err(e) => {
            // Try alternative libc path
            let result2 = eval_source(
                r#"
                (def libc (ffi/native "libc.so.6"))
                (def abs-ptr (ffi/lookup libc "abs"))
                (def abs-sig (ffi/signature :int [:int]))
                (ffi/call abs-ptr abs-sig -42)
            "#,
            );
            assert_eq!(
                result2.unwrap(),
                Value::int(42),
                "Neither /lib64/libc.so.6 nor libc.so.6 worked: {}",
                e
            );
        }
    }
}

#[cfg(target_os = "linux")]
#[test]
fn test_ffi_call_strlen() {
    let result = eval_source(
        r#"
        (def libc (ffi/native "/lib64/libc.so.6"))
        (def strlen-ptr (ffi/lookup libc "strlen"))
        (def strlen-sig (ffi/signature :size [:string]))
        (ffi/call strlen-ptr strlen-sig "hello")
    "#,
    );
    match &result {
        Ok(v) => assert_eq!(*v, Value::int(5)),
        Err(e) => {
            let result2 = eval_source(
                r#"
                (def libc (ffi/native "libc.so.6"))
                (def strlen-ptr (ffi/lookup libc "strlen"))
                (def strlen-sig (ffi/signature :size [:string]))
                (ffi/call strlen-ptr strlen-sig "hello")
            "#,
            );
            assert_eq!(
                result2.unwrap(),
                Value::int(5),
                "Neither /lib64/libc.so.6 nor libc.so.6 worked: {}",
                e
            );
        }
    }
}

#[cfg(target_os = "linux")]
#[test]
fn test_ffi_call_sqrt() {
    // sqrt() is in libm — tests double argument and double return (float ABI)
    let result = eval_source(
        r#"
        (def libm (ffi/native "/lib64/libm.so.6"))
        (def sqrt-ptr (ffi/lookup libm "sqrt"))
        (def sqrt-sig (ffi/signature :double [:double]))
        (def result (ffi/call sqrt-ptr sqrt-sig 4.0))
        (= result 2.0)
    "#,
    );
    match &result {
        Ok(v) => assert_eq!(*v, Value::TRUE),
        Err(e) => {
            let result2 = eval_source(
                r#"
                (def libm (ffi/native "libm.so.6"))
                (def sqrt-ptr (ffi/lookup libm "sqrt"))
                (def sqrt-sig (ffi/signature :double [:double]))
                (def result (ffi/call sqrt-ptr sqrt-sig 4.0))
                (= result 2.0)
            "#,
            );
            assert_eq!(
                result2.unwrap(),
                Value::TRUE,
                "Neither /lib64/libm.so.6 nor libm.so.6 worked: {}",
                e
            );
        }
    }
}

// ── Error handling ──────────────────────────────────────────────────

#[test]
fn test_ffi_native_missing_library() {
    let result = eval_source(r#"(ffi/native "/nonexistent/lib.so")"#);
    assert!(result.is_err());
}

#[test]
fn test_ffi_call_nil_pointer() {
    let result = eval_source(
        r#"
        (def sig (ffi/signature :void []))
        (ffi/call nil sig)
    "#,
    );
    assert!(result.is_err());
}

#[test]
fn test_ffi_call_wrong_arg_count() {
    // Signature says 1 arg, we pass 0
    let result = eval_source(
        r#"
        (def sig (ffi/signature :int [:int]))
        (def ptr (ffi/malloc 1))
        (ffi/call ptr sig)
    "#,
    );
    // This should error because we pass 0 C args but sig expects 1
    // The ffi/call primitive passes call_args = args[2..], which is empty
    assert!(result.is_err());
}
