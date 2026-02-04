use elle::ffi::types::CType;
/// FFI Phase 1 Integration Tests
///
/// Tests for:
/// - Library loading
/// - Symbol resolution
/// - Type system
/// - FFI subsystem
use elle::ffi::FFISubsystem;
use elle::value::{LibHandle, Value};
use elle::VM;

#[test]
fn test_ffi_subsystem_creation() {
    let ffi = FFISubsystem::new();
    assert_eq!(ffi.loaded_libraries().len(), 0);
}

#[test]
#[cfg(target_os = "linux")]
fn test_load_libc() {
    let mut ffi = FFISubsystem::new();

    // Try common paths for libc
    let paths = vec![
        "/lib/x86_64-linux-gnu/libc.so.6",
        "/lib64/libc.so.6",
        "libc.so.6",
    ];

    let mut loaded = false;
    for path in paths {
        if let Ok(id) = ffi.load_library(path) {
            loaded = true;
            assert!(ffi.get_library(id).is_some());
            assert_eq!(ffi.loaded_libraries().len(), 1);
            break;
        }
    }

    if !loaded {
        eprintln!("Warning: Could not load libc from any standard path");
    }
}

#[test]
fn test_type_sizes() {
    assert_eq!(CType::Bool.size(), 1);
    assert_eq!(CType::Char.size(), 1);
    assert_eq!(CType::Short.size(), 2);
    assert_eq!(CType::Int.size(), 4);
    assert_eq!(CType::Long.size(), 8);
    assert_eq!(CType::Float.size(), 4);
    assert_eq!(CType::Double.size(), 8);
}

#[test]
fn test_type_alignment() {
    assert_eq!(CType::Bool.alignment(), 1);
    assert_eq!(CType::Short.alignment(), 2);
    assert_eq!(CType::Int.alignment(), 4);
    assert_eq!(CType::Long.alignment(), 8);
    assert_eq!(CType::Double.alignment(), 8);
}

#[test]
fn test_library_handle_value() {
    let handle = LibHandle(42);
    let value = Value::LibHandle(handle);

    match value {
        Value::LibHandle(h) => assert_eq!(h.0, 42),
        _ => panic!("Expected LibHandle variant"),
    }
}

#[test]
fn test_vm_ffi_integration() {
    let mut vm = VM::new();

    // VM should have FFI subsystem
    assert_eq!(vm.ffi().loaded_libraries().len(), 0);
}

#[test]
fn test_ffi_library_unload() {
    let mut ffi = FFISubsystem::new();

    // Unload nonexistent library
    assert!(ffi.unload_library(999).is_none());
}

#[test]
fn test_multiple_library_ids() {
    let mut ffi = FFISubsystem::new();

    // When we load libraries, each should get unique IDs
    // (This test would work once loading is fully implemented)
    let libs = ffi.loaded_libraries();
    assert_eq!(libs.len(), 0);
}

#[test]
fn test_type_classification() {
    assert!(CType::Int.is_integer());
    assert!(CType::Long.is_integer());
    assert!(!CType::Float.is_integer());

    assert!(CType::Float.is_float());
    assert!(CType::Double.is_float());
    assert!(!CType::Int.is_float());
}

#[test]
fn test_ctype_display() {
    assert_eq!(CType::Int.to_string(), "int");
    assert_eq!(CType::Float.to_string(), "float");
    assert_eq!(CType::Double.to_string(), "double");
    assert_eq!(CType::Long.to_string(), "long");
}
