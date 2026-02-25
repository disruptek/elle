// Property tests for the FFI module.
//
// Tests cover: pointer NaN-boxing invariants, marshal range checking,
// memory read-write roundtrips, TypeDesc size/align consistency,
// and string marshalling edge cases.

use elle::ffi::marshal::MarshalledArg;
use elle::ffi::types::TypeDesc;
use elle::primitives::ffi::{prim_ffi_free, prim_ffi_malloc, prim_ffi_read, prim_ffi_write};
use elle::value::fiber::SIG_OK;
use elle::value::repr::{INT_MAX, INT_MIN};
use elle::Value;
use proptest::prelude::*;

// =========================================================================
// A. Pointer NaN-boxing invariants
// =========================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(1000))]

    // Pointer roundtrip: any 48-bit address survives the Value round-trip
    #[test]
    fn pointer_roundtrip(addr in 1usize..=0x0000_FFFF_FFFF_FFFFusize) {
        let v = Value::pointer(addr);
        prop_assert_eq!(v.as_pointer(), Some(addr));
    }

    // Pointer type discrimination: pointers are ONLY pointers
    #[test]
    fn pointer_is_only_pointer(addr in 1usize..=0x0000_FFFF_FFFF_FFFFusize) {
        let v = Value::pointer(addr);
        prop_assert!(v.is_pointer());
        prop_assert!(!v.is_int());
        prop_assert!(!v.is_float());
        prop_assert!(!v.is_nil());
        prop_assert!(!v.is_bool());
        prop_assert!(!v.is_symbol());
        prop_assert!(!v.is_keyword());
        prop_assert!(!v.is_heap());
        prop_assert!(!v.is_empty_list());
    }

    // Pointer truthiness: all non-null pointers are truthy
    #[test]
    fn pointer_is_truthy(addr in 1usize..=0x0000_FFFF_FFFF_FFFFusize) {
        prop_assert!(Value::pointer(addr).is_truthy());
    }

    // Pointer equality: same address -> equal values
    #[test]
    fn pointer_eq_same_addr(addr in 1usize..=0x0000_FFFF_FFFF_FFFFusize) {
        prop_assert_eq!(Value::pointer(addr), Value::pointer(addr));
    }

    // Pointer inequality: different addresses -> different values
    #[test]
    fn pointer_neq_diff_addr(
        a in 1usize..=0x0000_FFFF_FFFF_FFFFusize,
        b in 1usize..=0x0000_FFFF_FFFF_FFFFusize,
    ) {
        prop_assume!(a != b);
        prop_assert_ne!(Value::pointer(a), Value::pointer(b));
    }
}

// NULL pointer becomes NIL (not inside proptest -- deterministic)
#[test]
fn pointer_null_is_nil() {
    assert_eq!(Value::pointer(0), Value::NIL);
}

// NIL is NOT a pointer (as_pointer returns None)
#[test]
fn nil_is_not_pointer() {
    assert_eq!(Value::NIL.as_pointer(), None);
    assert!(!Value::NIL.is_pointer());
}

// =========================================================================
// B. Marshal integer range checking
// =========================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(500))]

    // i8 in-range: -128..127 accepted
    #[test]
    fn marshal_i8_in_range(n in -128i64..=127) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::I8).is_ok());
    }

    // i8 out-of-range: rejected
    #[test]
    fn marshal_i8_out_of_range(n in prop_oneof![
        (INT_MIN..=-129i64),
        (128i64..=INT_MAX),
    ]) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::I8).is_err());
    }

    // u8 in-range: 0..255 accepted
    #[test]
    fn marshal_u8_in_range(n in 0i64..=255) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::U8).is_ok());
    }

    // u8 out-of-range: negative or >255
    #[test]
    fn marshal_u8_out_of_range(n in prop_oneof![
        (INT_MIN..=-1i64),
        (256i64..=INT_MAX),
    ]) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::U8).is_err());
    }

    // i16 in-range
    #[test]
    fn marshal_i16_in_range(n in -32768i64..=32767) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::I16).is_ok());
    }

    // u16 in-range
    #[test]
    fn marshal_u16_in_range(n in 0i64..=65535) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::U16).is_ok());
    }

    // i32 in-range
    #[test]
    fn marshal_i32_in_range(n in i32::MIN as i64..=i32::MAX as i64) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::I32).is_ok());
    }

    // i32 out-of-range
    #[test]
    fn marshal_i32_out_of_range(n in prop_oneof![
        (INT_MIN..=i32::MIN as i64 - 1),
        (i32::MAX as i64 + 1..=INT_MAX),
    ]) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::I32).is_err());
    }

    // u32 in-range
    #[test]
    fn marshal_u32_in_range(n in 0i64..=u32::MAX as i64) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::U32).is_ok());
    }

    // i64 always in-range (Elle ints are i48 which fits in i64)
    #[test]
    fn marshal_i64_always_ok(n in INT_MIN..=INT_MAX) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::I64).is_ok());
    }

    // Float marshalling: any float is accepted
    #[test]
    fn marshal_float_from_float(f in prop::num::f64::NORMAL) {
        let v = Value::float(f);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::Float).is_ok());
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::Double).is_ok());
    }

    // Float marshalling: integers also accepted as floats
    #[test]
    fn marshal_float_from_int(n in INT_MIN..=INT_MAX) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::Float).is_ok());
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::Double).is_ok());
    }

    // Bool marshalling: any value accepted (truthiness-based)
    #[test]
    fn marshal_bool_from_int(n in INT_MIN..=INT_MAX) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::Bool).is_ok());
    }

    // Pointer marshalling: actual pointers accepted
    #[test]
    fn marshal_ptr_from_pointer(addr in 1usize..=0x0000_FFFF_FFFF_FFFFusize) {
        let v = Value::pointer(addr);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::Ptr).is_ok());
    }

    // Pointer marshalling: non-pointer/non-nil rejected
    #[test]
    fn marshal_ptr_from_int_rejected(n in INT_MIN..=INT_MAX) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::Ptr).is_err());
    }
}

// Nil accepted as pointer (becomes NULL)
#[test]
fn marshal_ptr_nil_accepted() {
    assert!(MarshalledArg::new(&Value::NIL, &TypeDesc::Ptr).is_ok());
}

// =========================================================================
// C. Memory read-write roundtrip
// =========================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(200))]

    // i32 write-read roundtrip
    #[test]
    fn memory_roundtrip_i32(n in i32::MIN as i64..=i32::MAX as i64) {
        let alloc = prim_ffi_malloc(&[Value::int(4)]);
        prop_assert_eq!(alloc.0, SIG_OK);
        let ptr = alloc.1;

        let write = prim_ffi_write(&[ptr, Value::keyword("i32"), Value::int(n)]);
        prop_assert_eq!(write.0, SIG_OK);

        let read = prim_ffi_read(&[ptr, Value::keyword("i32")]);
        prop_assert_eq!(read.0, SIG_OK);
        prop_assert_eq!(read.1.as_int(), Some(n));

        prim_ffi_free(&[ptr]);
    }

    // i64 write-read roundtrip
    #[test]
    fn memory_roundtrip_i64(n in INT_MIN..=INT_MAX) {
        let alloc = prim_ffi_malloc(&[Value::int(8)]);
        prop_assert_eq!(alloc.0, SIG_OK);
        let ptr = alloc.1;

        let write = prim_ffi_write(&[ptr, Value::keyword("i64"), Value::int(n)]);
        prop_assert_eq!(write.0, SIG_OK);

        let read = prim_ffi_read(&[ptr, Value::keyword("i64")]);
        prop_assert_eq!(read.0, SIG_OK);
        prop_assert_eq!(read.1.as_int(), Some(n));

        prim_ffi_free(&[ptr]);
    }

    // double write-read roundtrip
    #[test]
    fn memory_roundtrip_double(f in prop::num::f64::NORMAL) {
        let alloc = prim_ffi_malloc(&[Value::int(8)]);
        prop_assert_eq!(alloc.0, SIG_OK);
        let ptr = alloc.1;

        let write = prim_ffi_write(&[ptr, Value::keyword("double"), Value::float(f)]);
        prop_assert_eq!(write.0, SIG_OK);

        let read = prim_ffi_read(&[ptr, Value::keyword("double")]);
        prop_assert_eq!(read.0, SIG_OK);
        let readback = read.1.as_float().unwrap();
        prop_assert_eq!(readback.to_bits(), f.to_bits(),
            "double roundtrip failed: wrote {} got {}", f, readback);

        prim_ffi_free(&[ptr]);
    }

    // u8 write-read roundtrip
    #[test]
    fn memory_roundtrip_u8(n in 0u8..=255) {
        let alloc = prim_ffi_malloc(&[Value::int(1)]);
        prop_assert_eq!(alloc.0, SIG_OK);
        let ptr = alloc.1;

        let write = prim_ffi_write(&[ptr, Value::keyword("u8"), Value::int(n as i64)]);
        prop_assert_eq!(write.0, SIG_OK);

        let read = prim_ffi_read(&[ptr, Value::keyword("u8")]);
        prop_assert_eq!(read.0, SIG_OK);
        prop_assert_eq!(read.1.as_int(), Some(n as i64));

        prim_ffi_free(&[ptr]);
    }

    // pointer write-read roundtrip
    #[test]
    fn memory_roundtrip_ptr(addr in 0usize..=0x0000_FFFF_FFFF_FFFFusize) {
        let alloc = prim_ffi_malloc(&[Value::int(8)]);
        prop_assert_eq!(alloc.0, SIG_OK);
        let ptr = alloc.1;

        // Write: nil for 0, pointer for nonzero
        let val = if addr == 0 { Value::NIL } else { Value::pointer(addr) };
        let write = prim_ffi_write(&[ptr, Value::keyword("ptr"), val]);
        prop_assert_eq!(write.0, SIG_OK);

        let read = prim_ffi_read(&[ptr, Value::keyword("ptr")]);
        prop_assert_eq!(read.0, SIG_OK);
        // NULL -> Value::pointer(0) -> Value::NIL
        if addr == 0 {
            prop_assert!(read.1.is_nil() || read.1.as_pointer() == Some(0),
                "reading NULL pointer should give nil, got {:?}", read.1);
        } else {
            prop_assert_eq!(read.1.as_pointer(), Some(addr));
        }

        prim_ffi_free(&[ptr]);
    }
}

// =========================================================================
// D. TypeDesc size/align consistency
// =========================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    // Alignment is always a power of 2 (for non-void types)
    #[test]
    fn type_align_is_power_of_two(idx in 0usize..22) {
        let types = [
            TypeDesc::Bool, TypeDesc::I8, TypeDesc::U8, TypeDesc::I16, TypeDesc::U16,
            TypeDesc::I32, TypeDesc::U32, TypeDesc::I64, TypeDesc::U64,
            TypeDesc::Float, TypeDesc::Double,
            TypeDesc::Int, TypeDesc::UInt, TypeDesc::Long, TypeDesc::ULong,
            TypeDesc::Char, TypeDesc::UChar, TypeDesc::Short, TypeDesc::UShort,
            TypeDesc::Size, TypeDesc::SSize, TypeDesc::Ptr,
        ];
        if idx < types.len() {
            let align = types[idx].align().unwrap();
            prop_assert!(align.is_power_of_two(),
                "alignment of {:?} is {} (not power of 2)", types[idx], align);
        }
    }

    // Size >= alignment for all types
    #[test]
    fn type_size_ge_align(idx in 0usize..22) {
        let types = [
            TypeDesc::Bool, TypeDesc::I8, TypeDesc::U8, TypeDesc::I16, TypeDesc::U16,
            TypeDesc::I32, TypeDesc::U32, TypeDesc::I64, TypeDesc::U64,
            TypeDesc::Float, TypeDesc::Double,
            TypeDesc::Int, TypeDesc::UInt, TypeDesc::Long, TypeDesc::ULong,
            TypeDesc::Char, TypeDesc::UChar, TypeDesc::Short, TypeDesc::UShort,
            TypeDesc::Size, TypeDesc::SSize, TypeDesc::Ptr,
        ];
        if idx < types.len() {
            let size = types[idx].size().unwrap();
            let align = types[idx].align().unwrap();
            prop_assert!(size >= align,
                "{:?}: size {} < align {}", types[idx], size, align);
        }
    }
}

// =========================================================================
// E. String marshalling edge cases
// =========================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(500))]

    // Any ASCII string without nulls marshals successfully
    #[test]
    fn marshal_string_ascii(s in "[a-zA-Z0-9 ]{0,100}") {
        let v = Value::string(s);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::Str).is_ok());
    }

    // Strings with embedded nulls are rejected
    #[test]
    fn marshal_string_with_null(
        prefix in "[a-zA-Z]{1,10}",
        suffix in "[a-zA-Z]{1,10}",
    ) {
        let s = format!("{}\0{}", prefix, suffix);
        let v = Value::string(s);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::Str).is_err());
    }

    // Non-string values rejected for :string type
    #[test]
    fn marshal_string_rejects_int(n in INT_MIN..=INT_MAX) {
        let v = Value::int(n);
        prop_assert!(MarshalledArg::new(&v, &TypeDesc::Str).is_err());
    }
}
