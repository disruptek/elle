// Bytes and blob type tests
//
// Tests for the immutable bytes and mutable blob types.

use crate::common::eval_source;
use elle::Value;

#[test]
fn test_bytes_constructor() {
    let result = eval_source("(bytes 72 101 108 108 111)").unwrap();
    assert!(result.is_bytes());
    let bytes = result.as_bytes().unwrap();
    assert_eq!(bytes.len(), 5);
    assert_eq!(bytes, &[72, 101, 108, 108, 111]);
}

#[test]
fn test_bytes_empty() {
    let result = eval_source("(bytes)").unwrap();
    assert!(result.is_bytes());
    let bytes = result.as_bytes().unwrap();
    assert_eq!(bytes.len(), 0);
}

#[test]
fn test_blob_constructor() {
    let result = eval_source("(blob 72 101 108 108 111)").unwrap();
    assert!(result.is_blob());
    let blob = result.as_blob().unwrap();
    assert_eq!(blob.borrow().len(), 5);
    assert_eq!(&blob.borrow()[..], &[72, 101, 108, 108, 111]);
}

#[test]
fn test_blob_empty() {
    let result = eval_source("(blob)").unwrap();
    assert!(result.is_blob());
    let blob = result.as_blob().unwrap();
    assert_eq!(blob.borrow().len(), 0);
}

#[test]
fn test_bytes_predicate() {
    let result = eval_source("(bytes? (bytes 1 2 3))").unwrap();
    assert!(result.is_bool());
    assert!(result.as_bool().unwrap());
}

#[test]
fn test_blob_predicate() {
    let result = eval_source("(blob? (blob 1 2 3))").unwrap();
    assert!(result.is_bool());
    assert!(result.as_bool().unwrap());
}

#[test]
fn test_string_to_bytes() {
    let result = eval_source(r#"(string->bytes "hello")"#).unwrap();
    assert!(result.is_bytes());
    let bytes = result.as_bytes().unwrap();
    assert_eq!(bytes, b"hello");
}

#[test]
fn test_string_to_blob() {
    let result = eval_source(r#"(string->blob "hello")"#).unwrap();
    assert!(result.is_blob());
    let blob = result.as_blob().unwrap();
    assert_eq!(&blob.borrow()[..], b"hello");
}

#[test]
fn test_bytes_to_string() {
    let result = eval_source(r#"(bytes->string (bytes 104 105))"#).unwrap();
    assert!(result.is_string());
    assert_eq!(result.as_string().unwrap(), "hi");
}

#[test]
fn test_blob_to_string() {
    let result = eval_source(r#"(blob->string (blob 104 105))"#).unwrap();
    assert!(result.is_string());
    assert_eq!(result.as_string().unwrap(), "hi");
}

#[test]
fn test_bytes_to_hex() {
    let result = eval_source("(bytes->hex (bytes 72 101 108))").unwrap();
    assert!(result.is_string());
    assert_eq!(result.as_string().unwrap(), "48656c");
}

#[test]
fn test_blob_to_hex() {
    let result = eval_source("(blob->hex (blob 72 101 108))").unwrap();
    assert!(result.is_string());
    assert_eq!(result.as_string().unwrap(), "48656c");
}

#[test]
fn test_bytes_length() {
    let result = eval_source("(length (bytes 1 2 3 4 5))").unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int().unwrap(), 5);
}

#[test]
fn test_blob_length() {
    let result = eval_source("(length (blob 1 2 3 4 5))").unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int().unwrap(), 5);
}

#[test]
fn test_bytes_get() {
    let result = eval_source("(get (bytes 72 101 108) 1)").unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int().unwrap(), 101);
}

#[test]
fn test_bytes_get_oob() {
    let result = eval_source("(get (bytes 72 101 108) 10)");
    assert!(result.is_err(), "get on bytes with OOB index should error");
}

#[test]
fn test_blob_get() {
    let result = eval_source("(get (blob 72 101 108) 1)").unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int().unwrap(), 101);
}

#[test]
fn test_blob_get_oob() {
    let result = eval_source("(get (blob 72 101 108) 10)");
    assert!(result.is_err(), "get on blob with OOB index should error");
}

#[test]
fn test_sha256_empty_string() {
    // SHA-256("") = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
    let result = eval_source(r#"(bytes->hex (crypto/sha256 ""))"#).unwrap();
    assert!(result.is_string());
    assert_eq!(
        result.as_string().unwrap(),
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    );
}

#[test]
fn test_sha256_hello() {
    // SHA-256("hello") = 2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
    let result = eval_source(r#"(bytes->hex (crypto/sha256 "hello"))"#).unwrap();
    assert!(result.is_string());
    assert_eq!(
        result.as_string().unwrap(),
        "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
    );
}

#[test]
fn test_hmac_sha256() {
    let result = eval_source(r#"(bytes->hex (crypto/hmac-sha256 "key" "message"))"#).unwrap();
    assert!(result.is_string());
    // Just verify it produces a 64-character hex string (32 bytes)
    let hex = result.as_string().unwrap();
    assert_eq!(hex.len(), 64);
}

#[test]
fn test_uri_encode_simple() {
    let result = eval_source(r#"(uri-encode "hello")"#).unwrap();
    assert!(result.is_string());
    assert_eq!(result.as_string().unwrap(), "hello");
}

#[test]
fn test_uri_encode_space() {
    let result = eval_source(r#"(uri-encode "hello world")"#).unwrap();
    assert!(result.is_string());
    assert_eq!(result.as_string().unwrap(), "hello%20world");
}

#[test]
fn test_uri_encode_special() {
    let result = eval_source(r#"(uri-encode "a/b")"#).unwrap();
    assert!(result.is_string());
    assert_eq!(result.as_string().unwrap(), "a%2Fb");
}

#[test]
fn test_sigv4_demo_runs() {
    // Run the demo and verify it completes without error
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_elle"))
        .arg("demos/aws-sigv4/sigv4.lisp")
        .output()
        .expect("failed to run sigv4 demo");
    assert!(
        output.status.success(),
        "sigv4 demo failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("=== Complete ==="), "demo did not complete");
    // Verify real crypto output (SHA-256 of empty string)
    assert!(
        stdout.contains("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),
        "SHA-256 of empty string incorrect"
    );
}
