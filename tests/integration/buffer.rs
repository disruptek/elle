// Buffer type tests
//
// Tests for the mutable buffer type (@"..." literals and operations).

use crate::common::eval_source;
use elle::Value;

#[test]
fn test_buffer_literal() {
    let result = eval_source(r#"@"hello""#).unwrap();
    assert!(result.is_buffer());
}

#[test]
fn test_buffer_empty() {
    let result = eval_source(r#"@"""#).unwrap();
    assert!(result.is_buffer());
    let buf = result.as_buffer().unwrap();
    assert_eq!(buf.borrow().len(), 0);
}

#[test]
fn test_buffer_display() {
    let result = eval_source(r#"@"hello""#).unwrap();
    let display = format!("{}", result);
    assert_eq!(display, r#"@"hello""#);
}

#[test]
fn test_buffer_display_empty() {
    let result = eval_source(r#"@"""#).unwrap();
    let display = format!("{}", result);
    assert_eq!(display, r#"@"""#);
}

#[test]
fn test_buffer_constructor() {
    let result = eval_source("(buffer)").unwrap();
    assert!(result.is_buffer());
    let buf = result.as_buffer().unwrap();
    assert_eq!(buf.borrow().len(), 0);
}

#[test]
fn test_buffer_constructor_with_bytes() {
    let result = eval_source("(buffer 72 101 108 108 111)").unwrap();
    assert!(result.is_buffer());
    let buf = result.as_buffer().unwrap();
    let borrowed = buf.borrow();
    assert_eq!(borrowed.len(), 5);
    assert_eq!(borrowed[0], 72); // 'H'
    assert_eq!(borrowed[1], 101); // 'e'
}

#[test]
fn test_string_to_buffer() {
    let result = eval_source(r#"(string->buffer "hello")"#).unwrap();
    assert!(result.is_buffer());
    let buf = result.as_buffer().unwrap();
    let borrowed = buf.borrow();
    assert_eq!(borrowed.len(), 5);
    assert_eq!(&borrowed[..], b"hello");
}

#[test]
fn test_buffer_to_string() {
    let result = eval_source(r#"(buffer->string @"hello")"#).unwrap();
    assert!(result.is_string());
    assert_eq!(result.as_string().unwrap(), "hello");
}

#[test]
fn test_buffer_to_string_empty() {
    let result = eval_source(r#"(buffer->string @"")"#).unwrap();
    assert!(result.is_string());
    assert_eq!(result.as_string().unwrap(), "");
}

#[test]
fn test_buffer_predicate() {
    let result = eval_source(r#"(buffer? @"hello")"#).unwrap();
    assert_eq!(result, Value::bool(true));
}

#[test]
fn test_buffer_predicate_false() {
    let result = eval_source(r#"(buffer? "hello")"#).unwrap();
    assert_eq!(result, Value::bool(false));
}

#[test]
fn test_buffer_get() {
    // 'h' is 104 in ASCII
    let result = eval_source(r#"(get @"hello" 0)"#).unwrap();
    assert_eq!(result.as_int(), Some(104));
}

#[test]
fn test_buffer_get_out_of_bounds() {
    let result = eval_source(r#"(get @"hello" 100)"#).unwrap();
    assert_eq!(result, Value::NIL);
}

#[test]
fn test_buffer_get_with_default() {
    let result = eval_source(r#"(get @"hello" 100 99)"#).unwrap();
    assert_eq!(result.as_int(), Some(99));
}

#[test]
fn test_buffer_put() {
    let result = eval_source(r#"(begin (var b @"hello") (put b 0 88) b)"#).unwrap();
    assert!(result.is_buffer());
    let buf = result.as_buffer().unwrap();
    let borrowed = buf.borrow();
    assert_eq!(borrowed[0], 88); // 'X'
    assert_eq!(borrowed[1], 101); // 'e'
}

#[test]
fn test_buffer_push() {
    let result = eval_source(r#"(begin (var b @"hi") (push b 33) b)"#).unwrap();
    assert!(result.is_buffer());
    let buf = result.as_buffer().unwrap();
    let borrowed = buf.borrow();
    assert_eq!(borrowed.len(), 3);
    assert_eq!(borrowed[2], 33); // '!'
}

#[test]
fn test_buffer_pop() {
    let result = eval_source(r#"(begin (var b @"hi") (pop b))"#).unwrap();
    assert_eq!(result.as_int(), Some(105)); // 'i'
}

#[test]
fn test_buffer_pop_empty() {
    let result = eval_source(r#"(begin (var b @"") (pop b))"#);
    // Should error on empty pop
    assert!(result.is_err());
}

#[test]
fn test_buffer_length() {
    let result = eval_source(r#"(length @"hello")"#).unwrap();
    assert_eq!(result.as_int(), Some(5));
}

#[test]
fn test_buffer_length_empty() {
    let result = eval_source(r#"(length @"")"#).unwrap();
    assert_eq!(result.as_int(), Some(0));
}

#[test]
fn test_buffer_empty_predicate() {
    let result = eval_source(r#"(empty? @"")"#).unwrap();
    assert_eq!(result, Value::bool(true));
}

#[test]
fn test_buffer_empty_predicate_false() {
    let result = eval_source(r#"(empty? @"hello")"#).unwrap();
    assert_eq!(result, Value::bool(false));
}

#[test]
fn test_buffer_append() {
    let result = eval_source(r#"(begin (var b @"hello") (append b @" world") b)"#).unwrap();
    assert!(result.is_buffer());
    let buf = result.as_buffer().unwrap();
    let borrowed = buf.borrow();
    assert_eq!(borrowed.len(), 11);
    assert_eq!(&borrowed[..], b"hello world");
}

#[test]
fn test_buffer_concat() {
    let result = eval_source(r#"(concat @"hello" @" world")"#).unwrap();
    assert!(result.is_buffer());
    let buf = result.as_buffer().unwrap();
    let borrowed = buf.borrow();
    assert_eq!(borrowed.len(), 11);
    assert_eq!(&borrowed[..], b"hello world");
}

#[test]
fn test_buffer_roundtrip() {
    let result = eval_source(r#"(buffer->string (string->buffer "hello"))"#).unwrap();
    assert_eq!(result.as_string().unwrap(), "hello");
}

#[test]
fn test_buffer_literal_roundtrip() {
    let result = eval_source(r#"(buffer->string @"hello")"#).unwrap();
    assert_eq!(result.as_string().unwrap(), "hello");
}

#[test]
fn test_buffer_insert() {
    let result = eval_source(r#"(begin (var b @"hllo") (insert b 1 101) b)"#).unwrap();
    assert!(result.is_buffer());
    let buf = result.as_buffer().unwrap();
    let borrowed = buf.borrow();
    assert_eq!(&borrowed[..], b"hello");
}

#[test]
fn test_buffer_remove() {
    let result = eval_source(r#"(begin (var b @"hello") (remove b 1) b)"#).unwrap();
    assert!(result.is_buffer());
    let buf = result.as_buffer().unwrap();
    let borrowed = buf.borrow();
    assert_eq!(&borrowed[..], b"hllo");
}

#[test]
fn test_buffer_remove_multiple() {
    let result = eval_source(r#"(begin (var b @"hello") (remove b 1 2) b)"#).unwrap();
    assert!(result.is_buffer());
    let buf = result.as_buffer().unwrap();
    let borrowed = buf.borrow();
    assert_eq!(&borrowed[..], b"hlo");
}

#[test]
fn test_buffer_popn() {
    // "hello" = [104, 101, 108, 108, 111]
    // popn 2 removes last 2 bytes: [108, 111] ('l', 'o')
    let result = eval_source(r#"(begin (var b @"hello") (popn b 2))"#).unwrap();
    assert!(result.is_buffer());
    let buf = result.as_buffer().unwrap();
    let borrowed = buf.borrow();
    assert_eq!(borrowed.len(), 2);
    assert_eq!(borrowed[0], 108); // 'l'
    assert_eq!(borrowed[1], 111); // 'o'
}
