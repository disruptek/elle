// Parameter type tests
//
// Tests for Racket-style dynamic parameters (make-parameter, parameter?, callability).

use crate::common::eval_source;
use elle::Value;

#[test]
fn test_make_parameter_returns_parameter() {
    let result = eval_source("(make-parameter 42)").unwrap();
    assert!(result.is_parameter());
}

#[test]
fn test_parameter_predicate_true() {
    let result = eval_source("(parameter? (make-parameter 1))").unwrap();
    assert_eq!(result, Value::bool(true));
}

#[test]
fn test_parameter_predicate_false_int() {
    let result = eval_source("(parameter? 42)").unwrap();
    assert_eq!(result, Value::bool(false));
}

#[test]
fn test_parameter_predicate_false_string() {
    let result = eval_source("(parameter? \"hello\")").unwrap();
    assert_eq!(result, Value::bool(false));
}

#[test]
fn test_parameter_predicate_false_closure() {
    let result = eval_source("(parameter? (fn () 1))").unwrap();
    assert_eq!(result, Value::bool(false));
}

#[test]
fn test_parameter_call_reads_default() {
    let result = eval_source("((make-parameter 42))").unwrap();
    assert_eq!(result, Value::int(42));
}

#[test]
fn test_parameter_call_default_string() {
    let result = eval_source("((make-parameter \"hello\"))").unwrap();
    result.with_string(|s| assert_eq!(s, "hello")).unwrap();
}

#[test]
fn test_parameter_call_default_nil() {
    let result = eval_source("((make-parameter nil))").unwrap();
    assert!(result.is_nil());
}

#[test]
fn test_parameter_call_with_args_errors() {
    let result = eval_source("((make-parameter 42) 1)");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("expected 0 arguments"), "got: {}", err);
}

#[test]
fn test_parameter_via_def() {
    let result = eval_source("(begin (def p (make-parameter 99)) (p))").unwrap();
    assert_eq!(result, Value::int(99));
}

#[test]
fn test_parameter_type_of() {
    let result = eval_source("(type (make-parameter 0))").unwrap();
    assert_eq!(result.as_keyword_name(), Some("parameter"));
}
