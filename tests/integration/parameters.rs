// Parameter type tests
//
// Tests for Racket-style dynamic parameters (make-parameter, parameter?, callability)
// and the parameterize special form.

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

// === parameterize special form ===

#[test]
fn test_parameterize_basic() {
    let result = eval_source("(let ((p (make-parameter 1))) (parameterize ((p 2)) (p)))").unwrap();
    assert_eq!(result, Value::int(2));
}

#[test]
fn test_parameterize_reverts_after() {
    let result = eval_source(
        "(let ((p (make-parameter 1)))
           (parameterize ((p 2)) (p))
           (p))",
    )
    .unwrap();
    assert_eq!(result, Value::int(1));
}

#[test]
fn test_parameterize_nested_shadows() {
    let result = eval_source(
        "(let ((p (make-parameter 1)))
           (parameterize ((p 2))
             (parameterize ((p 3))
               (p))))",
    )
    .unwrap();
    assert_eq!(result, Value::int(3));
}

#[test]
fn test_parameterize_nested_outer_visible() {
    let result = eval_source(
        "(let ((p (make-parameter 1)))
           (parameterize ((p 2))
             (parameterize ((p 3))
               (p))
             (p)))",
    )
    .unwrap();
    assert_eq!(result, Value::int(2));
}

#[test]
fn test_parameterize_multiple_bindings() {
    let result = eval_source(
        "(let ((a (make-parameter 1))
               (b (make-parameter 10)))
           (parameterize ((a 2) (b 20))
             (+ (a) (b))))",
    )
    .unwrap();
    assert_eq!(result, Value::int(22));
}

#[test]
fn test_parameterize_body_is_begin() {
    let result = eval_source(
        "(let ((p (make-parameter 0)))
           (parameterize ((p 42))
             (def x (p))
             x))",
    )
    .unwrap();
    assert_eq!(result, Value::int(42));
}

#[test]
fn test_parameterize_non_parameter_errors() {
    let result = eval_source("(parameterize ((42 1)) 0)");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("not a parameter"),
        "expected 'not a parameter' error, got: {}",
        err
    );
}

// === fiber inheritance ===

#[test]
fn test_fiber_inherits_parent_parameterize() {
    let result = eval_source(
        "(let ((p (make-parameter 1)))
           (parameterize ((p 42))
             (let ((f (fiber/new (fn () (p)) 1)))
               (fiber/resume f nil)
               (fiber/value f))))",
    )
    .unwrap();
    assert_eq!(result, Value::int(42));
}
