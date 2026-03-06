// Integration tests for file-scope compilation (issue #469).
// Tests immutable vs mutable capture behavior at runtime.

use crate::common::eval_source;
use elle::Value;

// ============================================================================
// SECTION 1: Immutable captures (def) — no cell wrapping
// ============================================================================

#[test]
fn test_immutable_def_captured_by_closure() {
    // A def (immutable) binding captured by a closure should work correctly.
    // The value is captured by value, no LocalCell indirection.
    let code = r#"
        (begin
          (def x 42)
          (def f (fn () x))
          (f))
    "#;
    assert_eq!(eval_source(code).unwrap(), Value::int(42));
}

#[test]
fn test_immutable_def_captured_nested() {
    // Immutable capture through multiple nesting levels.
    let code = r#"
        (begin
          (def x 10)
          (def f (fn () (fn () x)))
          ((f)))
    "#;
    assert_eq!(eval_source(code).unwrap(), Value::int(10));
}

#[test]
fn test_immutable_def_multiple_closures() {
    // Multiple closures capturing the same immutable binding.
    let code = r#"
        (begin
          (def x 5)
          (def f (fn () x))
          (def g (fn () (+ x x)))
          (+ (f) (g)))
    "#;
    assert_eq!(eval_source(code).unwrap(), Value::int(15));
}

#[test]
fn test_immutable_let_captured_by_closure() {
    // let bindings are immutable by default and captured by closures.
    let code = r#"
        (let ((x 99))
          (let ((f (fn () x)))
            (f)))
    "#;
    assert_eq!(eval_source(code).unwrap(), Value::int(99));
}

// ============================================================================
// SECTION 2: Mutable captures (var) — cell wrapping required
// ============================================================================

#[test]
fn test_mutable_var_captured_by_closure() {
    // A var (mutable) binding captured by a closure needs a cell.
    // The closure must see mutations.
    let code = r#"
        (begin
          (var x 1)
          (def f (fn () (begin (set x 2) x)))
          (f))
    "#;
    assert_eq!(eval_source(code).unwrap(), Value::int(2));
}

#[test]
fn test_mutable_var_shared_between_closures() {
    // Two closures sharing a mutable capture via cell.
    let code = r#"
        (begin
          (var x 0)
          (def inc (fn () (set x (+ x 1))))
          (def get (fn () x))
          (inc)
          (inc)
          (get))
    "#;
    assert_eq!(eval_source(code).unwrap(), Value::int(2));
}

#[test]
fn test_mutable_var_mutation_visible_after_call() {
    // Mutation through closure is visible in the enclosing scope.
    let code = r#"
        (begin
          (var x 0)
          (def inc (fn () (set x (+ x 1))))
          (inc)
          (inc)
          (inc)
          x)
    "#;
    assert_eq!(eval_source(code).unwrap(), Value::int(3));
}

// ============================================================================
// SECTION 3: Mixed immutable and mutable captures
// ============================================================================

#[test]
fn test_mixed_def_and_var_captures() {
    // A closure capturing both an immutable def and a mutable var.
    let code = r#"
        (begin
          (def base 10)
          (var count 0)
          (def f (fn () (begin (set count (+ count 1)) (+ base count))))
          (f)
          (f)
          (f))
    "#;
    assert_eq!(eval_source(code).unwrap(), Value::int(13));
}

#[test]
fn test_def_fn_captured_by_sibling() {
    // A def'd function captured by a sibling function.
    let code = r#"
        (begin
          (def helper (fn (n) (+ n 1)))
          (def caller (fn (n) (helper n)))
          (caller 41))
    "#;
    assert_eq!(eval_source(code).unwrap(), Value::int(42));
}
