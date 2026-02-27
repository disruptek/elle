use crate::common::eval_source;
use elle::Value;

// ── Basic splice in function calls ────────────────────────────────

#[test]
fn test_splice_basic_call() {
    // (+ ;@[1 2 3]) should spread [1 2 3] as args to +
    let result = eval_source("(+ ;@[1 2 3])");
    assert_eq!(result.unwrap(), Value::int(6));
}

#[test]
fn test_splice_mixed_args() {
    // (+ 10 ;@[1 2 3]) should be (+ 10 1 2 3) = 16
    let result = eval_source("(+ 10 ;@[1 2 3])");
    assert_eq!(result.unwrap(), Value::int(16));
}

#[test]
fn test_splice_multiple_splices() {
    // (+ ;@[1 2] ;@[3 4]) should be (+ 1 2 3 4) = 10
    let result = eval_source("(+ ;@[1 2] ;@[3 4])");
    assert_eq!(result.unwrap(), Value::int(10));
}

#[test]
fn test_splice_with_normal_args_between() {
    // (+ 1 ;@[2 3] 4 ;@[5 6]) should be (+ 1 2 3 4 5 6) = 21
    let result = eval_source("(+ 1 ;@[2 3] 4 ;@[5 6])");
    assert_eq!(result.unwrap(), Value::int(21));
}

#[test]
fn test_splice_empty_array() {
    // (+ 1 ;@[] 2) should be (+ 1 2) = 3
    let result = eval_source("(+ 1 ;@[] 2)");
    assert_eq!(result.unwrap(), Value::int(3));
}

#[test]
fn test_splice_tuple() {
    // Splice should work on tuples too
    let result = eval_source("(+ ;[1 2 3])");
    assert_eq!(result.unwrap(), Value::int(6));
}

// ── Splice in data constructors ──────────────────────────────────

#[test]
fn test_splice_in_array_literal() {
    // @[1 ;@[2 3] 4] should be @[1 2 3 4]
    let result = eval_source(
        r#"(let ((a @[1 ;@[2 3] 4]))
             (length a))"#,
    );
    assert_eq!(result.unwrap(), Value::int(4));
}

#[test]
fn test_splice_in_tuple_literal() {
    // [1 ;@[2 3] 4] should be [1 2 3 4]
    let result = eval_source(
        r#"(let ((t [1 ;@[2 3] 4]))
             (length t))"#,
    );
    assert_eq!(result.unwrap(), Value::int(4));
}

// ── Splice with closures ─────────────────────────────────────────

#[test]
fn test_splice_with_closure() {
    let result = eval_source(
        r#"(begin
             (defn add3 (a b c) (+ a b c))
             (def args @[1 2 3])
             (add3 ;args))"#,
    );
    assert_eq!(result.unwrap(), Value::int(6));
}

#[test]
fn test_splice_with_variadic_fn() {
    let result = eval_source(
        r#"(begin
             (defn sum (& nums) (apply-helper nums))
             (defn apply-helper (nums)
               (if (empty? nums) 0
                   (+ (first nums) (apply-helper (rest nums)))))
             (sum ;@[1 2 3 4 5]))"#,
    );
    assert_eq!(result.unwrap(), Value::int(15));
}

// ── Long form (splice expr) ──────────────────────────────────────

#[test]
fn test_splice_long_form() {
    let result = eval_source("(+ (splice @[1 2 3]))");
    assert_eq!(result.unwrap(), Value::int(6));
}

#[test]
fn test_splice_long_form_mixed() {
    let result = eval_source("(+ 10 (splice @[1 2 3]))");
    assert_eq!(result.unwrap(), Value::int(16));
}

// ── Error cases ───────────────────────────────────────────────────

#[test]
fn test_splice_invalid_context() {
    // Splice at top level should error
    let result = eval_source(";@[1 2 3]");
    assert!(result.is_err(), "splice at top level should error");
    assert!(
        result.unwrap_err().contains("splice"),
        "error should mention splice"
    );
}

#[test]
fn test_splice_wrong_type() {
    // Splicing a non-indexed type should error at runtime
    let result = eval_source("(+ ;42)");
    assert!(result.is_err(), "splicing an integer should error");
}

#[test]
fn test_splice_in_struct_rejected() {
    // Splice in struct literal should be rejected at compile time
    let result = eval_source("{:a 1 ;@[:b 2]}");
    assert!(result.is_err(), "splice in struct should error");
}

#[test]
fn test_splice_in_table_rejected() {
    // Splice in table literal should be rejected at compile time
    let result = eval_source("@{:a 1 ;@[:b 2]}");
    assert!(result.is_err(), "splice in table should error");
}

// ── Reader tests ──────────────────────────────────────────────────

#[test]
fn test_semicolon_is_splice_not_comment() {
    // After PR1, ; is splice, not comment. ;@[1 2] should be (splice @[1 2])
    let result = eval_source("(+ ;@[1 2])");
    assert_eq!(result.unwrap(), Value::int(3));
}

#[test]
fn test_hash_is_comment() {
    // # is now the comment character
    let result = eval_source("(+ 1 2) # this is a comment");
    assert_eq!(result.unwrap(), Value::int(3));
}
