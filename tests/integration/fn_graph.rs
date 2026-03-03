use crate::common::eval_source;
use elle::Value;

// ── DOT format tests (via fn/cfg with :dot) ─────────────────────────

#[test]
fn test_fn_cfg_dot_returns_string() {
    let result = eval_source("(string? (fn/cfg (fn (x) x) :dot))").unwrap();
    assert_eq!(result, Value::TRUE);
}

#[test]
fn test_fn_cfg_dot_starts_with_digraph() {
    let result =
        eval_source(r#"(string/starts-with? (fn/cfg (fn (x) x) :dot) "digraph {")"#).unwrap();
    assert_eq!(result, Value::TRUE);
}

#[test]
fn test_fn_cfg_dot_ends_with_closing_brace() {
    let result = eval_source(
        r#"
        (let ((dot (fn/cfg (fn (x) x) :dot)))
          (string/ends-with? dot "}\n"))
        "#,
    )
    .unwrap();
    assert_eq!(result, Value::TRUE);
}

#[test]
fn test_fn_cfg_dot_contains_block0() {
    let result = eval_source(r#"(string/contains? (fn/cfg (fn (x) x) :dot) "block0")"#).unwrap();
    assert_eq!(result, Value::TRUE);
}

#[test]
fn test_fn_cfg_dot_contains_shape_record() {
    let result =
        eval_source(r#"(string/contains? (fn/cfg (fn (x) x) :dot) "shape=record")"#).unwrap();
    assert_eq!(result, Value::TRUE);
}

#[test]
fn test_fn_cfg_dot_unnamed_defn_shows_anonymous() {
    // LirFunction.name is not currently set during lowering for defn-defined
    // closures, so :name is nil and the graph label shows "anonymous".
    let result = eval_source(
        r#"
        (defn my-fn (x) (+ x 1))
        (string/contains? (fn/cfg my-fn :dot) "anonymous")
        "#,
    )
    .unwrap();
    assert_eq!(result, Value::TRUE);
}

#[test]
fn test_fn_cfg_dot_branching_has_edges() {
    let result =
        eval_source(r#"(string/contains? (fn/cfg (fn (x) (if x 1 2)) :dot) "->")"#).unwrap();
    assert_eq!(result, Value::TRUE);
}

#[test]
fn test_fn_cfg_dot_shows_docstring_in_label() {
    let result = eval_source(
        r#"
        (defn my-fn (x) "Does stuff." (+ x 1))
        (string/contains? (fn/cfg my-fn :dot) "Does stuff.")
        "#,
    )
    .unwrap();
    assert_eq!(result, Value::TRUE);
}

// ── Mermaid format tests ────────────────────────────────────────────

#[test]
fn test_fn_cfg_default_is_mermaid() {
    let result = eval_source(r#"(string/starts-with? (fn/cfg (fn (x) x)) "flowchart")"#).unwrap();
    assert_eq!(result, Value::TRUE);
}

#[test]
fn test_fn_cfg_mermaid_explicit() {
    let result =
        eval_source(r#"(string/starts-with? (fn/cfg (fn (x) x) :mermaid) "flowchart")"#).unwrap();
    assert_eq!(result, Value::TRUE);
}

#[test]
fn test_fn_cfg_mermaid_contains_block() {
    let result =
        eval_source(r#"(string/contains? (fn/cfg (fn (x) x) :mermaid) "block0")"#).unwrap();
    assert_eq!(result, Value::TRUE);
}

#[test]
fn test_fn_cfg_mermaid_branching_has_edges() {
    let result =
        eval_source(r#"(string/contains? (fn/cfg (fn (x) (if x 1 2)) :mermaid) "-->")"#).unwrap();
    assert_eq!(result, Value::TRUE);
}

#[test]
fn test_fn_cfg_mermaid_default_equals_explicit() {
    let result = eval_source(
        r#"
        (let ((f (fn (x) (if x 1 2))))
          (= (fn/cfg f) (fn/cfg f :mermaid)))
        "#,
    )
    .unwrap();
    assert_eq!(result, Value::TRUE);
}

// ── Error handling ──────────────────────────────────────────────────

#[test]
fn test_fn_cfg_invalid_format_errors() {
    let result = eval_source("(fn/cfg (fn (x) x) :png)");
    assert!(result.is_err());
}

#[test]
fn test_fn_cfg_too_many_args_errors() {
    let result = eval_source("(fn/cfg (fn (x) x) :dot :extra)");
    assert!(result.is_err());
}

#[test]
fn test_fn_cfg_non_closure_errors() {
    let result = eval_source("(fn/cfg 42)");
    assert!(result.is_err());
}

// ── Fiber support ───────────────────────────────────────────────────

#[test]
fn test_fn_cfg_fiber() {
    let result = eval_source("(string? (fn/cfg (fiber/new (fn () 42) 0)))").unwrap();
    assert_eq!(result, Value::TRUE);
}

#[test]
fn test_fn_flow_fiber() {
    let result = eval_source("(struct? (fn/flow (fiber/new (fn () 42) 0)))").unwrap();
    assert_eq!(result, Value::TRUE);
}
