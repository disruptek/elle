// Integration tests for macro hygiene (sets-of-scopes)
//
// These tests verify that macro-introduced bindings don't capture
// call-site names and vice versa.

use elle::ffi::primitives::context::set_symbol_table;
use elle::pipeline::{compile, compile_all};
use elle::primitives::register_primitives;
use elle::{SymbolTable, Value, VM};

fn eval(input: &str) -> Result<Value, String> {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    let _effects = register_primitives(&mut vm, &mut symbols);
    set_symbol_table(&mut symbols as *mut SymbolTable);

    match compile(input, &mut symbols) {
        Ok(result) => vm.execute(&result.bytecode).map_err(|e| e.to_string()),
        Err(_) => {
            let wrapped = format!("(begin {})", input);
            match compile(&wrapped, &mut symbols) {
                Ok(result) => vm.execute(&result.bytecode).map_err(|e| e.to_string()),
                Err(_) => {
                    let results = compile_all(input, &mut symbols)?;
                    let mut last_result = Value::NIL;
                    for result in results {
                        last_result = vm.execute(&result.bytecode).map_err(|e| e.to_string())?;
                    }
                    Ok(last_result)
                }
            }
        }
    }
}

// ============================================================================
// SECTION 1: Macro hygiene — no accidental capture
// ============================================================================

#[test]
fn test_macro_no_capture() {
    // The swap macro introduces a `tmp` binding. The caller also has `tmp`.
    // The macro's `tmp` must not shadow the caller's `tmp`.
    let code = r#"
        (defmacro my-swap (a b)
          `(let ((tmp ,a)) (set! ,a ,b) (set! ,b tmp)))

        (let ((tmp 10) (x 1) (y 2))
          (my-swap x y)
          tmp)
    "#;
    assert_eq!(eval(code).unwrap(), Value::int(10));
}

#[test]
fn test_macro_no_leak() {
    // The macro introduces an internal binding. The caller should not
    // be able to see it.
    let code = r#"
        (defmacro with-internal (body)
          `(let ((internal-var 42)) ,body))

        (with-internal (+ 1 2))
    "#;
    // The body (+ 1 2) should evaluate to 3, not reference internal-var
    assert_eq!(eval(code).unwrap(), Value::int(3));
}

#[test]
fn test_nested_macro_hygiene() {
    // Two different macros both introduce `tmp`. They must not interfere.
    let code = r#"
        (defmacro add-tmp-a (x)
          `(let ((tmp ,x)) (+ tmp 1)))

        (defmacro add-tmp-b (x)
          `(let ((tmp ,x)) (+ tmp 2)))

        (+ (add-tmp-a 10) (add-tmp-b 20))
    "#;
    assert_eq!(eval(code).unwrap(), Value::int(33)); // (10+1) + (20+2)
}

// ============================================================================
// SECTION 2: Non-macro code unchanged
// ============================================================================

#[test]
fn test_non_macro_code_unchanged() {
    // Code without macros should work identically.
    let code = r#"
        (let ((x 10) (y 20))
          (+ x y))
    "#;
    assert_eq!(eval(code).unwrap(), Value::int(30));
}

#[test]
fn test_non_macro_shadowing_unchanged() {
    // Normal shadowing (no macros) should still work.
    let code = r#"
        (let ((x 10))
          (let ((x 20))
            x))
    "#;
    assert_eq!(eval(code).unwrap(), Value::int(20));
}

// ============================================================================
// SECTION 3: Macro argument resolution
// ============================================================================

#[test]
fn test_macro_with_expression_arg() {
    // Macro argument variable reference resolves to the caller's binding.
    let code = r#"
        (defmacro double (x)
          `(+ ,x ,x))

        (let ((val 7))
          (double val))
    "#;
    assert_eq!(eval(code).unwrap(), Value::int(14));
}

#[test]
fn test_macro_closure_captures_callsite() {
    // A macro-generated closure should capture a call-site variable correctly.
    let code = r#"
        (defmacro make-adder (n)
          `(fn (x) (+ x ,n)))

        (let ((amount 5))
          (let ((f (make-adder amount)))
            (f 10)))
    "#;
    assert_eq!(eval(code).unwrap(), Value::int(15));
}

// ============================================================================
// SECTION 4: Macro with conditional body (regression)
// ============================================================================

#[test]
fn test_macro_with_conditional_body_regression() {
    // This was a regression: wrapping #f in a syntax object made it truthy.
    // The hybrid wrapping approach (atoms via Quote, compounds via SyntaxLiteral)
    // fixes this.
    let code = r#"
        (defmacro when-true (cond body)
          `(if ,cond ,body nil))

        (when-true #f 42)
    "#;
    assert_eq!(eval(code).unwrap(), Value::NIL);
}

#[test]
fn test_macro_with_conditional_body_true() {
    let code = r#"
        (defmacro when-true (cond body)
          `(if ,cond ,body nil))

        (when-true #t 42)
    "#;
    assert_eq!(eval(code).unwrap(), Value::int(42));
}

// ============================================================================
// SECTION 5: Swap macro end-to-end
// ============================================================================

#[test]
fn test_swap_actually_swaps() {
    // Verify the swap macro actually swaps values, not just that it's hygienic.
    let code = r#"
        (defmacro my-swap (a b)
          `(let ((tmp ,a)) (set! ,a ,b) (set! ,b tmp)))

        (let ((x 1) (y 2))
          (my-swap x y)
          (list x y))
    "#;
    let result = eval(code).unwrap();
    // After swap: x=2, y=1
    let items = result.list_to_vec().unwrap();
    assert_eq!(items[0], Value::int(2));
    assert_eq!(items[1], Value::int(1));
}

#[test]
fn test_swap_with_same_named_tmp() {
    // The real hygiene test: swap when caller has a variable named `tmp`.
    let code = r#"
        (defmacro my-swap (a b)
          `(let ((tmp ,a)) (set! ,a ,b) (set! ,b tmp)))

        (let ((tmp 100) (x 1) (y 2))
          (my-swap x y)
          (list tmp x y))
    "#;
    let result = eval(code).unwrap();
    let items = result.list_to_vec().unwrap();
    // tmp should be unchanged (100), x and y should be swapped
    assert_eq!(items[0], Value::int(100));
    assert_eq!(items[1], Value::int(2));
    assert_eq!(items[2], Value::int(1));
}

// ============================================================================
// SECTION 6: gensym returns symbols (not strings)
// ============================================================================

#[test]
fn test_gensym_in_macro() {
    // gensym should return a symbol that works in quasiquote templates.
    // This was broken (#306): gensym returned a string, producing
    // string literals where symbols were needed.
    let code = r#"
        (defmacro with-temp (body)
          (let ((tmp (gensym "tmp")))
            `(let ((,tmp 42)) ,body)))

        (with-temp (+ 1 2))
    "#;
    assert_eq!(eval(code).unwrap(), Value::int(3));
}

#[test]
fn test_nested_macro_scope_preservation() {
    // Macro A expands to code that invokes macro B, passing A's arguments
    // through to B. Arguments from A's call site must retain their scopes
    // through B's expansion. This exercises the Value::syntax round-trip
    // for nested expansions.
    let code = r#"
        (defmacro inner-add (x y)
          `(+ ,x ,y))

        (defmacro outer-add (a b)
          `(inner-add ,a ,b))

        (let ((x 10) (y 20))
          (outer-add x y))
    "#;
    assert_eq!(eval(code).unwrap(), Value::int(30));
}

#[test]
fn test_gensym_produces_unique_bindings() {
    // Two gensym calls produce different symbols, so two macro
    // expansions don't interfere.
    let code = r#"
        (defmacro bind-val (val body)
          (let ((g (gensym "v")))
            `(let ((,g ,val)) ,body)))

        (bind-val 10 (bind-val 20 (+ 1 2)))
    "#;
    assert_eq!(eval(code).unwrap(), Value::int(3));
}
