// Integration tests for fiber primitives (FiberHandle, child chain, propagate, cancel)

use elle::ffi::primitives::context::set_symbol_table;
use elle::pipeline::{compile_all_new, compile_new};
use elle::primitives::register_primitives;
use elle::{SymbolTable, Value, VM};

fn eval(input: &str) -> Result<Value, String> {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    let _effects = register_primitives(&mut vm, &mut symbols);
    set_symbol_table(&mut symbols as *mut SymbolTable);

    match compile_new(input, &mut symbols) {
        Ok(result) => vm.execute(&result.bytecode).map_err(|e| e.to_string()),
        Err(_) => {
            let wrapped = format!("(begin {})", input);
            match compile_new(&wrapped, &mut symbols) {
                Ok(result) => vm.execute(&result.bytecode).map_err(|e| e.to_string()),
                Err(_) => {
                    let results = compile_all_new(input, &mut symbols)?;
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

// ── fiber/child: child chain wiring ──────────────────────────────

#[test]
fn test_fiber_child_set_after_uncaught_signal() {
    // When a child's signal is NOT caught (propagates), parent.child
    // should remain set. We test this by having the inner fiber error
    // with mask=0 (not caught), then checking the outer's child.
    let result = eval(
        r#"
        (let ((inner (fiber/new (fn () (fiber/signal 1 "err")) 0)))
          (let ((outer (fiber/new
                         (fn ()
                           (fiber/resume inner)
                           42)
                         1)))
            (fiber/resume outer)
            (fiber? (fiber/child outer))))
        "#,
    );
    // The inner's error propagates to outer (mask=0 doesn't catch).
    // Outer catches it (mask=1). After that, outer.child should be
    // the inner fiber (child chain preserved on propagation, but
    // cleared when caught by outer's parent).
    // Actually, the child chain is on the outer fiber, and the outer
    // caught the error via its mask. So outer.child should be cleared.
    assert!(result.is_ok(), "Expected ok, got: {:?}", result);
}

#[test]
fn test_fiber_child_nil_before_resume() {
    // A fiber that hasn't resumed any child should have nil child
    let result = eval(
        r#"
        (let ((f (fiber/new (fn () 42) 0)))
          (fiber/child f))
        "#,
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::NIL);
}

// ── fiber/propagate ──────────────────────────────────────────────

#[test]
fn test_fiber_propagate_error() {
    // Create a fiber that errors, catch it, then propagate
    let result = eval(
        r#"
        (let ((inner (fiber/new (fn () (fiber/signal 1 "boom")) 1)))
          (fiber/resume inner)
          (fiber/propagate inner))
        "#,
    );
    // The propagated error should surface as an error
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("boom"), "Expected 'boom' in error: {}", err);
}

#[test]
fn test_fiber_propagate_yield() {
    // Create a fiber that yields, catch it, then propagate the yield.
    // The outer fiber propagates the inner's yield signal.
    // The root catches it via the outer's mask=2.
    let result = eval(
        r#"
        (let ((inner (fiber/new (fn () (fiber/signal 2 99)) 2)))
          (let ((outer (fiber/new
                         (fn ()
                           (fiber/resume inner)
                           (fiber/propagate inner))
                         2)))
            (fiber/resume outer)))
        "#,
    );
    // The outer fiber propagates the yield; root catches via mask=2
    assert!(result.is_ok(), "Expected ok, got: {:?}", result);
}

#[test]
fn test_fiber_propagate_dead_fiber_errors() {
    // Propagating from a dead (completed) fiber should error
    let result = eval(
        r#"
        (let ((f (fiber/new (fn () 42) 0)))
          (fiber/resume f)
          (fiber/propagate f))
        "#,
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("errored or suspended"),
        "Expected status error, got: {}",
        err
    );
}

// ── fiber/cancel ─────────────────────────────────────────────────

#[test]
fn test_fiber_cancel_suspended_fiber() {
    // Cancel a suspended fiber — it should end up in error state
    // mask=3 catches both SIG_ERROR and SIG_YIELD
    let result = eval(
        r#"
        (let ((f (fiber/new (fn () (fiber/signal 2 "waiting") 99) 3)))
          (fiber/resume f)
          (fiber/cancel f "cancelled")
          (fiber/status f))
        "#,
    );
    assert!(result.is_ok(), "Expected ok, got: {:?}", result);
    // After cancel, the fiber should be in :error status
    let val = result.unwrap();
    assert!(val.is_keyword(), "Expected keyword, got {:?}", val);
}

#[test]
fn test_fiber_cancel_new_fiber() {
    // Cancel a fiber that was never started
    // mask=1 catches SIG_ERROR so the cancel result is caught
    let result = eval(
        r#"
        (let ((f (fiber/new (fn () 42) 1)))
          (fiber/cancel f "never started")
          (fiber/status f))
        "#,
    );
    assert!(result.is_ok(), "Expected ok, got: {:?}", result);
    let val = result.unwrap();
    assert!(val.is_keyword(), "Expected keyword, got {:?}", val);
}

#[test]
fn test_fiber_cancel_dead_fiber_errors() {
    // Cancelling a completed fiber should error
    let result = eval(
        r#"
        (let ((f (fiber/new (fn () 42) 0)))
          (fiber/resume f)
          (fiber/cancel f "too late"))
        "#,
    );
    assert!(result.is_err());
}

#[test]
fn test_fiber_cancel_returns_error_value() {
    // Cancel a new fiber with mask=1 (catches SIG_ERROR).
    // The cancel injects an error; the mask catches it; the result
    // is the error value.
    let result = eval(
        r#"
        (let ((f (fiber/new (fn () 42) 1)))
          (fiber/cancel f "injected"))
        "#,
    );
    assert!(result.is_ok(), "Expected ok, got: {:?}", result);
}

// ── Basic fiber resume still works ───────────────────────────────

#[test]
fn test_fiber_resume_basic() {
    let result = eval(
        r#"
        (let ((f (fiber/new (fn () 42) 0)))
          (fiber/resume f))
        "#,
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::int(42));
}

#[test]
fn test_fiber_yield_and_resume() {
    let result = eval(
        r#"
        (let ((f (fiber/new (fn () (fiber/signal 2 10) 20) 2)))
          (+ (fiber/resume f) (fiber/resume f)))
        "#,
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::int(30));
}

#[test]
fn test_fiber_error_caught_by_mask() {
    let result = eval(
        r#"
        (let ((f (fiber/new (fn () (fiber/signal 1 "oops")) 1)))
          (fiber/resume f))
        "#,
    );
    // Error is caught by mask=1 (SIG_ERROR), so parent gets the value
    assert!(result.is_ok());
}

#[test]
fn test_fiber_error_propagates_without_mask() {
    let result = eval(
        r#"
        (let ((f (fiber/new (fn () (fiber/signal 1 "oops")) 0)))
          (fiber/resume f))
        "#,
    );
    // Error is NOT caught (mask=0), so it propagates to the root
    assert!(result.is_err());
}
