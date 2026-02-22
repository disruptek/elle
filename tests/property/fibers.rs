// Property-based tests for fiber primitives
//
// Tests the FiberHandle system, child chain wiring, propagate, and cancel
// using generated inputs to exercise edge cases that example-based tests miss.

use elle::ffi::primitives::context::set_symbol_table;
use elle::pipeline::{compile_all_new, compile_new};
use elle::primitives::register_primitives;
use elle::{SymbolTable, Value, VM};
use proptest::prelude::*;

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

// ============================================================================
// Property 1: Fiber yield/resume produces values in order
//
// For any sequence of yield values, resuming the fiber produces them in
// the same order, followed by the final return value.
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    #[test]
    fn fiber_yield_resume_order(
        values in prop::collection::vec(-1000i64..1000, 1..=8),
        final_val in -1000i64..1000,
    ) {
        let n = values.len();

        // Build: (fn () (fiber/signal 2 v1) (fiber/signal 2 v2) ... final)
        let signals: Vec<String> = values.iter()
            .map(|v| format!("(fiber/signal 2 {})", v))
            .collect();
        let body = format!("{} {}", signals.join(" "), final_val);

        // mask=2 catches SIG_YIELD
        let code = format!(
            r#"(let ((f (fiber/new (fn () {}) 2)))
                 (list {}))"#,
            body,
            (0..=n).map(|_| "(fiber/resume f)".to_string())
                .collect::<Vec<_>>().join(" ")
        );

        let result = eval(&code);
        prop_assert!(result.is_ok(), "Eval failed: {:?}", result);

        // Collect the list of results
        let list_val = result.unwrap();
        let mut collected = Vec::new();
        let mut current = list_val;
        while let Some(cons) = current.as_cons() {
            if let Some(n) = cons.first.as_int() {
                collected.push(n);
            }
            current = cons.rest;
        }

        // First N values are the yields, last is the final return
        let mut expected: Vec<i64> = values.clone();
        expected.push(final_val);
        prop_assert_eq!(collected, expected,
            "Yield/resume order mismatch for values {:?} final {}", values, final_val);
    }
}

// ============================================================================
// Property 2: Signal mask determines catch behavior
//
// For any signal bits in the user range and any mask, the signal is caught
// by the parent iff (mask & bits) != 0. When caught, the parent gets the
// value. When not caught, the signal propagates as an error.
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(30))]

    #[test]
    fn signal_mask_catch_behavior(
        // Use bits 1 (error) and 2 (yield) — the user-visible signal bits
        signal_bit in prop::sample::select(vec![1u32, 2]),
        mask in 0u32..4,
        payload in -100i64..100,
    ) {
        let caught = (mask & signal_bit) != 0;

        let code = format!(
            r#"(let ((f (fiber/new (fn () (fiber/signal {} {})) {})))
                 (fiber/resume f))"#,
            signal_bit, payload, mask
        );

        let result = eval(&code);

        if caught {
            prop_assert!(result.is_ok(),
                "Signal {} with mask {} should be caught, got: {:?}",
                signal_bit, mask, result);
            // The caught value should be the payload
            let val = result.unwrap();
            if let Some(n) = val.as_int() {
                prop_assert_eq!(n, payload,
                    "Caught value mismatch: expected {}, got {}", payload, n);
            }
        } else {
            // Uncaught signal propagates — for SIG_ERROR this is an error,
            // for SIG_YIELD it also propagates as an error to the root
            prop_assert!(result.is_err(),
                "Signal {} with mask {} should propagate, got: {:?}",
                signal_bit, mask, result);
        }
    }
}

// ============================================================================
// Property 3: fiber/cancel delivers the error value
//
// For any fiber in New or Suspended state, cancel injects the error value.
// The parent (with mask catching SIG_ERROR) receives the injected value.
// After cancel, fiber/value on the cancelled fiber returns the error value.
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(30))]

    #[test]
    fn cancel_delivers_value_to_new_fiber(payload in -100i64..100) {
        // Cancel a New fiber. mask=1 catches SIG_ERROR.
        // The cancel result (caught by mask) should be the injected value.
        // fiber/value on the cancelled fiber should also return it.
        let code = format!(
            r#"(let ((f (fiber/new (fn () 42) 1)))
                 (let ((result (fiber/cancel f {})))
                   (list result (fiber/value f))))"#,
            payload
        );
        let result = eval(&code);
        prop_assert!(result.is_ok(), "Cancel new fiber failed: {:?}", result);

        // Extract the list: (cancel-result fiber-value)
        let list = result.unwrap();
        let items = list.list_to_vec();
        prop_assert!(items.is_ok(), "Expected list, got {:?}", list);
        let items = items.unwrap();
        prop_assert_eq!(items.len(), 2, "Expected 2-element list, got {:?}", items);

        // The cancel result should be the payload we injected
        let cancel_result = items[0].as_int();
        prop_assert_eq!(cancel_result, Some(payload),
            "Cancel result: expected Some({}), got {:?} (raw: {:?})",
            payload, cancel_result, items[0]);

        // fiber/value should also return the payload
        let fiber_value = items[1].as_int();
        prop_assert_eq!(fiber_value, Some(payload),
            "fiber/value after cancel: expected Some({}), got {:?} (raw: {:?})",
            payload, fiber_value, items[1]);
    }

    #[test]
    fn cancel_delivers_value_to_suspended_fiber(payload in -100i64..100) {
        // Suspend a fiber via yield, then cancel it.
        // mask=3 catches both SIG_YIELD and SIG_ERROR.
        let code = format!(
            r#"(let ((f (fiber/new (fn () (fiber/signal 2 0) 99) 3)))
                 (fiber/resume f)
                 (let ((result (fiber/cancel f {})))
                   (list result (fiber/value f))))"#,
            payload
        );
        let result = eval(&code);
        prop_assert!(result.is_ok(), "Cancel suspended fiber failed: {:?}", result);

        let list = result.unwrap();
        let items = list.list_to_vec();
        prop_assert!(items.is_ok(), "Expected list, got {:?}", list);
        let items = items.unwrap();
        prop_assert_eq!(items.len(), 2, "Expected 2-element list, got {:?}", items);

        // The cancel result should be the payload
        let cancel_result = items[0].as_int();
        prop_assert_eq!(cancel_result, Some(payload),
            "Cancel result: expected Some({}), got {:?} (raw: {:?})",
            payload, cancel_result, items[0]);

        // fiber/value should also return the payload
        let fiber_value = items[1].as_int();
        prop_assert_eq!(fiber_value, Some(payload),
            "fiber/value after cancel: expected Some({}), got {:?} (raw: {:?})",
            payload, fiber_value, items[1]);
    }
}

// ============================================================================
// Property 4: fiber/propagate valid/invalid boundary
//
// Propagate succeeds iff the fiber is in Error or Suspended state with a
// signal. It fails for New, Alive, and Dead fibers.
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(20))]

    #[test]
    fn propagate_rejects_dead_fibers(final_val in -100i64..100) {
        // A fiber that completed normally (Dead) should not be propagatable
        let code = format!(
            r#"(let ((f (fiber/new (fn () {}) 0)))
                 (fiber/resume f)
                 (fiber/propagate f))"#,
            final_val
        );
        let result = eval(&code);
        prop_assert!(result.is_err(),
            "Propagate from dead fiber should fail, got: {:?}", result);
        let err = result.unwrap_err();
        prop_assert!(err.contains("errored or suspended"),
            "Expected status error, got: {}", err);
    }

    #[test]
    fn propagate_succeeds_for_errored_fibers(payload in -100i64..100) {
        // A fiber that errored (mask=1 catches it) should be propagatable
        let code = format!(
            r#"(let ((f (fiber/new (fn () (fiber/signal 1 {})) 1)))
                 (fiber/resume f)
                 (fiber/propagate f))"#,
            payload
        );
        let result = eval(&code);
        // Propagate re-raises the error — it should surface as an error
        // to the root (since the root has no mask for it)
        prop_assert!(result.is_err(),
            "Propagated error should surface, got: {:?}", result);
    }
}

// ============================================================================
// Property 5: fiber/cancel rejects invalid states
//
// Cancel should fail for Dead, Error, and Alive fibers.
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(20))]

    #[test]
    fn cancel_rejects_dead_fibers(final_val in -100i64..100) {
        let code = format!(
            r#"(let ((f (fiber/new (fn () {}) 0)))
                 (fiber/resume f)
                 (fiber/cancel f "too late"))"#,
            final_val
        );
        let result = eval(&code);
        prop_assert!(result.is_err(),
            "Cancel dead fiber should fail, got: {:?}", result);
    }

    #[test]
    fn cancel_rejects_errored_fibers(payload in -100i64..100) {
        // Create a fiber that errors, catch it via mask, then try to cancel
        let code = format!(
            r#"(let ((f (fiber/new (fn () (fiber/signal 1 {})) 1)))
                 (fiber/resume f)
                 (fiber/cancel f "already errored"))"#,
            payload
        );
        let result = eval(&code);
        prop_assert!(result.is_err(),
            "Cancel errored fiber should fail, got: {:?}", result);
    }
}

// ============================================================================
// Property 6: Nested fiber resume preserves values
//
// When fiber A resumes fiber B which yields, A gets B's yield value.
// This tests the FiberHandle take/put protocol under nesting.
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(30))]

    #[test]
    fn nested_fiber_resume_preserves_values(
        inner_val in -100i64..100,
        outer_val in -100i64..100,
    ) {
        let code = format!(
            r#"(let ((inner (fiber/new (fn () (fiber/signal 2 {})) 2)))
                 (let ((outer (fiber/new
                                (fn ()
                                  (let ((v (fiber/resume inner)))
                                    (+ v {})))
                                0)))
                   (fiber/resume outer)))"#,
            inner_val, outer_val
        );
        let result = eval(&code);
        prop_assert!(result.is_ok(), "Nested resume failed: {:?}", result);
        let val = result.unwrap();
        let n = val.as_int();
        prop_assert_eq!(n, Some(inner_val + outer_val),
            "Expected Some({}), got {:?} (raw: {:?})",
            inner_val + outer_val, n, val);
    }
}
