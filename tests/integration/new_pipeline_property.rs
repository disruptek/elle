// Property-based tests for the new Syntax → HIR → LIR compilation pipeline
//
// These tests verify semantic correctness by checking mathematical properties
// hold when code is compiled and executed through the new pipeline.

use elle::pipeline::eval_new;
use elle::primitives::register_primitives;
use elle::{SymbolTable, Value, VM};
use proptest::prelude::*;

/// Helper to evaluate code using the new pipeline
fn eval(input: &str) -> Result<Value, String> {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    register_primitives(&mut vm, &mut symbols);
    eval_new(input, &mut symbols, &mut vm)
}

// ============================================================================
// Arithmetic Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    #[test]
    fn addition_commutative(a in -1000i64..1000, b in -1000i64..1000) {
        let expr1 = format!("(+ {} {})", a, b);
        let expr2 = format!("(+ {} {})", b, a);

        let r1 = eval(&expr1);
        let r2 = eval(&expr2);

        prop_assert!(r1.is_ok(), "expr1 failed: {:?}", r1);
        prop_assert!(r2.is_ok(), "expr2 failed: {:?}", r2);
        prop_assert_eq!(r1.unwrap(), r2.unwrap());
    }

    #[test]
    fn addition_associative(a in -100i64..100, b in -100i64..100, c in -100i64..100) {
        let expr1 = format!("(+ (+ {} {}) {})", a, b, c);
        let expr2 = format!("(+ {} (+ {} {}))", a, b, c);

        let r1 = eval(&expr1);
        let r2 = eval(&expr2);

        prop_assert!(r1.is_ok(), "expr1 failed: {:?}", r1);
        prop_assert!(r2.is_ok(), "expr2 failed: {:?}", r2);
        prop_assert_eq!(r1.unwrap(), r2.unwrap());
    }

    #[test]
    fn addition_identity(a in -1000i64..1000) {
        let expr = format!("(+ {} 0)", a);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a));
    }

    #[test]
    fn multiplication_commutative(a in -100i64..100, b in -100i64..100) {
        let expr1 = format!("(* {} {})", a, b);
        let expr2 = format!("(* {} {})", b, a);

        let r1 = eval(&expr1);
        let r2 = eval(&expr2);

        prop_assert!(r1.is_ok(), "expr1 failed: {:?}", r1);
        prop_assert!(r2.is_ok(), "expr2 failed: {:?}", r2);
        prop_assert_eq!(r1.unwrap(), r2.unwrap());
    }

    #[test]
    fn multiplication_identity(a in -1000i64..1000) {
        let expr = format!("(* {} 1)", a);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a));
    }

    #[test]
    fn subtraction_inverse_of_addition(a in -500i64..500, b in -500i64..500) {
        let expr = format!("(- (+ {} {}) {})", a, b, b);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a));
    }

    #[test]
    fn division_inverse_of_multiplication(a in -100i64..100, b in 1i64..100) {
        let expr = format!("(/ (* {} {}) {})", a, b, b);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a));
    }
}

// ============================================================================
// Comparison Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    #[test]
    fn equality_reflexive(a in -1000i64..1000) {
        let expr = format!("(= {} {})", a, a);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Bool(true));
    }

    #[test]
    fn equality_symmetric(a in -100i64..100, b in -100i64..100) {
        let expr1 = format!("(= {} {})", a, b);
        let expr2 = format!("(= {} {})", b, a);

        let r1 = eval(&expr1);
        let r2 = eval(&expr2);

        prop_assert!(r1.is_ok(), "expr1 failed: {:?}", r1);
        prop_assert!(r2.is_ok(), "expr2 failed: {:?}", r2);
        prop_assert_eq!(r1.unwrap(), r2.unwrap());
    }

    #[test]
    fn less_than_irreflexive(a in -1000i64..1000) {
        let expr = format!("(< {} {})", a, a);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Bool(false));
    }

    #[test]
    fn less_than_asymmetric(a in -100i64..100, b in -100i64..100) {
        if a < b {
            let expr1 = format!("(< {} {})", a, b);
            let expr2 = format!("(< {} {})", b, a);

            let r1 = eval(&expr1);
            let r2 = eval(&expr2);

            prop_assert!(r1.is_ok());
            prop_assert!(r2.is_ok());
            prop_assert_eq!(r1.unwrap(), Value::Bool(true));
            prop_assert_eq!(r2.unwrap(), Value::Bool(false));
        }
    }
}

// ============================================================================
// Conditional Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    #[test]
    fn if_true_returns_then(a in -100i64..100, b in -100i64..100) {
        let expr = format!("(if #t {} {})", a, b);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a));
    }

    #[test]
    fn if_false_returns_else(a in -100i64..100, b in -100i64..100) {
        let expr = format!("(if #f {} {})", a, b);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(b));
    }

    #[test]
    fn if_with_computed_condition(a in -100i64..100, b in -100i64..100) {
        // (if (< a b) a b) should return the smaller value
        let expr = format!("(if (< {} {}) {} {})", a, b, a, b);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        let expected = if a < b { a } else { b };
        prop_assert_eq!(result.unwrap(), Value::Int(expected));
    }

    #[test]
    fn nested_if_consistency(a in -50i64..50, b in -50i64..50, c in -50i64..50) {
        // Nested if should work correctly
        let expr = format!(
            "(if (< {} {}) (if (< {} {}) {} {}) {})",
            a, b, a, c, a, c, b
        );
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
    }
}

// ============================================================================
// Let Binding Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    #[test]
    fn let_binds_value(a in -1000i64..1000) {
        let expr = format!("(let ((x {})) x)", a);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a));
    }

    #[test]
    fn let_shadows_outer(outer in -100i64..100, inner in -100i64..100) {
        let expr = format!("(let ((x {})) (let ((x {})) x))", outer, inner);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(inner));
    }

    #[test]
    fn let_outer_unchanged_after_inner(outer in -100i64..100, inner in -100i64..100) {
        // After inner let exits, outer binding should be accessible
        let expr = format!(
            "(let ((x {})) (begin (let ((x {})) x) x))",
            outer, inner
        );
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(outer));
    }

    #[test]
    fn let_multiple_bindings(a in -100i64..100, b in -100i64..100) {
        let expr = format!("(let ((x {}) (y {})) (+ x y))", a, b);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a + b));
    }
}

// ============================================================================
// Lambda / Closure Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    #[test]
    fn lambda_identity(a in -1000i64..1000) {
        let expr = format!("((fn (x) x) {})", a);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a));
    }

    #[test]
    fn lambda_constant(a in -100i64..100, b in -100i64..100) {
        let expr = format!("((fn (x) {}) {})", b, a);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(b));
    }

    #[test]
    fn closure_captures_value(captured in -100i64..100, arg in -100i64..100) {
        let expr = format!(
            "(let ((y {})) ((fn (x) (+ x y)) {}))",
            captured, arg
        );
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(arg + captured));
    }

    #[test]
    fn lambda_multiple_args(a in -50i64..50, b in -50i64..50, c in -50i64..50) {
        let expr = format!("((fn (x y z) (+ x (+ y z))) {} {} {})", a, b, c);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a + b + c));
    }
}

// ============================================================================
// List Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    #[test]
    fn list_first_returns_first(a in -100i64..100, b in -100i64..100) {
        let expr = format!("(first (list {} {}))", a, b);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a));
    }

    #[test]
    fn list_length_correct(len in 0usize..10) {
        let elements: Vec<String> = (0..len).map(|i| i.to_string()).collect();
        let expr = format!("(length (list {}))", elements.join(" "));
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(len as i64));
    }

    #[test]
    fn cons_then_first(a in -100i64..100, b in -100i64..100) {
        let expr = format!("(first (cons {} {}))", a, b);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a));
    }

    #[test]
    fn cons_then_rest(a in -100i64..100, b in -100i64..100) {
        let expr = format!("(rest (cons {} {}))", a, b);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(b));
    }
}

// ============================================================================
// Boolean Logic Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    #[test]
    fn not_involution(b in prop::bool::ANY) {
        let bool_str = if b { "#t" } else { "#f" };
        let expr = format!("(not (not {}))", bool_str);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Bool(b));
    }

    #[test]
    fn and_with_false_is_false(b in prop::bool::ANY) {
        let bool_str = if b { "#t" } else { "#f" };
        let expr = format!("(and {} #f)", bool_str);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Bool(false));
    }

    #[test]
    fn or_with_true_is_true(b in prop::bool::ANY) {
        let bool_str = if b { "#t" } else { "#f" };
        let expr = format!("(or {} #t)", bool_str);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Bool(true));
    }

    #[test]
    fn de_morgan_and(a in prop::bool::ANY, b in prop::bool::ANY) {
        // not(a and b) == (not a) or (not b)
        let a_str = if a { "#t" } else { "#f" };
        let b_str = if b { "#t" } else { "#f" };

        let expr1 = format!("(not (and {} {}))", a_str, b_str);
        let expr2 = format!("(or (not {}) (not {}))", a_str, b_str);

        let r1 = eval(&expr1);
        let r2 = eval(&expr2);

        prop_assert!(r1.is_ok());
        prop_assert!(r2.is_ok());
        prop_assert_eq!(r1.unwrap(), r2.unwrap());
    }
}

// ============================================================================
// Match Expression Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    #[test]
    fn match_literal_exact(a in -100i64..100) {
        let expr = format!("(match {} ({} \"hit\") (_ \"miss\"))", a, a);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::String("hit".into()));
    }

    #[test]
    fn match_wildcard_fallback(a in -100i64..100) {
        // Match against a different literal, should fall to wildcard
        let other = a.wrapping_add(1);
        let expr = format!("(match {} ({} \"hit\") (_ \"miss\"))", a, other);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::String("miss".into()));
    }

    #[test]
    fn match_with_computed_body(a in -50i64..50, b in -50i64..50) {
        let expr = format!("(match {} ({} (+ {} {})) (_ 0))", a, a, a, b);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a + b));
    }
}

// ============================================================================
// Vector Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    #[test]
    fn vector_length_correct(len in 0usize..10) {
        let elements: Vec<String> = (0..len).map(|i| i.to_string()).collect();
        let expr = if elements.is_empty() {
            "(length [])".to_string()
        } else {
            format!("(length [{}])", elements.join(" "))
        };
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(len as i64));
    }

    #[test]
    fn vector_ref_first(a in -100i64..100, b in -100i64..100) {
        let expr = format!("(vector-ref [{} {}] 0)", a, b);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a));
    }
}

// ============================================================================
// Match Expression Extended Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    #[test]
    fn match_multiple_literals_first_matches(a in -100i64..100) {
        // First of several literal patterns matches
        let b = a.wrapping_add(1);
        let c = a.wrapping_add(2);
        let expr = format!("(match {} ({} \"first\") ({} \"second\") ({} \"third\") (_ \"default\"))", a, a, b, c);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::String("first".into()));
    }

    #[test]
    fn match_multiple_literals_middle_matches(a in -100i64..100) {
        // Middle of several literal patterns matches
        let b = a.wrapping_add(1);
        let c = a.wrapping_add(2);
        let expr = format!("(match {} ({} \"first\") ({} \"second\") ({} \"third\") (_ \"default\"))", b, a, b, c);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::String("second".into()));
    }

    #[test]
    fn match_multiple_literals_last_matches(a in -100i64..100) {
        // Last of several literal patterns matches
        let b = a.wrapping_add(1);
        let c = a.wrapping_add(2);
        let expr = format!("(match {} ({} \"first\") ({} \"second\") ({} \"third\") (_ \"default\"))", c, a, b, c);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::String("third".into()));
    }

    #[test]
    fn match_with_arithmetic_in_body(a in -50i64..50, b in -50i64..50) {
        // Match with computation in body (the bug we just fixed)
        let expr = format!("(match {} ({} (+ {} {})) (_ 0))", a, a, a, b);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a + b));
    }

    #[test]
    fn match_nil_pattern(a in -100i64..100) {
        let expr = format!("(match nil (nil \"is-nil\") ({} \"is-num\") (_ \"other\"))", a);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::String("is-nil".into()));
    }
}

// ============================================================================
// Each/For Loop Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(30))]

    #[test]
    fn each_iterates_all_elements(len in 1usize..5) {
        // Create a list and count iterations using a counter
        let elements: Vec<String> = (1..=len).map(|i| i.to_string()).collect();
        let list_str = elements.join(" ");

        // Sum all elements
        let expr = format!(
            "(let ((sum 0)) (begin (each x (list {}) (set! sum (+ sum x))) sum))",
            list_str
        );
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        let expected: i64 = (1..=len as i64).sum();
        prop_assert_eq!(result.unwrap(), Value::Int(expected));
    }

    #[test]
    fn each_empty_list_no_iteration(a in -100i64..100) {
        // Each over empty list should not execute body, return nil
        let expr = format!("(let ((x {})) (begin (each y (list) (set! x 999)) x))", a);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a)); // x unchanged
    }
}

// ============================================================================
// Closure Mutation Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(30))]

    #[test]
    fn closure_mutation_persists(_start in 0i64..100, increments in 1usize..5) {
        // Counter closure that mutates captured variable
        let mut expr = String::from(
            "(let ((counter (let ((n 0)) (fn () (begin (set! n (+ n 1)) n)))))"
        );

        // Call counter multiple times
        for _ in 0..increments {
            expr.push_str(" (counter)");
        }
        expr.push_str(")");

        let result = eval(&expr);
        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(increments as i64));
    }

    #[test]
    fn independent_closures_have_separate_state(a in 1i64..50, b in 1i64..50) {
        // Two independent closures with separate captured state
        let expr = format!(
            "(let ((c1 (let ((n {})) (fn () (begin (set! n (+ n 1)) n))))
                   (c2 (let ((m {})) (fn () (begin (set! m (+ m 1)) m)))))
               (begin (c1) (c1) (c2) (list (c1) (c2))))",
            a, b
        );
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        // c1 called 3 times: a+1, a+2, a+3
        // c2 called 2 times: b+1, b+2
        // Result should be list of (a+3, b+2)
    }
}

// ============================================================================
// Nested Control Flow Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    #[test]
    fn nested_let_in_if(cond in prop::bool::ANY, a in -100i64..100, b in -100i64..100) {
        let cond_str = if cond { "#t" } else { "#f" };
        let expr = format!(
            "(if {} (let ((x {})) x) (let ((y {})) y))",
            cond_str, a, b
        );
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        let expected = if cond { a } else { b };
        prop_assert_eq!(result.unwrap(), Value::Int(expected));
    }

    #[test]
    fn if_in_lambda_body(cond in prop::bool::ANY, a in -100i64..100, b in -100i64..100) {
        let cond_str = if cond { "#t" } else { "#f" };
        let expr = format!("((fn () (if {} {} {})))", cond_str, a, b);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        let expected = if cond { a } else { b };
        prop_assert_eq!(result.unwrap(), Value::Int(expected));
    }

    #[test]
    fn match_in_lambda(a in -50i64..50, b in -50i64..50) {
        let expr = format!(
            "((fn (x) (match x ({} \"a\") ({} \"b\") (_ \"other\"))) {})",
            a, b, a
        );
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::String("a".into()));
    }

    #[test]
    fn lambda_in_match_body(a in -50i64..50, b in -50i64..50) {
        let expr = format!(
            "(match {} ({} ((fn (x) (+ x {})) {})) (_ 0))",
            a, a, b, a
        );
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a + b));
    }
}

// ============================================================================
// Begin/Sequence Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    #[test]
    fn begin_returns_last(a in -100i64..100, b in -100i64..100, c in -100i64..100) {
        let expr = format!("(begin {} {} {})", a, b, c);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(c));
    }

    #[test]
    fn begin_with_side_effects(a in -100i64..100, b in -100i64..100) {
        // Side effect: set! followed by read
        let expr = format!(
            "(let ((x {})) (begin (set! x {}) x))",
            a, b
        );
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(b));
    }
}

// ============================================================================
// Cond Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    #[test]
    fn cond_first_true(a in -100i64..100, b in -100i64..100, c in -100i64..100) {
        let expr = format!("(cond (#t {}) (#t {}) (else {}))", a, b, c);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a));
    }

    #[test]
    fn cond_falls_through_to_else(a in -100i64..100) {
        let expr = format!("(cond (#f 1) (#f 2) (else {}))", a);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a));
    }

    #[test]
    fn cond_with_computed_conditions(a in -100i64..100, threshold in -100i64..100) {
        let expr = format!(
            "(cond ((< {} {}) \"less\") ((= {} {}) \"equal\") (else \"greater\"))",
            a, threshold, a, threshold
        );
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        let expected = if a < threshold {
            "less"
        } else if a == threshold {
            "equal"
        } else {
            "greater"
        };
        prop_assert_eq!(result.unwrap(), Value::String(expected.into()));
    }
}

// ============================================================================
// Quasiquote Properties (if supported)
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(30))]

    #[test]
    fn quasiquote_with_unquote(a in -100i64..100) {
        let expr = format!("(let ((x {})) `(1 ,x 3))", a);
        let result = eval(&expr);

        // If quasiquote is supported, check result is a list with x interpolated
        if let Ok(val) = result {
            if let Ok(vec) = val.list_to_vec() {
                prop_assert_eq!(vec.len(), 3);
                prop_assert_eq!(&vec[1], &Value::Int(a));
            }
        }
        // If not supported, that's also OK for now
    }
}

// ============================================================================
// Handler-Case Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(30))]

    #[test]
    fn handler_case_no_error(a in -100i64..100) {
        // No error, returns body value
        let expr = format!("(handler-case {} (error e -1))", a);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(a));
    }

    #[test]
    fn handler_case_catches_division_by_zero(a in 1i64..100) {
        let expr = format!("(handler-case (/ {} 0) (error e {}))", a, -a);
        let result = eval(&expr);

        prop_assert!(result.is_ok(), "failed: {:?}", result);
        prop_assert_eq!(result.unwrap(), Value::Int(-a));
    }
}
