//! Coroutine primitives for Elle
//!
//! Provides the user-facing API for colorless coroutines:
//! - make-coroutine: Create a coroutine from a function
//! - coroutine-resume: Resume a suspended coroutine
//! - coroutine-status: Get the status of a coroutine
//! - coroutine-value: Get the last yielded value
//! - yield-from: Delegate to a sub-coroutine
//! - coroutine->iterator: Convert coroutine to iterator
//! - coroutine-next: Get next value from coroutine iterator

use crate::value::fiber::{SignalBits, SIG_ERROR, SIG_RESUME};
use crate::value::{Condition, Coroutine, CoroutineState, ResumeOp, Value, SIG_OK};

/// F1: Create a coroutine from a function
///
/// (make-coroutine fn) -> coroutine
pub fn prim_make_coroutine(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "make-coroutine: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    if let Some(c) = args[0].as_closure() {
        if !c.effect.may_yield() {
            eprintln!("warning: make-coroutine: closure cannot yield and will complete without suspending");
        }
        let coroutine = Coroutine::new((*c).clone());
        (SIG_OK, Value::coroutine(coroutine))
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(format!(
                "make-coroutine: expected function, got {}",
                args[0].type_name()
            ))),
        )
    }
}

/// F3: Get the status of a coroutine
///
/// (coroutine-status co) -> string
pub fn prim_coroutine_status(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "coroutine-status: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    if let Some(co) = args[0].as_coroutine() {
        let borrowed = co.borrow();
        let status = match &borrowed.state {
            CoroutineState::Created => "created",
            CoroutineState::Running => "running",
            CoroutineState::Suspended => "suspended",
            CoroutineState::Done => "done",
            CoroutineState::Error(_) => "error",
        };
        (SIG_OK, Value::string(status))
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(format!(
                "coroutine-status: expected coroutine, got {}",
                args[0].type_name()
            ))),
        )
    }
}

/// Check if a coroutine is done
///
/// (coroutine-done? co) -> bool
pub fn prim_coroutine_done(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "coroutine-done?: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    if let Some(co) = args[0].as_coroutine() {
        let borrowed = co.borrow();
        (
            SIG_OK,
            Value::bool(matches!(
                borrowed.state,
                CoroutineState::Done | CoroutineState::Error(_)
            )),
        )
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(format!(
                "coroutine-done?: expected coroutine, got {}",
                args[0].type_name()
            ))),
        )
    }
}

/// Get the last yielded value from a coroutine
///
/// (coroutine-value co) -> value
pub fn prim_coroutine_value(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "coroutine-value: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    if let Some(co) = args[0].as_coroutine() {
        let borrowed = co.borrow();
        // yielded_value is now Option<crate::value::Value> directly
        (SIG_OK, borrowed.yielded_value.unwrap_or(Value::NIL))
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(format!(
                "coroutine-value: expected coroutine, got {}",
                args[0].type_name()
            ))),
        )
    }
}

/// Check if a value is a coroutine
///
/// (coroutine? val) -> bool
pub fn prim_is_coroutine(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "coroutine?: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    (SIG_OK, Value::bool(args[0].is_coroutine()))
}

/// F2/F4: Resume a coroutine
///
/// (coroutine-resume co) -> value
/// (coroutine-resume co val) -> value
///
/// Validates arguments and returns SIG_RESUME for the VM to handle.
/// The actual execution is done by the VM's SIG_RESUME handler.
pub fn prim_coroutine_resume(args: &[Value]) -> (SignalBits, Value) {
    if args.is_empty() || args.len() > 2 {
        let cond = Condition::arity_error(format!(
            "coroutine-resume: expected 1-2 arguments, got {}",
            args.len()
        ));
        return (SIG_ERROR, Value::condition(cond));
    }

    let resume_value = args.get(1).copied().unwrap_or(Value::EMPTY_LIST);

    if let Some(co) = args[0].as_coroutine() {
        // Quick validation: check state before handing off to VM
        {
            let borrowed = co.borrow();
            match &borrowed.state {
                CoroutineState::Running => {
                    return (
                        SIG_ERROR,
                        Value::condition(Condition::error(
                            "coroutine-resume: coroutine is already running",
                        )),
                    );
                }
                CoroutineState::Done => {
                    return (
                        SIG_ERROR,
                        Value::condition(Condition::error(
                            "coroutine-resume: cannot resume completed coroutine",
                        )),
                    );
                }
                CoroutineState::Error(e) => {
                    return (
                        SIG_ERROR,
                        Value::condition(Condition::error(format!(
                            "coroutine-resume: cannot resume errored coroutine: {}",
                            e
                        ))),
                    );
                }
                CoroutineState::Created | CoroutineState::Suspended => {
                    // Valid states for resume — will be handled by VM
                }
            }
        }

        // Set the resume operation for the VM handler
        co.borrow_mut().resume_op = Some(ResumeOp::Resume(resume_value));
        (SIG_RESUME, args[0])
    } else {
        let cond = Condition::type_error(format!(
            "coroutine-resume: expected coroutine, got {}",
            args[0].type_name()
        ));
        (SIG_ERROR, Value::condition(cond))
    }
}

/// F5: Delegate to a sub-coroutine
///
/// (yield-from co) -> value
///
/// Yields all values from the sub-coroutine until it completes,
/// then returns the sub-coroutine's final value.
///
/// Validates arguments and returns SIG_RESUME for the VM to handle.
/// The VM's SIG_RESUME handler performs the actual delegation setup.
pub fn prim_yield_from(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        let cond = Condition::arity_error(format!(
            "yield-from: expected 1 argument, got {}",
            args.len()
        ));
        return (SIG_ERROR, Value::condition(cond));
    }

    if let Some(sub_co) = args[0].as_coroutine() {
        // Check sub-coroutine state
        let state = {
            let borrowed = sub_co.borrow();
            borrowed.state.clone()
        };

        match &state {
            CoroutineState::Done => {
                // Sub-coroutine already done - just return its final value
                let borrowed = sub_co.borrow();
                (SIG_OK, borrowed.yielded_value.unwrap_or(Value::EMPTY_LIST))
            }
            CoroutineState::Error(e) => {
                let cond = Condition::error(format!("yield-from: sub-coroutine errored: {}", e));
                (SIG_ERROR, Value::condition(cond))
            }
            CoroutineState::Running => {
                let cond = Condition::error("yield-from: sub-coroutine is already running");
                (SIG_ERROR, Value::condition(cond))
            }
            CoroutineState::Created | CoroutineState::Suspended => {
                // Set the resume operation for the VM handler
                sub_co.borrow_mut().resume_op = Some(ResumeOp::YieldFrom);
                (SIG_RESUME, args[0])
            }
        }
    } else {
        let cond = Condition::type_error(format!(
            "yield-from: expected coroutine, got {}",
            args[0].type_name()
        ));
        (SIG_ERROR, Value::condition(cond))
    }
}

/// F6: Get an iterator from a coroutine
///
/// (coroutine->iterator co) -> iterator
///
/// Creates an iterator that yields values from the coroutine.
pub fn prim_coroutine_to_iterator(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "coroutine->iterator: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    if args[0].is_coroutine() {
        // For now, just return the coroutine itself
        // The for loop implementation will need to recognize coroutines
        (SIG_OK, args[0])
    } else {
        (
            SIG_ERROR,
            Value::condition(Condition::type_error(format!(
                "coroutine->iterator: expected coroutine, got {}",
                args[0].type_name()
            ))),
        )
    }
}

/// Get the next value from a coroutine iterator
///
/// (coroutine-next co) -> (value . done?)
///
/// Returns a pair of (value, done-flag).
/// If the coroutine is already done, returns immediately.
/// Otherwise, returns SIG_RESUME for the VM to handle.
pub fn prim_coroutine_next(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        let cond = Condition::arity_error(format!(
            "coroutine-next: expected 1 argument, got {}",
            args.len()
        ));
        return (SIG_ERROR, Value::condition(cond));
    }

    if let Some(co) = args[0].as_coroutine() {
        let is_done = {
            let borrowed = co.borrow();
            matches!(borrowed.state, CoroutineState::Done)
        };

        if is_done {
            // Return (nil . #t) to indicate done
            (
                SIG_OK,
                crate::value::cons(Value::EMPTY_LIST, Value::bool(true)),
            )
        } else {
            // Set the resume operation for the VM handler
            co.borrow_mut().resume_op = Some(ResumeOp::Next);
            (SIG_RESUME, args[0])
        }
    } else {
        let cond = Condition::type_error(format!(
            "coroutine-next: expected coroutine, got {}",
            args[0].type_name()
        ));
        (SIG_ERROR, Value::condition(cond))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::effects::Effect;
    use crate::value::{Arity, Closure};
    use std::rc::Rc;

    fn make_test_closure() -> Value {
        use crate::compiler::bytecode::Instruction;
        let bytecode = vec![
            Instruction::LoadConst as u8,
            0,
            0,
            Instruction::Return as u8,
        ];

        Value::closure(Closure {
            bytecode: Rc::new(bytecode),
            arity: Arity::Exact(0),
            env: Rc::new(vec![]),
            num_locals: 0,
            num_captures: 0,
            constants: Rc::new(vec![Value::NIL]),
            effect: Effect::none(),
            cell_params_mask: 0,
            symbol_names: Rc::new(std::collections::HashMap::new()),
            location_map: Rc::new(crate::error::LocationMap::new()),
            jit_code: None,
            lir_function: None,
        })
    }

    #[test]
    fn test_make_coroutine() {
        let closure = make_test_closure();
        let (sig, result_val) = prim_make_coroutine(&[closure]);
        assert_eq!(sig, SIG_OK);
        if let Some(co) = result_val.as_coroutine() {
            let borrowed = co.borrow();
            assert!(matches!(borrowed.state, CoroutineState::Created));
        } else {
            panic!("Expected coroutine");
        }
    }

    #[test]
    fn test_make_coroutine_wrong_type() {
        let (sig, _) = prim_make_coroutine(&[Value::int(42)]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_coroutine_status() {
        let closure = make_test_closure();
        let (sig, co) = prim_make_coroutine(&[closure]);
        assert_eq!(sig, SIG_OK);
        let (sig, status) = prim_coroutine_status(&[co]);
        assert_eq!(sig, SIG_OK);
        assert_eq!(status, Value::string("created"));
    }

    #[test]
    fn test_coroutine_done() {
        let closure = make_test_closure();
        let (sig, co) = prim_make_coroutine(&[closure]);
        assert_eq!(sig, SIG_OK);
        let (sig, done) = prim_coroutine_done(&[co]);
        assert_eq!(sig, SIG_OK);
        assert_eq!(done, Value::bool(false));
    }

    #[test]
    fn test_is_coroutine() {
        let closure = make_test_closure();
        let (sig, co) = prim_make_coroutine(&[closure]);
        assert_eq!(sig, SIG_OK);

        let (sig, result) = prim_is_coroutine(&[co]);
        assert_eq!(sig, SIG_OK);
        assert_eq!(result, Value::bool(true));

        let (sig, result) = prim_is_coroutine(&[Value::int(42)]);
        assert_eq!(sig, SIG_OK);
        assert_eq!(result, Value::bool(false));
    }

    #[test]
    fn test_coroutine_value() {
        let closure = make_test_closure();
        let (sig, co) = prim_make_coroutine(&[closure]);
        assert_eq!(sig, SIG_OK);
        let (sig, value) = prim_coroutine_value(&[co]);
        assert_eq!(sig, SIG_OK);
        assert_eq!(value, Value::NIL);
    }

    #[test]
    fn test_coroutine_resume_wrong_type() {
        let (sig, _) = prim_coroutine_resume(&[Value::int(42)]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_coroutine_resume_created_returns_sig_resume() {
        // Primitives now return SIG_RESUME for the VM to handle
        let closure = make_test_closure();
        let (sig, co) = prim_make_coroutine(&[closure]);
        assert_eq!(sig, SIG_OK);
        let (sig, val) = prim_coroutine_resume(&[co]);
        assert_eq!(sig, SIG_RESUME);
        // The returned value is the coroutine itself
        assert!(val.is_coroutine());
        // The resume_op should be set
        if let Some(co_ref) = val.as_coroutine() {
            let borrowed = co_ref.borrow();
            assert!(matches!(borrowed.resume_op, Some(ResumeOp::Resume(_))));
        }
    }

    #[test]
    fn test_coroutine_resume_done_returns_error() {
        let closure = make_test_closure();
        let (_, co) = prim_make_coroutine(&[closure]);
        // Manually set to Done
        if let Some(co_ref) = co.as_coroutine() {
            co_ref.borrow_mut().state = CoroutineState::Done;
        }
        let (sig, _) = prim_coroutine_resume(&[co]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_yield_from_wrong_type() {
        let (sig, _) = prim_yield_from(&[Value::int(42)]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_yield_from_returns_sig_resume() {
        let closure = make_test_closure();
        let (_, co) = prim_make_coroutine(&[closure]);
        let (sig, val) = prim_yield_from(&[co]);
        assert_eq!(sig, SIG_RESUME);
        assert!(val.is_coroutine());
        if let Some(co_ref) = val.as_coroutine() {
            let borrowed = co_ref.borrow();
            assert!(matches!(borrowed.resume_op, Some(ResumeOp::YieldFrom)));
        }
    }

    #[test]
    fn test_coroutine_to_iterator() {
        let closure = make_test_closure();
        let (sig, co) = prim_make_coroutine(&[closure]);
        assert_eq!(sig, SIG_OK);
        let (sig, iter) = prim_coroutine_to_iterator(std::slice::from_ref(&co));
        assert_eq!(sig, SIG_OK);
        assert!(iter.is_coroutine());
    }

    #[test]
    fn test_coroutine_next_wrong_type() {
        let (sig, _) = prim_coroutine_next(&[Value::int(42)]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_coroutine_next_done() {
        let closure = make_test_closure();
        let (_, co) = prim_make_coroutine(&[closure]);
        // Manually set to Done
        if let Some(co_ref) = co.as_coroutine() {
            co_ref.borrow_mut().state = CoroutineState::Done;
        }
        let (sig, result) = prim_coroutine_next(&[co]);
        assert_eq!(sig, SIG_OK);
        if let Some(cons) = result.as_cons() {
            assert_eq!(cons.first, Value::EMPTY_LIST);
            assert_eq!(cons.rest, Value::bool(true));
        } else {
            panic!("Expected cons pair");
        }
    }

    #[test]
    fn test_coroutine_next_not_done_returns_sig_resume() {
        let closure = make_test_closure();
        let (_, co) = prim_make_coroutine(&[closure]);
        let (sig, val) = prim_coroutine_next(&[co]);
        assert_eq!(sig, SIG_RESUME);
        assert!(val.is_coroutine());
        if let Some(co_ref) = val.as_coroutine() {
            let borrowed = co_ref.borrow();
            assert!(matches!(borrowed.resume_op, Some(ResumeOp::Next)));
        }
    }
}
