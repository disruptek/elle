//! Fiber primitives for Elle.
//!
//! Fibers are independent execution contexts with their own stack, frames,
//! and signal state. They communicate via signals — a fiber can emit a
//! signal, and its parent can catch or propagate it based on the mask.
//!
//! Primitives:
//! - fiber/new: Create a fiber from a closure with a signal mask
//! - fiber/resume: Resume a suspended fiber, delivering a value
//! - fiber/signal: Emit a signal from the current fiber
//! - fiber/status: Get fiber lifecycle status
//! - fiber/value: Get signal payload from last signal
//! - fiber/bits: Get signal bits from last signal
//! - fiber/mask: Get the fiber's signal mask
//! - fiber?: Type predicate

use crate::value::fiber::{Fiber, FiberStatus, SignalBits, SIG_ERROR, SIG_OK, SIG_RESUME};
use crate::value::{Condition, Value};

/// (fiber/new fn mask) → fiber
///
/// Create a fiber from a closure and a signal mask. The mask determines
/// which signals the parent catches when resuming this fiber.
pub fn prim_fiber_new(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 2 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "fiber/new: expected 2 arguments, got {}",
                args.len()
            ))),
        );
    }

    let closure = match args[0].as_closure() {
        Some(c) => c.clone(),
        None => {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(format!(
                    "fiber/new: expected closure, got {}",
                    args[0].type_name()
                ))),
            );
        }
    };

    let mask = match args[1].as_int() {
        Some(m) => m as SignalBits,
        None => {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(format!(
                    "fiber/new: expected integer mask, got {}",
                    args[1].type_name()
                ))),
            );
        }
    };

    let fiber = Fiber::new(closure, mask);
    (SIG_OK, Value::fiber(fiber))
}

/// (fiber/resume fiber) → value
/// (fiber/resume fiber value) → value
///
/// Resume a fiber. If the fiber is New, starts execution. If Suspended,
/// delivers the value and continues from where it left off.
///
/// Returns SIG_RESUME — the VM handles the actual fiber swap.
pub fn prim_fiber_resume(args: &[Value]) -> (SignalBits, Value) {
    if args.is_empty() || args.len() > 2 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "fiber/resume: expected 1-2 arguments, got {}",
                args.len()
            ))),
        );
    }

    let fiber_rc = match args[0].as_fiber() {
        Some(f) => f.clone(),
        None => {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(format!(
                    "fiber/resume: expected fiber, got {}",
                    args[0].type_name()
                ))),
            );
        }
    };

    let resume_value = args.get(1).copied().unwrap_or(Value::NIL);

    // Validate fiber status
    {
        let fiber = fiber_rc.borrow();
        match fiber.status {
            FiberStatus::New | FiberStatus::Suspended => {
                // Valid for resume
            }
            FiberStatus::Alive => {
                return (
                    SIG_ERROR,
                    Value::condition(Condition::error("fiber/resume: fiber is already running")),
                );
            }
            FiberStatus::Dead => {
                return (
                    SIG_ERROR,
                    Value::condition(Condition::error(
                        "fiber/resume: cannot resume completed fiber",
                    )),
                );
            }
            FiberStatus::Error => {
                return (
                    SIG_ERROR,
                    Value::condition(Condition::error(
                        "fiber/resume: cannot resume errored fiber",
                    )),
                );
            }
        }
    }

    // Store the resume value on the fiber for the VM to use
    fiber_rc.borrow_mut().signal = Some((SIG_OK, resume_value));

    // Return SIG_RESUME — VM will handle the fiber swap
    (SIG_RESUME, args[0])
}

/// (fiber/signal bits value) → suspends
///
/// Emit a signal from the current fiber. The signal bits and value are
/// returned directly — the VM's dispatch loop stores them in fiber.signal
/// and suspends the fiber.
pub fn prim_fiber_signal(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 2 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "fiber/signal: expected 2 arguments, got {}",
                args.len()
            ))),
        );
    }

    let bits = match args[0].as_int() {
        Some(b) => b as SignalBits,
        None => {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(format!(
                    "fiber/signal: expected integer bits, got {}",
                    args[0].type_name()
                ))),
            );
        }
    };

    // Return the signal bits and value directly.
    // The VM's handle_primitive_signal catch-all stores (bits, value)
    // in fiber.signal and returns Some(bits), suspending the fiber.
    (bits, args[1])
}

/// (fiber/status fiber) → keyword
///
/// Returns the fiber's lifecycle status as a keyword.
pub fn prim_fiber_status(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "fiber/status: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    let fiber_rc = match args[0].as_fiber() {
        Some(f) => f.clone(),
        None => {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(format!(
                    "fiber/status: expected fiber, got {}",
                    args[0].type_name()
                ))),
            );
        }
    };

    let status = fiber_rc.borrow().status;
    (SIG_OK, Value::string(status.as_str()))
}

/// (fiber/value fiber) → value
///
/// Returns the signal payload from the fiber's last signal or return value.
/// Returns nil if the fiber has no signal.
pub fn prim_fiber_value(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "fiber/value: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    let fiber_rc = match args[0].as_fiber() {
        Some(f) => f.clone(),
        None => {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(format!(
                    "fiber/value: expected fiber, got {}",
                    args[0].type_name()
                ))),
            );
        }
    };

    let value = fiber_rc
        .borrow()
        .signal
        .as_ref()
        .map(|(_, v)| *v)
        .unwrap_or(Value::NIL);
    (SIG_OK, value)
}

/// (fiber/bits fiber) → int
///
/// Returns the signal bits from the fiber's last signal.
/// Returns 0 if the fiber has no signal.
pub fn prim_fiber_bits(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "fiber/bits: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    let fiber_rc = match args[0].as_fiber() {
        Some(f) => f.clone(),
        None => {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(format!(
                    "fiber/bits: expected fiber, got {}",
                    args[0].type_name()
                ))),
            );
        }
    };

    let bits = fiber_rc
        .borrow()
        .signal
        .as_ref()
        .map(|(b, _)| *b)
        .unwrap_or(0);
    (SIG_OK, Value::int(bits as i64))
}

/// (fiber/mask fiber) → int
///
/// Returns the fiber's signal mask.
pub fn prim_fiber_mask(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "fiber/mask: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    let fiber_rc = match args[0].as_fiber() {
        Some(f) => f.clone(),
        None => {
            return (
                SIG_ERROR,
                Value::condition(Condition::type_error(format!(
                    "fiber/mask: expected fiber, got {}",
                    args[0].type_name()
                ))),
            );
        }
    };

    let mask = fiber_rc.borrow().mask;
    (SIG_OK, Value::int(mask as i64))
}

/// (fiber? value) → bool
///
/// Type predicate: returns #t if the value is a fiber.
pub fn prim_is_fiber(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            Value::condition(Condition::arity_error(format!(
                "fiber?: expected 1 argument, got {}",
                args.len()
            ))),
        );
    }

    (SIG_OK, Value::bool(args[0].is_fiber()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::effects::Effect;
    use crate::error::LocationMap;
    use crate::value::fiber::{SIG_ERROR as FIBER_SIG_ERROR, SIG_YIELD};
    use crate::value::{Arity, Closure};
    use std::collections::HashMap;
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
            constants: Rc::new(vec![Value::int(42)]),
            effect: Effect::none(),
            cell_params_mask: 0,
            symbol_names: Rc::new(HashMap::new()),
            location_map: Rc::new(LocationMap::new()),
            jit_code: None,
            lir_function: None,
        })
    }

    #[test]
    fn test_fiber_new() {
        let closure = make_test_closure();
        let mask = Value::int((SIG_ERROR | SIG_YIELD) as i64);
        let (sig, fiber_val) = prim_fiber_new(&[closure, mask]);
        assert_eq!(sig, SIG_OK);
        assert!(fiber_val.is_fiber());

        let fiber_rc = fiber_val.as_fiber().unwrap();
        let fiber = fiber_rc.borrow();
        assert_eq!(fiber.status, FiberStatus::New);
        assert_eq!(fiber.mask, FIBER_SIG_ERROR | SIG_YIELD);
    }

    #[test]
    fn test_fiber_new_wrong_type() {
        let (sig, _) = prim_fiber_new(&[Value::int(42), Value::int(0)]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_fiber_new_wrong_arity() {
        let (sig, _) = prim_fiber_new(&[make_test_closure()]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_fiber_resume_returns_sig_resume() {
        let closure = make_test_closure();
        let (_, fiber_val) = prim_fiber_new(&[closure, Value::int(0)]);
        let (sig, val) = prim_fiber_resume(&[fiber_val]);
        assert_eq!(sig, SIG_RESUME);
        assert!(val.is_fiber());
    }

    #[test]
    fn test_fiber_resume_dead_fiber() {
        let closure = make_test_closure();
        let (_, fiber_val) = prim_fiber_new(&[closure, Value::int(0)]);
        // Manually set to Dead
        fiber_val.as_fiber().unwrap().borrow_mut().status = FiberStatus::Dead;
        let (sig, _) = prim_fiber_resume(&[fiber_val]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_fiber_resume_alive_fiber() {
        let closure = make_test_closure();
        let (_, fiber_val) = prim_fiber_new(&[closure, Value::int(0)]);
        fiber_val.as_fiber().unwrap().borrow_mut().status = FiberStatus::Alive;
        let (sig, _) = prim_fiber_resume(&[fiber_val]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_fiber_resume_with_value() {
        let closure = make_test_closure();
        let (_, fiber_val) = prim_fiber_new(&[closure, Value::int(0)]);
        let (sig, _) = prim_fiber_resume(&[fiber_val, Value::int(99)]);
        assert_eq!(sig, SIG_RESUME);
        // Check that the resume value was stored
        let fiber_rc = fiber_val.as_fiber().unwrap();
        let fiber = fiber_rc.borrow();
        assert_eq!(fiber.signal, Some((SIG_OK, Value::int(99))));
    }

    #[test]
    fn test_fiber_signal() {
        let bits = Value::int(SIG_YIELD as i64);
        let value = Value::int(42);
        let (sig, val) = prim_fiber_signal(&[bits, value]);
        assert_eq!(sig, SIG_YIELD);
        assert_eq!(val, Value::int(42));
    }

    #[test]
    fn test_fiber_signal_wrong_arity() {
        let (sig, _) = prim_fiber_signal(&[Value::int(0)]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_fiber_status() {
        let closure = make_test_closure();
        let (_, fiber_val) = prim_fiber_new(&[closure, Value::int(0)]);
        let (sig, status) = prim_fiber_status(&[fiber_val]);
        assert_eq!(sig, SIG_OK);
        assert_eq!(status, Value::string("new"));
    }

    #[test]
    fn test_fiber_status_transitions() {
        let closure = make_test_closure();
        let (_, fiber_val) = prim_fiber_new(&[closure, Value::int(0)]);

        // Test each status
        for (status, expected) in [
            (FiberStatus::New, "new"),
            (FiberStatus::Alive, "alive"),
            (FiberStatus::Suspended, "suspended"),
            (FiberStatus::Dead, "dead"),
            (FiberStatus::Error, "error"),
        ] {
            fiber_val.as_fiber().unwrap().borrow_mut().status = status;
            let (sig, val) = prim_fiber_status(&[fiber_val]);
            assert_eq!(sig, SIG_OK);
            assert_eq!(val, Value::string(expected));
        }
    }

    #[test]
    fn test_fiber_value() {
        let closure = make_test_closure();
        let (_, fiber_val) = prim_fiber_new(&[closure, Value::int(0)]);

        // No signal yet — returns nil
        let (sig, val) = prim_fiber_value(&[fiber_val]);
        assert_eq!(sig, SIG_OK);
        assert_eq!(val, Value::NIL);

        // Set a signal
        fiber_val.as_fiber().unwrap().borrow_mut().signal = Some((SIG_YIELD, Value::int(42)));
        let (sig, val) = prim_fiber_value(&[fiber_val]);
        assert_eq!(sig, SIG_OK);
        assert_eq!(val, Value::int(42));
    }

    #[test]
    fn test_fiber_bits() {
        let closure = make_test_closure();
        let (_, fiber_val) = prim_fiber_new(&[closure, Value::int(0)]);

        // No signal yet — returns 0
        let (sig, val) = prim_fiber_bits(&[fiber_val]);
        assert_eq!(sig, SIG_OK);
        assert_eq!(val, Value::int(0));

        // Set a signal
        fiber_val.as_fiber().unwrap().borrow_mut().signal = Some((SIG_YIELD, Value::int(42)));
        let (sig, val) = prim_fiber_bits(&[fiber_val]);
        assert_eq!(sig, SIG_OK);
        assert_eq!(val, Value::int(SIG_YIELD as i64));
    }

    #[test]
    fn test_fiber_mask() {
        let closure = make_test_closure();
        let mask = (FIBER_SIG_ERROR | SIG_YIELD) as i64;
        let (_, fiber_val) = prim_fiber_new(&[closure, Value::int(mask)]);
        let (sig, val) = prim_fiber_mask(&[fiber_val]);
        assert_eq!(sig, SIG_OK);
        assert_eq!(val, Value::int(mask));
    }

    #[test]
    fn test_is_fiber() {
        let closure = make_test_closure();
        let (_, fiber_val) = prim_fiber_new(&[closure, Value::int(0)]);

        let (sig, val) = prim_is_fiber(&[fiber_val]);
        assert_eq!(sig, SIG_OK);
        assert_eq!(val, Value::bool(true));

        let (sig, val) = prim_is_fiber(&[Value::int(42)]);
        assert_eq!(sig, SIG_OK);
        assert_eq!(val, Value::bool(false));
    }

    #[test]
    fn test_fiber_resume_wrong_type() {
        let (sig, _) = prim_fiber_resume(&[Value::int(42)]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_fiber_status_wrong_type() {
        let (sig, _) = prim_fiber_status(&[Value::int(42)]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_fiber_value_wrong_type() {
        let (sig, _) = prim_fiber_value(&[Value::int(42)]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_fiber_bits_wrong_type() {
        let (sig, _) = prim_fiber_bits(&[Value::int(42)]);
        assert_eq!(sig, SIG_ERROR);
    }

    #[test]
    fn test_fiber_mask_wrong_type() {
        let (sig, _) = prim_fiber_mask(&[Value::int(42)]);
        assert_eq!(sig, SIG_ERROR);
    }
}
