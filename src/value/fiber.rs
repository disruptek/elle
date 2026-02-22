//! Fiber types for the Elle runtime.
//!
//! A fiber is an independent execution context: it owns its operand stack,
//! call frames, and signal state. The VM dispatches into the current fiber;
//! suspended fibers are stored as heap values.

use crate::value::closure::Closure;
use crate::value::Value;
use smallvec::SmallVec;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

/// Saved execution context for a suspended fiber.
///
/// When a fiber suspends (via `fiber/signal`), the VM saves the bytecode,
/// constants, environment, and instruction pointer so execution can resume
/// from exactly where it left off.
#[derive(Debug, Clone)]
pub struct SavedContext {
    pub bytecode: Vec<u8>,
    pub constants: Vec<Value>,
    pub env: Option<Rc<Vec<Value>>>,
    pub ip: usize,
}

/// Signal type bits. The first 16 are compiler-reserved.
pub type SignalBits = u32;

pub const SIG_OK: SignalBits = 0; // no bits set = normal return
pub const SIG_ERROR: SignalBits = 1 << 0; // exception / panic
pub const SIG_YIELD: SignalBits = 1 << 1; // cooperative suspension
pub const SIG_DEBUG: SignalBits = 1 << 2; // breakpoint / trace
pub const SIG_RESUME: SignalBits = 1 << 3; // fiber resumption (VM-internal)
pub const SIG_FFI: SignalBits = 1 << 4; // calls foreign code

// Signal bit partitioning:
//
//   Bits 0-2:   User-facing signals (error, yield, debug)
//   Bit  3:     VM operation (resume) — not visible to user code
//   Bit  4:     FFI — calls foreign code
//   Bits 5-15:  Reserved for future use
//   Bits 16-31: User-defined signal types
//
// The VM dispatch loop checks all bits. User code only sees
// bits 0-2 and 16-31. Bits 3-15 are internal.

/// Fiber status. Matches Janet's model.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FiberStatus {
    /// Not yet started (has closure but hasn't been resumed)
    New,
    /// Currently executing (on the VM's run stack)
    Alive,
    /// Suspended by a signal (waiting for resume)
    Suspended,
    /// Completed normally (returned a value)
    Dead,
    /// Terminated by an unhandled error signal
    Error,
}

impl FiberStatus {
    /// Human-readable name for display formatting.
    pub fn as_str(self) -> &'static str {
        match self {
            FiberStatus::New => "new",
            FiberStatus::Alive => "alive",
            FiberStatus::Suspended => "suspended",
            FiberStatus::Dead => "dead",
            FiberStatus::Error => "error",
        }
    }
}

/// A single call frame within a fiber (for execution dispatch).
#[derive(Debug, Clone)]
pub struct Frame {
    /// The closure being executed
    pub closure: Rc<Closure>,
    /// Instruction pointer (byte offset into bytecode)
    pub ip: usize,
    /// Base index in the fiber's operand stack for this frame's temporaries
    pub base: usize,
}

/// Call frame for stack traces (name + ip + frame_base).
/// Separate from Frame because stack traces need human-readable names,
/// while execution dispatch needs closure references.
#[derive(Debug, Clone)]
pub struct CallFrame {
    pub name: String,
    pub ip: usize,
    pub frame_base: usize,
}

/// The fiber: an independent execution context.
///
/// Holds all per-execution state that was previously on the VM struct:
/// operand stack, call frames, exception handlers.
/// The VM retains only global/shared state (globals, modules, JIT cache, FFI).
pub struct Fiber {
    /// Operand stack (temporaries). SmallVec avoids heap allocation for
    /// fibers with fewer than 256 stack entries.
    pub stack: SmallVec<[Value; 256]>,
    /// Call frame stack (for fiber execution — closure + ip + base)
    pub frames: Vec<Frame>,
    /// Current status
    pub status: FiberStatus,
    /// Signal mask: which of this fiber's signals are caught by its parent.
    /// Set at creation time by the parent. Immutable after creation.
    pub mask: SignalBits,
    /// Parent fiber (Weak to avoid Rc cycles)
    pub parent: Option<Weak<RefCell<Fiber>>>,
    /// Most recently resumed child (for stack traces, not ownership)
    pub child: Option<Rc<RefCell<Fiber>>>,
    /// The closure this fiber was created from
    pub closure: Rc<Closure>,
    /// Dynamic bindings (fiber-scoped state)
    pub env: Option<HashMap<u32, Value>>,
    /// Signal value from this fiber. Canonical location for both
    /// signal payloads and normal return values.
    /// - On signal: (bits, payload) before suspending
    /// - On normal return: (SIG_OK, return_value) before completing
    pub signal: Option<(SignalBits, Value)>,
    /// Saved execution context for resuming a suspended fiber.
    /// Set when the fiber suspends; consumed when it resumes.
    pub saved_context: Option<SavedContext>,
    /// IP at the point the dispatch loop exited due to a signal.
    /// Set by the dispatch loop; consumed by fiber resume.
    pub suspended_ip: Option<usize>,

    // --- Execution state migrated from VM ---
    /// Call depth counter (for stack overflow detection)
    pub call_depth: usize,
    /// Call stack for stack traces (name + ip + frame_base)
    pub call_stack: Vec<CallFrame>,
    /// Storage for the continuation value on yield.
    /// Used by the yield instruction to capture the call chain so
    /// yield-through-nested-calls can resume from the exact point.
    pub continuation: Option<Value>,
}

impl Fiber {
    /// Create a new fiber from a closure with the given signal mask.
    pub fn new(closure: Rc<Closure>, mask: SignalBits) -> Self {
        Fiber {
            stack: SmallVec::new(),
            frames: Vec::new(),
            status: FiberStatus::New,
            mask,
            parent: None,
            child: None,
            closure,
            env: None,
            signal: None,
            saved_context: None,
            suspended_ip: None,
            call_depth: 0,
            call_stack: Vec::new(),
            continuation: None,
        }
    }
}

impl std::fmt::Debug for Fiber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<fiber:{} frames={} stack={}>",
            self.status.as_str(),
            self.frames.len(),
            self.stack.len()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::effects::Effect;
    use crate::error::LocationMap;
    use crate::value::types::Arity;

    fn test_closure() -> Rc<Closure> {
        Rc::new(Closure {
            bytecode: Rc::new(vec![]),
            arity: Arity::Exact(0),
            env: Rc::new(vec![]),
            num_locals: 0,
            num_captures: 0,
            constants: Rc::new(vec![]),
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
        let fiber = Fiber::new(test_closure(), SIG_ERROR | SIG_YIELD);
        assert_eq!(fiber.status, FiberStatus::New);
        assert_eq!(fiber.mask, SIG_ERROR | SIG_YIELD);
        assert!(fiber.stack.is_empty());
        assert!(fiber.frames.is_empty());
        assert!(fiber.parent.is_none());
        assert!(fiber.child.is_none());
        assert!(fiber.env.is_none());
        assert!(fiber.signal.is_none());
    }

    #[test]
    fn test_fiber_status_transitions() {
        let mut fiber = Fiber::new(test_closure(), 0);
        assert_eq!(fiber.status, FiberStatus::New);

        fiber.status = FiberStatus::Alive;
        assert_eq!(fiber.status, FiberStatus::Alive);

        fiber.status = FiberStatus::Suspended;
        fiber.signal = Some((SIG_YIELD, Value::int(42)));
        assert_eq!(fiber.status, FiberStatus::Suspended);
        assert_eq!(fiber.signal, Some((SIG_YIELD, Value::int(42))));

        fiber.status = FiberStatus::Dead;
        fiber.signal = Some((SIG_OK, Value::int(99)));
        assert_eq!(fiber.status, FiberStatus::Dead);
        assert_eq!(fiber.signal, Some((SIG_OK, Value::int(99))));

        // Reset and test error path
        let mut fiber2 = Fiber::new(test_closure(), 0);
        fiber2.status = FiberStatus::Error;
        fiber2.signal = Some((SIG_ERROR, Value::string("boom")));
        assert_eq!(fiber2.status, FiberStatus::Error);
    }

    #[test]
    fn test_fiber_stack_operations() {
        let mut fiber = Fiber::new(test_closure(), 0);
        fiber.stack.push(Value::int(1));
        fiber.stack.push(Value::int(2));
        fiber.stack.push(Value::int(3));
        assert_eq!(fiber.stack.len(), 3);
        assert_eq!(fiber.stack.pop(), Some(Value::int(3)));
        assert_eq!(fiber.stack.len(), 2);
    }

    #[test]
    fn test_fiber_frame_operations() {
        let closure = test_closure();
        let mut fiber = Fiber::new(closure.clone(), 0);

        let frame = Frame {
            closure: closure.clone(),
            ip: 0,
            base: 0,
        };
        fiber.frames.push(frame);
        assert_eq!(fiber.frames.len(), 1);
        assert_eq!(fiber.frames[0].ip, 0);
        assert_eq!(fiber.frames[0].base, 0);

        let frame2 = Frame {
            closure,
            ip: 10,
            base: 3,
        };
        fiber.frames.push(frame2);
        assert_eq!(fiber.frames.len(), 2);
        assert_eq!(fiber.frames[1].ip, 10);
        assert_eq!(fiber.frames[1].base, 3);
    }

    #[test]
    fn test_fiber_parent_child() {
        let parent = Rc::new(RefCell::new(Fiber::new(test_closure(), 0)));
        let child = Rc::new(RefCell::new(Fiber::new(test_closure(), SIG_ERROR)));

        child.borrow_mut().parent = Some(Rc::downgrade(&parent));
        parent.borrow_mut().child = Some(child.clone());

        // Parent can reach child
        assert!(parent.borrow().child.is_some());

        // Child can reach parent (via upgrade)
        {
            let child_ref = child.borrow();
            let parent_ref = child_ref.parent.as_ref().unwrap().upgrade();
            assert!(parent_ref.is_some());
        }

        // Drop parent — child's weak ref becomes invalid
        drop(parent);
        let child_ref = child.borrow();
        let parent_ref = child_ref.parent.as_ref().unwrap().upgrade();
        assert!(parent_ref.is_none());
    }

    #[test]
    fn test_signal_bits() {
        assert_eq!(SIG_OK, 0);
        assert_eq!(SIG_ERROR, 1);
        assert_eq!(SIG_YIELD, 2);
        assert_eq!(SIG_DEBUG, 4);
        assert_eq!(SIG_RESUME, 8);

        // Mask catches error and yield but not debug
        let mask = SIG_ERROR | SIG_YIELD;
        assert_ne!(mask & SIG_ERROR, 0);
        assert_ne!(mask & SIG_YIELD, 0);
        assert_eq!(mask & SIG_DEBUG, 0);
        assert_eq!(mask & SIG_RESUME, 0);

        // User-defined signals in upper 16 bits
        let user_sig: SignalBits = 1 << 16;
        assert_eq!(user_sig & mask, 0);
    }

    #[test]
    fn test_fiber_status_display() {
        assert_eq!(FiberStatus::New.as_str(), "new");
        assert_eq!(FiberStatus::Alive.as_str(), "alive");
        assert_eq!(FiberStatus::Suspended.as_str(), "suspended");
        assert_eq!(FiberStatus::Dead.as_str(), "dead");
        assert_eq!(FiberStatus::Error.as_str(), "error");
    }

    #[test]
    fn test_fiber_debug_format() {
        let fiber = Fiber::new(test_closure(), 0);
        let debug = format!("{:?}", fiber);
        assert!(debug.contains("fiber:new"));
        assert!(debug.contains("frames=0"));
        assert!(debug.contains("stack=0"));
    }

    #[test]
    fn test_fiber_zero_mask() {
        // A fiber with mask=0 propagates all signals
        let fiber = Fiber::new(test_closure(), 0);
        assert_eq!(fiber.mask & SIG_ERROR, 0);
        assert_eq!(fiber.mask & SIG_YIELD, 0);
    }

    #[test]
    fn test_fiber_full_mask() {
        // A fiber with all bits set catches everything
        let fiber = Fiber::new(test_closure(), u32::MAX);
        assert_ne!(fiber.mask & SIG_ERROR, 0);
        assert_ne!(fiber.mask & SIG_YIELD, 0);
        assert_ne!(fiber.mask & SIG_DEBUG, 0);
        assert_ne!(fiber.mask & SIG_RESUME, 0);
    }
}
