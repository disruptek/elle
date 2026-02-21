//! Bytecode execution entry points and helpers.
//!
//! This module contains the public execution methods and the tail call loop.
//! The `run()` method is the new signal-based entry point; the older
//! `execute_bytecode*` methods are kept for backward compatibility and
//! will be removed in Step 8.

use crate::value::{ExceptionHandler, SignalBits, Value, SIG_OK};
use smallvec::SmallVec;
use std::rc::Rc;

use super::core::VM;

impl VM {
    /// Execute bytecode starting from a specific instruction pointer.
    /// This is the main entry point for bytecode execution.
    /// Handler isolation happens at the Call instruction level, not here.
    fn execute_bytecode_inner_with_ip(
        &mut self,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        start_ip: usize,
    ) -> SignalBits {
        // Each bytecode frame has its own exception handler scope.
        let saved_handlers = std::mem::take(&mut self.fiber.exception_handlers);
        let saved_handling = self.fiber.handling_exception;
        self.fiber.handling_exception = false;

        let bits = self.execute_bytecode_inner_impl(bytecode, constants, closure_env, start_ip);

        self.fiber.exception_handlers = saved_handlers;
        self.fiber.handling_exception = saved_handling;

        bits
    }

    /// Wrapper that calls execute_bytecode_inner_impl with start_ip = 0
    pub(super) fn execute_bytecode_inner(
        &mut self,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
    ) -> SignalBits {
        self.execute_bytecode_inner_with_ip(bytecode, constants, closure_env, 0)
    }

    /// Execute bytecode starting from a specific instruction pointer.
    /// Used for resuming coroutines from where they yielded.
    pub fn execute_bytecode_from_ip(
        &mut self,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        start_ip: usize,
    ) -> SignalBits {
        self.execute_bytecode_inner_with_ip(bytecode, constants, closure_env, start_ip)
    }

    /// Execute bytecode starting from a specific IP with pre-set exception handler state.
    ///
    /// This is used when resuming a continuation frame that had active exception handlers
    /// when it was captured. Unlike `execute_bytecode_from_ip`, this method:
    /// 1. Sets the exception handlers to the provided state before execution
    /// 2. Restores the outer handlers after execution
    ///
    /// This ensures that `handler-case` blocks active at yield time remain active
    /// after resume.
    pub fn execute_bytecode_from_ip_with_state(
        &mut self,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        start_ip: usize,
        handlers: SmallVec<[ExceptionHandler; 2]>,
        handling: bool,
    ) -> SignalBits {
        // Save outer state
        let saved_handlers = std::mem::replace(&mut self.fiber.exception_handlers, handlers);
        let saved_handling = std::mem::replace(&mut self.fiber.handling_exception, handling);

        // Execute with tail call loop
        let mut current_bytecode = bytecode.to_vec();
        let mut current_constants = constants.to_vec();
        let mut current_env = closure_env.cloned();
        let mut current_ip = start_ip;

        let bits = loop {
            let bits = self.execute_bytecode_inner_impl(
                &current_bytecode,
                &current_constants,
                current_env.as_ref(),
                current_ip,
            );

            // If not OK, break immediately (yield, error, etc.)
            if bits != SIG_OK {
                break bits;
            }

            // Check for pending tail call
            if let Some((tail_bytecode, tail_constants, tail_env)) = self.pending_tail_call.take() {
                current_bytecode = tail_bytecode;
                current_constants = tail_constants;
                current_env = Some(tail_env);
                current_ip = 0; // Tail calls start from the beginning
            } else {
                break bits;
            }
        };

        // Restore outer state
        self.fiber.exception_handlers = saved_handlers;
        self.fiber.handling_exception = saved_handling;

        bits
    }

    /// Execute bytecode returning SignalBits (for fiber/closure execution).
    /// The result value is stored in `self.fiber.signal`.
    pub fn execute_bytecode_coroutine(
        &mut self,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
    ) -> SignalBits {
        // Save the caller's stack
        let saved_stack = std::mem::take(&mut self.fiber.stack);

        let mut current_bytecode = bytecode.to_vec();
        let mut current_constants = constants.to_vec();
        let mut current_env = closure_env.cloned();

        let bits = loop {
            let bits = self.execute_bytecode_inner(
                &current_bytecode,
                &current_constants,
                current_env.as_ref(),
            );

            // If not OK, break immediately (yield, error, etc.)
            if bits != SIG_OK {
                break bits;
            }

            if let Some((tail_bytecode, tail_constants, tail_env)) = self.pending_tail_call.take() {
                current_bytecode = tail_bytecode;
                current_constants = tail_constants;
                current_env = Some(tail_env);
            } else {
                break bits;
            }
        };

        // Restore the caller's stack
        self.fiber.stack = saved_stack;

        bits
    }
}
