//! Bytecode execution entry points and helpers.

use crate::value::{SignalBits, Value, SIG_OK};
use std::rc::Rc;

use super::core::VM;

impl VM {
    /// Wrapper that calls execute_bytecode_inner_impl with start_ip = 0
    pub(super) fn execute_bytecode_inner(
        &mut self,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
    ) -> SignalBits {
        self.execute_bytecode_inner_impl(bytecode, constants, closure_env, 0)
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
        // Execute with tail call loop
        let mut current_bytecode = bytecode.to_vec();
        let mut current_constants = constants.to_vec();
        let mut current_env = closure_env.cloned();
        let mut current_ip = start_ip;

        loop {
            let bits = self.execute_bytecode_inner_impl(
                &current_bytecode,
                &current_constants,
                current_env.as_ref(),
                current_ip,
            );

            if bits != SIG_OK {
                break bits;
            }

            if let Some((tail_bytecode, tail_constants, tail_env)) = self.pending_tail_call.take() {
                current_bytecode = tail_bytecode;
                current_constants = tail_constants;
                current_env = Some(tail_env);
                current_ip = 0;
            } else {
                break bits;
            }
        }
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
