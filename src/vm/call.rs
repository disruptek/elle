//! Call and TailCall instruction handlers.
//!
//! These are the most complex instructions in the VM, handling:
//! - Native function calls (unified signal-based dispatch)
//! - Closure calls with environment setup
//! - Fiber-aware execution (signal propagation, yield-through-calls)
//! - Tail call optimization
//! - JIT compilation and dispatch (when jit feature is enabled)

use crate::value::error_val;
use crate::value::fiber::{FiberStatus, SavedContext};
use crate::value::{
    Fiber, SignalBits, Value, SIG_CANCEL, SIG_ERROR, SIG_OK, SIG_PROPAGATE, SIG_RESUME, SIG_YIELD,
};
use std::rc::Rc;

/// Helper: set an error signal on the fiber.
fn set_error(fiber: &mut Fiber, kind: &str, msg: impl Into<String>) {
    fiber.signal = Some((SIG_ERROR, error_val(kind, msg)));
}

use super::core::VM;

use crate::jit::{JitCode, JitCompiler, TAIL_CALL_SENTINEL};

impl VM {
    /// Handle the Call instruction.
    ///
    /// Pops the function and arguments from the stack, calls the function,
    /// and pushes the result. Handles native functions, VM-aware functions,
    /// and closures with proper environment setup.
    ///
    /// Returns `Some(SignalBits)` if execution should return immediately,
    /// or `None` if the dispatch loop should continue.
    pub(super) fn handle_call(
        &mut self,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        ip: &mut usize,
    ) -> Option<SignalBits> {
        let arg_count = self.read_u8(bytecode, ip) as usize;
        let func = self
            .fiber
            .stack
            .pop()
            .expect("VM bug: Stack underflow on Call");

        let mut args = Vec::with_capacity(arg_count);
        for _ in 0..arg_count {
            args.push(
                self.fiber
                    .stack
                    .pop()
                    .expect("VM bug: Stack underflow on Call"),
            );
        }
        args.reverse();

        if let Some(f) = func.as_native_fn() {
            let (bits, value) = f(args.as_slice());
            return self.handle_primitive_signal(bits, value, bytecode, constants, closure_env, ip);
        }

        if let Some(closure) = func.as_closure() {
            self.fiber.call_depth += 1;
            if self.fiber.call_depth > 1000 {
                set_error(&mut self.fiber, "error", "Stack overflow");
                return Some(SIG_ERROR);
            }

            // Validate argument count
            if !self.check_arity(&closure.arity, args.len()) {
                self.fiber.call_depth -= 1;
                self.fiber.stack.push(Value::NIL);
                return None;
            }

            // JIT compilation and dispatch — only for non-suspending closures
            // Suspending closures can never be JIT-compiled, so skip profiling overhead
            if !closure.effect.may_suspend() {
                let bytecode_ptr = closure.bytecode.as_ptr();
                let is_hot = self.record_closure_call(bytecode_ptr);

                // Check if we already have JIT code for this closure
                if let Some(jit_code) = self.jit_cache.get(&bytecode_ptr).cloned() {
                    let result = self.call_jit(&jit_code, closure, &args, func);
                    // Check if the JIT function (or a callee) set an error
                    if matches!(self.fiber.signal, Some((SIG_ERROR, _))) {
                        self.fiber.call_depth -= 1;
                        self.fiber.stack.push(Value::NIL);
                        return None; // Let the dispatch loop's error check deal with it
                    }
                    // Check for pending tail call (JIT function did a TailCall)
                    if result.to_bits() == TAIL_CALL_SENTINEL {
                        if let Some((tail_bc, tail_consts, tail_env)) =
                            self.pending_tail_call.take()
                        {
                            match self.execute_bytecode(&tail_bc, &tail_consts, Some(&tail_env)) {
                                Ok(val) => {
                                    self.fiber.call_depth -= 1;
                                    self.fiber.stack.push(val);
                                    return None;
                                }
                                Err(e) => {
                                    self.fiber.call_depth -= 1;
                                    set_error(&mut self.fiber, "error", e);
                                    self.fiber.stack.push(Value::NIL);
                                    return None;
                                }
                            }
                        }
                    }
                    self.fiber.call_depth -= 1;
                    self.fiber.stack.push(result);
                    return None;
                }

                // If hot, attempt JIT compilation
                if is_hot {
                    if let Some(ref lir_func) = closure.lir_function {
                        match JitCompiler::new() {
                            Ok(compiler) => {
                                match compiler.compile(lir_func) {
                                    Ok(jit_code) => {
                                        let jit_code = Rc::new(jit_code);
                                        self.jit_cache.insert(bytecode_ptr, jit_code.clone());
                                        let result = self.call_jit(&jit_code, closure, &args, func);
                                        if matches!(self.fiber.signal, Some((SIG_ERROR, _))) {
                                            self.fiber.call_depth -= 1;
                                            self.fiber.stack.push(Value::NIL);
                                            return None;
                                        }
                                        if result.to_bits() == TAIL_CALL_SENTINEL {
                                            if let Some((tail_bc, tail_consts, tail_env)) =
                                                self.pending_tail_call.take()
                                            {
                                                match self.execute_bytecode(
                                                    &tail_bc,
                                                    &tail_consts,
                                                    Some(&tail_env),
                                                ) {
                                                    Ok(val) => {
                                                        self.fiber.call_depth -= 1;
                                                        self.fiber.stack.push(val);
                                                        return None;
                                                    }
                                                    Err(e) => {
                                                        self.fiber.call_depth -= 1;
                                                        set_error(&mut self.fiber, "error", e);
                                                        self.fiber.stack.push(Value::NIL);
                                                        return None;
                                                    }
                                                }
                                            }
                                        }
                                        self.fiber.call_depth -= 1;
                                        self.fiber.stack.push(result);
                                        return None;
                                    }
                                    Err(e) => {
                                        match &e {
                                            crate::jit::JitError::UnsupportedInstruction(_) => {
                                                // MakeClosure and other instructions not yet in JIT.
                                                // Fall back to interpreter — the function still works.
                                            }
                                            _ => {
                                                panic!(
                                                    "JIT compilation failed for pure function: {}. \
                                                     This is a bug — pure functions should be JIT-compilable. \
                                                     Error: {}",
                                                    closure.lir_function.as_ref()
                                                        .map(|f| f.name.as_deref().unwrap_or("<anon>"))
                                                        .unwrap_or("<no lir>"),
                                                    e
                                                );
                                            }
                                        }
                                    }
                                }
                            }
                            Err(e) => {
                                panic!("JIT compiler creation failed: {}. This is a bug.", e);
                            }
                        }
                    }
                }
            }

            // Build the new environment
            let new_env_rc = self.build_closure_env(closure, &args);

            // Execute the closure using execute_bytecode_coroutine which
            // saves/restores the caller's stack and propagates signals.
            // Essential for fiber/signal propagation and yield-through-nested-calls.
            let bits = self.execute_bytecode_coroutine(
                &closure.bytecode,
                &closure.constants,
                Some(&new_env_rc),
            );

            self.fiber.call_depth -= 1;

            match bits {
                SIG_OK => {
                    let (_, value) = self.fiber.signal.take().unwrap();
                    self.fiber.stack.push(value);
                }
                SIG_YIELD => {
                    // Yield propagated from a nested call. Two cases:
                    //
                    // 1. yield instruction: continuation exists — append the
                    //    caller's frame so resume replays the full call stack.
                    //
                    // 2. fiber/signal: no continuation — just propagate the
                    //    signal. The fiber uses SavedContext for resumption,
                    //    which resumes from the fiber body level (intermediate
                    //    frames are abandoned).
                    if let Some(continuation) = self.fiber.continuation.take() {
                        let (_, value) = self.fiber.signal.take().unwrap();

                        let caller_stack: Vec<Value> = self.fiber.stack.drain(..).collect();
                        let caller_frame = crate::value::ContinuationFrame {
                            bytecode: Rc::new(bytecode.to_vec()),
                            constants: Rc::new(constants.to_vec()),
                            env: closure_env.cloned().unwrap_or_else(|| Rc::new(vec![])),
                            ip: *ip,
                            stack: caller_stack,
                        };

                        let mut cont_data = continuation
                            .as_continuation()
                            .expect("Yielded continuation must be a continuation value")
                            .as_ref()
                            .clone();
                        cont_data.append_frame(caller_frame);

                        let new_continuation = Value::continuation(cont_data);
                        self.fiber.signal = Some((SIG_YIELD, value));
                        self.fiber.continuation = Some(new_continuation);
                    }
                    return Some(SIG_YIELD);
                }
                _ => {
                    // Other signal (error, etc.) — propagate to caller.
                    return Some(bits);
                }
            }
            return None;
        }

        // Cannot call this value
        set_error(
            &mut self.fiber,
            "type-error",
            format!("Cannot call {:?}", func),
        );
        self.fiber.stack.push(Value::NIL);
        None
    }

    /// Handle the TailCall instruction.
    ///
    /// Similar to Call but sets up a pending tail call instead of recursing,
    /// enabling tail call optimization.
    ///
    /// Returns `Some(SignalBits)` if execution should return immediately,
    /// or `None` if the dispatch loop should continue.
    pub(super) fn handle_tail_call(
        &mut self,
        ip: &mut usize,
        bytecode: &[u8],
    ) -> Option<SignalBits> {
        let arg_count = self.read_u8(bytecode, ip) as usize;
        let func = self
            .fiber
            .stack
            .pop()
            .expect("VM bug: Stack underflow on TailCall");

        let mut args = Vec::with_capacity(arg_count);
        for _ in 0..arg_count {
            args.push(
                self.fiber
                    .stack
                    .pop()
                    .expect("VM bug: Stack underflow on TailCall"),
            );
        }
        args.reverse();

        if let Some(f) = func.as_native_fn() {
            let (bits, value) = f(&args);
            return Some(self.handle_primitive_signal_tail(bits, value));
        }

        if let Some(closure) = func.as_closure() {
            // Validate argument count
            if !self.check_arity(&closure.arity, args.len()) {
                // check_arity sets fiber.signal to (SIG_ERROR, ...)
                return Some(SIG_ERROR);
            }

            // Build proper environment using cached vector
            self.tail_call_env_cache.clear();
            let needed = closure.env_capacity();
            if self.tail_call_env_cache.capacity() < needed {
                self.tail_call_env_cache
                    .reserve(needed - self.tail_call_env_cache.len());
            }
            self.tail_call_env_cache
                .extend((*closure.env).iter().cloned());

            // Add parameters, wrapping in local cells if needed
            for (i, arg) in args.iter().enumerate() {
                if i < 64 && (closure.cell_params_mask & (1 << i)) != 0 {
                    self.tail_call_env_cache.push(Value::local_cell(*arg));
                } else {
                    self.tail_call_env_cache.push(*arg);
                }
            }

            // Calculate and add locally-defined variables
            let num_params = match closure.arity {
                crate::value::Arity::Exact(n) => n,
                crate::value::Arity::AtLeast(n) => n,
                crate::value::Arity::Range(min, _) => min,
            };
            let num_locally_defined = closure.num_locals.saturating_sub(num_params);

            for _ in 0..num_locally_defined {
                self.tail_call_env_cache.push(Value::local_cell(Value::NIL));
            }

            let new_env_rc = Rc::new(self.tail_call_env_cache.clone());

            // Store the tail call information
            self.pending_tail_call = Some((
                (*closure.bytecode).clone(),
                (*closure.constants).clone(),
                new_env_rc,
            ));

            self.fiber.signal = Some((SIG_OK, Value::NIL));
            return Some(SIG_OK);
        }

        // Cannot call this value
        set_error(
            &mut self.fiber,
            "type-error",
            format!("Cannot call {:?}", func),
        );
        Some(SIG_ERROR)
    }

    /// Call a JIT-compiled function.
    ///
    /// # Safety
    /// The JIT code must have been compiled from the same LIR function that
    /// produced the closure's bytecode. The calling convention must match.
    ///
    /// `func_value` is the original Value representing the closure, used for
    /// self-tail-call detection in the JIT code.
    fn call_jit(
        &mut self,
        jit_code: &JitCode,
        closure: &crate::value::Closure,
        args: &[Value],
        func_value: Value,
    ) -> Value {
        // Convert args to bits for the JIT calling convention
        // We need to pass Value bits, not Value pointers
        let args_bits: Vec<u64> = args.iter().map(|v| v.to_bits()).collect();

        // Get environment pointer (captures)
        // The JIT expects a pointer to an array of Value bits (u64)
        let env_bits: Vec<u64> = closure.env.iter().map(|v| v.to_bits()).collect();
        let env_ptr = if env_bits.is_empty() {
            std::ptr::null()
        } else {
            env_bits.as_ptr()
        };

        // Call the JIT-compiled function
        // Pass func_value.to_bits() as self_bits for self-tail-call detection
        let result_bits = unsafe {
            jit_code.call(
                env_ptr,
                args_bits.as_ptr(),
                args.len() as u32,
                self as *mut VM as *mut (),
                func_value.to_bits(),
            )
        };

        // Convert result back to Value
        unsafe { Value::from_bits(result_bits) }
    }

    /// Build a closure environment from captured variables and arguments.
    fn build_closure_env(&self, closure: &crate::value::Closure, args: &[Value]) -> Rc<Vec<Value>> {
        let mut new_env = Vec::with_capacity(closure.env_capacity());
        new_env.extend((*closure.env).iter().cloned());

        // Add parameters, wrapping in local cells if cell_params_mask indicates
        for (i, arg) in args.iter().enumerate() {
            if i < 64 && (closure.cell_params_mask & (1 << i)) != 0 {
                new_env.push(Value::local_cell(*arg));
            } else {
                new_env.push(*arg);
            }
        }

        // Calculate number of locally-defined variables
        let num_params = match closure.arity {
            crate::value::Arity::Exact(n) => n,
            crate::value::Arity::AtLeast(n) => n,
            crate::value::Arity::Range(min, _) => min,
        };
        let num_locally_defined = closure.num_locals.saturating_sub(num_params);

        // Add empty LocalCells for locally-defined variables
        for _ in 0..num_locally_defined {
            new_env.push(Value::local_cell(Value::NIL));
        }

        Rc::new(new_env)
    }

    // ── Primitive signal dispatch ───────────────────────────────────

    /// Handle signal bits returned by a primitive in a Call position.
    ///
    /// Returns `None` to continue the dispatch loop, or `Some(bits)` to
    /// return from the dispatch loop (for yields/signals).
    pub(super) fn handle_primitive_signal(
        &mut self,
        bits: SignalBits,
        value: Value,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        ip: &mut usize,
    ) -> Option<SignalBits> {
        match bits {
            SIG_OK => {
                self.fiber.stack.push(value);
                None
            }
            SIG_ERROR => {
                // Store the error in fiber.signal. The dispatch loop will
                // see it and return SIG_ERROR.
                self.fiber.signal = Some((SIG_ERROR, value));
                self.fiber.stack.push(Value::NIL);
                None
            }
            SIG_RESUME => {
                // Primitive returned SIG_RESUME — dispatch to fiber handler
                self.handle_fiber_resume_signal(value, bytecode, constants, closure_env, ip)
            }
            SIG_PROPAGATE => {
                // fiber/propagate: re-raise the child fiber's signal
                self.handle_fiber_propagate_signal(value)
            }
            SIG_CANCEL => {
                // fiber/cancel: inject error into suspended fiber
                self.handle_fiber_cancel_signal(value, bytecode, constants, closure_env, ip)
            }
            _ => {
                // Any other signal (SIG_YIELD, user-defined)
                self.fiber.signal = Some((bits, value));
                Some(bits)
            }
        }
    }

    /// Handle signal bits returned by a primitive in a TailCall position.
    ///
    /// Always returns SignalBits (tail calls always return from the dispatch loop).
    fn handle_primitive_signal_tail(&mut self, bits: SignalBits, value: Value) -> SignalBits {
        match bits {
            SIG_OK => {
                self.fiber.signal = Some((SIG_OK, value));
                SIG_OK
            }
            SIG_ERROR => {
                self.fiber.signal = Some((SIG_ERROR, value));
                SIG_ERROR
            }
            SIG_RESUME => {
                // Primitive in tail position — dispatch to fiber handler
                self.handle_fiber_resume_signal_tail(value)
            }
            SIG_PROPAGATE => {
                // fiber/propagate in tail position
                self.handle_fiber_propagate_signal_tail(value)
            }
            SIG_CANCEL => {
                // fiber/cancel in tail position
                self.handle_fiber_cancel_signal_tail(value)
            }
            _ => {
                self.fiber.signal = Some((bits, value));
                bits
            }
        }
    }

    // ── SIG_RESUME: fiber execution ───────────────────────────────

    /// Handle SIG_RESUME from a fiber primitive (Call position).
    ///
    /// Swaps the child fiber into `vm.fiber`, executes it, then swaps back.
    /// The child's signal determines what the parent sees:
    /// - If the child's signal bits are caught by the mask → parent handles
    /// - If not caught → propagate up
    ///
    /// Returns `None` to continue the dispatch loop (result pushed to stack),
    /// or `Some(bits)` to exit (e.g., signal propagation).
    fn handle_fiber_resume_signal(
        &mut self,
        fiber_value: Value,
        _bytecode: &[u8],
        _constants: &[Value],
        _closure_env: Option<&Rc<Vec<Value>>>,
        _ip: &mut usize,
    ) -> Option<SignalBits> {
        let handle = match fiber_value.as_fiber() {
            Some(h) => h.clone(),
            None => {
                set_error(&mut self.fiber, "error", "SIG_RESUME with non-fiber value");
                self.fiber.stack.push(Value::NIL);
                return None;
            }
        };

        let (result_bits, result_value) = self.do_fiber_resume(&handle);

        // Check the child's signal against its mask
        let mask = handle.with(|fiber| fiber.mask);

        if result_bits == SIG_OK {
            // Child completed normally — clear child chain
            self.fiber.child = None;
            self.fiber.stack.push(result_value);
            None
        } else if mask & result_bits != 0 {
            // Signal is caught by the mask — parent handles it, clear child chain
            self.fiber.child = None;
            self.fiber.stack.push(result_value);
            None
        } else {
            // Signal is NOT caught — propagate to parent.
            // Leave parent.child set (preserves trace chain for stack traces).
            self.fiber.signal = Some((result_bits, result_value));
            if result_bits == SIG_ERROR {
                self.fiber.stack.push(Value::NIL);
                None // dispatch loop will see the error signal
            } else {
                Some(result_bits)
            }
        }
    }

    /// Handle SIG_RESUME from a fiber primitive (TailCall position).
    fn handle_fiber_resume_signal_tail(&mut self, fiber_value: Value) -> SignalBits {
        let handle = match fiber_value.as_fiber() {
            Some(h) => h.clone(),
            None => {
                set_error(&mut self.fiber, "error", "SIG_RESUME with non-fiber value");
                return SIG_ERROR;
            }
        };

        let (result_bits, result_value) = self.do_fiber_resume(&handle);

        let mask = handle.with(|fiber| fiber.mask);

        if result_bits == SIG_OK {
            // Child completed normally — clear child chain
            self.fiber.child = None;
            self.fiber.signal = Some((SIG_OK, result_value));
            SIG_OK
        } else if mask & result_bits != 0 {
            // Signal is caught by the mask — parent handles it, clear child chain
            self.fiber.child = None;
            self.fiber.signal = Some((SIG_OK, result_value));
            SIG_OK
        } else {
            // Signal not caught — propagate (leave child chain for traces)
            self.fiber.signal = Some((result_bits, result_value));
            result_bits
        }
    }

    /// Execute a fiber resume: swap fibers, run, swap back.
    ///
    /// Uses FiberHandle's take/put semantics — no dummy fiber allocation.
    /// Returns (signal_bits, signal_value) from the child fiber's execution.
    fn do_fiber_resume(&mut self, child_handle: &crate::value::FiberHandle) -> (SignalBits, Value) {
        // Extract resume value and status before taking the fiber
        let (resume_value, is_first_resume) = child_handle.with_mut(|child| {
            let rv = child.signal.take().map(|(_, v)| v).unwrap_or(Value::NIL);
            let first = child.status == FiberStatus::New;
            (rv, first)
        });

        // 1. Take child fiber out of its handle (sets slot to None)
        let mut child_fiber = child_handle.take();

        // 2. Wire up parent/child chain (Janet semantics)
        //    - parent.child = child_handle (for downward traversal / stack traces)
        //    - child.parent = parent_handle (for upward traversal / fiber/parent)
        self.fiber.child = Some(child_handle.clone());
        child_fiber.parent = self.current_fiber_handle.as_ref().map(|h| h.downgrade());

        // 3. Swap parent out, child in; track the child's handle
        let parent_handle = self.current_fiber_handle.take();
        self.current_fiber_handle = Some(child_handle.clone());
        std::mem::swap(&mut self.fiber, &mut child_fiber);
        // Now: self.fiber = child, child_fiber = parent

        // 4. Set child status to Alive
        self.fiber.status = FiberStatus::Alive;

        // 5. Execute the child
        let bits = if is_first_resume {
            self.do_fiber_first_resume()
        } else {
            self.do_fiber_subsequent_resume(resume_value)
        };

        // 6. Update child status based on result
        match bits {
            SIG_OK => {
                self.fiber.status = FiberStatus::Dead;
            }
            SIG_ERROR => {
                self.fiber.status = FiberStatus::Error;
            }
            _ => {
                // Any signal (SIG_YIELD, user-defined) → suspended
                self.fiber.status = FiberStatus::Suspended;
            }
        }

        // 7. Extract the result before swapping back
        let result_value = self
            .fiber
            .signal
            .as_ref()
            .map(|(_, v)| *v)
            .unwrap_or(Value::NIL);
        let result_bits = self.fiber.signal.as_ref().map(|(b, _)| *b).unwrap_or(bits);

        // 8. Swap back: parent in, child out; restore parent's handle
        std::mem::swap(&mut self.fiber, &mut child_fiber);
        // Now: self.fiber = parent, child_fiber = child
        self.current_fiber_handle = parent_handle;

        // 9. Put child fiber back into its handle
        child_handle.put(child_fiber);

        (result_bits, result_value)
    }

    /// First resume of a New fiber — build env and execute closure bytecode.
    ///
    /// Uses execute_bytecode_coroutine (not execute_bytecode_inner) because the
    /// fiber body may end with a TailCall. execute_bytecode_coroutine handles
    /// pending tail calls in a loop, while execute_bytecode_inner does not.
    /// Without this, a TailCall at the end of the fiber body returns SIG_OK
    /// immediately, losing the tail-called function's execution entirely.
    fn do_fiber_first_resume(&mut self) -> SignalBits {
        let closure = self.fiber.closure.clone();
        let env_rc = self.build_closure_env(&closure, &[]);

        // Execute the closure's bytecode.
        // execute_bytecode_coroutine handles pending tail calls and propagates
        // signals (SIG_YIELD, SIG_ERROR) correctly.
        let bits =
            self.execute_bytecode_coroutine(&closure.bytecode, &closure.constants, Some(&env_rc));

        // If the fiber signaled, save the execution context for resumption.
        // The dispatch loop saves the IP into fiber.suspended_ip before returning.
        if bits != SIG_OK {
            let ip = self.fiber.suspended_ip.take().unwrap_or(0);
            self.fiber.saved_context = Some(SavedContext {
                bytecode: closure.bytecode.to_vec(),
                constants: closure.constants.to_vec(),
                env: Some(env_rc),
                ip,
            });
        }

        bits
    }

    /// Resume a Suspended fiber — continue from saved context or continuation.
    fn do_fiber_subsequent_resume(&mut self, resume_value: Value) -> SignalBits {
        // If the fiber has a continuation (from yield instruction), use it.
        // Continuations capture the full call chain so yield-through-nested-calls
        // resumes from the exact point of yield.
        if let Some(continuation) = self.fiber.continuation.take() {
            return self.resume_continuation(continuation, resume_value);
        }

        // Otherwise, use SavedContext (from fiber/signal).
        // This resumes from the fiber body level — intermediate frames are abandoned.
        let ctx = match self.fiber.saved_context.take() {
            Some(ctx) => ctx,
            None => {
                set_error(
                    &mut self.fiber,
                    "error",
                    "fiber/resume: suspended fiber has no saved context",
                );
                return SIG_ERROR;
            }
        };

        // Push the resume value onto the child's stack.
        // This is the return value of the fiber/signal call that suspended it.
        self.fiber.stack.push(resume_value);

        // Resume from saved IP
        let bits =
            self.execute_bytecode_from_ip(&ctx.bytecode, &ctx.constants, ctx.env.as_ref(), ctx.ip);

        // If signaled again, save context for next resume
        if bits != SIG_OK {
            let ip = self.fiber.suspended_ip.take().unwrap_or(0);
            self.fiber.saved_context = Some(SavedContext {
                bytecode: ctx.bytecode,
                constants: ctx.constants,
                env: ctx.env,
                ip,
            });
        }

        bits
    }

    // ── SIG_PROPAGATE: re-raise caught signal ─────────────────────

    /// Handle SIG_PROPAGATE from fiber/propagate (Call position).
    ///
    /// Extracts the child fiber's signal and propagates it as if the
    /// current fiber had signaled. Sets parent.child to preserve the
    /// trace chain for stack traces.
    fn handle_fiber_propagate_signal(&mut self, fiber_value: Value) -> Option<SignalBits> {
        let handle = match fiber_value.as_fiber() {
            Some(h) => h.clone(),
            None => {
                set_error(
                    &mut self.fiber,
                    "error",
                    "SIG_PROPAGATE with non-fiber value",
                );
                self.fiber.stack.push(Value::NIL);
                return None;
            }
        };

        // Extract the child's signal
        let (child_bits, child_value) = handle
            .with(|fiber| fiber.signal)
            .unwrap_or((SIG_ERROR, error_val("error", "fiber/propagate: no signal")));

        // Set parent.child to preserve trace chain
        self.fiber.child = Some(handle);

        // Propagate the signal as our own
        self.fiber.signal = Some((child_bits, child_value));

        if child_bits == SIG_ERROR {
            self.fiber.stack.push(Value::NIL);
            None // dispatch loop will see the error signal
        } else {
            Some(child_bits)
        }
    }

    /// Handle SIG_PROPAGATE from fiber/propagate (TailCall position).
    fn handle_fiber_propagate_signal_tail(&mut self, fiber_value: Value) -> SignalBits {
        let handle = match fiber_value.as_fiber() {
            Some(h) => h.clone(),
            None => {
                set_error(
                    &mut self.fiber,
                    "error",
                    "SIG_PROPAGATE with non-fiber value",
                );
                return SIG_ERROR;
            }
        };

        let (child_bits, child_value) = handle
            .with(|fiber| fiber.signal)
            .unwrap_or((SIG_ERROR, error_val("error", "fiber/propagate: no signal")));

        self.fiber.child = Some(handle);
        self.fiber.signal = Some((child_bits, child_value));
        child_bits
    }

    // ── SIG_CANCEL: inject error into fiber ───────────────────────

    /// Handle SIG_CANCEL from fiber/cancel (Call position).
    ///
    /// Resumes the target fiber with the error injected as a signal.
    /// The fiber's error handlers get a chance to run (the dispatch loop
    /// sees the error signal and unwinds). This is Janet's "cancel":
    /// resume a fiber with an error signal instead of a normal value.
    fn handle_fiber_cancel_signal(
        &mut self,
        fiber_value: Value,
        _bytecode: &[u8],
        _constants: &[Value],
        _closure_env: Option<&Rc<Vec<Value>>>,
        _ip: &mut usize,
    ) -> Option<SignalBits> {
        let handle = match fiber_value.as_fiber() {
            Some(h) => h.clone(),
            None => {
                set_error(&mut self.fiber, "error", "SIG_CANCEL with non-fiber value");
                self.fiber.stack.push(Value::NIL);
                return None;
            }
        };

        // do_fiber_cancel resumes the fiber with the error injected.
        let (result_bits, result_value) = self.do_fiber_cancel(&handle);

        let mask = handle.with(|fiber| fiber.mask);
        let caught = result_bits == SIG_OK || (mask & result_bits != 0);

        if caught {
            self.fiber.child = None;
            self.fiber.stack.push(result_value);
            None
        } else {
            self.fiber.signal = Some((result_bits, result_value));
            if result_bits == SIG_ERROR {
                self.fiber.stack.push(Value::NIL);
                None
            } else {
                Some(result_bits)
            }
        }
    }

    /// Handle SIG_CANCEL from fiber/cancel (TailCall position).
    fn handle_fiber_cancel_signal_tail(&mut self, fiber_value: Value) -> SignalBits {
        let handle = match fiber_value.as_fiber() {
            Some(h) => h.clone(),
            None => {
                set_error(&mut self.fiber, "error", "SIG_CANCEL with non-fiber value");
                return SIG_ERROR;
            }
        };

        let (result_bits, result_value) = self.do_fiber_cancel(&handle);

        let mask = handle.with(|fiber| fiber.mask);
        let caught = result_bits == SIG_OK || (mask & result_bits != 0);

        if caught {
            self.fiber.child = None;
            self.fiber.signal = Some((SIG_OK, result_value));
            SIG_OK
        } else {
            self.fiber.signal = Some((result_bits, result_value));
            result_bits
        }
    }

    /// Execute a fiber cancel: inject error, resume, let error handlers run.
    ///
    /// Like `do_fiber_resume` but instead of pushing the resume value onto
    /// the child's stack, sets `fiber.signal = (SIG_ERROR, error_value)` so
    /// the dispatch loop sees the error and unwinds through error handlers.
    fn do_fiber_cancel(&mut self, child_handle: &crate::value::FiberHandle) -> (SignalBits, Value) {
        // The error value was stored on the fiber by prim_fiber_cancel
        let error_value = child_handle
            .with(|fiber| fiber.signal.as_ref().map(|(_, v)| *v))
            .unwrap_or(Value::NIL);

        // 1. Take child fiber out of its handle
        let mut child_fiber = child_handle.take();

        // 2. Wire up parent/child chain
        self.fiber.child = Some(child_handle.clone());
        child_fiber.parent = self.current_fiber_handle.as_ref().map(|h| h.downgrade());

        // 3. Swap parent out, child in
        let parent_handle = self.current_fiber_handle.take();
        self.current_fiber_handle = Some(child_handle.clone());
        std::mem::swap(&mut self.fiber, &mut child_fiber);

        // 4. Inject the error signal — the dispatch loop will see this
        //    and unwind through error handlers
        self.fiber.status = FiberStatus::Alive;
        self.fiber.signal = Some((SIG_ERROR, error_value));

        // 5. Resume the fiber so error handlers can run.
        //    For a Suspended fiber with a saved context, we resume from
        //    the saved IP. The dispatch loop will immediately see the
        //    error signal and return SIG_ERROR.
        let bits = if self.fiber.saved_context.is_some() {
            // Resume from saved context — the error signal is already set,
            // so the dispatch loop will see it on the first instruction check
            let ctx = self.fiber.saved_context.take().unwrap();
            self.execute_bytecode_from_ip(&ctx.bytecode, &ctx.constants, ctx.env.as_ref(), ctx.ip)
        } else if self.fiber.continuation.is_some() {
            let continuation = self.fiber.continuation.take().unwrap();
            self.resume_continuation(continuation, Value::NIL)
        } else {
            // New fiber that was never started — just mark as errored
            SIG_ERROR
        };

        // 6. Update child status
        match bits {
            SIG_OK => self.fiber.status = FiberStatus::Dead,
            SIG_ERROR => self.fiber.status = FiberStatus::Error,
            _ => self.fiber.status = FiberStatus::Suspended,
        }

        // 7. Extract result
        let result_value = self
            .fiber
            .signal
            .as_ref()
            .map(|(_, v)| *v)
            .unwrap_or(Value::NIL);
        let result_bits = self.fiber.signal.as_ref().map(|(b, _)| *b).unwrap_or(bits);

        // 8. Swap back
        std::mem::swap(&mut self.fiber, &mut child_fiber);
        self.current_fiber_handle = parent_handle;

        // 9. Put child back
        child_handle.put(child_fiber);

        (result_bits, result_value)
    }
}
