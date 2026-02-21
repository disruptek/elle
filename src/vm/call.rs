//! Call and TailCall instruction handlers.
//!
//! These are the most complex instructions in the VM, handling:
//! - Native function calls (unified signal-based dispatch)
//! - Closure calls with environment setup
//! - Coroutine-aware execution
//! - Tail call optimization
//! - JIT compilation and dispatch (when jit feature is enabled)

use crate::value::fiber::{FiberStatus, SavedContext};
use crate::value::{
    Condition, CoroutineState, Fiber, ResumeOp, SignalBits, Value, SIG_ERROR, SIG_OK, SIG_RESUME,
    SIG_YIELD,
};
use std::cell::RefCell;
use std::rc::Rc;

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
                let cond = Condition::error("Stack overflow");
                self.fiber.current_exception = Some(Rc::new(cond));
                self.fiber.signal = Some((SIG_ERROR, Value::NIL));
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
                    // Check if the JIT function (or a callee) set an exception
                    if self.fiber.current_exception.is_some() {
                        self.fiber.call_depth -= 1;
                        self.fiber.stack.push(Value::NIL);
                        return None; // Let the dispatch loop's interrupt handler deal with it
                    }
                    // Check for pending tail call (JIT function did a TailCall)
                    if result.to_bits() == TAIL_CALL_SENTINEL {
                        if let Some((tail_bc, tail_consts, tail_env)) =
                            self.pending_tail_call.take()
                        {
                            // Hand off to interpreter's trampoline which handles further tail calls
                            match self.execute_bytecode(&tail_bc, &tail_consts, Some(&tail_env)) {
                                Ok(val) => {
                                    self.fiber.call_depth -= 1;
                                    self.fiber.stack.push(val);
                                    return None;
                                }
                                Err(e) => {
                                    // execute_bytecode returned Err — convert to exception
                                    self.fiber.call_depth -= 1;
                                    let cond = Condition::error(e);
                                    self.fiber.current_exception = Some(Rc::new(cond));
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
                                        // Cache the JIT code
                                        self.jit_cache.insert(bytecode_ptr, jit_code.clone());
                                        // Execute via JIT
                                        let result = self.call_jit(&jit_code, closure, &args, func);
                                        // Check if the JIT function (or a callee) set an exception
                                        if self.fiber.current_exception.is_some() {
                                            self.fiber.call_depth -= 1;
                                            self.fiber.stack.push(Value::NIL);
                                            return None; // Let the dispatch loop's interrupt handler deal with it
                                        }
                                        // Check for pending tail call (JIT function did a TailCall)
                                        if result.to_bits() == TAIL_CALL_SENTINEL {
                                            if let Some((tail_bc, tail_consts, tail_env)) =
                                                self.pending_tail_call.take()
                                            {
                                                // Hand off to interpreter's trampoline
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
                                                        let cond = Condition::error(e);
                                                        self.fiber.current_exception =
                                                            Some(Rc::new(cond));
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

            // Execute the closure (interpreter path)
            if self.in_coroutine() {
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
                        let (_, value) = self.fiber.signal.take().unwrap();
                        let continuation = self.fiber.continuation.take().unwrap();

                        // Capture the caller's frame and append it to the continuation
                        let caller_stack: Vec<Value> = self.fiber.stack.drain(..).collect();

                        let caller_frame = crate::value::ContinuationFrame {
                            bytecode: Rc::new(bytecode.to_vec()),
                            constants: Rc::new(constants.to_vec()),
                            env: closure_env.cloned().unwrap_or_else(|| Rc::new(vec![])),
                            ip: *ip,
                            stack: caller_stack,
                            exception_handlers: self.fiber.exception_handlers.clone(),
                            handling_exception: self.fiber.handling_exception,
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
                        return Some(SIG_YIELD);
                    }
                    _ => {
                        // Error or other signal — propagate
                        return Some(bits);
                    }
                }
            } else {
                // Non-coroutine path: use execute_bytecode_coroutine to
                // save/restore the caller's stack, and propagate signals.
                // This is essential for fibers — fiber/signal in a nested
                // call must propagate up through the call chain.
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
                    _ => {
                        // Signal (yield, error, etc.) — propagate to caller.
                        return Some(bits);
                    }
                }
            }
            return None;
        }

        // Cannot call this value — set exception
        let cond = Condition::type_error(format!("Cannot call {:?}", func));
        self.fiber.current_exception = Some(Rc::new(cond));
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
                if self.fiber.current_exception.is_some()
                    && !self.fiber.handling_exception
                    && self.fiber.exception_handlers.is_empty()
                {
                    self.fiber.signal = Some((SIG_OK, Value::NIL));
                    return Some(SIG_OK);
                }
                self.fiber.signal = Some((SIG_OK, Value::NIL));
                return Some(SIG_OK);
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

        // Cannot call this value — set exception
        let cond = Condition::type_error(format!("Cannot call {:?}", func));
        self.fiber.current_exception = Some(Rc::new(cond));
        self.fiber.signal = Some((SIG_OK, Value::NIL));
        Some(SIG_OK)
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

    /// Handle a pending yield after a primitive call that triggered yield-from.
    fn handle_pending_yield_after_call(
        &mut self,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        ip: usize,
        yielded_value: Value,
    ) -> Option<SignalBits> {
        let coroutine = match self.current_coroutine() {
            Some(co) => co.clone(),
            None => {
                let cond = Condition::error("pending yield outside of coroutine");
                self.fiber.current_exception = Some(Rc::new(cond));
                self.fiber.signal = Some((SIG_ERROR, Value::NIL));
                return Some(SIG_ERROR);
            }
        };

        let saved_stack: Vec<Value> = self.fiber.stack.drain(..).collect();

        let frame = crate::value::ContinuationFrame {
            bytecode: Rc::new(bytecode.to_vec()),
            constants: Rc::new(constants.to_vec()),
            env: closure_env.cloned().unwrap_or_else(|| Rc::new(vec![])),
            ip,
            stack: saved_stack,
            exception_handlers: self.fiber.exception_handlers.clone(),
            handling_exception: self.fiber.handling_exception,
        };

        let cont_data = crate::value::ContinuationData::new(frame);
        let continuation = Value::continuation(cont_data);

        {
            let mut co = coroutine.borrow_mut();
            co.state = CoroutineState::Suspended;
            co.yielded_value = Some(yielded_value);
        }

        self.fiber.signal = Some((SIG_YIELD, yielded_value));
        self.fiber.continuation = Some(continuation);
        Some(SIG_YIELD)
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
                // Check for pending yield from yield-from delegation
                if let Some(yielded_value) = self.take_pending_yield() {
                    return self.handle_pending_yield_after_call(
                        bytecode,
                        constants,
                        closure_env,
                        *ip,
                        yielded_value,
                    );
                }
                None
            }
            SIG_ERROR => {
                // value is a heap-allocated Condition
                if let Some(cond) = value.as_condition() {
                    self.fiber.current_exception = Some(Rc::new(cond.clone()));
                } else {
                    // Primitive returned SIG_ERROR with non-Condition value —
                    // wrap in a generic error
                    let msg = format!("{}", value);
                    let cond = Condition::error(msg);
                    self.fiber.current_exception = Some(Rc::new(cond));
                }
                self.fiber.stack.push(Value::NIL);
                None // let the dispatch loop's interrupt handler deal with it
            }
            SIG_RESUME => {
                // Primitive returned SIG_RESUME — dispatch to coroutine or fiber handler
                if value.is_fiber() {
                    self.handle_fiber_resume_signal(value, bytecode, constants, closure_env, ip)
                } else {
                    self.handle_coroutine_resume_signal(value, bytecode, constants, closure_env, ip)
                }
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
                if let Some(cond) = value.as_condition() {
                    self.fiber.current_exception = Some(Rc::new(cond.clone()));
                } else {
                    let msg = format!("{}", value);
                    let cond = Condition::error(msg);
                    self.fiber.current_exception = Some(Rc::new(cond));
                }
                self.fiber.signal = Some((SIG_OK, Value::NIL));
                SIG_OK
            }
            SIG_RESUME => {
                // Primitive in tail position — dispatch to coroutine or fiber handler
                if value.is_fiber() {
                    self.handle_fiber_resume_signal_tail(value)
                } else {
                    self.handle_coroutine_resume_signal_tail(value)
                }
            }
            _ => {
                self.fiber.signal = Some((bits, value));
                bits
            }
        }
    }

    /// Handle SIG_RESUME from a coroutine primitive in tail position.
    ///
    /// Similar to `handle_coroutine_resume_signal` but stores the result
    /// in `fiber.signal` instead of pushing to the stack.
    fn handle_coroutine_resume_signal_tail(&mut self, coroutine_value: Value) -> SignalBits {
        let co = match coroutine_value.as_coroutine() {
            Some(co) => co.clone(),
            None => {
                let cond = Condition::error("SIG_RESUME with non-coroutine value");
                self.fiber.current_exception = Some(Rc::new(cond));
                self.fiber.signal = Some((SIG_OK, Value::NIL));
                return SIG_OK;
            }
        };

        let op = match co.borrow_mut().resume_op.take() {
            Some(op) => op,
            None => {
                let cond = Condition::error("SIG_RESUME with no resume_op set");
                self.fiber.current_exception = Some(Rc::new(cond));
                self.fiber.signal = Some((SIG_OK, Value::NIL));
                return SIG_OK;
            }
        };

        // For tail position, we use a simplified path: execute the coroutine
        // and store the result directly in fiber.signal.
        // We use empty bytecode/constants/env since we won't need pending yield
        // propagation (we're returning from this frame anyway).
        let dummy_bytecode: &[u8] = &[];
        let dummy_constants: &[Value] = &[];
        let dummy_env: Option<&Rc<Vec<Value>>> = None;
        let mut dummy_ip: usize = 0;

        match op {
            ResumeOp::Resume(resume_value) => {
                let result = self.do_coroutine_resume(
                    &co,
                    resume_value,
                    dummy_bytecode,
                    dummy_constants,
                    dummy_env,
                    &mut dummy_ip,
                );
                // do_coroutine_resume pushes to stack — pop and store in signal
                if result.is_some() {
                    // Yield propagation — pass through
                    return result.unwrap_or(SIG_OK);
                }
                let value = self.fiber.stack.pop().unwrap_or(Value::NIL);
                if self.fiber.current_exception.is_some() {
                    self.fiber.signal = Some((SIG_OK, Value::NIL));
                    SIG_OK
                } else {
                    self.fiber.signal = Some((SIG_OK, value));
                    SIG_OK
                }
            }
            ResumeOp::YieldFrom => {
                // yield-from in tail position is unusual but possible
                let result = self.do_yield_from(
                    &co,
                    coroutine_value,
                    dummy_bytecode,
                    dummy_constants,
                    dummy_env,
                    &mut dummy_ip,
                );
                if result.is_some() {
                    return result.unwrap_or(SIG_OK);
                }
                let value = self.fiber.stack.pop().unwrap_or(Value::NIL);
                self.fiber.signal = Some((SIG_OK, value));
                SIG_OK
            }
            ResumeOp::Next => {
                let result = self.do_coroutine_next(
                    &co,
                    dummy_bytecode,
                    dummy_constants,
                    dummy_env,
                    &mut dummy_ip,
                );
                if result.is_some() {
                    return result.unwrap_or(SIG_OK);
                }
                let value = self.fiber.stack.pop().unwrap_or(Value::NIL);
                self.fiber.signal = Some((SIG_OK, value));
                SIG_OK
            }
        }
    }

    // ── SIG_RESUME: coroutine execution ────────────────────────────

    /// Handle SIG_RESUME from a coroutine primitive.
    ///
    /// The coroutine's `resume_op` field tells us what to do:
    /// - Resume: execute the coroutine (first time or subsequent)
    /// - YieldFrom: resume sub-coroutine, set up delegation
    /// - Next: resume and wrap result in (value . done?) pair
    ///
    /// Returns `None` to continue the dispatch loop (result pushed to stack),
    /// or `Some(bits)` to exit (e.g., SIG_YIELD for yield propagation).
    fn handle_coroutine_resume_signal(
        &mut self,
        coroutine_value: Value,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        ip: &mut usize,
    ) -> Option<SignalBits> {
        let co = match coroutine_value.as_coroutine() {
            Some(co) => co.clone(),
            None => {
                let cond = Condition::error("SIG_RESUME with non-coroutine value");
                self.fiber.current_exception = Some(Rc::new(cond));
                self.fiber.stack.push(Value::NIL);
                return None;
            }
        };

        // Take the resume operation
        let op = match co.borrow_mut().resume_op.take() {
            Some(op) => op,
            None => {
                let cond = Condition::error("SIG_RESUME with no resume_op set");
                self.fiber.current_exception = Some(Rc::new(cond));
                self.fiber.stack.push(Value::NIL);
                return None;
            }
        };

        match op {
            ResumeOp::Resume(resume_value) => {
                self.do_coroutine_resume(&co, resume_value, bytecode, constants, closure_env, ip)
            }
            ResumeOp::YieldFrom => {
                self.do_yield_from(&co, coroutine_value, bytecode, constants, closure_env, ip)
            }
            ResumeOp::Next => self.do_coroutine_next(&co, bytecode, constants, closure_env, ip),
        }
    }

    /// Execute a coroutine resume (Created or Suspended state).
    ///
    /// This contains the logic formerly in `prim_coroutine_resume`.
    fn do_coroutine_resume(
        &mut self,
        co: &Rc<std::cell::RefCell<crate::value::Coroutine>>,
        resume_value: Value,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        ip: &mut usize,
    ) -> Option<SignalBits> {
        // Check for yield-from delegation first
        {
            let borrowed = co.borrow();
            if let Some(delegate_val) = borrowed.delegate {
                drop(borrowed);
                return self.do_delegated_resume(
                    co,
                    delegate_val,
                    resume_value,
                    bytecode,
                    constants,
                    closure_env,
                    ip,
                );
            }
        }

        let state = {
            let borrowed = co.borrow();
            borrowed.state.clone()
        };

        match state {
            CoroutineState::Created => {
                self.do_first_resume(co, bytecode, constants, closure_env, ip)
            }
            CoroutineState::Suspended => {
                self.do_subsequent_resume(co, resume_value, bytecode, constants, closure_env, ip)
            }
            // Running/Done/Error should have been caught by the primitive
            _ => {
                let cond = Condition::error("coroutine-resume: invalid state for resume");
                self.fiber.current_exception = Some(Rc::new(cond));
                self.fiber.stack.push(Value::NIL);
                None
            }
        }
    }

    /// First resume of a Created coroutine — set up env and execute bytecode.
    fn do_first_resume(
        &mut self,
        co: &Rc<std::cell::RefCell<crate::value::Coroutine>>,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        ip: &mut usize,
    ) -> Option<SignalBits> {
        let (bc, consts, env_rc) = {
            let mut borrowed = co.borrow_mut();
            if !borrowed.closure.effect.may_yield() {
                eprintln!("warning: coroutine-resume: closure cannot yield; it will complete without suspending");
            }
            borrowed.state = CoroutineState::Running;

            let bc = borrowed.closure.bytecode.clone();
            let consts = borrowed.closure.constants.clone();
            let closure_env_inner = borrowed.closure.env.clone();
            let num_locals = borrowed.closure.num_locals;
            let num_captures = borrowed.closure.num_captures;

            let mut env = (*closure_env_inner).clone();
            let num_locally_defined = num_locals.saturating_sub(num_captures);
            for _ in env.len()..num_captures + num_locally_defined {
                env.push(Value::local_cell(Value::EMPTY_LIST));
            }
            (bc, consts, Rc::new(env))
        };

        self.enter_coroutine(co.clone());
        let bits = self.execute_bytecode_coroutine(&bc, &consts, Some(&env_rc));

        // Check for pending yield from yield-from delegation
        if let Some(yielded_value) = self.take_pending_yield() {
            self.exit_coroutine();
            let mut borrowed = co.borrow_mut();
            borrowed.state = CoroutineState::Suspended;
            borrowed.yielded_value = Some(yielded_value);
            // Push yielded value and check for pending yield propagation
            self.fiber.stack.push(yielded_value);
            return self.handle_pending_yield_after_call(
                bytecode,
                constants,
                closure_env,
                *ip,
                yielded_value,
            );
        }

        self.exit_coroutine();
        self.finish_coroutine_execution(co, bits, bytecode, constants, closure_env, ip)
    }

    /// Resume a Suspended coroutine using its saved continuation.
    fn do_subsequent_resume(
        &mut self,
        co: &Rc<std::cell::RefCell<crate::value::Coroutine>>,
        resume_value: Value,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        ip: &mut usize,
    ) -> Option<SignalBits> {
        let continuation = {
            let mut borrowed = co.borrow_mut();
            match borrowed.saved_value_continuation.take() {
                Some(cont) => {
                    borrowed.state = CoroutineState::Running;
                    cont
                }
                None => {
                    let cond = Condition::error("Suspended coroutine has no saved continuation");
                    self.fiber.current_exception = Some(Rc::new(cond));
                    self.fiber.stack.push(Value::NIL);
                    return None;
                }
            }
        };

        self.enter_coroutine(co.clone());
        let bits = self.resume_continuation(continuation, resume_value);
        self.exit_coroutine();

        self.finish_coroutine_execution(co, bits, bytecode, constants, closure_env, ip)
    }

    /// Common post-execution logic for coroutine resume.
    /// Handles SIG_OK (done), SIG_YIELD (suspended), and SIG_ERROR.
    fn finish_coroutine_execution(
        &mut self,
        co: &Rc<std::cell::RefCell<crate::value::Coroutine>>,
        bits: SignalBits,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        ip: &mut usize,
    ) -> Option<SignalBits> {
        let mut borrowed = co.borrow_mut();
        match bits {
            SIG_OK => {
                let (_, value) = self.fiber.signal.take().unwrap_or((SIG_OK, Value::NIL));
                if self.fiber.current_exception.is_some() {
                    let msg = self
                        .fiber
                        .current_exception
                        .as_ref()
                        .map(|e| e.message.clone())
                        .unwrap_or_default();
                    borrowed.state = CoroutineState::Error(msg);
                    // Exception is already set — let dispatch loop handle it
                    self.fiber.stack.push(Value::NIL);
                    None
                } else {
                    borrowed.state = CoroutineState::Done;
                    borrowed.yielded_value = Some(value);
                    self.fiber.stack.push(value);
                    None
                }
            }
            SIG_YIELD => {
                let (_, value) = self.fiber.signal.take().unwrap_or((SIG_YIELD, Value::NIL));
                let continuation = self.fiber.continuation.take().unwrap();
                borrowed.state = CoroutineState::Suspended;
                borrowed.yielded_value = Some(value);
                borrowed.saved_value_continuation = Some(continuation);
                drop(borrowed);
                // Push the yielded value as the result of coroutine-resume
                self.fiber.stack.push(value);
                // Check for pending yield propagation (nested coroutines)
                if let Some(yielded_value) = self.take_pending_yield() {
                    return self.handle_pending_yield_after_call(
                        bytecode,
                        constants,
                        closure_env,
                        *ip,
                        yielded_value,
                    );
                }
                None
            }
            _ => {
                // SIG_ERROR or other
                let msg = self
                    .fiber
                    .current_exception
                    .as_ref()
                    .map(|e| e.message.clone())
                    .unwrap_or_else(|| "unknown error".to_string());
                borrowed.state = CoroutineState::Error(msg.clone());
                let cond = Condition::error(msg);
                self.fiber.current_exception = Some(Rc::new(cond));
                self.fiber.stack.push(Value::NIL);
                None
            }
        }
    }

    /// Handle delegated resume (yield-from delegation).
    #[allow(clippy::too_many_arguments)]
    fn do_delegated_resume(
        &mut self,
        outer_co: &Rc<std::cell::RefCell<crate::value::Coroutine>>,
        delegate_val: Value,
        resume_value: Value,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        ip: &mut usize,
    ) -> Option<SignalBits> {
        if let Some(delegate_co) = delegate_val.as_coroutine() {
            // Set up the delegate for resume
            delegate_co.borrow_mut().resume_op = Some(ResumeOp::Resume(resume_value));

            // Recursively resume the delegate
            let result = self.do_coroutine_resume(
                delegate_co,
                resume_value,
                bytecode,
                constants,
                closure_env,
                ip,
            );

            // Check delegate state after resume
            let delegate_state = {
                let borrowed = delegate_co.borrow();
                borrowed.state.clone()
            };

            match delegate_state {
                CoroutineState::Done => {
                    // Delegate completed — clear delegation
                    let delegate_value = {
                        let borrowed = delegate_co.borrow();
                        borrowed.yielded_value.unwrap_or(Value::NIL)
                    };
                    let mut outer = outer_co.borrow_mut();
                    outer.delegate = None;
                    // The outer coroutine should continue after yield-from.
                    // The value is already on the stack from do_coroutine_resume.
                    // But we need to update the outer's yielded_value too.
                    outer.yielded_value = Some(delegate_value);
                }
                CoroutineState::Error(ref e) => {
                    let mut outer = outer_co.borrow_mut();
                    outer.delegate = None;
                    outer.state = CoroutineState::Error(e.clone());
                }
                _ => {
                    // Delegate yielded or is in another state — outer stays as-is
                    // The yielded value is already on the stack
                    let delegate_value = {
                        let borrowed = delegate_co.borrow();
                        borrowed.yielded_value.unwrap_or(Value::NIL)
                    };
                    let mut outer = outer_co.borrow_mut();
                    outer.yielded_value = Some(delegate_value);
                }
            }

            result
        } else {
            // Delegate is not a coroutine — clear delegation and error
            let mut outer = outer_co.borrow_mut();
            outer.delegate = None;
            let cond = Condition::error("yield-from: delegate is not a coroutine");
            self.fiber.current_exception = Some(Rc::new(cond));
            self.fiber.stack.push(Value::NIL);
            None
        }
    }

    /// Handle yield-from: resume sub-coroutine, set up delegation on outer.
    fn do_yield_from(
        &mut self,
        sub_co: &Rc<std::cell::RefCell<crate::value::Coroutine>>,
        sub_co_value: Value,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        ip: &mut usize,
    ) -> Option<SignalBits> {
        // Get the outer (current) coroutine
        let outer_co = match self.current_coroutine() {
            Some(co) => co.clone(),
            None => {
                let cond = Condition::error("yield-from: not inside a coroutine");
                self.fiber.current_exception = Some(Rc::new(cond));
                self.fiber.stack.push(Value::NIL);
                return None;
            }
        };

        // Resume the sub-coroutine once to get its first value
        sub_co.borrow_mut().resume_op = Some(ResumeOp::Resume(Value::EMPTY_LIST));
        // Pop whatever the resume pushes — we'll handle the result ourselves
        let stack_before = self.fiber.stack.len();
        let _result = self.do_coroutine_resume(
            sub_co,
            Value::EMPTY_LIST,
            bytecode,
            constants,
            closure_env,
            ip,
        );

        // Get the value that was pushed by do_coroutine_resume
        let result_value = if self.fiber.stack.len() > stack_before {
            self.fiber.stack.pop().unwrap_or(Value::NIL)
        } else {
            Value::NIL
        };

        // Check sub-coroutine state after resume
        let state_after = {
            let borrowed = sub_co.borrow();
            borrowed.state.clone()
        };

        match state_after {
            CoroutineState::Done => {
                // Sub-coroutine completed immediately — return its final value
                self.fiber.stack.push(result_value);
                None
            }
            CoroutineState::Suspended => {
                // Sub-coroutine yielded — set up delegation
                {
                    let mut borrowed = outer_co.borrow_mut();
                    borrowed.delegate = Some(sub_co_value);
                }

                // Trigger a yield from the outer coroutine using pending_yield
                self.set_pending_yield(result_value);

                // Push the result (will be overridden by pending yield handling)
                self.fiber.stack.push(result_value);
                // Check for pending yield propagation
                if let Some(yielded_value) = self.take_pending_yield() {
                    return self.handle_pending_yield_after_call(
                        bytecode,
                        constants,
                        closure_env,
                        *ip,
                        yielded_value,
                    );
                }
                None
            }
            CoroutineState::Error(e) => {
                let cond = Condition::error(format!("yield-from: sub-coroutine errored: {}", e));
                self.fiber.current_exception = Some(Rc::new(cond));
                self.fiber.stack.push(Value::NIL);
                None
            }
            _ => {
                // Unexpected state
                self.fiber.stack.push(result_value);
                None
            }
        }
    }

    /// Handle coroutine-next: resume and wrap result in (value . done?) pair.
    fn do_coroutine_next(
        &mut self,
        co: &Rc<std::cell::RefCell<crate::value::Coroutine>>,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        ip: &mut usize,
    ) -> Option<SignalBits> {
        // Resume the coroutine
        co.borrow_mut().resume_op = Some(ResumeOp::Resume(Value::EMPTY_LIST));
        let stack_before = self.fiber.stack.len();
        let result =
            self.do_coroutine_resume(co, Value::EMPTY_LIST, bytecode, constants, closure_env, ip);

        // If the resume caused a yield propagation, pass it through
        if result.is_some() {
            return result;
        }

        // Get the value that was pushed by do_coroutine_resume
        let result_value = if self.fiber.stack.len() > stack_before {
            self.fiber.stack.pop().unwrap_or(Value::NIL)
        } else {
            Value::NIL
        };

        // Check if done after resume
        let is_done = {
            let borrowed = co.borrow();
            matches!(
                borrowed.state,
                CoroutineState::Done | CoroutineState::Error(_)
            )
        };

        // Wrap in (value . done?) pair
        let pair = crate::value::cons(result_value, Value::bool(is_done));
        self.fiber.stack.push(pair);
        None
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
        let fiber_rc = match fiber_value.as_fiber() {
            Some(f) => f.clone(),
            None => {
                let cond = Condition::error("SIG_RESUME with non-fiber value");
                self.fiber.current_exception = Some(Rc::new(cond));
                self.fiber.stack.push(Value::NIL);
                return None;
            }
        };

        let (result_bits, result_value) = self.do_fiber_resume(&fiber_rc);

        // Check the child's signal against its mask
        let mask = fiber_rc.borrow().mask;

        if result_bits == SIG_OK {
            // Child completed normally — push return value
            self.fiber.stack.push(result_value);
            None
        } else if mask & result_bits != 0 {
            // Signal is caught by the mask — parent handles it.
            // Push the signal value; parent can inspect via fiber/bits, fiber/value.
            self.fiber.stack.push(result_value);
            None
        } else {
            // Signal is NOT caught — propagate to parent.
            // Save parent's execution context for potential resumption.
            self.fiber.signal = Some((result_bits, result_value));
            Some(result_bits)
        }
    }

    /// Handle SIG_RESUME from a fiber primitive (TailCall position).
    fn handle_fiber_resume_signal_tail(&mut self, fiber_value: Value) -> SignalBits {
        let fiber_rc = match fiber_value.as_fiber() {
            Some(f) => f.clone(),
            None => {
                let cond = Condition::error("SIG_RESUME with non-fiber value");
                self.fiber.current_exception = Some(Rc::new(cond));
                self.fiber.signal = Some((SIG_OK, Value::NIL));
                return SIG_OK;
            }
        };

        let (result_bits, result_value) = self.do_fiber_resume(&fiber_rc);

        let mask = fiber_rc.borrow().mask;

        if result_bits == SIG_OK {
            self.fiber.signal = Some((SIG_OK, result_value));
            SIG_OK
        } else if mask & result_bits != 0 {
            // Caught by mask — return value to parent
            self.fiber.signal = Some((SIG_OK, result_value));
            SIG_OK
        } else {
            // Propagate signal
            self.fiber.signal = Some((result_bits, result_value));
            result_bits
        }
    }

    /// Execute a fiber resume: swap fibers, run, swap back.
    ///
    /// Returns (signal_bits, signal_value) from the child fiber's execution.
    fn do_fiber_resume(&mut self, fiber_rc: &Rc<RefCell<Fiber>>) -> (SignalBits, Value) {
        // Take the resume value that prim_fiber_resume stored
        let resume_value = {
            let mut child = fiber_rc.borrow_mut();
            child.signal.take().map(|(_, v)| v).unwrap_or(Value::NIL)
        };

        let is_first_resume = fiber_rc.borrow().status == FiberStatus::New;

        // 1. Take child fiber out of its Rc<RefCell>
        let dummy = Fiber::new(
            Rc::new(crate::value::Closure {
                bytecode: Rc::new(vec![]),
                arity: crate::value::Arity::Exact(0),
                env: Rc::new(vec![]),
                num_locals: 0,
                num_captures: 0,
                constants: Rc::new(vec![]),
                effect: crate::effects::Effect::none(),
                cell_params_mask: 0,
                symbol_names: Rc::new(std::collections::HashMap::new()),
                location_map: Rc::new(crate::error::LocationMap::new()),
                jit_code: None,
                lir_function: None,
            }),
            0,
        );
        let mut child_fiber = std::mem::replace(&mut *fiber_rc.borrow_mut(), dummy);

        // 2. Swap parent out, child in
        std::mem::swap(&mut self.fiber, &mut child_fiber);
        // Now: self.fiber = child, child_fiber = parent

        // 3. Set child status to Alive
        self.fiber.status = FiberStatus::Alive;

        // 4. Execute the child
        let bits = if is_first_resume {
            self.do_fiber_first_resume()
        } else {
            self.do_fiber_subsequent_resume(resume_value)
        };

        // 5. Update child status based on result
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

        // 6. Extract the result before swapping back
        let result_value = self
            .fiber
            .signal
            .as_ref()
            .map(|(_, v)| *v)
            .unwrap_or(Value::NIL);
        let result_bits = self.fiber.signal.as_ref().map(|(b, _)| *b).unwrap_or(bits);

        // 7. Swap back: parent in, child out
        std::mem::swap(&mut self.fiber, &mut child_fiber);
        // Now: self.fiber = parent, child_fiber = child

        // 8. Put child fiber back into its Rc<RefCell>
        *fiber_rc.borrow_mut() = child_fiber;

        (result_bits, result_value)
    }

    /// First resume of a New fiber — build env and execute closure bytecode.
    fn do_fiber_first_resume(&mut self) -> SignalBits {
        let closure = self.fiber.closure.clone();
        let env_rc = self.build_closure_env(&closure, &[]);

        // Execute the closure's bytecode
        let bits =
            self.execute_bytecode_inner(&closure.bytecode, &closure.constants, Some(&env_rc));

        // If the fiber signaled, save the execution context for resumption.
        // The dispatch loop saves the IP into fiber.suspended_ip before returning.
        if bits != SIG_OK {
            let ip = self.fiber.suspended_ip.take().unwrap_or(0);
            self.fiber.saved_context = Some(SavedContext {
                bytecode: closure.bytecode.to_vec(),
                constants: closure.constants.to_vec(),
                env: Some(env_rc),
                ip,
                exception_handlers: self.fiber.exception_handlers.clone(),
                handling_exception: self.fiber.handling_exception,
            });
        }

        bits
    }

    /// Resume a Suspended fiber — continue from saved context.
    fn do_fiber_subsequent_resume(&mut self, resume_value: Value) -> SignalBits {
        let ctx = match self.fiber.saved_context.take() {
            Some(ctx) => ctx,
            None => {
                let cond = Condition::error("fiber/resume: suspended fiber has no saved context");
                self.fiber.current_exception = Some(Rc::new(cond));
                self.fiber.signal = Some((SIG_ERROR, Value::NIL));
                return SIG_ERROR;
            }
        };

        // Push the resume value onto the child's stack.
        // This is the return value of the fiber/signal call that suspended it.
        self.fiber.stack.push(resume_value);

        // Resume from saved IP with saved exception handler state
        let bits = self.execute_bytecode_from_ip_with_state(
            &ctx.bytecode,
            &ctx.constants,
            ctx.env.as_ref(),
            ctx.ip,
            ctx.exception_handlers,
            ctx.handling_exception,
        );

        // If signaled again, save context for next resume
        if bits != SIG_OK {
            let ip = self.fiber.suspended_ip.take().unwrap_or(0);
            self.fiber.saved_context = Some(SavedContext {
                bytecode: ctx.bytecode,
                constants: ctx.constants,
                env: ctx.env,
                ip,
                exception_handlers: self.fiber.exception_handlers.clone(),
                handling_exception: self.fiber.handling_exception,
            });
        }

        bits
    }
}
