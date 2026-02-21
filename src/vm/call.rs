//! Call and TailCall instruction handlers.
//!
//! These are the most complex instructions in the VM, handling:
//! - Native function calls (unified signal-based dispatch)
//! - Closure calls with environment setup
//! - Coroutine-aware execution
//! - Tail call optimization
//! - JIT compilation and dispatch (when jit feature is enabled)

use crate::value::{
    Condition, CoroutineState, ResumeOp, SignalBits, Value, SIG_ERROR, SIG_OK, SIG_RESUME,
    SIG_YIELD,
};
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
                match self.execute_bytecode(
                    &closure.bytecode,
                    &closure.constants,
                    Some(&new_env_rc),
                ) {
                    Ok(result) => {
                        self.fiber.call_depth -= 1;
                        self.fiber.stack.push(result);
                    }
                    Err(e) => {
                        self.fiber.call_depth -= 1;
                        let cond = Condition::error(e);
                        self.fiber.current_exception = Some(Rc::new(cond));
                        self.fiber.stack.push(Value::NIL);
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
                // Coroutine primitive returned SIG_RESUME — execute the coroutine
                self.handle_coroutine_resume_signal(value, bytecode, constants, closure_env, ip)
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
                // Coroutine primitive in tail position — execute the coroutine
                // and store the result in fiber.signal.
                self.handle_coroutine_resume_signal_tail(value)
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
}
