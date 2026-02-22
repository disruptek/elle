//! Main instruction dispatch loop.
//!
//! This module contains the core bytecode execution loop that dispatches
//! instructions to their handlers.

use crate::compiler::bytecode::Instruction;
use crate::value::{error_val, SignalBits, Value, SIG_ERROR, SIG_OK, SIG_YIELD};
use std::rc::Rc;

use super::core::VM;
use super::{
    arithmetic, closure, comparison, control, data, literals, scope, stack, types, variables,
};

impl VM {
    /// Inner execution loop that handles all instructions.
    /// This is the implementation; the public wrapper handles handler isolation.
    ///
    /// Returns SignalBits. The result value is stored in `self.fiber.signal`.
    /// For yields, the continuation is stored in `self.fiber.continuation`.
    pub(super) fn execute_bytecode_inner_impl(
        &mut self,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&Rc<Vec<Value>>>,
        start_ip: usize,
    ) -> SignalBits {
        let mut ip = start_ip;
        let mut instruction_count = 0;
        const MAX_INSTRUCTIONS: usize = 100000;

        loop {
            instruction_count += 1;
            if instruction_count > MAX_INSTRUCTIONS {
                let instr_byte = if ip < bytecode.len() {
                    bytecode[ip]
                } else {
                    255
                };
                self.fiber.signal = Some((
                    SIG_ERROR,
                    error_val(
                        "error",
                        format!(
                            "Instruction limit exceeded at ip={} (instr={}), stack depth={}",
                            ip,
                            instr_byte,
                            self.fiber.stack.len(),
                        ),
                    ),
                ));
                return SIG_ERROR;
            }

            // If an error signal is pending, propagate immediately.
            if matches!(self.fiber.signal, Some((SIG_ERROR, _))) {
                return SIG_ERROR;
            }

            if ip >= bytecode.len() {
                panic!("VM bug: Unexpected end of bytecode");
            }

            let instr_byte = bytecode[ip];
            ip += 1;

            let instr: Instruction = unsafe { std::mem::transmute(instr_byte) };

            match instr {
                // Stack operations
                Instruction::LoadConst => {
                    stack::handle_load_const(self, bytecode, &mut ip, constants);
                }
                Instruction::LoadLocal => {
                    stack::handle_load_local(self, bytecode, &mut ip);
                }
                Instruction::Pop => {
                    stack::handle_pop(self);
                }
                Instruction::Dup => {
                    stack::handle_dup(self);
                }
                Instruction::DupN => {
                    stack::handle_dup_n(self, bytecode, &mut ip);
                }

                // Variable access
                Instruction::LoadGlobal => {
                    variables::handle_load_global(self, bytecode, &mut ip, constants);
                }
                Instruction::StoreGlobal => {
                    variables::handle_store_global(self, bytecode, &mut ip, constants);
                }
                Instruction::StoreLocal => {
                    variables::handle_store_local(self, bytecode, &mut ip);
                }
                Instruction::LoadUpvalue => {
                    variables::handle_load_upvalue(self, bytecode, &mut ip, closure_env);
                }
                Instruction::LoadUpvalueRaw => {
                    variables::handle_load_upvalue_raw(self, bytecode, &mut ip, closure_env);
                }
                Instruction::StoreUpvalue => {
                    variables::handle_store_upvalue(self, bytecode, &mut ip, closure_env);
                }

                // Control flow
                Instruction::Jump => {
                    control::handle_jump(bytecode, &mut ip, self);
                }
                Instruction::JumpIfFalse => {
                    control::handle_jump_if_false(bytecode, &mut ip, self);
                }
                Instruction::JumpIfTrue => {
                    control::handle_jump_if_true(bytecode, &mut ip, self);
                }
                Instruction::Return => {
                    return self.handle_return(bytecode, constants, closure_env, ip);
                }

                // Call instructions
                Instruction::Call => {
                    if let Some(bits) = self.handle_call(bytecode, constants, closure_env, &mut ip)
                    {
                        self.fiber.suspended_ip = Some(ip);
                        return bits;
                    }
                }
                Instruction::TailCall => {
                    if let Some(bits) = self.handle_tail_call(&mut ip, bytecode) {
                        self.fiber.suspended_ip = Some(ip);
                        return bits;
                    }
                }

                // Closures
                Instruction::MakeClosure => {
                    closure::handle_make_closure(self, bytecode, &mut ip, constants);
                }

                // Data structures
                Instruction::Cons => {
                    data::handle_cons(self);
                }
                Instruction::Car => {
                    data::handle_car(self);
                }
                Instruction::Cdr => {
                    data::handle_cdr(self);
                }
                Instruction::MakeVector => {
                    data::handle_make_vector(self, bytecode, &mut ip);
                }
                Instruction::VectorRef => {
                    data::handle_vector_ref(self);
                }
                Instruction::VectorSet => {
                    data::handle_vector_set(self);
                }

                // Arithmetic (integer)
                Instruction::AddInt => {
                    arithmetic::handle_add_int(self);
                }
                Instruction::SubInt => {
                    arithmetic::handle_sub_int(self);
                }
                Instruction::MulInt => {
                    arithmetic::handle_mul_int(self);
                }
                Instruction::DivInt => {
                    arithmetic::handle_div_int(self);
                }

                // Arithmetic (polymorphic)
                Instruction::Add => {
                    arithmetic::handle_add(self);
                }
                Instruction::Sub => {
                    arithmetic::handle_sub(self);
                }
                Instruction::Mul => {
                    arithmetic::handle_mul(self);
                }
                Instruction::Div => {
                    arithmetic::handle_div(self);
                }
                Instruction::Rem => {
                    arithmetic::handle_rem(self);
                }

                // Bitwise operations
                Instruction::BitAnd => {
                    arithmetic::handle_bit_and(self);
                }
                Instruction::BitOr => {
                    arithmetic::handle_bit_or(self);
                }
                Instruction::BitXor => {
                    arithmetic::handle_bit_xor(self);
                }
                Instruction::BitNot => {
                    arithmetic::handle_bit_not(self);
                }
                Instruction::Shl => {
                    arithmetic::handle_shl(self);
                }
                Instruction::Shr => {
                    arithmetic::handle_shr(self);
                }

                // Comparisons
                Instruction::Eq => {
                    comparison::handle_eq(self);
                }
                Instruction::Lt => {
                    comparison::handle_lt(self);
                }
                Instruction::Gt => {
                    comparison::handle_gt(self);
                }
                Instruction::Le => {
                    comparison::handle_le(self);
                }
                Instruction::Ge => {
                    comparison::handle_ge(self);
                }

                // Type checks
                Instruction::IsNil => {
                    types::handle_is_nil(self);
                }
                Instruction::IsEmptyList => {
                    types::handle_is_empty_list(self);
                }
                Instruction::IsPair => {
                    types::handle_is_pair(self);
                }
                Instruction::IsNumber => {
                    types::handle_is_number(self);
                }
                Instruction::IsSymbol => {
                    types::handle_is_symbol(self);
                }
                Instruction::Not => {
                    types::handle_not(self);
                }

                // Literals
                Instruction::Nil => {
                    literals::handle_nil(self);
                }
                Instruction::EmptyList => {
                    literals::handle_empty_list(self);
                }
                Instruction::True => {
                    literals::handle_true(self);
                }
                Instruction::False => {
                    literals::handle_false(self);
                }

                // Scope management
                Instruction::PushScope => {
                    let scope_type_byte = bytecode[ip];
                    ip += 1;
                    scope::handle_push_scope(self, scope_type_byte);
                }
                Instruction::PopScope => {
                    scope::handle_pop_scope(self);
                }
                Instruction::DefineLocal => {
                    scope::handle_define_local(self, bytecode, &mut ip, constants);
                }

                // Cell operations
                Instruction::MakeCell => {
                    scope::handle_make_cell(self);
                }
                Instruction::UnwrapCell => {
                    scope::handle_unwrap_cell(self);
                }
                Instruction::UpdateCell => {
                    scope::handle_update_cell(self);
                }

                // Yield — capture continuation and suspend
                Instruction::Yield => {
                    self.fiber.suspended_ip = Some(ip);
                    return self.handle_yield(bytecode, constants, closure_env, ip);
                }
            }

            // If an error signal was set by the instruction, propagate.
            if matches!(self.fiber.signal, Some((SIG_ERROR, _))) {
                return SIG_ERROR;
            }
        }
    }

    /// Handle the Return instruction.
    fn handle_return(
        &mut self,
        _bytecode: &[u8],
        _constants: &[Value],
        _closure_env: Option<&Rc<Vec<Value>>>,
        _ip: usize,
    ) -> SignalBits {
        let value = control::handle_return(self);
        self.fiber.signal = Some((SIG_OK, value));
        SIG_OK
    }

    /// Handle the Yield instruction.
    ///
    /// Captures a continuation frame (bytecode, constants, env, IP, stack)
    /// so that resume can continue from this exact point. Each call level
    /// appends its frame to the continuation chain.
    fn handle_yield(
        &mut self,
        bytecode: &[u8],
        constants: &[Value],
        closure_env: Option<&std::rc::Rc<Vec<Value>>>,
        ip: usize,
    ) -> SignalBits {
        let yielded_value = self
            .fiber
            .stack
            .pop()
            .expect("VM bug: Stack underflow on yield");

        let saved_stack: Vec<Value> = self.fiber.stack.drain(..).collect();

        let frame = crate::value::ContinuationFrame {
            bytecode: std::rc::Rc::new(bytecode.to_vec()),
            constants: std::rc::Rc::new(constants.to_vec()),
            env: closure_env
                .cloned()
                .unwrap_or_else(|| std::rc::Rc::new(vec![])),
            ip,
            stack: saved_stack,
        };

        let cont_data = crate::value::ContinuationData::new(frame);
        let continuation = Value::continuation(cont_data);

        self.fiber.signal = Some((SIG_YIELD, yielded_value));
        self.fiber.continuation = Some(continuation);
        SIG_YIELD
    }
}
