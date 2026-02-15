//! HIR to LIR lowering

use super::types::*;
use crate::hir::{BindingId, BindingInfo, BindingKind, Hir, HirKind};
use std::collections::HashMap;

/// Lowers HIR to LIR
pub struct Lowerer {
    /// Current function being built
    current_func: LirFunction,
    /// Current block being built
    current_block: BasicBlock,
    /// Next register ID
    next_reg: u32,
    /// Next label ID
    next_label: u32,
    /// Mapping from BindingId to local slot
    binding_to_slot: HashMap<BindingId, u16>,
    /// Binding metadata from analysis
    bindings: HashMap<BindingId, BindingInfo>,
    /// Whether we're currently lowering a lambda (closure)
    in_lambda: bool,
    /// Number of captured variables (for lambda context)
    num_captures: u16,
}

impl Lowerer {
    pub fn new() -> Self {
        Lowerer {
            current_func: LirFunction::new(0),
            current_block: BasicBlock::new(Label(0)),
            next_reg: 0,
            next_label: 1, // 0 is entry
            binding_to_slot: HashMap::new(),
            bindings: HashMap::new(),
            in_lambda: false,
            num_captures: 0,
        }
    }

    /// Set binding info from analysis
    pub fn with_bindings(mut self, bindings: HashMap<BindingId, BindingInfo>) -> Self {
        self.bindings = bindings;
        self
    }

    /// Lower a HIR expression to LIR
    pub fn lower(&mut self, hir: &Hir) -> Result<LirFunction, String> {
        self.current_func = LirFunction::new(0);
        self.current_block = BasicBlock::new(Label(0));
        self.next_reg = 0;
        self.next_label = 1;
        self.binding_to_slot.clear();

        let result_reg = self.lower_expr(hir)?;
        self.terminate(Terminator::Return(result_reg));
        self.finish_block();

        self.current_func.entry = Label(0);
        self.current_func.num_regs = self.next_reg;

        Ok(std::mem::replace(
            &mut self.current_func,
            LirFunction::new(0),
        ))
    }

    /// Lower a lambda to a separate LirFunction
    pub fn lower_lambda(
        &mut self,
        params: &[BindingId],
        captures: &[crate::hir::CaptureInfo],
        body: &Hir,
        num_locals: u16,
    ) -> Result<LirFunction, String> {
        // Save state
        let saved_func = std::mem::replace(
            &mut self.current_func,
            LirFunction::new(params.len() as u16),
        );
        let saved_block = std::mem::replace(&mut self.current_block, BasicBlock::new(Label(0)));
        let saved_reg = self.next_reg;
        let saved_label = self.next_label;
        let saved_bindings = std::mem::take(&mut self.binding_to_slot);
        let saved_in_lambda = self.in_lambda;
        let saved_num_captures = self.num_captures;

        self.next_reg = 0;
        self.next_label = 1;
        self.current_func.num_locals = num_locals;
        self.in_lambda = true;
        self.num_captures = captures.len() as u16;

        // In a closure, the environment is laid out as:
        // [captured_vars..., parameters..., locally_defined_cells...]
        // So:
        // - Captured variables are at indices [0, num_captures)
        // - Parameters are at indices [num_captures, num_captures + num_params)

        // Bind captured variables to upvalue indices
        for (i, cap) in captures.iter().enumerate() {
            self.binding_to_slot.insert(cap.binding, i as u16);
        }

        // Bind parameters to upvalue indices
        for (i, param) in params.iter().enumerate() {
            let upvalue_idx = self.num_captures + i as u16;
            self.binding_to_slot.insert(*param, upvalue_idx);
        }

        // Lower body
        let result_reg = self.lower_expr(body)?;
        self.terminate(Terminator::Return(result_reg));
        self.finish_block();

        self.current_func.entry = Label(0);
        self.current_func.num_regs = self.next_reg;

        let func = std::mem::replace(&mut self.current_func, saved_func);

        // Restore state
        self.current_block = saved_block;
        self.next_reg = saved_reg;
        self.next_label = saved_label;
        self.binding_to_slot = saved_bindings;
        self.in_lambda = saved_in_lambda;
        self.num_captures = saved_num_captures;

        Ok(func)
    }

    fn lower_expr(&mut self, hir: &Hir) -> Result<Reg, String> {
        match &hir.kind {
            HirKind::Nil => self.emit_const(LirConst::Nil),
            HirKind::Bool(b) => self.emit_const(LirConst::Bool(*b)),
            HirKind::Int(n) => self.emit_const(LirConst::Int(*n)),
            HirKind::Float(f) => self.emit_const(LirConst::Float(*f)),
            HirKind::String(s) => self.emit_const(LirConst::String(s.clone())),
            HirKind::Keyword(sym) => self.emit_const(LirConst::Symbol(*sym)),

            HirKind::Var(binding_id) => {
                if let Some(&slot) = self.binding_to_slot.get(binding_id) {
                    let dst = self.fresh_reg();
                    if self.in_lambda {
                        // In a lambda, parameters and captures are accessed via LoadCapture
                        self.emit(LirInstr::LoadCapture { dst, index: slot });
                    } else {
                        // In the main function, use LoadLocal
                        self.emit(LirInstr::LoadLocal { dst, slot });
                    }
                    Ok(dst)
                } else if let Some(info) = self.bindings.get(binding_id) {
                    match info.kind {
                        BindingKind::Global => {
                            let sym = info.name;
                            let dst = self.fresh_reg();
                            self.emit(LirInstr::LoadGlobal { dst, sym });
                            Ok(dst)
                        }
                        _ => Err(format!("Unbound variable: {:?}", binding_id)),
                    }
                } else {
                    Err(format!("Unknown binding: {:?}", binding_id))
                }
            }

            HirKind::Let { bindings, body } => {
                // Allocate slots and lower initializers
                for (binding_id, init) in bindings {
                    let init_reg = self.lower_expr(init)?;
                    let slot = self.allocate_slot(*binding_id);
                    self.emit(LirInstr::StoreLocal {
                        slot,
                        src: init_reg,
                    });
                }
                self.lower_expr(body)
            }

            HirKind::Letrec { bindings, body } => {
                // First allocate all slots with nil
                for (binding_id, _) in bindings {
                    let nil_reg = self.emit_const(LirConst::Nil)?;
                    let slot = self.allocate_slot(*binding_id);
                    self.emit(LirInstr::StoreLocal { slot, src: nil_reg });
                }
                // Then initialize
                for (binding_id, init) in bindings {
                    let init_reg = self.lower_expr(init)?;
                    let slot = self.binding_to_slot[binding_id];
                    self.emit(LirInstr::StoreLocal {
                        slot,
                        src: init_reg,
                    });
                }
                self.lower_expr(body)
            }

            HirKind::Lambda {
                params,
                captures,
                body,
                num_locals,
            } => {
                // Collect capture registers
                let mut capture_regs = Vec::new();
                for cap in captures {
                    if let Some(&slot) = self.binding_to_slot.get(&cap.binding) {
                        let reg = self.fresh_reg();
                        self.emit(LirInstr::LoadLocal { dst: reg, slot });
                        capture_regs.push(reg);
                    } else if let Some(info) = self.bindings.get(&cap.binding) {
                        // If the capture is a global, load it as a global
                        match info.kind {
                            BindingKind::Global => {
                                let sym = info.name;
                                let reg = self.fresh_reg();
                                self.emit(LirInstr::LoadGlobal { dst: reg, sym });
                                capture_regs.push(reg);
                            }
                            _ => {
                                return Err(format!(
                                    "Cannot capture non-local binding: {:?}",
                                    cap.binding
                                ));
                            }
                        }
                    } else {
                        return Err(format!("Captured variable not found: {:?}", cap.binding));
                    }
                }

                // Lower the lambda body to a separate LirFunction
                let nested_lir = self.lower_lambda(params, captures, body, *num_locals)?;

                // Create closure with the nested function
                let dst = self.fresh_reg();
                self.emit(LirInstr::MakeClosure {
                    dst,
                    func: Box::new(nested_lir),
                    captures: capture_regs,
                });
                Ok(dst)
            }

            HirKind::If {
                cond,
                then_branch,
                else_branch,
            } => {
                // Lower condition
                let cond_reg = self.lower_expr(cond)?;

                // Allocate label IDs for inline jumps
                let else_label_id = self.next_label;
                self.next_label += 1;
                let end_label_id = self.next_label;
                self.next_label += 1;

                // Emit conditional jump to else (will jump if condition is false)
                self.emit(LirInstr::JumpIfFalseInline {
                    cond: cond_reg,
                    label_id: else_label_id,
                });

                // Lower then branch
                let then_reg = self.lower_expr(then_branch)?;

                // Emit unconditional jump to end
                self.emit(LirInstr::JumpInline {
                    label_id: end_label_id,
                });

                // Emit else label marker
                self.emit(LirInstr::LabelMarker {
                    label_id: else_label_id,
                });

                // Lower else branch
                let _else_reg = self.lower_expr(else_branch)?;

                // Emit end label marker
                self.emit(LirInstr::LabelMarker {
                    label_id: end_label_id,
                });

                // Both branches should produce the same result register
                // For now, use then_reg as the result (emitter will handle this)
                Ok(then_reg)
            }

            HirKind::Begin(exprs) => {
                let mut last_reg = self.emit_const(LirConst::Nil)?;
                for expr in exprs {
                    last_reg = self.lower_expr(expr)?;
                }
                Ok(last_reg)
            }

            HirKind::Block(exprs) => {
                let mut last_reg = self.emit_const(LirConst::Nil)?;
                for expr in exprs {
                    last_reg = self.lower_expr(expr)?;
                }
                Ok(last_reg)
            }

            HirKind::Call {
                func,
                args,
                is_tail,
            } => {
                // Lower arguments first, then function
                // This ensures the stack is in the right order for the Call instruction
                let mut arg_regs = Vec::new();
                for arg in args {
                    arg_regs.push(self.lower_expr(arg)?);
                }
                let func_reg = self.lower_expr(func)?;

                if *is_tail {
                    self.emit(LirInstr::TailCall {
                        func: func_reg,
                        args: arg_regs,
                    });
                    // After tail call, we need a placeholder reg
                    Ok(self.fresh_reg())
                } else {
                    let dst = self.fresh_reg();
                    self.emit(LirInstr::Call {
                        dst,
                        func: func_reg,
                        args: arg_regs,
                    });
                    Ok(dst)
                }
            }

            HirKind::Set { target, value } => {
                let value_reg = self.lower_expr(value)?;
                if let Some(&slot) = self.binding_to_slot.get(target) {
                    self.emit(LirInstr::StoreLocal {
                        slot,
                        src: value_reg,
                    });
                } else if let Some(info) = self.bindings.get(target) {
                    let sym = info.name;
                    match info.kind {
                        BindingKind::Global => {
                            self.emit(LirInstr::StoreGlobal {
                                sym,
                                src: value_reg,
                            });
                        }
                        _ => {
                            return Err(format!("Cannot set unbound variable: {:?}", target));
                        }
                    }
                } else {
                    return Err(format!("Unknown binding: {:?}", target));
                }
                Ok(value_reg)
            }

            HirKind::Define { name, value } => {
                let value_reg = self.lower_expr(value)?;
                self.emit(LirInstr::StoreGlobal {
                    sym: *name,
                    src: value_reg,
                });
                Ok(value_reg)
            }

            HirKind::While { cond, body } => {
                let loop_label = self.fresh_label();
                let body_label = self.fresh_label();
                let exit_label = self.fresh_label();

                self.terminate(Terminator::Jump(loop_label));
                self.finish_block();

                // Loop header
                self.start_block(loop_label);
                let cond_reg = self.lower_expr(cond)?;
                self.terminate(Terminator::Branch {
                    cond: cond_reg,
                    then_label: body_label,
                    else_label: exit_label,
                });
                self.finish_block();

                // Loop body
                self.start_block(body_label);
                self.lower_expr(body)?;
                self.terminate(Terminator::Jump(loop_label));
                self.finish_block();

                // Exit
                self.start_block(exit_label);
                self.emit_const(LirConst::Nil)
            }

            HirKind::For { var, iter, body } => {
                // Lower as: let iter_val = iter; while (pair? iter_val) { var = car(iter_val); body; iter_val = cdr(iter_val) }
                let iter_reg = self.lower_expr(iter)?;
                let iter_slot = self.allocate_slot(*var);
                self.emit(LirInstr::StoreLocal {
                    slot: iter_slot,
                    src: iter_reg,
                });

                let loop_label = self.fresh_label();
                let body_label = self.fresh_label();
                let exit_label = self.fresh_label();

                self.terminate(Terminator::Jump(loop_label));
                self.finish_block();

                // Loop header - check if iter is pair
                self.start_block(loop_label);
                let current_iter = self.fresh_reg();
                self.emit(LirInstr::LoadLocal {
                    dst: current_iter,
                    slot: iter_slot,
                });
                let is_pair = self.fresh_reg();
                self.emit(LirInstr::IsPair {
                    dst: is_pair,
                    src: current_iter,
                });
                self.terminate(Terminator::Branch {
                    cond: is_pair,
                    then_label: body_label,
                    else_label: exit_label,
                });
                self.finish_block();

                // Body
                self.start_block(body_label);
                let car_reg = self.fresh_reg();
                let iter_load = self.fresh_reg();
                self.emit(LirInstr::LoadLocal {
                    dst: iter_load,
                    slot: iter_slot,
                });
                self.emit(LirInstr::Car {
                    dst: car_reg,
                    pair: iter_load,
                });
                self.emit(LirInstr::StoreLocal {
                    slot: iter_slot,
                    src: car_reg,
                }); // Store car as var

                self.lower_expr(body)?;

                // Advance iterator
                let iter_load2 = self.fresh_reg();
                self.emit(LirInstr::LoadLocal {
                    dst: iter_load2,
                    slot: iter_slot,
                });
                let cdr_reg = self.fresh_reg();
                self.emit(LirInstr::Cdr {
                    dst: cdr_reg,
                    pair: iter_load2,
                });
                self.emit(LirInstr::StoreLocal {
                    slot: iter_slot,
                    src: cdr_reg,
                });

                self.terminate(Terminator::Jump(loop_label));
                self.finish_block();

                // Exit
                self.start_block(exit_label);
                self.emit_const(LirConst::Nil)
            }

            HirKind::And(exprs) => {
                if exprs.is_empty() {
                    return self.emit_const(LirConst::Bool(true));
                }

                let exit_label = self.fresh_label();
                let mut last_reg = self.emit_const(LirConst::Bool(true))?;

                for (i, expr) in exprs.iter().enumerate() {
                    last_reg = self.lower_expr(expr)?;
                    if i < exprs.len() - 1 {
                        let next_label = self.fresh_label();
                        self.terminate(Terminator::Branch {
                            cond: last_reg,
                            then_label: next_label,
                            else_label: exit_label,
                        });
                        self.finish_block();
                        self.start_block(next_label);
                    }
                }

                self.terminate(Terminator::Jump(exit_label));
                self.finish_block();
                self.start_block(exit_label);
                Ok(last_reg)
            }

            HirKind::Or(exprs) => {
                if exprs.is_empty() {
                    return self.emit_const(LirConst::Bool(false));
                }

                let exit_label = self.fresh_label();
                let mut last_reg = self.emit_const(LirConst::Bool(false))?;

                for (i, expr) in exprs.iter().enumerate() {
                    last_reg = self.lower_expr(expr)?;
                    if i < exprs.len() - 1 {
                        let next_label = self.fresh_label();
                        self.terminate(Terminator::Branch {
                            cond: last_reg,
                            then_label: exit_label,
                            else_label: next_label,
                        });
                        self.finish_block();
                        self.start_block(next_label);
                    }
                }

                self.terminate(Terminator::Jump(exit_label));
                self.finish_block();
                self.start_block(exit_label);
                Ok(last_reg)
            }

            HirKind::Yield(value) => {
                let value_reg = self.lower_expr(value)?;
                let dst = self.fresh_reg();
                self.emit(LirInstr::Yield {
                    dst,
                    value: value_reg,
                });
                Ok(dst)
            }

            HirKind::Quote(_syntax) => {
                // Quote produces a runtime value - for now, just emit nil
                // Full implementation would compile syntax to value construction
                self.emit_const(LirConst::Nil)
            }

            HirKind::Throw(value) => {
                let value_reg = self.lower_expr(value)?;
                self.emit(LirInstr::Throw { value: value_reg });
                self.emit_const(LirConst::Nil) // Unreachable but need a result
            }

            HirKind::Cond {
                clauses,
                else_branch,
            } => {
                if clauses.is_empty() {
                    return if let Some(else_expr) = else_branch {
                        self.lower_expr(else_expr)
                    } else {
                        self.emit_const(LirConst::Nil)
                    };
                }

                let exit_label = self.fresh_label();
                let result_reg = self.fresh_reg();

                for (test, body) in clauses {
                    let test_reg = self.lower_expr(test)?;
                    let body_label = self.fresh_label();
                    let next_label = self.fresh_label();

                    self.terminate(Terminator::Branch {
                        cond: test_reg,
                        then_label: body_label,
                        else_label: next_label,
                    });
                    self.finish_block();

                    self.start_block(body_label);
                    let body_reg = self.lower_expr(body)?;
                    self.emit(LirInstr::Move {
                        dst: result_reg,
                        src: body_reg,
                    });
                    self.terminate(Terminator::Jump(exit_label));
                    self.finish_block();

                    self.start_block(next_label);
                }

                // Else branch
                if let Some(else_expr) = else_branch {
                    let else_reg = self.lower_expr(else_expr)?;
                    self.emit(LirInstr::Move {
                        dst: result_reg,
                        src: else_reg,
                    });
                } else {
                    let nil_reg = self.emit_const(LirConst::Nil)?;
                    self.emit(LirInstr::Move {
                        dst: result_reg,
                        src: nil_reg,
                    });
                }
                self.terminate(Terminator::Jump(exit_label));
                self.finish_block();

                self.start_block(exit_label);
                Ok(result_reg)
            }

            // Stubs for complex forms - full implementation later
            HirKind::Try { body, .. } => self.lower_expr(body),
            HirKind::Match { value, .. } => {
                // Simplified: just evaluate value, ignore patterns
                self.lower_expr(value)
            }
            HirKind::HandlerCase { body, .. } => self.lower_expr(body),
            HirKind::HandlerBind { body, .. } => self.lower_expr(body),
            HirKind::Module { body, .. } => self.lower_expr(body),
            HirKind::Import { .. } => self.emit_const(LirConst::Nil),
            HirKind::ModuleRef { .. } => self.emit_const(LirConst::Nil),
        }
    }

    // === Helper Methods ===

    fn fresh_reg(&mut self) -> Reg {
        let r = Reg::new(self.next_reg);
        self.next_reg += 1;
        r
    }

    fn fresh_label(&mut self) -> Label {
        let l = Label::new(self.next_label);
        self.next_label += 1;
        l
    }

    fn allocate_slot(&mut self, binding: BindingId) -> u16 {
        let slot = self.current_func.num_locals;
        self.current_func.num_locals += 1;
        self.binding_to_slot.insert(binding, slot);
        slot
    }

    fn emit(&mut self, instr: LirInstr) {
        self.current_block.instructions.push(instr);
    }

    fn emit_const(&mut self, c: LirConst) -> Result<Reg, String> {
        let dst = self.fresh_reg();
        self.emit(LirInstr::Const { dst, value: c });
        Ok(dst)
    }

    fn terminate(&mut self, term: Terminator) {
        self.current_block.terminator = term;
    }

    fn finish_block(&mut self) {
        let block = std::mem::replace(&mut self.current_block, BasicBlock::new(Label(0)));
        self.current_func.blocks.push(block);
    }

    fn start_block(&mut self, label: Label) {
        self.current_block = BasicBlock::new(label);
    }
}

impl Default for Lowerer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::Span;

    fn make_span() -> Span {
        Span::new(0, 0, 1, 1)
    }

    #[test]
    fn test_lower_int() {
        let mut lowerer = Lowerer::new();
        let hir = Hir::pure(HirKind::Int(42), make_span());
        let func = lowerer.lower(&hir).unwrap();
        assert!(!func.blocks.is_empty());
    }

    #[test]
    fn test_lower_if() {
        let mut lowerer = Lowerer::new();
        let hir = Hir::pure(
            HirKind::If {
                cond: Box::new(Hir::pure(HirKind::Bool(true), make_span())),
                then_branch: Box::new(Hir::pure(HirKind::Int(1), make_span())),
                else_branch: Box::new(Hir::pure(HirKind::Int(2), make_span())),
            },
            make_span(),
        );
        let func = lowerer.lower(&hir).unwrap();
        // If is now emitted inline in a single block
        assert_eq!(func.blocks.len(), 1);
        // Should have inline jump instructions
        assert!(func.blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, LirInstr::JumpIfFalseInline { .. })));
    }

    #[test]
    fn test_lower_begin() {
        let mut lowerer = Lowerer::new();
        let hir = Hir::pure(
            HirKind::Begin(vec![
                Hir::pure(HirKind::Int(1), make_span()),
                Hir::pure(HirKind::Int(2), make_span()),
            ]),
            make_span(),
        );
        let func = lowerer.lower(&hir).unwrap();
        assert!(!func.blocks.is_empty());
    }
}
