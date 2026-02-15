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
    /// Set of bindings that are upvalues (captures/parameters in lambda)
    /// These use LoadCapture/StoreCapture, not LoadLocal/StoreLocal
    upvalue_bindings: std::collections::HashSet<BindingId>,
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
            upvalue_bindings: std::collections::HashSet::new(),
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
        _num_locals: u16,
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
        let saved_upvalue_bindings = std::mem::take(&mut self.upvalue_bindings);

        self.next_reg = 0;
        self.next_label = 1;
        // Start with 0 locals - they will be allocated as we encounter LocalDefine
        // The num_locals from HIR is just a hint for the total count
        self.current_func.num_locals = 0;
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
            self.upvalue_bindings.insert(cap.binding);
        }

        // Bind parameters to upvalue indices
        for (i, param) in params.iter().enumerate() {
            let upvalue_idx = self.num_captures + i as u16;
            self.binding_to_slot.insert(*param, upvalue_idx);
            self.upvalue_bindings.insert(*param);
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
        self.upvalue_bindings = saved_upvalue_bindings;

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
                    // Check if this binding needs cell unwrapping
                    let needs_cell = self
                        .bindings
                        .get(binding_id)
                        .map(|info| info.needs_cell())
                        .unwrap_or(false);

                    // Check if this is an upvalue (capture or parameter) or a local
                    let is_upvalue = self.upvalue_bindings.contains(binding_id);

                    let dst = self.fresh_reg();
                    if self.in_lambda && is_upvalue {
                        // In a lambda, captures and parameters are accessed via LoadCapture
                        // Note: LoadCapture (which emits LoadUpvalue) auto-unwraps LocalCell,
                        // so we don't need to emit LoadCell for captured variables
                        self.emit(LirInstr::LoadCapture { dst, index: slot });
                        Ok(dst)
                    } else {
                        // Local variables (including those defined inside lambda) use LoadLocal
                        self.emit(LirInstr::LoadLocal { dst, slot });

                        if needs_cell {
                            // Unwrap the cell to get the actual value
                            // Only needed for locals, not captures (LoadCapture auto-unwraps)
                            let value_reg = self.fresh_reg();
                            self.emit(LirInstr::LoadCell {
                                dst: value_reg,
                                cell: dst,
                            });
                            Ok(value_reg)
                        } else {
                            Ok(dst)
                        }
                    }
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

                    // Check if this binding needs to be wrapped in a cell
                    let needs_cell = self
                        .bindings
                        .get(binding_id)
                        .map(|info| info.needs_cell())
                        .unwrap_or(false);

                    if needs_cell {
                        // Wrap the value in a cell
                        let cell_reg = self.fresh_reg();
                        self.emit(LirInstr::MakeCell {
                            dst: cell_reg,
                            value: init_reg,
                        });
                        self.emit(LirInstr::StoreLocal {
                            slot,
                            src: cell_reg,
                        });
                    } else {
                        self.emit(LirInstr::StoreLocal {
                            slot,
                            src: init_reg,
                        });
                    }
                }
                self.lower_expr(body)
            }

            HirKind::Letrec { bindings, body } => {
                // First allocate all slots with nil (or cells containing nil)
                for (binding_id, _) in bindings {
                    let nil_reg = self.emit_const(LirConst::Nil)?;
                    let slot = self.allocate_slot(*binding_id);

                    // Check if this binding needs to be wrapped in a cell
                    let needs_cell = self
                        .bindings
                        .get(binding_id)
                        .map(|info| info.needs_cell())
                        .unwrap_or(false);

                    if needs_cell {
                        // Create a cell containing nil initially
                        let cell_reg = self.fresh_reg();
                        self.emit(LirInstr::MakeCell {
                            dst: cell_reg,
                            value: nil_reg,
                        });
                        self.emit(LirInstr::StoreLocal {
                            slot,
                            src: cell_reg,
                        });
                    } else {
                        self.emit(LirInstr::StoreLocal { slot, src: nil_reg });
                    }
                }
                // Then initialize
                for (binding_id, init) in bindings {
                    let init_reg = self.lower_expr(init)?;
                    let slot = self.binding_to_slot[binding_id];

                    // Check if this binding needs cell update
                    let needs_cell = self
                        .bindings
                        .get(binding_id)
                        .map(|info| info.needs_cell())
                        .unwrap_or(false);

                    if needs_cell {
                        // Load the cell and update it
                        let cell_reg = self.fresh_reg();
                        self.emit(LirInstr::LoadLocal {
                            dst: cell_reg,
                            slot,
                        });
                        self.emit(LirInstr::StoreCell {
                            cell: cell_reg,
                            value: init_reg,
                        });
                    } else {
                        self.emit(LirInstr::StoreLocal {
                            slot,
                            src: init_reg,
                        });
                    }
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
                    use crate::hir::CaptureKind;

                    let reg = self.fresh_reg();
                    match cap.kind {
                        CaptureKind::Local { index: _ } => {
                            // Load from parent's local/parameter slot
                            // Use binding_to_slot to find where this binding is in the current context
                            if let Some(&slot) = self.binding_to_slot.get(&cap.binding) {
                                // Check if this is an upvalue or a local in the current context
                                let is_upvalue = self.upvalue_bindings.contains(&cap.binding);
                                if self.in_lambda && is_upvalue {
                                    // In a lambda, captures and params are accessed via LoadCapture
                                    self.emit(LirInstr::LoadCapture {
                                        dst: reg,
                                        index: slot,
                                    });
                                } else {
                                    // Local variables (including those defined inside lambda) use LoadLocal
                                    self.emit(LirInstr::LoadLocal { dst: reg, slot });
                                }
                            } else {
                                // Binding not found in current context - this shouldn't happen
                                return Err(format!(
                                    "Capture binding {:?} not found in current context",
                                    cap.binding
                                ));
                            }
                            capture_regs.push(reg);
                        }
                        CaptureKind::Capture { index } => {
                            // Load from parent's capture (transitive capture)
                            // The index refers to the parent's capture array
                            if self.in_lambda {
                                // We're in a nested lambda - load from parent's captures
                                self.emit(LirInstr::LoadCapture { dst: reg, index });
                            } else {
                                // We're in the main function - this shouldn't happen
                                // (main function doesn't have captures to forward)
                                self.emit(LirInstr::LoadLocal {
                                    dst: reg,
                                    slot: index,
                                });
                            }
                            capture_regs.push(reg);
                        }
                        CaptureKind::Global { sym } => {
                            // Load global directly
                            self.emit(LirInstr::LoadGlobal { dst: reg, sym });
                            capture_regs.push(reg);
                        }
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
                // Pre-allocate slots for all LocalDefine bindings
                // This enables mutual recursion where lambda A captures variable B
                // before B's LocalDefine has been lowered
                for expr in exprs {
                    if let HirKind::LocalDefine { binding, .. } = &expr.kind {
                        // Allocate slot now so captures can find it
                        if !self.binding_to_slot.contains_key(binding) {
                            let slot = self.allocate_slot(*binding);

                            // Check if this binding needs a cell
                            let needs_cell = self
                                .bindings
                                .get(binding)
                                .map(|info| info.needs_cell())
                                .unwrap_or(false);

                            if needs_cell {
                                // Create a cell containing nil
                                // This cell will be captured by nested lambdas
                                // and updated when the LocalDefine is lowered
                                let nil_reg = self.emit_const(LirConst::Nil)?;
                                let cell_reg = self.fresh_reg();
                                self.emit(LirInstr::MakeCell {
                                    dst: cell_reg,
                                    value: nil_reg,
                                });
                                self.emit(LirInstr::StoreLocal {
                                    slot,
                                    src: cell_reg,
                                });
                            }
                        }
                    }
                }

                // Now lower all expressions (slots are available for capture lookup)
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

                // Check if this binding needs cell update
                let needs_cell = self
                    .bindings
                    .get(target)
                    .map(|info| info.needs_cell())
                    .unwrap_or(false);

                // Check if this is an upvalue (capture or parameter) or a local
                let is_upvalue = self.upvalue_bindings.contains(target);

                if let Some(&slot) = self.binding_to_slot.get(target) {
                    if self.in_lambda && is_upvalue {
                        // For captured variables, use StoreCapture which handles cells automatically
                        // StoreUpvalue checks if the upvalue is a cell and updates it
                        self.emit(LirInstr::StoreCapture {
                            index: slot,
                            src: value_reg,
                        });
                    } else if needs_cell {
                        // For local variables that need cells, load the cell and update it
                        let cell_reg = self.fresh_reg();
                        self.emit(LirInstr::LoadLocal {
                            dst: cell_reg,
                            slot,
                        });
                        self.emit(LirInstr::StoreCell {
                            cell: cell_reg,
                            value: value_reg,
                        });
                    } else {
                        // For simple local variables, store directly
                        self.emit(LirInstr::StoreLocal {
                            slot,
                            src: value_reg,
                        });
                    }
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

            HirKind::LocalDefine { binding, value } => {
                // Allocate the slot BEFORE lowering the value so that recursive
                // references can find the binding (like letrec)
                // The slot might already be allocated by the Begin pre-pass
                let slot = if let Some(&existing_slot) = self.binding_to_slot.get(binding) {
                    existing_slot
                } else {
                    self.allocate_slot(*binding)
                };

                // Check if this binding needs to be wrapped in a cell
                let needs_cell = self
                    .bindings
                    .get(binding)
                    .map(|info| info.needs_cell())
                    .unwrap_or(false);

                // Now lower the value (which can reference the binding)
                let value_reg = self.lower_expr(value)?;

                if needs_cell {
                    // The cell was already created in the Begin pre-pass
                    // Load it and update it with the value
                    let cell_reg = self.fresh_reg();
                    self.emit(LirInstr::LoadLocal {
                        dst: cell_reg,
                        slot,
                    });
                    self.emit(LirInstr::StoreCell {
                        cell: cell_reg,
                        value: value_reg,
                    });
                } else {
                    self.emit(LirInstr::StoreLocal {
                        slot,
                        src: value_reg,
                    });
                }
                Ok(value_reg)
            }

            HirKind::While { cond, body } => {
                let loop_label_id = self.next_label;
                self.next_label += 1;
                let exit_label_id = self.next_label;
                self.next_label += 1;

                // Emit loop label marker
                self.emit(LirInstr::LabelMarker {
                    label_id: loop_label_id,
                });

                // Evaluate condition
                let cond_reg = self.lower_expr(cond)?;

                // Jump to exit if condition is false
                self.emit(LirInstr::JumpIfFalseInline {
                    cond: cond_reg,
                    label_id: exit_label_id,
                });

                // Evaluate body
                self.lower_expr(body)?;

                // Jump back to loop start
                self.emit(LirInstr::JumpInline {
                    label_id: loop_label_id,
                });

                // Exit label
                self.emit(LirInstr::LabelMarker {
                    label_id: exit_label_id,
                });

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

                let loop_label_id = self.next_label;
                self.next_label += 1;
                let exit_label_id = self.next_label;
                self.next_label += 1;

                // Emit loop label marker
                self.emit(LirInstr::LabelMarker {
                    label_id: loop_label_id,
                });

                // Load current iterator and check if it's a pair
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

                // Jump to exit if not a pair
                self.emit(LirInstr::JumpIfFalseInline {
                    cond: is_pair,
                    label_id: exit_label_id,
                });

                // Extract car and store as var
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

                // Evaluate body
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

                // Jump back to loop start
                self.emit(LirInstr::JumpInline {
                    label_id: loop_label_id,
                });

                // Exit label
                self.emit(LirInstr::LabelMarker {
                    label_id: exit_label_id,
                });

                self.emit_const(LirConst::Nil)
            }

            HirKind::And(exprs) => {
                if exprs.is_empty() {
                    return self.emit_const(LirConst::Bool(true));
                }

                let exit_label_id = self.next_label;
                self.next_label += 1;

                let result_reg = self.fresh_reg();

                for (i, expr) in exprs.iter().enumerate() {
                    let expr_reg = self.lower_expr(expr)?;

                    // Move result to result_reg
                    self.emit(LirInstr::Move {
                        dst: result_reg,
                        src: expr_reg,
                    });

                    // If not last expression, check and potentially short-circuit
                    if i < exprs.len() - 1 {
                        // If false, jump to exit (short-circuit)
                        self.emit(LirInstr::JumpIfFalseInline {
                            cond: expr_reg,
                            label_id: exit_label_id,
                        });
                    }
                }

                // Exit label
                self.emit(LirInstr::LabelMarker {
                    label_id: exit_label_id,
                });

                Ok(result_reg)
            }

            HirKind::Or(exprs) => {
                if exprs.is_empty() {
                    return self.emit_const(LirConst::Bool(false));
                }

                let exit_label_id = self.next_label;
                self.next_label += 1;

                let result_reg = self.fresh_reg();

                for (i, expr) in exprs.iter().enumerate() {
                    let expr_reg = self.lower_expr(expr)?;

                    // Move result to result_reg
                    self.emit(LirInstr::Move {
                        dst: result_reg,
                        src: expr_reg,
                    });

                    // If not last expression, check and potentially short-circuit
                    if i < exprs.len() - 1 {
                        // If true, jump to exit (short-circuit)
                        // We use JumpIfFalseInline to skip to next, then JumpInline to exit
                        let next_label_id = self.next_label;
                        self.next_label += 1;

                        self.emit(LirInstr::JumpIfFalseInline {
                            cond: expr_reg,
                            label_id: next_label_id,
                        });
                        // If we get here, expr was true, jump to exit
                        self.emit(LirInstr::JumpInline {
                            label_id: exit_label_id,
                        });
                        self.emit(LirInstr::LabelMarker {
                            label_id: next_label_id,
                        });
                    }
                }

                self.emit(LirInstr::LabelMarker {
                    label_id: exit_label_id,
                });
                Ok(result_reg)
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

            HirKind::Quote(value) => {
                // Quote produces the pre-computed Value as a constant
                self.emit_value_const(value.clone())
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

                let exit_label_id = self.next_label;
                self.next_label += 1;

                let result_reg = self.fresh_reg();

                for (test, body) in clauses {
                    let test_reg = self.lower_expr(test)?;
                    let next_label_id = self.next_label;
                    self.next_label += 1;

                    // Jump to next clause if test is false
                    self.emit(LirInstr::JumpIfFalseInline {
                        cond: test_reg,
                        label_id: next_label_id,
                    });

                    // Evaluate body
                    let body_reg = self.lower_expr(body)?;
                    self.emit(LirInstr::Move {
                        dst: result_reg,
                        src: body_reg,
                    });

                    // Jump to exit
                    self.emit(LirInstr::JumpInline {
                        label_id: exit_label_id,
                    });

                    // Next clause label
                    self.emit(LirInstr::LabelMarker {
                        label_id: next_label_id,
                    });
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

                // Exit label
                self.emit(LirInstr::LabelMarker {
                    label_id: exit_label_id,
                });

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

    fn emit_value_const(&mut self, value: crate::value::Value) -> Result<Reg, String> {
        let dst = self.fresh_reg();
        self.emit(LirInstr::ValueConst { dst, value });
        Ok(dst)
    }

    fn terminate(&mut self, term: Terminator) {
        self.current_block.terminator = term;
    }

    fn finish_block(&mut self) {
        let block = std::mem::replace(&mut self.current_block, BasicBlock::new(Label(0)));
        self.current_func.blocks.push(block);
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
