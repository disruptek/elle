//! Pattern matching lowering

use super::*;
use crate::hir::{HirPattern, PatternLiteral};

impl Lowerer {
    /// Lower pattern matching code
    /// Emits code that checks if value_reg matches the pattern
    /// If it doesn't match, branches to fail_label
    /// If it matches, binds any variables and continues in the current block
    pub(super) fn lower_pattern_match(
        &mut self,
        pattern: &HirPattern,
        value_reg: Reg,
        fail_label: Label,
    ) -> Result<(), String> {
        match pattern {
            HirPattern::Wildcard => {
                // Wildcard always matches, do nothing
                Ok(())
            }
            HirPattern::Nil => {
                // Check if value is nil (NOT empty_list)
                // nil and '() are distinct values with distinct semantics
                let is_nil_reg = self.fresh_reg();
                self.emit(LirInstr::IsNil {
                    dst: is_nil_reg,
                    src: value_reg,
                });

                // If NOT nil, fail; otherwise continue
                let continue_label = self.fresh_label();
                self.terminate(Terminator::Branch {
                    cond: is_nil_reg,
                    then_label: continue_label,
                    else_label: fail_label,
                });
                self.finish_block();
                self.current_block = BasicBlock::new(continue_label);

                Ok(())
            }
            HirPattern::Literal(lit) => {
                // Check if value equals literal
                let lit_reg = match lit {
                    PatternLiteral::Bool(b) => self.emit_const(LirConst::Bool(*b))?,
                    PatternLiteral::Int(n) => self.emit_const(LirConst::Int(*n))?,
                    PatternLiteral::Float(f) => self.emit_const(LirConst::Float(*f))?,
                    PatternLiteral::String(s) => self.emit_const(LirConst::String(s.clone()))?,
                    PatternLiteral::Keyword(sym) => self.emit_const(LirConst::Keyword(*sym))?,
                };

                let eq_reg = self.fresh_reg();
                self.emit(LirInstr::Compare {
                    dst: eq_reg,
                    op: CmpOp::Eq,
                    lhs: value_reg,
                    rhs: lit_reg,
                });

                let continue_label = self.fresh_label();
                self.terminate(Terminator::Branch {
                    cond: eq_reg,
                    then_label: continue_label,
                    else_label: fail_label,
                });
                self.finish_block();
                self.current_block = BasicBlock::new(continue_label);

                Ok(())
            }
            HirPattern::Var(binding_id) => {
                // Bind the value to the variable
                let slot = self.allocate_slot(*binding_id);
                self.emit(LirInstr::StoreLocal {
                    slot,
                    src: value_reg,
                });
                Ok(())
            }
            HirPattern::Cons { head, tail } => {
                // Check if value is a pair
                let is_pair_reg = self.fresh_reg();
                self.emit(LirInstr::IsPair {
                    dst: is_pair_reg,
                    src: value_reg,
                });

                let continue_label = self.fresh_label();
                self.terminate(Terminator::Branch {
                    cond: is_pair_reg,
                    then_label: continue_label,
                    else_label: fail_label,
                });
                self.finish_block();
                self.current_block = BasicBlock::new(continue_label);

                // Extract head and tail
                let head_reg = self.fresh_reg();
                self.emit(LirInstr::Car {
                    dst: head_reg,
                    pair: value_reg,
                });

                let tail_reg = self.fresh_reg();
                self.emit(LirInstr::Cdr {
                    dst: tail_reg,
                    pair: value_reg,
                });

                // Recursively match head and tail
                // Both must match, so they both branch to fail_label on failure
                self.lower_pattern_match(head, head_reg, fail_label)?;
                self.lower_pattern_match(tail, tail_reg, fail_label)?;

                Ok(())
            }
            HirPattern::List(patterns) => {
                // Check if value is a list of the right length
                // Iterate through patterns and match each element

                let mut current_reg = value_reg;

                for pat in patterns.iter() {
                    // Check if current is a pair
                    let is_pair_reg = self.fresh_reg();
                    self.emit(LirInstr::IsPair {
                        dst: is_pair_reg,
                        src: current_reg,
                    });

                    let continue_label = self.fresh_label();
                    self.terminate(Terminator::Branch {
                        cond: is_pair_reg,
                        then_label: continue_label,
                        else_label: fail_label,
                    });
                    self.finish_block();
                    self.current_block = BasicBlock::new(continue_label);

                    // Store current to a temporary slot so we can load it twice
                    let temp_slot = self.current_func.num_locals;
                    self.current_func.num_locals += 1;
                    self.emit(LirInstr::StoreLocal {
                        slot: temp_slot,
                        src: current_reg,
                    });

                    // Load for car extraction
                    let current_for_car = self.fresh_reg();
                    self.emit(LirInstr::LoadLocal {
                        dst: current_for_car,
                        slot: temp_slot,
                    });

                    // Extract head
                    let head_reg = self.fresh_reg();
                    self.emit(LirInstr::Car {
                        dst: head_reg,
                        pair: current_for_car,
                    });

                    // Match head against pattern
                    self.lower_pattern_match(pat, head_reg, fail_label)?;

                    // Load for cdr extraction
                    let current_for_cdr = self.fresh_reg();
                    self.emit(LirInstr::LoadLocal {
                        dst: current_for_cdr,
                        slot: temp_slot,
                    });

                    // Extract tail for next iteration
                    let tail_reg = self.fresh_reg();
                    self.emit(LirInstr::Cdr {
                        dst: tail_reg,
                        pair: current_for_cdr,
                    });

                    current_reg = tail_reg;
                }

                // Check that tail is empty_list (list ends)
                // Proper lists end with empty_list ()

                // Load current_reg for the empty_list check
                let empty_list_reg = self.fresh_reg();
                self.emit(LirInstr::ValueConst {
                    dst: empty_list_reg,
                    value: Value::EMPTY_LIST,
                });
                let is_empty_reg = self.fresh_reg();
                self.emit(LirInstr::Compare {
                    dst: is_empty_reg,
                    op: CmpOp::Eq,
                    lhs: current_reg,
                    rhs: empty_list_reg,
                });

                // If NOT empty_list, fail
                let continue_label = self.fresh_label();
                self.terminate(Terminator::Branch {
                    cond: is_empty_reg,
                    then_label: continue_label,
                    else_label: fail_label,
                });
                self.finish_block();
                self.current_block = BasicBlock::new(continue_label);

                Ok(())
            }
            HirPattern::Vector(_patterns) => {
                // TODO: Implement vector pattern matching
                Err("Vector pattern matching not yet implemented".to_string())
            }
        }
    }
}
