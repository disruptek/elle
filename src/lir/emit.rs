//! LIR to Bytecode emission
//!
//! Converts register-based LIR to stack-based bytecode.
//! Uses a simple stack simulation to track register values.

use super::types::*;
use crate::compiler::bytecode::{Bytecode, Instruction};
use crate::value::Value;
use std::collections::HashMap;

/// Emits bytecode from LIR
pub struct Emitter {
    /// Output bytecode
    bytecode: Bytecode,
    /// Map from Label to bytecode offset
    label_offsets: HashMap<Label, usize>,
    /// Pending jumps that need patching (instruction position, target label)
    pending_jumps: Vec<(usize, Label)>,
    /// Stack simulation: which register's value is at each stack position
    stack: Vec<Reg>,
    /// Register to stack position mapping (for finding values)
    reg_to_stack: HashMap<Reg, usize>,
}

impl Emitter {
    pub fn new() -> Self {
        Emitter {
            bytecode: Bytecode::new(),
            label_offsets: HashMap::new(),
            pending_jumps: Vec::new(),
            stack: Vec::new(),
            reg_to_stack: HashMap::new(),
        }
    }

    /// Emit bytecode from a LIR function
    pub fn emit(&mut self, func: &LirFunction) -> Bytecode {
        self.bytecode = Bytecode::new();
        self.label_offsets.clear();
        self.pending_jumps.clear();
        self.stack.clear();
        self.reg_to_stack.clear();

        // First pass: record label offsets (simplified - emit all blocks in order)
        // Second pass handled inline since we emit sequentially

        // Sort blocks by label for deterministic output
        let mut blocks: Vec<_> = func.blocks.iter().collect();
        blocks.sort_by_key(|b| b.label.0);

        for block in &blocks {
            self.label_offsets
                .insert(block.label, self.bytecode.current_pos());
            self.emit_block(block, func);
        }

        // Patch jumps
        for (pos, label) in &self.pending_jumps {
            if let Some(&target) = self.label_offsets.get(label) {
                let offset = (target as i32 - *pos as i32 - 2) as i16;
                self.bytecode.patch_jump(*pos, offset);
            }
        }

        std::mem::take(&mut self.bytecode)
    }

    fn emit_block(&mut self, block: &BasicBlock, func: &LirFunction) {
        // Reset stack state at block entry
        self.stack.clear();
        self.reg_to_stack.clear();

        for instr in &block.instructions {
            self.emit_instr(instr, func);
        }

        self.emit_terminator(&block.terminator);
    }

    fn emit_instr(&mut self, instr: &LirInstr, func: &LirFunction) {
        match instr {
            LirInstr::Const { dst, value } => {
                self.emit_const(value, func);
                self.push_reg(*dst);
            }

            LirInstr::LoadLocal { dst, slot } => {
                self.bytecode.emit(Instruction::LoadLocal);
                self.bytecode.emit_byte(0); // depth 0 for now
                self.bytecode.emit_byte(*slot as u8);
                self.push_reg(*dst);
            }

            LirInstr::StoreLocal { slot, src } => {
                self.ensure_on_top(*src);
                self.bytecode.emit(Instruction::StoreLocal);
                self.bytecode.emit_byte(0); // depth 0
                self.bytecode.emit_byte(*slot as u8);
                self.pop();
            }

            LirInstr::LoadCapture { dst, index } => {
                self.bytecode.emit(Instruction::LoadUpvalue);
                self.bytecode.emit_byte(*index as u8);
                self.push_reg(*dst);
            }

            LirInstr::LoadGlobal { dst, sym } => {
                // Add symbol to constants
                let const_idx = self.bytecode.add_constant(Value::Symbol(*sym));
                self.bytecode.emit(Instruction::LoadConst);
                self.bytecode.emit_u16(const_idx);
                self.bytecode.emit(Instruction::LoadGlobal);
                self.push_reg(*dst);
            }

            LirInstr::StoreGlobal { sym, src } => {
                self.ensure_on_top(*src);
                let const_idx = self.bytecode.add_constant(Value::Symbol(*sym));
                self.bytecode.emit(Instruction::LoadConst);
                self.bytecode.emit_u16(const_idx);
                // Stack now has: value symbol (top)
                // StoreGlobal expects value on stack, symbol on stack
                self.bytecode.emit(Instruction::StoreGlobal);
                self.pop();
            }

            LirInstr::MakeClosure {
                dst,
                func_id,
                captures,
            } => {
                // Push captures onto stack
                for cap in captures {
                    self.ensure_on_top(*cap);
                }
                // MakeClosure expects: const_idx for function, num_upvalues
                self.bytecode.emit(Instruction::MakeClosure);
                self.bytecode.emit_u16(*func_id as u16);
                self.bytecode.emit_byte(captures.len() as u8);
                // Pop captures, push closure
                for _ in captures {
                    self.pop();
                }
                self.push_reg(*dst);
            }

            LirInstr::Call { dst, func, args } => {
                // Push args first, then func
                for arg in args {
                    self.ensure_on_top(*arg);
                }
                self.ensure_on_top(*func);
                self.bytecode.emit(Instruction::Call);
                self.bytecode.emit_byte(args.len() as u8);
                // Pop func and args, push result
                self.pop(); // func
                for _ in args {
                    self.pop();
                }
                self.push_reg(*dst);
            }

            LirInstr::TailCall { func, args } => {
                for arg in args {
                    self.ensure_on_top(*arg);
                }
                self.ensure_on_top(*func);
                self.bytecode.emit(Instruction::TailCall);
                self.bytecode.emit_byte(args.len() as u8);
            }

            LirInstr::Cons { dst, head, tail } => {
                self.ensure_on_top(*tail);
                self.ensure_on_top(*head);
                self.bytecode.emit(Instruction::Cons);
                self.pop();
                self.pop();
                self.push_reg(*dst);
            }

            LirInstr::MakeVector { dst, elements } => {
                for elem in elements {
                    self.ensure_on_top(*elem);
                }
                self.bytecode.emit(Instruction::MakeVector);
                self.bytecode.emit_byte(elements.len() as u8);
                for _ in elements {
                    self.pop();
                }
                self.push_reg(*dst);
            }

            LirInstr::Car { dst, pair } => {
                self.ensure_on_top(*pair);
                self.bytecode.emit(Instruction::Car);
                self.pop();
                self.push_reg(*dst);
            }

            LirInstr::Cdr { dst, pair } => {
                self.ensure_on_top(*pair);
                self.bytecode.emit(Instruction::Cdr);
                self.pop();
                self.push_reg(*dst);
            }

            LirInstr::BinOp { dst, op, lhs, rhs } => {
                self.ensure_on_top(*lhs);
                self.ensure_on_top(*rhs);
                let instr = match op {
                    BinOp::Add => Instruction::Add,
                    BinOp::Sub => Instruction::Sub,
                    BinOp::Mul => Instruction::Mul,
                    BinOp::Div => Instruction::Div,
                    BinOp::Rem => Instruction::Div, // TODO: handle Rem properly
                    BinOp::BitAnd => Instruction::Add, // TODO: handle bitwise ops
                    BinOp::BitOr => Instruction::Add,
                    BinOp::BitXor => Instruction::Add,
                    BinOp::Shl => Instruction::Add,
                    BinOp::Shr => Instruction::Add,
                };
                self.bytecode.emit(instr);
                self.pop();
                self.pop();
                self.push_reg(*dst);
            }

            LirInstr::Compare { dst, op, lhs, rhs } => {
                self.ensure_on_top(*lhs);
                self.ensure_on_top(*rhs);
                let instr = match op {
                    CmpOp::Eq => Instruction::Eq,
                    CmpOp::Lt => Instruction::Lt,
                    CmpOp::Gt => Instruction::Gt,
                    CmpOp::Le => Instruction::Le,
                    CmpOp::Ge => Instruction::Ge,
                    CmpOp::Ne => Instruction::Eq, // Will need Not after
                };
                self.bytecode.emit(instr);
                if matches!(op, CmpOp::Ne) {
                    self.bytecode.emit(Instruction::Not);
                }
                self.pop();
                self.pop();
                self.push_reg(*dst);
            }

            LirInstr::UnaryOp { dst, op, src } => {
                self.ensure_on_top(*src);
                match op {
                    UnaryOp::Not => self.bytecode.emit(Instruction::Not),
                    UnaryOp::Neg => {
                        // Emit: push 0, swap, sub
                        // Actually, we need to emit 0 - src
                        // Stack has src on top, we need 0 on top then sub
                        let zero_idx = self.bytecode.add_constant(Value::Int(0));
                        self.bytecode.emit(Instruction::LoadConst);
                        self.bytecode.emit_u16(zero_idx);
                        self.bytecode.emit(Instruction::Sub);
                    }
                    UnaryOp::BitNot => {
                        // TODO: implement bitwise not
                        self.bytecode.emit(Instruction::Not);
                    }
                }
                self.pop();
                self.push_reg(*dst);
            }

            LirInstr::IsNil { dst, src } => {
                self.ensure_on_top(*src);
                self.bytecode.emit(Instruction::IsNil);
                self.pop();
                self.push_reg(*dst);
            }

            LirInstr::IsPair { dst, src } => {
                self.ensure_on_top(*src);
                self.bytecode.emit(Instruction::IsPair);
                self.pop();
                self.push_reg(*dst);
            }

            LirInstr::MakeCell { dst, value } => {
                self.ensure_on_top(*value);
                self.bytecode.emit(Instruction::MakeCell);
                self.pop();
                self.push_reg(*dst);
            }

            LirInstr::LoadCell { dst, cell } => {
                self.ensure_on_top(*cell);
                self.bytecode.emit(Instruction::UnwrapCell);
                self.pop();
                self.push_reg(*dst);
            }

            LirInstr::StoreCell { cell, value } => {
                self.ensure_on_top(*cell);
                self.ensure_on_top(*value);
                self.bytecode.emit(Instruction::UpdateCell);
                self.pop();
                self.pop();
            }

            LirInstr::Move { dst, src } => {
                // Duplicate the value
                self.ensure_on_top(*src);
                self.bytecode.emit(Instruction::Dup);
                self.push_reg(*dst);
            }

            LirInstr::Yield { dst, value } => {
                self.ensure_on_top(*value);
                self.bytecode.emit(Instruction::Yield);
                self.pop();
                self.push_reg(*dst);
            }

            LirInstr::PushHandler { handler_label } => {
                self.bytecode.emit(Instruction::PushHandler);
                // Placeholder for handler offset - will need patching
                let pos = self.bytecode.current_pos();
                self.bytecode.emit_i16(0);
                self.bytecode.emit_i16(-1); // no finally
                self.pending_jumps.push((pos, *handler_label));
            }

            LirInstr::PopHandler => {
                self.bytecode.emit(Instruction::PopHandler);
            }

            LirInstr::Throw { value } => {
                self.ensure_on_top(*value);
                // Use existing exception mechanism
                // For now, just leave value on stack
            }
        }
    }

    fn emit_terminator(&mut self, term: &Terminator) {
        match term {
            Terminator::Return(reg) => {
                self.ensure_on_top(*reg);
                self.bytecode.emit(Instruction::Return);
            }

            Terminator::Jump(label) => {
                self.bytecode.emit(Instruction::Jump);
                let pos = self.bytecode.current_pos();
                self.bytecode.emit_i16(0); // placeholder
                self.pending_jumps.push((pos, *label));
            }

            Terminator::Branch {
                cond,
                then_label,
                else_label,
            } => {
                self.ensure_on_top(*cond);

                // JumpIfFalse to else_label
                self.bytecode.emit(Instruction::JumpIfFalse);
                let else_pos = self.bytecode.current_pos();
                self.bytecode.emit_i16(0); // placeholder
                self.pending_jumps.push((else_pos, *else_label));

                // Fall through or jump to then_label
                self.bytecode.emit(Instruction::Jump);
                let then_pos = self.bytecode.current_pos();
                self.bytecode.emit_i16(0); // placeholder
                self.pending_jumps.push((then_pos, *then_label));
            }

            Terminator::Unreachable => {
                // Emit nil and return as fallback
                self.bytecode.emit(Instruction::Nil);
                self.bytecode.emit(Instruction::Return);
            }
        }
    }

    fn emit_const(&mut self, value: &LirConst, _func: &LirFunction) {
        match value {
            LirConst::Nil => {
                self.bytecode.emit(Instruction::Nil);
            }
            LirConst::Bool(true) => {
                self.bytecode.emit(Instruction::True);
            }
            LirConst::Bool(false) => {
                self.bytecode.emit(Instruction::False);
            }
            LirConst::Int(n) => {
                let idx = self.bytecode.add_constant(Value::Int(*n));
                self.bytecode.emit(Instruction::LoadConst);
                self.bytecode.emit_u16(idx);
            }
            LirConst::Float(f) => {
                let idx = self.bytecode.add_constant(Value::Float(*f));
                self.bytecode.emit(Instruction::LoadConst);
                self.bytecode.emit_u16(idx);
            }
            LirConst::String(s) => {
                let idx = self
                    .bytecode
                    .add_constant(Value::String(std::rc::Rc::from(s.as_str())));
                self.bytecode.emit(Instruction::LoadConst);
                self.bytecode.emit_u16(idx);
            }
            LirConst::Symbol(sym) => {
                let idx = self.bytecode.add_constant(Value::Symbol(*sym));
                self.bytecode.emit(Instruction::LoadConst);
                self.bytecode.emit_u16(idx);
            }
        }
    }

    // Stack management helpers

    fn push_reg(&mut self, reg: Reg) {
        let pos = self.stack.len();
        self.stack.push(reg);
        self.reg_to_stack.insert(reg, pos);
    }

    fn pop(&mut self) {
        if let Some(reg) = self.stack.pop() {
            self.reg_to_stack.remove(&reg);
        }
    }

    fn ensure_on_top(&mut self, reg: Reg) {
        // If register value isn't on stack, we need to reload it
        // This is a simplified approach - a real implementation would
        // track spills and reloads more carefully

        if let Some(&pos) = self.reg_to_stack.get(&reg) {
            // Value is on stack at position pos
            // If not on top, we'd need to shuffle
            // For simplicity, we duplicate if needed
            if pos != self.stack.len() - 1 {
                // Value is not on top - would need stack manipulation
                // For now, assume linear execution keeps values accessible
            }
        } else {
            // Register not on stack - this shouldn't happen in well-formed LIR
            // Emit nil as fallback
            self.bytecode.emit(Instruction::Nil);
            self.push_reg(reg);
        }
    }
}

impl Default for Emitter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_emit_simple() {
        let mut emitter = Emitter::new();

        let mut func = LirFunction::new(0);
        let mut block = BasicBlock::new(Label(0));
        block.instructions.push(LirInstr::Const {
            dst: Reg(0),
            value: LirConst::Int(42),
        });
        block.terminator = Terminator::Return(Reg(0));
        func.blocks.push(block);
        func.entry = Label(0);

        let bytecode = emitter.emit(&func);
        assert!(!bytecode.instructions.is_empty());
    }

    #[test]
    fn test_emit_branch() {
        let mut emitter = Emitter::new();

        let mut func = LirFunction::new(0);

        // Entry block
        let mut entry = BasicBlock::new(Label(0));
        entry.instructions.push(LirInstr::Const {
            dst: Reg(0),
            value: LirConst::Bool(true),
        });
        entry.terminator = Terminator::Branch {
            cond: Reg(0),
            then_label: Label(1),
            else_label: Label(2),
        };
        func.blocks.push(entry);

        // Then block
        let mut then_block = BasicBlock::new(Label(1));
        then_block.instructions.push(LirInstr::Const {
            dst: Reg(1),
            value: LirConst::Int(1),
        });
        then_block.terminator = Terminator::Return(Reg(1));
        func.blocks.push(then_block);

        // Else block
        let mut else_block = BasicBlock::new(Label(2));
        else_block.instructions.push(LirInstr::Const {
            dst: Reg(2),
            value: LirConst::Int(2),
        });
        else_block.terminator = Terminator::Return(Reg(2));
        func.blocks.push(else_block);

        func.entry = Label(0);

        let bytecode = emitter.emit(&func);
        assert!(!bytecode.instructions.is_empty());
        // Should have Jump instructions for control flow
        assert!(bytecode
            .instructions
            .iter()
            .any(|&b| b == Instruction::Jump as u8 || b == Instruction::JumpIfFalse as u8));
    }
}
