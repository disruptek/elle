use super::expression::Compiler;
use super::bytecode::Instruction;
use super::ast::Pattern;

/// Compile pattern matching check. Returns list of jump positions to patch if pattern fails.
pub fn compile_pattern_check(compiler: &mut Compiler, pattern: &Pattern) -> Vec<usize> {
    match pattern {
        Pattern::Wildcard => {
            // Wildcard matches anything, no check needed
            Vec::new()
        }
        Pattern::Nil => {
            // Check if value is nil
            compiler.bytecode.emit(Instruction::Dup);
            compiler.bytecode.emit(Instruction::Nil);
            compiler.bytecode.emit(Instruction::Eq);
            compiler.bytecode.emit(Instruction::JumpIfFalse);
            let fail_jump = compiler.bytecode.instructions.len();
            compiler.bytecode.emit_i16(0);
            vec![fail_jump]
        }
        Pattern::Literal(val) => {
            // Check if value equals literal
            compiler.bytecode.emit(Instruction::Dup);
            let const_idx = compiler.bytecode.add_constant(val.clone());
            compiler.bytecode.emit(Instruction::LoadConst);
            compiler.bytecode.emit_u16(const_idx);
            compiler.bytecode.emit(Instruction::Eq);
            compiler.bytecode.emit(Instruction::JumpIfFalse);
            let fail_jump = compiler.bytecode.instructions.len();
            compiler.bytecode.emit_i16(0);
            vec![fail_jump]
        }
        Pattern::Var(_var_id) => {
            // Variable pattern always matches - store the value
            // In Phase 2, we'll just accept any value (binding handled later)
            Vec::new()
        }
        Pattern::Cons { head: _, tail: _ } => {
            // Cons pattern: check if it's a pair/cons cell
            compiler.bytecode.emit(Instruction::Dup);
            compiler.bytecode.emit(Instruction::IsPair);
            compiler.bytecode.emit(Instruction::JumpIfFalse);
            let fail_jump = compiler.bytecode.instructions.len();
            compiler.bytecode.emit_i16(0);
            // Full cons pattern matching would recursively compile head/tail patterns
            // For Phase 2, just check if it's a pair
            vec![fail_jump]
        }
        Pattern::List(_patterns) => {
            // List pattern: for Phase 2, just check if it's a list
            // Full implementation would check length and match elements
            // For now, accept any value
            Vec::new()
        }
        Pattern::Guard {
            pattern: inner,
            condition: _,
        } => {
            // Guard pattern: check inner pattern first, then condition
            let fails = compile_pattern_check(compiler, inner);
            // Full guard implementation would evaluate the condition
            // For Phase 2, just check the pattern
            fails
        }
    }
}
