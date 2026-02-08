// Cranelift JIT compiler for Elle Lisp
//
// This module integrates Cranelift as a JIT backend to compile Elle Lisp
// functions directly to native x86_64 code, replacing the stack-based
// bytecode interpreter for hot functions.
//
// Architecture:
// 1. AST → Cranelift IR (CLIF)
// 2. Cranelift IR → x86_64 machine code
// 3. Runtime: Profile → JIT compile → Execute native
//
// Value Representation:
// - Values are `Value` enum (Rust side)
// - Passed by reference/pointer across native boundaries
// - Primitives (Int, Float, Bool, Nil) optimized as inline values

pub mod binop;
pub mod codegen;
pub mod compiler;
pub mod context;
pub mod primitives;
pub mod tests;

pub use binop::BinOpCompiler;
pub use codegen::IrEmitter;
pub use compiler::ExprCompiler;
pub use context::JITContext;
pub use primitives::{CompiledValue, PrimitiveEncoder};
