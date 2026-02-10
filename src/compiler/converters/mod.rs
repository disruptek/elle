//! Value to expression conversion
//!
//! Converts Elle Values (the parsed Lisp data) into internal Expr AST representation.
//! Handles all special forms, macros, quasiquotes, and variable scoping.

mod patterns;
mod quasiquote;
mod tail_calls;
mod value_to_expr_internal;
mod variables;

pub use tail_calls::mark_tail_calls;
pub use value_to_expr_internal::value_to_expr_with_scope;

use super::ast::Expr;
use super::capture_resolution;
use crate::symbol::SymbolTable;
use crate::value::Value;

/// Simple value-to-expr conversion for bootstrap
/// This is a simple tree-walking approach before full macro expansion
pub fn value_to_expr(value: &Value, symbols: &mut SymbolTable) -> Result<Expr, String> {
    let mut expr = value_to_expr_with_scope(value, symbols, &mut Vec::new())?;
    capture_resolution::resolve_captures(&mut expr);
    mark_tail_calls(&mut expr, true);
    Ok(expr)
}
