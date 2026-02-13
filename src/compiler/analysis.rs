use super::ast::Expr;
use crate::binding::VarRef;
use crate::value::SymbolId;
use std::collections::HashSet;

/// Analyze which variables from a given set are actually used in an expression
/// NOTE: With VarRef, this analysis is now done during parsing.
/// This function is kept for compatibility but returns an empty set.
pub fn analyze_capture_usage(
    _expr: &Expr,
    _local_bindings: &HashSet<SymbolId>,
    _candidates: &HashSet<SymbolId>,
) -> HashSet<SymbolId> {
    // With VarRef, capture analysis happens at parse time
    HashSet::new()
}

/// Analyze free variables in an expression
/// NOTE: With VarRef, free variable analysis is done during parsing.
/// This function is kept for compatibility but returns an empty set.
pub fn analyze_free_vars(_expr: &Expr, _local_bindings: &HashSet<SymbolId>) -> HashSet<SymbolId> {
    // With VarRef, free variable analysis happens at parse time
    HashSet::new()
}

/// Analyze which locally-defined variables in an expression are referenced by nested lambdas
/// NOTE: With VarRef, this is tracked during parsing.
pub fn analyze_local_vars_captured_by_nested_lambdas(_expr: &Expr) -> HashSet<SymbolId> {
    HashSet::new()
}

/// Analyze which variables are mutated with set! in an expression
/// This is used to determine which captured variables need cell boxing
pub fn analyze_mutated_vars(expr: &Expr) -> HashSet<SymbolId> {
    let mut mutated = HashSet::new();

    match expr {
        Expr::Set { target, .. } => {
            // Extract symbol from target if it's global
            if let VarRef::Global { sym } = target {
                mutated.insert(*sym);
            }
            // For local/upvalue, we don't have the symbol directly
            // The mutation tracking should happen at parse time
        }

        Expr::If { cond, then, else_ } => {
            mutated.extend(analyze_mutated_vars(cond));
            mutated.extend(analyze_mutated_vars(then));
            mutated.extend(analyze_mutated_vars(else_));
        }

        Expr::Begin(exprs) | Expr::Block(exprs) => {
            for e in exprs {
                mutated.extend(analyze_mutated_vars(e));
            }
        }

        Expr::Call { func, args, .. } => {
            mutated.extend(analyze_mutated_vars(func));
            for arg in args {
                mutated.extend(analyze_mutated_vars(arg));
            }
        }

        Expr::Lambda { body: _, .. } => {
            // Don't collect mutations from nested lambdas - only the current level matters
            // Nested lambda mutations are separate from the outer lambda's concerns
        }

        Expr::Let { bindings, body } => {
            for (_, expr) in bindings {
                mutated.extend(analyze_mutated_vars(expr));
            }
            mutated.extend(analyze_mutated_vars(body));
        }

        Expr::Letrec { bindings, body } => {
            for (_, expr) in bindings {
                mutated.extend(analyze_mutated_vars(expr));
            }
            mutated.extend(analyze_mutated_vars(body));
        }

        Expr::Define { value, .. } => {
            mutated.extend(analyze_mutated_vars(value));
        }

        Expr::While { cond, body } => {
            mutated.extend(analyze_mutated_vars(cond));
            mutated.extend(analyze_mutated_vars(body));
        }

        Expr::For { iter, body, .. } => {
            mutated.extend(analyze_mutated_vars(iter));
            mutated.extend(analyze_mutated_vars(body));
        }

        Expr::Match {
            value,
            patterns,
            default,
        } => {
            mutated.extend(analyze_mutated_vars(value));
            for (_, expr) in patterns {
                mutated.extend(analyze_mutated_vars(expr));
            }
            if let Some(default_expr) = default {
                mutated.extend(analyze_mutated_vars(default_expr));
            }
        }

        Expr::Try {
            body,
            catch,
            finally,
        } => {
            mutated.extend(analyze_mutated_vars(body));
            if let Some((_, handler)) = catch {
                mutated.extend(analyze_mutated_vars(handler));
            }
            if let Some(finally_expr) = finally {
                mutated.extend(analyze_mutated_vars(finally_expr));
            }
        }

        Expr::Cond { clauses, else_body } => {
            for (test, body) in clauses {
                mutated.extend(analyze_mutated_vars(test));
                mutated.extend(analyze_mutated_vars(body));
            }
            if let Some(else_expr) = else_body {
                mutated.extend(analyze_mutated_vars(else_expr));
            }
        }

        _ => {
            // Other expression types don't mutate variables
        }
    }

    mutated
}
