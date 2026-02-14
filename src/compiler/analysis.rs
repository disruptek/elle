use super::ast::Expr;
use crate::binding::VarRef;
use crate::value::SymbolId;
use std::collections::HashSet;

/// Analyze which variables from a given set are actually used in an expression
/// Returns the subset of candidates that are actually referenced as upvalues
pub fn analyze_capture_usage(
    expr: &Expr,
    _local_bindings: &HashSet<SymbolId>,
    candidates: &HashSet<SymbolId>,
) -> HashSet<SymbolId> {
    let mut used = HashSet::new();
    collect_used_captures(expr, candidates, &mut used);
    used
}

fn collect_used_captures(
    expr: &Expr,
    candidates: &HashSet<SymbolId>,
    used: &mut HashSet<SymbolId>,
) {
    match expr {
        Expr::Var(var_ref) => {
            if let VarRef::Upvalue { sym, .. } = var_ref {
                if candidates.contains(sym) {
                    used.insert(*sym);
                }
            }
        }
        Expr::If { cond, then, else_ } => {
            collect_used_captures(cond, candidates, used);
            collect_used_captures(then, candidates, used);
            collect_used_captures(else_, candidates, used);
        }
        Expr::Cond { clauses, else_body } => {
            for (test, body) in clauses {
                collect_used_captures(test, candidates, used);
                collect_used_captures(body, candidates, used);
            }
            if let Some(else_expr) = else_body {
                collect_used_captures(else_expr, candidates, used);
            }
        }
        Expr::Begin(exprs) | Expr::Block(exprs) | Expr::And(exprs) | Expr::Or(exprs) => {
            for e in exprs {
                collect_used_captures(e, candidates, used);
            }
        }
        Expr::Call { func, args, .. } => {
            collect_used_captures(func, candidates, used);
            for arg in args {
                collect_used_captures(arg, candidates, used);
            }
        }
        Expr::Lambda { body, .. } => {
            collect_used_captures(body, candidates, used);
        }
        Expr::Let { bindings, body } | Expr::Letrec { bindings, body } => {
            for (_, e) in bindings {
                collect_used_captures(e, candidates, used);
            }
            collect_used_captures(body, candidates, used);
        }
        Expr::Set { value, .. } => {
            collect_used_captures(value, candidates, used);
        }
        Expr::Define { value, .. } => {
            collect_used_captures(value, candidates, used);
        }
        Expr::While { cond, body } => {
            collect_used_captures(cond, candidates, used);
            collect_used_captures(body, candidates, used);
        }
        Expr::For { iter, body, .. } => {
            collect_used_captures(iter, candidates, used);
            collect_used_captures(body, candidates, used);
        }
        _ => {}
    }
}

/// Analyze free variables in an expression
/// Collects all VarRef::Upvalue symbols that need to be captured
pub fn analyze_free_vars(expr: &Expr, local_bindings: &HashSet<SymbolId>) -> HashSet<SymbolId> {
    let mut free_vars = HashSet::new();
    collect_free_vars(expr, local_bindings, &mut free_vars);
    free_vars
}

fn collect_free_vars(
    expr: &Expr,
    local_bindings: &HashSet<SymbolId>,
    free_vars: &mut HashSet<SymbolId>,
) {
    match expr {
        Expr::Var(var_ref) => {
            // Upvalues and LetBound variables are free variables that need to be captured
            match var_ref {
                VarRef::Upvalue { sym, .. } | VarRef::LetBound { sym } => {
                    if !local_bindings.contains(sym) {
                        free_vars.insert(*sym);
                    }
                }
                _ => {}
            }
        }
        Expr::If { cond, then, else_ } => {
            collect_free_vars(cond, local_bindings, free_vars);
            collect_free_vars(then, local_bindings, free_vars);
            collect_free_vars(else_, local_bindings, free_vars);
        }
        Expr::Cond { clauses, else_body } => {
            for (test, body) in clauses {
                collect_free_vars(test, local_bindings, free_vars);
                collect_free_vars(body, local_bindings, free_vars);
            }
            if let Some(else_expr) = else_body {
                collect_free_vars(else_expr, local_bindings, free_vars);
            }
        }
        Expr::Begin(exprs) | Expr::Block(exprs) | Expr::And(exprs) | Expr::Or(exprs) => {
            for e in exprs {
                collect_free_vars(e, local_bindings, free_vars);
            }
        }
        Expr::Call { func, args, .. } => {
            collect_free_vars(func, local_bindings, free_vars);
            for arg in args {
                collect_free_vars(arg, local_bindings, free_vars);
            }
        }
        Expr::Lambda { captures, .. } => {
            // Nested lambdas have their own captures already computed.
            // We need to propagate those captures as free variables of THIS scope
            // (unless they're already local to this scope).
            for capture_info in captures {
                if !local_bindings.contains(&capture_info.sym) {
                    free_vars.insert(capture_info.sym);
                }
            }
        }
        Expr::Let { bindings, body } | Expr::Letrec { bindings, body } => {
            // Collect free vars from binding expressions (using original local_bindings)
            for (_, expr) in bindings {
                collect_free_vars(expr, local_bindings, free_vars);
            }
            // Create extended local_bindings that includes the let-bound variables
            // This ensures nested lambdas' captures of these variables don't propagate up
            let mut extended_bindings = local_bindings.clone();
            for (sym, _) in bindings {
                extended_bindings.insert(*sym);
            }
            collect_free_vars(body, &extended_bindings, free_vars);
        }
        Expr::Set { value, .. } => {
            collect_free_vars(value, local_bindings, free_vars);
        }
        Expr::Define { value, .. } => {
            collect_free_vars(value, local_bindings, free_vars);
        }
        Expr::While { cond, body } => {
            collect_free_vars(cond, local_bindings, free_vars);
            collect_free_vars(body, local_bindings, free_vars);
        }
        Expr::For { iter, body, .. } => {
            collect_free_vars(iter, local_bindings, free_vars);
            collect_free_vars(body, local_bindings, free_vars);
        }
        Expr::Match {
            value,
            patterns,
            default,
        } => {
            collect_free_vars(value, local_bindings, free_vars);
            for (_, body) in patterns {
                collect_free_vars(body, local_bindings, free_vars);
            }
            if let Some(d) = default {
                collect_free_vars(d, local_bindings, free_vars);
            }
        }
        Expr::Yield(e) => {
            collect_free_vars(e, local_bindings, free_vars);
        }
        Expr::Try {
            body,
            catch,
            finally,
        } => {
            collect_free_vars(body, local_bindings, free_vars);
            if let Some((_, h)) = catch {
                collect_free_vars(h, local_bindings, free_vars);
            }
            if let Some(f) = finally {
                collect_free_vars(f, local_bindings, free_vars);
            }
        }
        Expr::Throw { value } => {
            collect_free_vars(value, local_bindings, free_vars);
        }
        Expr::Literal(_) | Expr::Quote(_) | Expr::Quasiquote(_) => {
            // No free variables in literals
        }
        // Handle any other variants that might exist
        _ => {}
    }
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
