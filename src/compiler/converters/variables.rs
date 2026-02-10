use super::super::ast::Expr;
use crate::value::SymbolId;

/// Adjust variable indices in an expression to account for the closure environment layout.
/// The closure environment is laid out as [captures..., parameters...]
///
/// During parsing, Var nodes contain indices relative to the scope_stack at parse time.
/// After computing captures, we need to convert these to absolute indices in the final environment.
///
/// For each Var:
/// - If it's a parameter of the current lambda: map to captures.len() + position_in_params
/// - If it's a captured value: map to position_in_captures_list
pub fn adjust_var_indices(
    expr: &mut Expr,
    captures: &[(SymbolId, usize, usize)],
    params: &[SymbolId],
) {
    // Build a map of captures to their position in the list
    let mut capture_map: std::collections::HashMap<SymbolId, usize> =
        std::collections::HashMap::new();
    for (i, (sym, _, _)) in captures.iter().enumerate() {
        capture_map.insert(*sym, i);
    }

    // Build a map of parameters to their position in the list
    let mut param_map: std::collections::HashMap<SymbolId, usize> =
        std::collections::HashMap::new();
    for (i, sym) in params.iter().enumerate() {
        param_map.insert(*sym, i);
    }

    match expr {
        Expr::Var(sym_id, _depth, index) => {
            // Adjust indices for variables that are captures or parameters of this lambda
            // Both depth==0 (current scope params) and depth>0 (captured vars) need adjustment
            // because the index is currently relative to scope_stack at parse time, not the
            // final closure environment [captures..., parameters...]
            if let Some(cap_pos) = capture_map.get(sym_id) {
                // This variable is a capture - map to its position in the captures list
                *index = *cap_pos;
            } else if let Some(param_pos) = param_map.get(sym_id) {
                // This variable is a parameter - map to captures.len() + position
                *index = captures.len() + param_pos;
            }
            // Otherwise, it's a global or something from an even outer scope
        }
        Expr::If { cond, then, else_ } => {
            adjust_var_indices(cond, captures, params);
            adjust_var_indices(then, captures, params);
            adjust_var_indices(else_, captures, params);
        }
        Expr::Cond { clauses, else_body } => {
            for (test, body) in clauses {
                adjust_var_indices(test, captures, params);
                adjust_var_indices(body, captures, params);
            }
            if let Some(else_expr) = else_body {
                adjust_var_indices(else_expr, captures, params);
            }
        }
        Expr::Begin(exprs) => {
            for e in exprs {
                adjust_var_indices(e, captures, params);
            }
        }
        Expr::Block(exprs) => {
            for e in exprs {
                adjust_var_indices(e, captures, params);
            }
        }
        Expr::Lambda {
            body,
            captures: lambda_captures,
            ..
        } => {
            adjust_var_indices(body, lambda_captures, params);
        }
        Expr::Call { func, args, .. } => {
            adjust_var_indices(func, captures, params);
            for arg in args {
                adjust_var_indices(arg, captures, params);
            }
        }
        Expr::Let { bindings, body } => {
            for (_, value) in bindings {
                adjust_var_indices(value, captures, params);
            }
            adjust_var_indices(body, captures, params);
        }
        Expr::Letrec { bindings, body } => {
            for (_, value) in bindings {
                adjust_var_indices(value, captures, params);
            }
            adjust_var_indices(body, captures, params);
        }
        Expr::Literal(_) | Expr::GlobalVar(_) => {
            // Nothing to adjust
        }
        Expr::Set { value, .. } => {
            adjust_var_indices(value, captures, params);
        }
        Expr::Define { value, .. } => {
            adjust_var_indices(value, captures, params);
        }
        Expr::While { cond, body } => {
            adjust_var_indices(cond, captures, params);
            adjust_var_indices(body, captures, params);
        }
        Expr::For { iter, body, .. } => {
            adjust_var_indices(iter, captures, params);
            adjust_var_indices(body, captures, params);
        }
        Expr::Match {
            value,
            patterns,
            default,
        } => {
            adjust_var_indices(value, captures, params);
            for (_, body) in patterns {
                adjust_var_indices(body, captures, params);
            }
            if let Some(d) = default {
                adjust_var_indices(d, captures, params);
            }
        }
        Expr::Try {
            body,
            catch,
            finally,
        } => {
            adjust_var_indices(body, captures, params);
            if let Some((_, handler)) = catch {
                adjust_var_indices(handler, captures, params);
            }
            if let Some(f) = finally {
                adjust_var_indices(f, captures, params);
            }
        }
        Expr::HandlerCase { body, handlers } => {
            adjust_var_indices(body, captures, params);
            for (_, _, handler) in handlers {
                adjust_var_indices(handler, captures, params);
            }
        }
        Expr::HandlerBind { handlers, body } => {
            for (_, handler) in handlers {
                adjust_var_indices(handler, captures, params);
            }
            adjust_var_indices(body, captures, params);
        }
        Expr::And(exprs) | Expr::Or(exprs) => {
            for expr in exprs {
                adjust_var_indices(expr, captures, params);
            }
        }
        Expr::DefMacro { body, .. } => {
            adjust_var_indices(body, captures, params);
        }
        Expr::Throw { value } => {
            adjust_var_indices(value, captures, params);
        }
        Expr::Quote(expr) | Expr::Quasiquote(expr) | Expr::Unquote(expr) => {
            adjust_var_indices(expr, captures, params);
        }
        Expr::Module { .. } => {
            // Module definitions don't need variable adjustment
        }
        Expr::Import { .. } => {
            // Imports don't contain expressions
        }
        Expr::Xor(exprs) => {
            for expr in exprs {
                adjust_var_indices(expr, captures, params);
            }
        }
        Expr::ScopeVar(_, _) | Expr::ScopeEntry(_) | Expr::ScopeExit | Expr::ModuleRef { .. } => {
            // Scope tracking and module ref expressions don't contain sub-expressions
        }
    }
}
