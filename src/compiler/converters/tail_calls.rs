use super::super::ast::Expr;

/// Mark Call expressions in tail position with tail=true for TCO
pub fn mark_tail_calls(expr: &mut Expr, in_tail: bool) {
    match expr {
        Expr::Call { func, args, tail } => {
            if in_tail {
                *tail = true;
            }
            // Function and arguments are NOT in tail position
            mark_tail_calls(func, false);
            for arg in args {
                mark_tail_calls(arg, false);
            }
        }
        Expr::If { cond, then, else_ } => {
            mark_tail_calls(cond, false);
            mark_tail_calls(then, in_tail);
            mark_tail_calls(else_, in_tail);
        }
        Expr::Cond { clauses, else_body } => {
            for (test, body) in clauses {
                mark_tail_calls(test, false);
                mark_tail_calls(body, in_tail);
            }
            if let Some(else_expr) = else_body {
                mark_tail_calls(else_expr, in_tail);
            }
        }
        Expr::Begin(exprs) => {
            let len = exprs.len();
            for (i, e) in exprs.iter_mut().enumerate() {
                let is_last = i == len - 1;
                mark_tail_calls(e, in_tail && is_last);
            }
        }
        Expr::Block(exprs) => {
            let len = exprs.len();
            for (i, e) in exprs.iter_mut().enumerate() {
                let is_last = i == len - 1;
                mark_tail_calls(e, in_tail && is_last);
            }
        }
        Expr::Lambda { body, .. } => {
            // Lambda body is in tail position (of the lambda)
            mark_tail_calls(body, true);
        }
        Expr::Let { bindings, body } => {
            for (_, e) in bindings {
                mark_tail_calls(e, false);
            }
            mark_tail_calls(body, in_tail);
        }
        Expr::Letrec { bindings, body } => {
            for (_, e) in bindings {
                mark_tail_calls(e, false);
            }
            mark_tail_calls(body, in_tail);
        }
        Expr::Set { value, .. } => {
            mark_tail_calls(value, false);
        }
        Expr::Define { value, .. } => {
            mark_tail_calls(value, false);
        }
        Expr::While { cond, body } => {
            mark_tail_calls(cond, false);
            mark_tail_calls(body, false);
        }
        Expr::For { iter, body, .. } => {
            mark_tail_calls(iter, false);
            mark_tail_calls(body, false);
        }
        Expr::Match {
            value,
            patterns,
            default,
        } => {
            mark_tail_calls(value, false);
            for (_, body) in patterns {
                mark_tail_calls(body, in_tail);
            }
            if let Some(d) = default {
                mark_tail_calls(d, in_tail);
            }
        }
        Expr::Try {
            body,
            catch,
            finally,
        } => {
            mark_tail_calls(body, false);
            if let Some((_, handler)) = catch {
                mark_tail_calls(handler, false);
            }
            if let Some(f) = finally {
                mark_tail_calls(f, false);
            }
        }
        Expr::And(exprs) | Expr::Or(exprs) => {
            let len = exprs.len();
            for (i, e) in exprs.iter_mut().enumerate() {
                let is_last = i == len - 1;
                mark_tail_calls(e, in_tail && is_last);
            }
        }
        Expr::DefMacro { body, .. } => {
            mark_tail_calls(body, false);
        }
        _ => {}
    }
}
