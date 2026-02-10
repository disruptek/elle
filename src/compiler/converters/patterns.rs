use super::super::ast::Pattern;
use crate::value::SymbolId;

/// Extract all variable bindings from a pattern
pub fn extract_pattern_variables(pattern: &Pattern) -> Vec<SymbolId> {
    let mut vars = Vec::new();
    match pattern {
        Pattern::Var(sym_id) => {
            vars.push(*sym_id);
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Nil => {
            // These don't bind variables
        }
        Pattern::List(patterns) => {
            // Extract variables from all list elements
            for p in patterns {
                vars.extend(extract_pattern_variables(p));
            }
        }
        Pattern::Cons { head, tail } => {
            // Extract from head and tail
            vars.extend(extract_pattern_variables(head));
            vars.extend(extract_pattern_variables(tail));
        }
        Pattern::Guard { pattern: inner, .. } => {
            // Extract from inner pattern
            vars.extend(extract_pattern_variables(inner));
        }
    }
    vars
}
