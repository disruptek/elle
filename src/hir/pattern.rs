//! Pattern matching for HIR
//!
//! Patterns are used in match expressions to destructure values.

use super::binding::BindingId;
use crate::value::SymbolId;

/// Pattern for pattern matching
#[derive(Debug, Clone)]
pub enum HirPattern {
    /// Match anything, bind to variable
    Var(BindingId),
    /// Match anything, don't bind
    Wildcard,
    /// Match nil
    Nil,
    /// Match specific integer
    Int(i64),
    /// Match specific bool
    Bool(bool),
    /// Match specific string
    String(String),
    /// Match specific symbol (quoted)
    Symbol(SymbolId),
    /// Match cons cell
    Cons {
        head: Box<HirPattern>,
        tail: Box<HirPattern>,
    },
    /// Match list of specific length
    List(Vec<HirPattern>),
    /// Match with guard condition
    Guard {
        pattern: Box<HirPattern>,
        condition: Box<super::expr::Hir>,
    },
}

impl HirPattern {
    /// Check if this pattern always matches (wildcard or guard with true condition)
    pub fn is_irrefutable(&self) -> bool {
        matches!(self, HirPattern::Wildcard | HirPattern::Var(_))
    }

    /// Get all bindings introduced by this pattern
    pub fn bindings(&self) -> Vec<BindingId> {
        match self {
            HirPattern::Var(id) => vec![*id],
            HirPattern::Wildcard
            | HirPattern::Nil
            | HirPattern::Int(_)
            | HirPattern::Bool(_)
            | HirPattern::String(_)
            | HirPattern::Symbol(_) => vec![],
            HirPattern::Cons { head, tail } => {
                let mut bindings = head.bindings();
                bindings.extend(tail.bindings());
                bindings
            }
            HirPattern::List(patterns) => patterns.iter().flat_map(|p| p.bindings()).collect(),
            HirPattern::Guard { pattern, .. } => pattern.bindings(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wildcard_irrefutable() {
        assert!(HirPattern::Wildcard.is_irrefutable());
    }

    #[test]
    fn test_var_irrefutable() {
        assert!(HirPattern::Var(BindingId(0)).is_irrefutable());
    }

    #[test]
    fn test_literal_refutable() {
        assert!(!HirPattern::Int(42).is_irrefutable());
        assert!(!HirPattern::Bool(true).is_irrefutable());
        assert!(!HirPattern::Nil.is_irrefutable());
    }

    #[test]
    fn test_var_bindings() {
        let pattern = HirPattern::Var(BindingId(0));
        let bindings = pattern.bindings();
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0], BindingId(0));
    }

    #[test]
    fn test_wildcard_bindings() {
        let pattern = HirPattern::Wildcard;
        let bindings = pattern.bindings();
        assert_eq!(bindings.len(), 0);
    }

    #[test]
    fn test_list_bindings() {
        let pattern = HirPattern::List(vec![
            HirPattern::Var(BindingId(0)),
            HirPattern::Var(BindingId(1)),
            HirPattern::Wildcard,
        ]);
        let bindings = pattern.bindings();
        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0], BindingId(0));
        assert_eq!(bindings[1], BindingId(1));
    }

    #[test]
    fn test_cons_bindings() {
        let pattern = HirPattern::Cons {
            head: Box::new(HirPattern::Var(BindingId(0))),
            tail: Box::new(HirPattern::Var(BindingId(1))),
        };
        let bindings = pattern.bindings();
        assert_eq!(bindings.len(), 2);
    }
}
