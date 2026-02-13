//! Variable reference types for lexical scope resolution
//!
//! VarRef represents a fully-resolved variable reference where all
//! scope resolution has been done at compile time.

use crate::value::SymbolId;

/// A resolved variable reference - all resolution happens at compile time
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VarRef {
    /// Local variable in current activation frame
    /// index is offset in frame's locals array: [params..., locals...]
    Local { index: usize },

    /// Captured variable from enclosing closure
    /// index is offset in closure's captures array
    Upvalue { index: usize },

    /// Global/top-level binding
    /// sym is used for runtime lookup in globals HashMap
    Global { sym: SymbolId },
}

/// Extended VarRef that includes cell-boxing information
/// Used during compilation to determine which instructions to emit
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ResolvedVar {
    /// The base variable reference
    pub var: VarRef,
    /// Whether this variable is boxed in a cell (for mutable captures)
    pub boxed: bool,
}

impl VarRef {
    /// Create a local variable reference
    pub fn local(index: usize) -> Self {
        VarRef::Local { index }
    }

    /// Create an upvalue (captured) variable reference
    pub fn upvalue(index: usize) -> Self {
        VarRef::Upvalue { index }
    }

    /// Create a global variable reference
    pub fn global(sym: SymbolId) -> Self {
        VarRef::Global { sym }
    }

    /// Check if this is a local variable
    pub fn is_local(&self) -> bool {
        matches!(self, VarRef::Local { .. })
    }

    /// Check if this is an upvalue (captured variable)
    pub fn is_upvalue(&self) -> bool {
        matches!(self, VarRef::Upvalue { .. })
    }

    /// Check if this is a global variable
    pub fn is_global(&self) -> bool {
        matches!(self, VarRef::Global { .. })
    }
}

impl ResolvedVar {
    /// Create a resolved variable (unboxed)
    pub fn new(var: VarRef) -> Self {
        ResolvedVar { var, boxed: false }
    }

    /// Create a resolved variable with boxing
    pub fn boxed(var: VarRef) -> Self {
        ResolvedVar { var, boxed: true }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_varref_local() {
        let v = VarRef::local(5);
        assert!(v.is_local());
        assert!(!v.is_upvalue());
        assert!(!v.is_global());
        assert_eq!(v, VarRef::Local { index: 5 });
    }

    #[test]
    fn test_varref_upvalue() {
        let v = VarRef::upvalue(3);
        assert!(!v.is_local());
        assert!(v.is_upvalue());
        assert!(!v.is_global());
        assert_eq!(v, VarRef::Upvalue { index: 3 });
    }

    #[test]
    fn test_varref_global() {
        let sym = SymbolId(42);
        let v = VarRef::global(sym);
        assert!(!v.is_local());
        assert!(!v.is_upvalue());
        assert!(v.is_global());
        assert_eq!(v, VarRef::Global { sym });
    }

    #[test]
    fn test_resolved_var() {
        let local = VarRef::local(0);
        let resolved = ResolvedVar::new(local);
        assert!(!resolved.boxed);

        let boxed_resolved = ResolvedVar::boxed(local);
        assert!(boxed_resolved.boxed);
    }
}
