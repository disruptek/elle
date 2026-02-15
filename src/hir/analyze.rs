//! Syntax to HIR analysis
//!
//! This module converts expanded Syntax trees into HIR by:
//! 1. Resolving all variable references to BindingIds
//! 2. Computing captures for closures
//! 3. Inferring effects
//! 4. Validating scope rules

use super::binding::{BindingId, BindingInfo, BindingKind, CaptureInfo};
use super::expr::Hir;
use crate::symbol::SymbolTable;
use crate::syntax::Syntax;
use std::collections::HashMap;

/// Analysis context tracking scopes and bindings
pub struct AnalysisContext {
    /// All bindings in the program
    bindings: HashMap<BindingId, BindingInfo>,
    /// Next binding ID to assign
    next_binding_id: u32,
    /// Scope stack for name resolution
    #[allow(dead_code)]
    scopes: Vec<Scope>,
}

/// A lexical scope
#[allow(dead_code)]
struct Scope {
    /// Bindings in this scope, by name
    bindings: HashMap<String, BindingId>,
    /// Is this a function scope (creates new capture boundary)
    is_function: bool,
    /// Captures accumulated for this function scope
    captures: Vec<CaptureInfo>,
    /// Next local index for this scope
    next_local: u16,
}

/// Analyzer that converts Syntax to HIR
#[allow(dead_code)]
pub struct Analyzer<'a> {
    ctx: AnalysisContext,
    symbols: &'a mut SymbolTable,
}

impl AnalysisContext {
    pub fn new() -> Self {
        AnalysisContext {
            bindings: HashMap::new(),
            next_binding_id: 0,
            scopes: Vec::new(),
        }
    }

    /// Create a fresh binding ID
    pub fn fresh_binding(&mut self) -> BindingId {
        let id = BindingId::new(self.next_binding_id);
        self.next_binding_id += 1;
        id
    }

    /// Register a binding
    pub fn register_binding(&mut self, info: BindingInfo) {
        self.bindings.insert(info.id, info);
    }

    /// Get binding info
    pub fn get_binding(&self, id: BindingId) -> Option<&BindingInfo> {
        self.bindings.get(&id)
    }

    /// Get mutable binding info
    pub fn get_binding_mut(&mut self, id: BindingId) -> Option<&mut BindingInfo> {
        self.bindings.get_mut(&id)
    }
}

impl Default for AnalysisContext {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Analyzer<'a> {
    pub fn new(symbols: &'a mut SymbolTable) -> Self {
        Analyzer {
            ctx: AnalysisContext::new(),
            symbols,
        }
    }

    /// Analyze a syntax tree into HIR
    pub fn analyze(&mut self, _syntax: &Syntax) -> Result<Hir, String> {
        // TODO: Implement full analysis
        // For now, return a stub error
        Err("HIR analysis not yet implemented".to_string())
    }

    /// Push a new scope
    #[allow(dead_code)]
    fn push_scope(&mut self, is_function: bool) {
        self.scopes_mut().push(Scope {
            bindings: HashMap::new(),
            is_function,
            captures: Vec::new(),
            next_local: 0,
        });
    }

    /// Pop current scope
    #[allow(dead_code)]
    fn pop_scope(&mut self) -> Option<Scope> {
        self.scopes_mut().pop()
    }

    #[allow(dead_code)]
    fn scopes_mut(&mut self) -> &mut Vec<Scope> {
        &mut self.ctx.scopes
    }

    /// Bind a name in the current scope
    #[allow(dead_code)]
    fn bind(&mut self, name: &str, kind: BindingKind) -> BindingId {
        let id = self.ctx.fresh_binding();
        let sym = self.symbols.intern(name);
        let info = match kind {
            BindingKind::Parameter { index } => BindingInfo::parameter(id, sym, index),
            BindingKind::Local { index } => BindingInfo::local(id, sym, index),
            BindingKind::Global => BindingInfo::global(id, sym),
        };
        self.ctx.register_binding(info);

        if let Some(scope) = self.scopes_mut().last_mut() {
            scope.bindings.insert(name.to_string(), id);
        }

        id
    }

    /// Look up a name, handling captures
    #[allow(dead_code)]
    fn lookup(&mut self, name: &str) -> Option<BindingId> {
        // Walk scopes from innermost to outermost
        let scopes = &self.ctx.scopes;

        for (depth, scope) in scopes.iter().enumerate().rev() {
            if let Some(&id) = scope.bindings.get(name) {
                // Found it - check if we need to capture
                if depth < scopes.len() - 1 {
                    // TODO: Handle capture across function boundaries
                }
                return Some(id);
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::SymbolId;

    #[test]
    fn test_fresh_binding_id() {
        let mut ctx = AnalysisContext::new();
        let id1 = ctx.fresh_binding();
        let id2 = ctx.fresh_binding();
        assert_ne!(id1, id2);
        assert_eq!(id1, BindingId::new(0));
        assert_eq!(id2, BindingId::new(1));
    }

    #[test]
    fn test_binding_info() {
        let id = BindingId::new(0);
        let sym = SymbolId(1);

        let mut info = BindingInfo::local(id, sym, 0);
        assert!(!info.is_mutated);
        assert!(!info.is_captured);
        assert!(!info.needs_cell());

        info.mark_mutated();
        assert!(info.is_mutated);
        assert!(!info.needs_cell()); // Not captured yet

        info.mark_captured();
        assert!(info.is_captured);
        assert!(info.needs_cell()); // Now needs cell
    }
}
