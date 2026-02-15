//! High-level Intermediate Representation (HIR) expressions
//!
//! This is the post-analysis representation where all names are resolved to binding IDs.
//! Unlike the Syntax tree, the HIR has:
//! - All variable references resolved to BindingIds or SymbolIds
//! - Capture information for closures
//! - Mutability information for bindings
//! - Special forms explicitly represented

use super::binding::{BindingId, CaptureInfo};
use super::pattern::HirPattern;
use crate::syntax::Span;
use crate::value::SymbolId;

/// High-level IR expression
#[derive(Debug, Clone)]
pub struct Hir {
    pub kind: HirKind,
    pub span: Span,
}

impl Hir {
    pub fn new(kind: HirKind, span: Span) -> Self {
        Hir { kind, span }
    }

    /// Create a synthetic HIR node (for generated code)
    pub fn synthetic(kind: HirKind) -> Self {
        Hir {
            kind,
            span: Span::synthetic(),
        }
    }
}

/// HIR expression kinds
#[derive(Debug, Clone)]
pub enum HirKind {
    // Literals (no Value - primitive types only)
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Keyword(SymbolId),

    // Quoted data (converted to runtime Value later)
    Quote(Box<Hir>),

    // Variable reference
    Var(VarRef),

    // Control flow
    If {
        cond: Box<Hir>,
        then_: Box<Hir>,
        else_: Box<Hir>,
    },

    // Sequence
    Begin(Vec<Hir>),

    // Function call
    Call {
        func: Box<Hir>,
        args: Vec<Hir>,
        tail: bool,
    },

    // Lambda
    Lambda {
        params: Vec<BindingId>,
        body: Box<Hir>,
        captures: Vec<CaptureInfo>,
        num_locals: usize,
    },

    // Let binding
    Let {
        bindings: Vec<(BindingId, Hir)>,
        body: Box<Hir>,
    },

    // Assignment
    Set {
        target: VarRef,
        value: Box<Hir>,
    },

    // Top-level definition
    Define {
        name: SymbolId,
        value: Box<Hir>,
    },

    // Loops
    While {
        cond: Box<Hir>,
        body: Box<Hir>,
    },

    For {
        var: BindingId,
        iter: Box<Hir>,
        body: Box<Hir>,
    },

    // Pattern matching
    Match {
        scrutinee: Box<Hir>,
        arms: Vec<(HirPattern, Hir)>,
    },

    // Exception handling
    Try {
        body: Box<Hir>,
        catch: Option<(BindingId, Box<Hir>)>,
        finally: Option<Box<Hir>>,
    },

    Throw(Box<Hir>),

    // Short-circuit logical
    And(Vec<Hir>),
    Or(Vec<Hir>),

    // Yield (for generators/coroutines)
    Yield(Box<Hir>),

    // Vector literal
    Vector(Vec<Hir>),
}

/// Resolved variable reference
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarRef {
    /// Local variable (parameter or let-binding in current function)
    Local(BindingId),
    /// Captured variable from enclosing scope
    Capture { index: usize },
    /// Global variable
    Global(SymbolId),
}

impl VarRef {
    /// Check if this is a local binding
    pub fn is_local(&self) -> bool {
        matches!(self, VarRef::Local(_))
    }

    /// Check if this is a captured binding
    pub fn is_capture(&self) -> bool {
        matches!(self, VarRef::Capture { .. })
    }

    /// Check if this is a global binding
    pub fn is_global(&self) -> bool {
        matches!(self, VarRef::Global(_))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hir_creation() {
        let span = Span::new(0, 5, 1, 1);
        let hir = Hir::new(HirKind::Nil, span.clone());
        assert_eq!(hir.span, span);
        matches!(hir.kind, HirKind::Nil);
    }

    #[test]
    fn test_hir_synthetic() {
        let hir = Hir::synthetic(HirKind::Bool(true));
        assert_eq!(hir.span, Span::synthetic());
        matches!(hir.kind, HirKind::Bool(true));
    }

    #[test]
    fn test_var_ref_local() {
        let var_ref = VarRef::Local(BindingId(0));
        assert!(var_ref.is_local());
        assert!(!var_ref.is_capture());
        assert!(!var_ref.is_global());
    }

    #[test]
    fn test_var_ref_capture() {
        let var_ref = VarRef::Capture { index: 0 };
        assert!(!var_ref.is_local());
        assert!(var_ref.is_capture());
        assert!(!var_ref.is_global());
    }

    #[test]
    fn test_var_ref_global() {
        let var_ref = VarRef::Global(SymbolId(0));
        assert!(!var_ref.is_local());
        assert!(!var_ref.is_capture());
        assert!(var_ref.is_global());
    }

    #[test]
    fn test_hir_int_literal() {
        let span = Span::new(0, 2, 1, 1);
        let hir = Hir::new(HirKind::Int(42), span);
        matches!(hir.kind, HirKind::Int(42));
    }

    #[test]
    fn test_hir_string_literal() {
        let span = Span::new(0, 5, 1, 1);
        let hir = Hir::new(HirKind::String("hello".to_string()), span);
        matches!(hir.kind, HirKind::String(ref s) if s == "hello");
    }

    #[test]
    fn test_hir_vector() {
        let span = Span::new(0, 5, 1, 1);
        let items = vec![
            Hir::new(HirKind::Int(1), span.clone()),
            Hir::new(HirKind::Int(2), span.clone()),
        ];
        let hir = Hir::new(HirKind::Vector(items), span);
        matches!(hir.kind, HirKind::Vector(ref v) if v.len() == 2);
    }

    #[test]
    fn test_hir_begin() {
        let span = Span::new(0, 5, 1, 1);
        let items = vec![
            Hir::new(HirKind::Int(1), span.clone()),
            Hir::new(HirKind::Int(2), span.clone()),
        ];
        let hir = Hir::new(HirKind::Begin(items), span);
        matches!(hir.kind, HirKind::Begin(ref v) if v.len() == 2);
    }

    #[test]
    fn test_hir_if() {
        let span = Span::new(0, 5, 1, 1);
        let cond = Box::new(Hir::new(HirKind::Bool(true), span.clone()));
        let then_ = Box::new(Hir::new(HirKind::Int(1), span.clone()));
        let else_ = Box::new(Hir::new(HirKind::Int(2), span.clone()));
        let hir = Hir::new(HirKind::If { cond, then_, else_ }, span);
        matches!(hir.kind, HirKind::If { .. });
    }

    #[test]
    fn test_hir_lambda() {
        let span = Span::new(0, 5, 1, 1);
        let params = vec![BindingId(0)];
        let body = Box::new(Hir::new(
            HirKind::Var(VarRef::Local(BindingId(0))),
            span.clone(),
        ));
        let hir = Hir::new(
            HirKind::Lambda {
                params,
                body,
                captures: vec![],
                num_locals: 1,
            },
            span,
        );
        matches!(hir.kind, HirKind::Lambda { .. });
    }

    #[test]
    fn test_hir_let() {
        let span = Span::new(0, 5, 1, 1);
        let bindings = vec![(BindingId(0), Hir::new(HirKind::Int(42), span.clone()))];
        let body = Box::new(Hir::new(
            HirKind::Var(VarRef::Local(BindingId(0))),
            span.clone(),
        ));
        let hir = Hir::new(HirKind::Let { bindings, body }, span);
        matches!(hir.kind, HirKind::Let { .. });
    }

    #[test]
    fn test_hir_call() {
        let span = Span::new(0, 5, 1, 1);
        let func = Box::new(Hir::new(
            HirKind::Var(VarRef::Global(SymbolId(0))),
            span.clone(),
        ));
        let args = vec![Hir::new(HirKind::Int(1), span.clone())];
        let hir = Hir::new(
            HirKind::Call {
                func,
                args,
                tail: false,
            },
            span,
        );
        matches!(hir.kind, HirKind::Call { .. });
    }
}
