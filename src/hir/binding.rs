//! Binding information for HIR
//!
//! This module defines how variables are tracked and captured in the HIR.
//! Each binding site (parameter, let-binding, etc.) gets a unique BindingId.

use crate::syntax::Span;

/// Unique identifier for a binding (variable, parameter, etc.)
/// Unlike SymbolId, each binding site gets a unique BindingId
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BindingId(pub u32);

impl std::fmt::Display for BindingId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "b{}", self.0)
    }
}

/// Information about a binding
#[derive(Debug, Clone)]
pub struct BindingInfo {
    pub id: BindingId,
    pub name: String,   // Original name (for debugging/errors)
    pub span: Span,     // Where it was defined
    pub mutable: bool,  // Is this binding mutated?
    pub captured: bool, // Is this binding captured by a closure?
}

impl BindingInfo {
    pub fn new(id: BindingId, name: String, span: Span) -> Self {
        BindingInfo {
            id,
            name,
            span,
            mutable: false,
            captured: false,
        }
    }
}

/// How a variable is captured
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaptureKind {
    /// Captured by value (immutable binding)
    ByValue,
    /// Captured by reference (mutable binding, needs cell boxing)
    ByRef,
}

/// A captured variable for closures
#[derive(Debug, Clone)]
pub struct CaptureInfo {
    pub binding: BindingId,
    pub kind: CaptureKind,
    pub index: usize, // Index in closure's capture array
}

impl CaptureInfo {
    pub fn new(binding: BindingId, kind: CaptureKind, index: usize) -> Self {
        CaptureInfo {
            binding,
            kind,
            index,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binding_id_creation() {
        let id1 = BindingId(0);
        let id2 = BindingId(1);
        assert_ne!(id1, id2);
        assert_eq!(id1, BindingId(0));
    }

    #[test]
    fn test_binding_id_display() {
        let id = BindingId(42);
        assert_eq!(id.to_string(), "b42");
    }

    #[test]
    fn test_binding_info_creation() {
        let span = Span::new(0, 5, 1, 1);
        let info = BindingInfo::new(BindingId(0), "x".to_string(), span.clone());
        assert_eq!(info.id, BindingId(0));
        assert_eq!(info.name, "x");
        assert!(!info.mutable);
        assert!(!info.captured);
    }

    #[test]
    fn test_capture_info_creation() {
        let capture = CaptureInfo::new(BindingId(0), CaptureKind::ByValue, 0);
        assert_eq!(capture.binding, BindingId(0));
        assert_eq!(capture.kind, CaptureKind::ByValue);
        assert_eq!(capture.index, 0);
    }

    #[test]
    fn test_capture_kind_equality() {
        assert_eq!(CaptureKind::ByValue, CaptureKind::ByValue);
        assert_eq!(CaptureKind::ByRef, CaptureKind::ByRef);
        assert_ne!(CaptureKind::ByValue, CaptureKind::ByRef);
    }
}
