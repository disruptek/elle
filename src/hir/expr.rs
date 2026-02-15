//! HIR expression types

use super::binding::{BindingId, CaptureInfo};
use super::pattern::HirPattern;
use crate::compiler::effects::Effect;
use crate::syntax::{Span, Syntax};
use crate::value::SymbolId;

/// HIR expression with source location and effect
#[derive(Debug, Clone)]
pub struct Hir {
    pub kind: HirKind,
    pub span: Span,
    pub effect: Effect,
}

impl Hir {
    /// Create a new HIR node
    pub fn new(kind: HirKind, span: Span, effect: Effect) -> Self {
        Hir { kind, span, effect }
    }

    /// Create a pure HIR node
    pub fn pure(kind: HirKind, span: Span) -> Self {
        Hir {
            kind,
            span,
            effect: Effect::Pure,
        }
    }
}

/// HIR expression kinds - fully analyzed forms
#[derive(Debug, Clone)]
pub enum HirKind {
    // === Literals ===
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Keyword(SymbolId),

    // === Variables ===
    /// Reference to a binding (fully resolved)
    Var(BindingId),

    // === Binding Forms ===
    /// Let binding
    Let {
        bindings: Vec<(BindingId, Hir)>,
        body: Box<Hir>,
    },

    /// Letrec (mutually recursive bindings)
    Letrec {
        bindings: Vec<(BindingId, Hir)>,
        body: Box<Hir>,
    },

    /// Lambda expression
    Lambda {
        params: Vec<BindingId>,
        captures: Vec<CaptureInfo>,
        body: Box<Hir>,
        /// Number of local slots needed (params + locals)
        num_locals: u16,
    },

    // === Control Flow ===
    /// If expression
    If {
        cond: Box<Hir>,
        then_branch: Box<Hir>,
        else_branch: Box<Hir>,
    },

    /// Multi-way conditional
    Cond {
        clauses: Vec<(Hir, Hir)>,
        else_branch: Option<Box<Hir>>,
    },

    /// Sequence of expressions
    Begin(Vec<Hir>),

    /// Block with its own scope
    Block(Vec<Hir>),

    // === Function Application ===
    /// Function call
    Call {
        func: Box<Hir>,
        args: Vec<Hir>,
        is_tail: bool,
    },

    // === Mutation ===
    /// Set! - mutate a binding
    Set {
        target: BindingId,
        value: Box<Hir>,
    },

    /// Define - create/update global binding
    Define {
        name: SymbolId,
        value: Box<Hir>,
    },

    // === Loops ===
    /// While loop
    While {
        cond: Box<Hir>,
        body: Box<Hir>,
    },

    /// For/each loop
    For {
        var: BindingId,
        iter: Box<Hir>,
        body: Box<Hir>,
    },

    // === Pattern Matching ===
    Match {
        value: Box<Hir>,
        arms: Vec<(HirPattern, Option<Hir>, Hir)>, // pattern, guard, body
    },

    // === Exception Handling ===
    Try {
        body: Box<Hir>,
        catch: Option<(BindingId, Box<Hir>)>,
        finally: Option<Box<Hir>>,
    },

    Throw(Box<Hir>),

    HandlerCase {
        body: Box<Hir>,
        handlers: Vec<(u32, BindingId, Box<Hir>)>,
    },

    HandlerBind {
        handlers: Vec<(u32, Box<Hir>)>,
        body: Box<Hir>,
    },

    // === Short-circuit Boolean ===
    And(Vec<Hir>),
    Or(Vec<Hir>),

    // === Coroutines ===
    Yield(Box<Hir>),

    // === Quote ===
    /// Quote preserves Syntax (not HIR) since it's unevaluated
    Quote(Syntax),

    // === Module System ===
    Module {
        name: SymbolId,
        exports: Vec<SymbolId>,
        body: Box<Hir>,
    },

    Import {
        module: SymbolId,
    },

    /// Module-qualified reference
    ModuleRef {
        module: SymbolId,
        name: SymbolId,
    },
}
