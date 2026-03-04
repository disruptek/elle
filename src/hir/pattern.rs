//! Pattern matching in HIR

use super::binding::Binding;
use crate::value::SymbolId;

/// HIR pattern for match expressions
#[derive(Debug, Clone)]
pub enum HirPattern {
    /// Match anything, don't bind
    Wildcard,

    /// Match nil
    Nil,

    /// Match a literal value
    Literal(PatternLiteral),

    /// Bind to a variable
    Var(Binding),

    /// Match cons cell and destructure
    Cons {
        head: Box<HirPattern>,
        tail: Box<HirPattern>,
    },

    /// Match a list pattern with optional rest
    List {
        elements: Vec<HirPattern>,
        rest: Option<Box<HirPattern>>,
    },

    /// Match a tuple \[...\] pattern with optional rest (emits IsTuple guard)
    Tuple {
        elements: Vec<HirPattern>,
        rest: Option<Box<HirPattern>>,
    },

    /// Match an array @\[...\] pattern with optional rest (emits IsArray guard)
    Array {
        elements: Vec<HirPattern>,
        rest: Option<Box<HirPattern>>,
    },

    /// Match a struct {...} by keyword keys (emits IsStruct guard)
    /// Each entry is (keyword_name, pattern_for_value)
    Struct { entries: Vec<(String, HirPattern)> },

    /// Match a table @{...} by keyword keys (emits IsTable guard)
    /// Each entry is (keyword_name, pattern_for_value)
    Table { entries: Vec<(String, HirPattern)> },

    /// Match any of the alternative patterns.
    /// All alternatives must bind the same set of variable names.
    Or(Vec<HirPattern>),
}

/// Literal values that can appear in patterns
#[derive(Debug, Clone)]
pub enum PatternLiteral {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Keyword(String),
}

/// Bindings introduced by a pattern
#[derive(Debug, Clone, Default)]
pub struct PatternBindings {
    pub bindings: Vec<Binding>,
}

impl PatternBindings {
    pub fn new() -> Self {
        PatternBindings {
            bindings: Vec::new(),
        }
    }

    pub fn add(&mut self, binding: Binding) {
        self.bindings.push(binding);
    }

    pub fn extend(&mut self, other: &PatternBindings) {
        self.bindings.extend(other.bindings.iter().copied());
    }
}

impl HirPattern {
    /// Collect all bindings introduced by this pattern
    pub fn bindings(&self) -> PatternBindings {
        let mut result = PatternBindings::new();
        self.collect_bindings(&mut result);
        result
    }

    fn collect_bindings(&self, out: &mut PatternBindings) {
        match self {
            HirPattern::Var(binding) => out.add(*binding),
            HirPattern::Cons { head, tail } => {
                head.collect_bindings(out);
                tail.collect_bindings(out);
            }
            HirPattern::List { elements, rest }
            | HirPattern::Tuple { elements, rest }
            | HirPattern::Array { elements, rest } => {
                for p in elements {
                    p.collect_bindings(out);
                }
                if let Some(r) = rest {
                    r.collect_bindings(out);
                }
            }
            HirPattern::Struct { entries } | HirPattern::Table { entries } => {
                for (_, pattern) in entries {
                    pattern.collect_bindings(out);
                }
            }
            HirPattern::Or(alternatives) => {
                // All alternatives bind the same variables; collect from the first
                if let Some(first) = alternatives.first() {
                    first.collect_bindings(out);
                }
            }
            HirPattern::Wildcard | HirPattern::Nil | HirPattern::Literal(_) => {}
        }
    }

    /// Return the set of SymbolIds bound by this pattern.
    pub fn binding_names(&self) -> std::collections::BTreeSet<SymbolId> {
        let mut names = std::collections::BTreeSet::new();
        self.collect_binding_names(&mut names);
        names
    }

    fn collect_binding_names(&self, out: &mut std::collections::BTreeSet<SymbolId>) {
        match self {
            HirPattern::Var(binding) => {
                out.insert(binding.name());
            }
            HirPattern::Cons { head, tail } => {
                head.collect_binding_names(out);
                tail.collect_binding_names(out);
            }
            HirPattern::List { elements, rest }
            | HirPattern::Tuple { elements, rest }
            | HirPattern::Array { elements, rest } => {
                for p in elements {
                    p.collect_binding_names(out);
                }
                if let Some(r) = rest {
                    r.collect_binding_names(out);
                }
            }
            HirPattern::Struct { entries } | HirPattern::Table { entries } => {
                for (_, pattern) in entries {
                    pattern.collect_binding_names(out);
                }
            }
            HirPattern::Or(alts) => {
                if let Some(first) = alts.first() {
                    first.collect_binding_names(out);
                }
            }
            HirPattern::Wildcard | HirPattern::Nil | HirPattern::Literal(_) => {}
        }
    }
}

/// Validate that all alternatives in an or-pattern bind the same set of variables.
pub(crate) fn validate_or_pattern_bindings(
    alternatives: &[HirPattern],
    span: &crate::syntax::Span,
) -> Result<(), String> {
    if alternatives.len() < 2 {
        return Ok(());
    }
    let reference_names = alternatives[0].binding_names();
    for (i, alt) in alternatives.iter().enumerate().skip(1) {
        let alt_names = alt.binding_names();
        if alt_names != reference_names {
            return Err(format!(
                "{}: or-pattern alternative {} binds different variables than alternative 1",
                span,
                i + 1
            ));
        }
    }
    Ok(())
}
