//! Syntax to HIR analysis
//!
//! This module converts expanded Syntax trees into HIR by:
//! 1. Resolving all variable references to BindingIds
//! 2. Computing captures for closures
//! 3. Inferring effects
//! 4. Validating scope rules

use super::binding::{BindingId, BindingInfo, BindingKind, CaptureInfo, CaptureKind};
use super::expr::{Hir, HirKind};
use super::pattern::{HirPattern, PatternLiteral};
use crate::compiler::effects::Effect;
use crate::symbol::SymbolTable;
use crate::syntax::{Span, Syntax, SyntaxKind};
use std::collections::HashMap;

/// Result of HIR analysis
pub struct AnalysisResult {
    /// The analyzed HIR expression
    pub hir: Hir,
    /// Binding metadata from analysis
    pub bindings: HashMap<BindingId, BindingInfo>,
}

/// Analysis context tracking scopes and bindings
pub struct AnalysisContext {
    /// All bindings in the program
    bindings: HashMap<BindingId, BindingInfo>,
    /// Next binding ID to assign
    next_binding_id: u32,
}

impl AnalysisContext {
    pub fn new() -> Self {
        AnalysisContext {
            bindings: HashMap::new(),
            next_binding_id: 0,
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

/// A lexical scope
struct Scope {
    /// Bindings in this scope, by name
    bindings: HashMap<String, BindingId>,
    /// Is this a function scope (creates new capture boundary)
    is_function: bool,
    /// Next local index for this scope
    next_local: u16,
}

impl Scope {
    fn with_start_index(is_function: bool, start_index: u16) -> Self {
        Scope {
            bindings: HashMap::new(),
            is_function,
            next_local: start_index,
        }
    }
}

/// Analyzer that converts Syntax to HIR
pub struct Analyzer<'a> {
    ctx: AnalysisContext,
    symbols: &'a mut SymbolTable,
    scopes: Vec<Scope>,
    /// Captures for the current function being analyzed
    current_captures: Vec<CaptureInfo>,
}

impl<'a> Analyzer<'a> {
    pub fn new(symbols: &'a mut SymbolTable) -> Self {
        Analyzer {
            ctx: AnalysisContext::new(),
            symbols,
            scopes: Vec::new(),
            current_captures: Vec::new(),
        }
    }

    /// Analyze a syntax tree into HIR
    pub fn analyze(&mut self, syntax: &Syntax) -> Result<AnalysisResult, String> {
        let hir = self.analyze_expr(syntax)?;
        let bindings = std::mem::take(&mut self.ctx.bindings);
        Ok(AnalysisResult { hir, bindings })
    }

    fn analyze_expr(&mut self, syntax: &Syntax) -> Result<Hir, String> {
        let span = syntax.span.clone();

        match &syntax.kind {
            // Literals
            SyntaxKind::Nil => Ok(Hir::pure(HirKind::Nil, span)),
            SyntaxKind::Bool(b) => Ok(Hir::pure(HirKind::Bool(*b), span)),
            SyntaxKind::Int(n) => Ok(Hir::pure(HirKind::Int(*n), span)),
            SyntaxKind::Float(f) => Ok(Hir::pure(HirKind::Float(*f), span)),
            SyntaxKind::String(s) => Ok(Hir::pure(HirKind::String(s.clone()), span)),
            SyntaxKind::Keyword(k) => {
                let sym = self.symbols.intern(k);
                Ok(Hir::pure(HirKind::Keyword(sym), span))
            }

            // Variable reference
            SyntaxKind::Symbol(name) => {
                match self.lookup(name) {
                    Some(id) => Ok(Hir::pure(HirKind::Var(id), span)),
                    None => {
                        // Treat as global reference
                        let sym = self.symbols.intern(name);
                        let id = self.ctx.fresh_binding();
                        self.ctx.register_binding(BindingInfo::global(id, sym));
                        Ok(Hir::pure(HirKind::Var(id), span))
                    }
                }
            }

            // Vector literal
            SyntaxKind::Vector(items) => {
                let mut args = Vec::new();
                let mut effect = Effect::Pure;
                for item in items {
                    let hir = self.analyze_expr(item)?;
                    effect = effect.combine(hir.effect);
                    args.push(hir);
                }
                // For now, treat vector literals as Begin of elements
                Ok(Hir::new(HirKind::Begin(args), span, effect))
            }

            // Quote - preserve as Syntax
            SyntaxKind::Quote(inner) => Ok(Hir::pure(HirKind::Quote((**inner).clone()), span)),

            // Quasiquote, Unquote, UnquoteSplicing should have been expanded
            SyntaxKind::Quasiquote(_) | SyntaxKind::Unquote(_) | SyntaxKind::UnquoteSplicing(_) => {
                Err(format!(
                    "{}: quasiquote forms should be expanded before analysis",
                    span
                ))
            }

            // List - could be special form or function call
            SyntaxKind::List(items) => {
                if items.is_empty() {
                    return Ok(Hir::pure(HirKind::Nil, span));
                }

                // Check for special forms
                if let SyntaxKind::Symbol(name) = &items[0].kind {
                    match name.as_str() {
                        "if" => return self.analyze_if(items, span),
                        "let" => return self.analyze_let(items, span),
                        "let*" => return self.analyze_let_star(items, span),
                        "letrec" => return self.analyze_letrec(items, span),
                        "fn" | "lambda" => return self.analyze_lambda(items, span),
                        "begin" => return self.analyze_begin(&items[1..], span),
                        "block" => return self.analyze_block(&items[1..], span),
                        "define" => return self.analyze_define(items, span),
                        "set!" => return self.analyze_set(items, span),
                        "while" => return self.analyze_while(items, span),
                        "each" => return self.analyze_for(items, span),
                        "and" => return self.analyze_and(&items[1..], span),
                        "or" => return self.analyze_or(&items[1..], span),
                        "quote" => {
                            if items.len() != 2 {
                                return Err(format!("{}: quote requires 1 argument", span));
                            }
                            return Ok(Hir::pure(HirKind::Quote(items[1].clone()), span));
                        }
                        "yield" => return self.analyze_yield(items, span),
                        "try" => return self.analyze_try(items, span),
                        "throw" => return self.analyze_throw(items, span),
                        "match" => return self.analyze_match(items, span),
                        "cond" => return self.analyze_cond(items, span),
                        "handler-case" => return self.analyze_handler_case(items, span),
                        "handler-bind" => return self.analyze_handler_bind(items, span),
                        "module" => return self.analyze_module(items, span),
                        "import" => return self.analyze_import(items, span),
                        _ => {}
                    }
                }

                // Regular function call
                self.analyze_call(items, span)
            }
        }
    }

    fn analyze_if(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() < 3 || items.len() > 4 {
            return Err(format!("{}: if requires 2 or 3 arguments", span));
        }

        let cond = self.analyze_expr(&items[1])?;
        let then_branch = self.analyze_expr(&items[2])?;
        let else_branch = if items.len() == 4 {
            self.analyze_expr(&items[3])?
        } else {
            Hir::pure(HirKind::Nil, span.clone())
        };

        let effect = cond
            .effect
            .combine(then_branch.effect)
            .combine(else_branch.effect);

        Ok(Hir::new(
            HirKind::If {
                cond: Box::new(cond),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            },
            span,
            effect,
        ))
    }

    fn analyze_let(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() < 3 {
            return Err(format!("{}: let requires bindings and body", span));
        }

        let bindings_syntax = items[1]
            .as_list()
            .ok_or_else(|| format!("{}: let bindings must be a list", span))?;

        self.push_scope(false);

        let mut bindings = Vec::new();
        let mut effect = Effect::Pure;

        for binding in bindings_syntax {
            let pair = binding
                .as_list()
                .ok_or_else(|| format!("{}: let binding must be a pair", span))?;
            if pair.len() != 2 {
                return Err(format!("{}: let binding must be (name value)", span));
            }

            let name = pair[0]
                .as_symbol()
                .ok_or_else(|| format!("{}: let binding name must be a symbol", span))?;
            let value = self.analyze_expr(&pair[1])?;
            effect = effect.combine(value.effect);

            let id = self.bind(
                name,
                BindingKind::Local {
                    index: self.current_local_index(),
                },
            );
            bindings.push((id, value));
        }

        // Analyze body expressions
        let body = self.analyze_body(&items[2..], span.clone())?;
        effect = effect.combine(body.effect);

        self.pop_scope();

        Ok(Hir::new(
            HirKind::Let {
                bindings,
                body: Box::new(body),
            },
            span,
            effect,
        ))
    }

    fn analyze_let_star(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        // let* is just nested lets - same as let for our simple scope model
        self.analyze_let(items, span)
    }

    fn analyze_letrec(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() < 3 {
            return Err(format!("{}: letrec requires bindings and body", span));
        }

        let bindings_syntax = items[1]
            .as_list()
            .ok_or_else(|| format!("{}: letrec bindings must be a list", span))?;

        self.push_scope(false);

        // First pass: bind all names (for mutual recursion)
        let mut binding_ids = Vec::new();
        for binding in bindings_syntax {
            let pair = binding
                .as_list()
                .ok_or_else(|| format!("{}: letrec binding must be a pair", span))?;
            if pair.len() != 2 {
                return Err(format!("{}: letrec binding must be (name value)", span));
            }
            let name = pair[0]
                .as_symbol()
                .ok_or_else(|| format!("{}: letrec binding name must be a symbol", span))?;
            let id = self.bind(
                name,
                BindingKind::Local {
                    index: self.current_local_index(),
                },
            );
            binding_ids.push(id);
        }

        // Second pass: analyze values
        let mut bindings = Vec::new();
        let mut effect = Effect::Pure;
        for (i, binding) in bindings_syntax.iter().enumerate() {
            let pair = binding.as_list().unwrap();
            let value = self.analyze_expr(&pair[1])?;
            effect = effect.combine(value.effect);
            bindings.push((binding_ids[i], value));
        }

        let body = self.analyze_body(&items[2..], span.clone())?;
        effect = effect.combine(body.effect);

        self.pop_scope();

        Ok(Hir::new(
            HirKind::Letrec {
                bindings,
                body: Box::new(body),
            },
            span,
            effect,
        ))
    }

    fn analyze_lambda(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() < 3 {
            return Err(format!("{}: lambda requires parameters and body", span));
        }

        let params_syntax = items[1]
            .as_list()
            .ok_or_else(|| format!("{}: lambda parameters must be a list", span))?;

        // Save current captures and start fresh for this lambda
        let saved_captures = std::mem::take(&mut self.current_captures);

        self.push_scope(true);

        // Bind parameters
        let mut params = Vec::new();
        for (i, param) in params_syntax.iter().enumerate() {
            let name = param
                .as_symbol()
                .ok_or_else(|| format!("{}: lambda parameter must be a symbol", span))?;
            let id = self.bind(name, BindingKind::Parameter { index: i as u16 });
            params.push(id);
        }

        // Analyze body
        let body = self.analyze_body(&items[2..], span.clone())?;
        let num_locals = self.current_local_count();

        self.pop_scope();
        let captures = std::mem::replace(&mut self.current_captures, saved_captures);

        // Lambda itself is pure, but captures the body's effect
        Ok(Hir::new(
            HirKind::Lambda {
                params,
                captures,
                body: Box::new(body),
                num_locals,
            },
            span,
            Effect::Pure, // Creating a closure is pure
        ))
    }

    fn analyze_begin(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.is_empty() {
            return Ok(Hir::pure(HirKind::Nil, span));
        }

        let mut exprs = Vec::new();
        let mut effect = Effect::Pure;

        for item in items {
            let hir = self.analyze_expr(item)?;
            effect = effect.combine(hir.effect);
            exprs.push(hir);
        }

        Ok(Hir::new(HirKind::Begin(exprs), span, effect))
    }

    fn analyze_block(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        self.push_scope(false);
        let result = self.analyze_begin(items, span.clone())?;
        self.pop_scope();

        let effect = result.effect;
        Ok(Hir::new(HirKind::Block(vec![result]), span, effect))
    }

    fn analyze_body(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() == 1 {
            self.analyze_expr(&items[0])
        } else {
            self.analyze_begin(items, span)
        }
    }

    fn analyze_define(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() != 3 {
            return Err(format!("{}: define requires name and value", span));
        }

        let name = items[1]
            .as_symbol()
            .ok_or_else(|| format!("{}: define name must be a symbol", span))?;
        let sym = self.symbols.intern(name);
        let value = self.analyze_expr(&items[2])?;

        Ok(Hir::new(
            HirKind::Define {
                name: sym,
                value: Box::new(value),
            },
            span,
            Effect::Pure,
        ))
    }

    fn analyze_set(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() != 3 {
            return Err(format!("{}: set! requires target and value", span));
        }

        let name = items[1]
            .as_symbol()
            .ok_or_else(|| format!("{}: set! target must be a symbol", span))?;

        let target = self
            .lookup(name)
            .ok_or_else(|| format!("{}: undefined variable '{}'", span, name))?;

        // Mark as mutated
        if let Some(info) = self.ctx.get_binding_mut(target) {
            info.mark_mutated();
        }

        let value = self.analyze_expr(&items[2])?;
        let effect = value.effect;

        Ok(Hir::new(
            HirKind::Set {
                target,
                value: Box::new(value),
            },
            span,
            effect,
        ))
    }

    fn analyze_while(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() != 3 {
            return Err(format!("{}: while requires condition and body", span));
        }

        let cond = self.analyze_expr(&items[1])?;
        let body = self.analyze_expr(&items[2])?;
        let effect = cond.effect.combine(body.effect);

        Ok(Hir::new(
            HirKind::While {
                cond: Box::new(cond),
                body: Box::new(body),
            },
            span,
            effect,
        ))
    }

    fn analyze_for(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        // (each var iter body) or (each var in iter body)
        if items.len() < 4 {
            return Err(format!(
                "{}: each requires variable, iterator, and body",
                span
            ));
        }

        let var_name = items[1]
            .as_symbol()
            .ok_or_else(|| format!("{}: each variable must be a symbol", span))?;

        // Check for optional 'in' keyword
        let (iter_idx, body_idx) = if items.len() == 5 {
            if items[2].as_symbol() == Some("in") {
                (3, 4)
            } else {
                return Err(format!(
                    "{}: each syntax is (each var iter body) or (each var in iter body)",
                    span
                ));
            }
        } else {
            (2, 3)
        };

        let iter = self.analyze_expr(&items[iter_idx])?;

        self.push_scope(false);
        let var_id = self.bind(var_name, BindingKind::Local { index: 0 });
        let body = self.analyze_expr(&items[body_idx])?;
        self.pop_scope();

        let effect = iter.effect.combine(body.effect);

        Ok(Hir::new(
            HirKind::For {
                var: var_id,
                iter: Box::new(iter),
                body: Box::new(body),
            },
            span,
            effect,
        ))
    }

    fn analyze_and(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.is_empty() {
            return Ok(Hir::pure(HirKind::Bool(true), span));
        }

        let mut exprs = Vec::new();
        let mut effect = Effect::Pure;

        for item in items {
            let hir = self.analyze_expr(item)?;
            effect = effect.combine(hir.effect);
            exprs.push(hir);
        }

        Ok(Hir::new(HirKind::And(exprs), span, effect))
    }

    fn analyze_or(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.is_empty() {
            return Ok(Hir::pure(HirKind::Bool(false), span));
        }

        let mut exprs = Vec::new();
        let mut effect = Effect::Pure;

        for item in items {
            let hir = self.analyze_expr(item)?;
            effect = effect.combine(hir.effect);
            exprs.push(hir);
        }

        Ok(Hir::new(HirKind::Or(exprs), span, effect))
    }

    fn analyze_yield(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() != 2 {
            return Err(format!("{}: yield requires 1 argument", span));
        }

        let value = self.analyze_expr(&items[1])?;

        Ok(Hir::new(
            HirKind::Yield(Box::new(value)),
            span,
            Effect::Yields, // Yield always has Yields effect
        ))
    }

    fn analyze_try(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        // (try body (catch var handler)? (finally cleanup)?)
        if items.len() < 2 {
            return Err(format!("{}: try requires a body", span));
        }

        let body = self.analyze_expr(&items[1])?;
        let mut effect = body.effect;
        let mut catch = None;
        let mut finally = None;

        for item in &items[2..] {
            let parts = item
                .as_list()
                .ok_or_else(|| format!("{}: try clause must be a list", span))?;
            if parts.is_empty() {
                continue;
            }

            match parts[0].as_symbol() {
                Some("catch") => {
                    if parts.len() != 3 {
                        return Err(format!("{}: catch requires variable and handler", span));
                    }
                    let var_name = parts[1]
                        .as_symbol()
                        .ok_or_else(|| format!("{}: catch variable must be a symbol", span))?;

                    self.push_scope(false);
                    let var_id = self.bind(var_name, BindingKind::Local { index: 0 });
                    let handler = self.analyze_expr(&parts[2])?;
                    self.pop_scope();

                    effect = effect.combine(handler.effect);
                    catch = Some((var_id, Box::new(handler)));
                }
                Some("finally") => {
                    if parts.len() != 2 {
                        return Err(format!("{}: finally requires a body", span));
                    }
                    let cleanup = self.analyze_expr(&parts[1])?;
                    effect = effect.combine(cleanup.effect);
                    finally = Some(Box::new(cleanup));
                }
                _ => return Err(format!("{}: unknown try clause", span)),
            }
        }

        Ok(Hir::new(
            HirKind::Try {
                body: Box::new(body),
                catch,
                finally,
            },
            span,
            effect,
        ))
    }

    fn analyze_throw(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() != 2 {
            return Err(format!("{}: throw requires 1 argument", span));
        }

        let value = self.analyze_expr(&items[1])?;
        let effect = value.effect;

        Ok(Hir::new(HirKind::Throw(Box::new(value)), span, effect))
    }

    fn analyze_match(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() < 3 {
            return Err(format!(
                "{}: match requires value and at least one arm",
                span
            ));
        }

        let value = self.analyze_expr(&items[1])?;
        let mut effect = value.effect;
        let mut arms = Vec::new();

        for arm in &items[2..] {
            let parts = arm
                .as_list()
                .ok_or_else(|| format!("{}: match arm must be a list", span))?;
            if parts.len() < 2 {
                return Err(format!("{}: match arm requires pattern and body", span));
            }

            self.push_scope(false);
            let pattern = self.analyze_pattern(&parts[0])?;

            // Check for guard
            let (guard, body_idx) = if parts.len() >= 3 && parts[1].as_symbol() == Some("when") {
                let guard_expr = self.analyze_expr(&parts[2])?;
                effect = effect.combine(guard_expr.effect);
                (Some(guard_expr), 3)
            } else {
                (None, 1)
            };

            let body = self.analyze_body(&parts[body_idx..], span.clone())?;
            effect = effect.combine(body.effect);
            self.pop_scope();

            arms.push((pattern, guard, body));
        }

        Ok(Hir::new(
            HirKind::Match {
                value: Box::new(value),
                arms,
            },
            span,
            effect,
        ))
    }

    fn analyze_pattern(&mut self, syntax: &Syntax) -> Result<HirPattern, String> {
        match &syntax.kind {
            SyntaxKind::Symbol(name) if name == "_" => Ok(HirPattern::Wildcard),
            SyntaxKind::Symbol(name) if name == "nil" => Ok(HirPattern::Nil),
            SyntaxKind::Symbol(name) => {
                let id = self.bind(
                    name,
                    BindingKind::Local {
                        index: self.current_local_index(),
                    },
                );
                Ok(HirPattern::Var(id))
            }
            SyntaxKind::Nil => Ok(HirPattern::Nil),
            SyntaxKind::Bool(b) => Ok(HirPattern::Literal(PatternLiteral::Bool(*b))),
            SyntaxKind::Int(n) => Ok(HirPattern::Literal(PatternLiteral::Int(*n))),
            SyntaxKind::Float(f) => Ok(HirPattern::Literal(PatternLiteral::Float(*f))),
            SyntaxKind::String(s) => Ok(HirPattern::Literal(PatternLiteral::String(s.clone()))),
            SyntaxKind::Keyword(k) => {
                let sym = self.symbols.intern(k);
                Ok(HirPattern::Literal(PatternLiteral::Keyword(sym)))
            }
            SyntaxKind::List(items) => {
                if items.is_empty() {
                    return Ok(HirPattern::Nil);
                }
                // Check for cons pattern (head . tail)
                if items.len() == 3 && items[1].as_symbol() == Some(".") {
                    let head = self.analyze_pattern(&items[0])?;
                    let tail = self.analyze_pattern(&items[2])?;
                    return Ok(HirPattern::Cons {
                        head: Box::new(head),
                        tail: Box::new(tail),
                    });
                }
                // List pattern
                let patterns: Result<Vec<_>, _> =
                    items.iter().map(|p| self.analyze_pattern(p)).collect();
                Ok(HirPattern::List(patterns?))
            }
            SyntaxKind::Vector(items) => {
                let patterns: Result<Vec<_>, _> =
                    items.iter().map(|p| self.analyze_pattern(p)).collect();
                Ok(HirPattern::Vector(patterns?))
            }
            _ => Err(format!("{}: invalid pattern", syntax.span)),
        }
    }

    fn analyze_cond(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() < 2 {
            return Ok(Hir::pure(HirKind::Nil, span));
        }

        let mut clauses = Vec::new();
        let mut else_branch = None;
        let mut effect = Effect::Pure;

        for clause in &items[1..] {
            let parts = clause
                .as_list()
                .ok_or_else(|| format!("{}: cond clause must be a list", span))?;
            if parts.is_empty() {
                continue;
            }

            if parts[0].as_symbol() == Some("else") {
                let body = self.analyze_body(&parts[1..], span.clone())?;
                effect = effect.combine(body.effect);
                else_branch = Some(Box::new(body));
                break;
            }

            let test = self.analyze_expr(&parts[0])?;
            let body = self.analyze_body(&parts[1..], span.clone())?;
            effect = effect.combine(test.effect).combine(body.effect);
            clauses.push((test, body));
        }

        Ok(Hir::new(
            HirKind::Cond {
                clauses,
                else_branch,
            },
            span,
            effect,
        ))
    }

    fn analyze_handler_case(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() < 2 {
            return Err(format!("{}: handler-case requires a body", span));
        }

        let body = self.analyze_expr(&items[1])?;
        let mut effect = body.effect;
        let mut handlers = Vec::new();

        for handler_syntax in &items[2..] {
            let parts = handler_syntax
                .as_list()
                .ok_or_else(|| format!("{}: handler must be a list", span))?;
            if parts.len() < 3 {
                return Err(format!(
                    "{}: handler requires condition type, var, and body",
                    span
                ));
            }

            // For now, use a simple numeric condition type
            let cond_type = match &parts[0].kind {
                SyntaxKind::Int(n) => *n as u32,
                SyntaxKind::Symbol(s) => {
                    // Map known condition names to IDs
                    match s.as_str() {
                        "error" => 0,
                        "warning" => 1,
                        _ => 0,
                    }
                }
                _ => 0,
            };

            let var_name = parts[1]
                .as_symbol()
                .ok_or_else(|| format!("{}: handler variable must be a symbol", span))?;

            self.push_scope(false);
            let var_id = self.bind(var_name, BindingKind::Local { index: 0 });
            let handler_body = self.analyze_body(&parts[2..], span.clone())?;
            effect = effect.combine(handler_body.effect);
            self.pop_scope();

            handlers.push((cond_type, var_id, Box::new(handler_body)));
        }

        Ok(Hir::new(
            HirKind::HandlerCase {
                body: Box::new(body),
                handlers,
            },
            span,
            effect,
        ))
    }

    fn analyze_handler_bind(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() < 2 {
            return Err(format!("{}: handler-bind requires handlers and body", span));
        }

        let handlers_syntax = items[1]
            .as_list()
            .ok_or_else(|| format!("{}: handler-bind handlers must be a list", span))?;

        let mut handlers = Vec::new();
        let mut effect = Effect::Pure;

        for handler_syntax in handlers_syntax {
            let parts = handler_syntax
                .as_list()
                .ok_or_else(|| format!("{}: handler must be a list", span))?;
            if parts.len() != 2 {
                return Err(format!(
                    "{}: handler requires condition type and function",
                    span
                ));
            }

            let cond_type = match &parts[0].kind {
                SyntaxKind::Int(n) => *n as u32,
                _ => 0,
            };

            let handler_fn = self.analyze_expr(&parts[1])?;
            effect = effect.combine(handler_fn.effect);
            handlers.push((cond_type, Box::new(handler_fn)));
        }

        let body = self.analyze_body(&items[2..], span.clone())?;
        effect = effect.combine(body.effect);

        Ok(Hir::new(
            HirKind::HandlerBind {
                handlers,
                body: Box::new(body),
            },
            span,
            effect,
        ))
    }

    fn analyze_module(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() < 2 {
            return Err(format!("{}: module requires a name", span));
        }

        let name = items[1]
            .as_symbol()
            .ok_or_else(|| format!("{}: module name must be a symbol", span))?;
        let name_sym = self.symbols.intern(name);

        let mut exports = Vec::new();
        let mut body_start = 2;

        // Check for :export clause
        if items.len() > 2 {
            if let SyntaxKind::Keyword(kw) = &items[2].kind {
                if kw == "export" && items.len() > 3 {
                    let export_list = items[3]
                        .as_list()
                        .ok_or_else(|| format!("{}: :export must be followed by a list", span))?;
                    for exp in export_list {
                        let exp_name = exp
                            .as_symbol()
                            .ok_or_else(|| format!("{}: export must be a symbol", span))?;
                        exports.push(self.symbols.intern(exp_name));
                    }
                    body_start = 4;
                }
            }
        }

        let body = self.analyze_body(&items[body_start..], span.clone())?;
        let effect = body.effect;

        Ok(Hir::new(
            HirKind::Module {
                name: name_sym,
                exports,
                body: Box::new(body),
            },
            span,
            effect,
        ))
    }

    fn analyze_import(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        if items.len() != 2 {
            return Err(format!("{}: import requires a module name", span));
        }

        let module = items[1]
            .as_symbol()
            .ok_or_else(|| format!("{}: import module must be a symbol", span))?;
        let module_sym = self.symbols.intern(module);

        Ok(Hir::pure(HirKind::Import { module: module_sym }, span))
    }

    fn analyze_call(&mut self, items: &[Syntax], span: Span) -> Result<Hir, String> {
        let func = self.analyze_expr(&items[0])?;
        let mut effect = func.effect;

        let mut args = Vec::new();
        for arg in &items[1..] {
            let hir = self.analyze_expr(arg)?;
            effect = effect.combine(hir.effect);
            args.push(hir);
        }

        Ok(Hir::new(
            HirKind::Call {
                func: Box::new(func),
                args,
                is_tail: false, // Tail call marking done in a later pass
            },
            span,
            effect,
        ))
    }

    // === Scope Management ===

    fn push_scope(&mut self, is_function: bool) {
        let start_index = if is_function {
            0
        } else {
            self.scopes.last().map(|s| s.next_local).unwrap_or(0)
        };
        self.scopes
            .push(Scope::with_start_index(is_function, start_index));
    }

    fn pop_scope(&mut self) -> Option<Scope> {
        self.scopes.pop()
    }

    fn bind(&mut self, name: &str, kind: BindingKind) -> BindingId {
        let id = self.ctx.fresh_binding();
        let sym = self.symbols.intern(name);

        let info = match kind {
            BindingKind::Parameter { index } => BindingInfo::parameter(id, sym, index),
            BindingKind::Local { index } => BindingInfo::local(id, sym, index),
            BindingKind::Global => BindingInfo::global(id, sym),
        };
        self.ctx.register_binding(info);

        if let Some(scope) = self.scopes.last_mut() {
            scope.bindings.insert(name.to_string(), id);
            if matches!(kind, BindingKind::Local { .. }) {
                scope.next_local += 1;
            }
        }

        id
    }

    fn lookup(&mut self, name: &str) -> Option<BindingId> {
        let mut found_in_scope = None;
        let mut crossed_function_boundary = false;

        // Walk scopes from innermost to outermost
        for (depth, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(&id) = scope.bindings.get(name) {
                found_in_scope = Some((depth, id, crossed_function_boundary));
                break;
            }
            if scope.is_function {
                crossed_function_boundary = true;
            }
        }

        if let Some((_found_depth, id, needs_capture)) = found_in_scope {
            if needs_capture {
                // Mark as captured
                if let Some(info) = self.ctx.get_binding_mut(id) {
                    info.mark_captured();
                }

                // Determine capture kind based on where it was found
                let capture_kind = if let Some(info) = self.ctx.get_binding(id) {
                    match info.kind {
                        BindingKind::Parameter { index } | BindingKind::Local { index } => {
                            CaptureKind::Local { index }
                        }
                        BindingKind::Global => CaptureKind::Global { sym: info.name },
                    }
                } else {
                    return Some(id);
                };

                // Add to current captures if not already present
                if !self.current_captures.iter().any(|c| c.binding == id) {
                    let is_mutated = self
                        .ctx
                        .get_binding(id)
                        .map(|i| i.is_mutated)
                        .unwrap_or(false);

                    self.current_captures.push(CaptureInfo {
                        binding: id,
                        kind: capture_kind,
                        is_mutated,
                    });
                }
            }
            return Some(id);
        }

        None
    }

    fn current_local_index(&self) -> u16 {
        self.scopes.last().map(|s| s.next_local).unwrap_or(0)
    }

    fn current_local_count(&self) -> u16 {
        self.scopes.last().map(|s| s.next_local).unwrap_or(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::Span;
    use crate::value::SymbolId;

    fn make_span() -> Span {
        Span::new(0, 0, 1, 1)
    }

    fn make_int(n: i64) -> Syntax {
        Syntax::new(SyntaxKind::Int(n), make_span())
    }

    fn make_symbol(name: &str) -> Syntax {
        Syntax::new(SyntaxKind::Symbol(name.to_string()), make_span())
    }

    fn make_list(items: Vec<Syntax>) -> Syntax {
        Syntax::new(SyntaxKind::List(items), make_span())
    }

    #[test]
    fn test_analyze_literal() {
        let mut symbols = SymbolTable::new();
        let mut analyzer = Analyzer::new(&mut symbols);

        let syntax = make_int(42);
        let result = analyzer.analyze(&syntax).unwrap();

        match result.hir.kind {
            HirKind::Int(n) => assert_eq!(n, 42),
            _ => panic!("Expected Int"),
        }
    }

    #[test]
    fn test_analyze_if() {
        let mut symbols = SymbolTable::new();
        let mut analyzer = Analyzer::new(&mut symbols);

        let syntax = make_list(vec![
            make_symbol("if"),
            Syntax::new(SyntaxKind::Bool(true), make_span()),
            make_int(1),
            make_int(2),
        ]);

        let result = analyzer.analyze(&syntax).unwrap();
        assert!(matches!(result.hir.kind, HirKind::If { .. }));
    }

    #[test]
    fn test_analyze_let() {
        let mut symbols = SymbolTable::new();
        let mut analyzer = Analyzer::new(&mut symbols);

        let syntax = make_list(vec![
            make_symbol("let"),
            make_list(vec![make_list(vec![make_symbol("x"), make_int(10)])]),
            make_symbol("x"),
        ]);

        let result = analyzer.analyze(&syntax).unwrap();
        assert!(matches!(result.hir.kind, HirKind::Let { .. }));
    }

    #[test]
    fn test_analyze_lambda() {
        let mut symbols = SymbolTable::new();
        let mut analyzer = Analyzer::new(&mut symbols);

        let syntax = make_list(vec![
            make_symbol("fn"),
            make_list(vec![make_symbol("x")]),
            make_symbol("x"),
        ]);

        let result = analyzer.analyze(&syntax).unwrap();
        assert!(matches!(result.hir.kind, HirKind::Lambda { .. }));
    }

    #[test]
    fn test_analyze_call() {
        let mut symbols = SymbolTable::new();
        let mut analyzer = Analyzer::new(&mut symbols);

        let syntax = make_list(vec![make_symbol("+"), make_int(1), make_int(2)]);

        let result = analyzer.analyze(&syntax).unwrap();
        assert!(matches!(result.hir.kind, HirKind::Call { .. }));
    }

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
        assert!(!info.needs_cell());

        info.mark_captured();
        assert!(info.is_captured);
        assert!(info.needs_cell());
    }
}
