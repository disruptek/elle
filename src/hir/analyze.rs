//! Syntax → HIR analyzer
//!
//! This module converts the Syntax tree (pre-analysis) into the HIR (post-analysis).
//! Key responsibilities:
//! - Scope tracking: Maintain a scope stack mapping names to BindingIds
//! - Binding allocation: Assign unique BindingIds to each binding site
//! - Capture analysis: Determine which bindings are captured and how
//! - Name resolution: Convert symbol names to VarRef (Local/Capture/Global)
//! - Mutability tracking: Mark bindings that are targets of `set!`
//! - Special form recognition: Handle `if`, `let`, `lambda`, `define`, etc.

use super::binding::{BindingId, BindingInfo, CaptureKind};
use super::expr::{Hir, HirKind, VarRef};
use super::pattern::HirPattern;
use crate::symbol::SymbolTable;
use crate::syntax::{Span, Syntax, SyntaxKind};
use std::collections::HashMap;

/// Analyzer state for converting Syntax to HIR
pub struct Analyzer<'a> {
    symbols: &'a mut SymbolTable,
    next_binding: u32,
    scopes: Vec<Scope>,
    bindings: HashMap<BindingId, BindingInfo>,
}

/// A scope level (function or block)
struct Scope {
    bindings: HashMap<String, BindingId>,
    #[allow(dead_code)]
    is_function: bool, // True for lambda scopes
    #[allow(dead_code)]
    captured: HashMap<BindingId, CaptureKind>, // Bindings captured from outer scopes
}

impl<'a> Analyzer<'a> {
    /// Create a new analyzer
    pub fn new(symbols: &'a mut SymbolTable) -> Self {
        Analyzer {
            symbols,
            next_binding: 0,
            scopes: vec![Scope {
                bindings: HashMap::new(),
                is_function: false,
                captured: HashMap::new(),
            }],
            bindings: HashMap::new(),
        }
    }

    /// Allocate a new binding ID
    fn alloc_binding(&mut self, name: String, span: Span) -> BindingId {
        let id = BindingId(self.next_binding);
        self.next_binding += 1;
        let info = BindingInfo::new(id, name, span);
        self.bindings.insert(id, info);
        id
    }

    /// Enter a new scope
    fn push_scope(&mut self, is_function: bool) {
        self.scopes.push(Scope {
            bindings: HashMap::new(),
            is_function,
            captured: HashMap::new(),
        });
    }

    /// Exit the current scope
    fn pop_scope(&mut self) -> Scope {
        self.scopes.pop().expect("scope stack underflow")
    }

    /// Get the current scope
    fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("empty scope stack")
    }

    /// Bind a name in the current scope
    fn bind(&mut self, name: String, span: Span) -> BindingId {
        let id = self.alloc_binding(name.clone(), span);
        self.current_scope().bindings.insert(name, id);
        id
    }

    /// Look up a name in the scope chain
    fn lookup(&self, name: &str) -> Option<VarRef> {
        // Search from innermost to outermost scope
        for (depth, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(&id) = scope.bindings.get(name) {
                let current_depth = self.scopes.len() - 1;
                if depth == current_depth {
                    // Local binding in current scope
                    return Some(VarRef::Local(id));
                } else {
                    // Captured binding from outer scope
                    // For now, return a placeholder - capture analysis happens later
                    return Some(VarRef::Local(id));
                }
            }
        }
        // Not found in any scope - must be global
        None
    }

    /// Analyze a syntax tree into HIR
    pub fn analyze(&mut self, syntax: &Syntax) -> Result<Hir, String> {
        self.analyze_expr(syntax)
    }

    /// Analyze an expression
    fn analyze_expr(&mut self, syntax: &Syntax) -> Result<Hir, String> {
        match &syntax.kind {
            // Literals
            SyntaxKind::Nil => Ok(Hir::new(HirKind::Nil, syntax.span.clone())),
            SyntaxKind::Bool(b) => Ok(Hir::new(HirKind::Bool(*b), syntax.span.clone())),
            SyntaxKind::Int(i) => Ok(Hir::new(HirKind::Int(*i), syntax.span.clone())),
            SyntaxKind::Float(f) => Ok(Hir::new(HirKind::Float(*f), syntax.span.clone())),
            SyntaxKind::String(s) => Ok(Hir::new(HirKind::String(s.clone()), syntax.span.clone())),
            SyntaxKind::Keyword(k) => {
                let sym_id = self.symbols.intern(k);
                Ok(Hir::new(HirKind::Keyword(sym_id), syntax.span.clone()))
            }

            // Vector literal
            SyntaxKind::Vector(items) => {
                let hir_items: Result<Vec<_>, _> =
                    items.iter().map(|s| self.analyze_expr(s)).collect();
                Ok(Hir::new(HirKind::Vector(hir_items?), syntax.span.clone()))
            }

            // Quote
            SyntaxKind::Quote(inner) => {
                let hir = self.analyze_expr(inner)?;
                Ok(Hir::new(HirKind::Quote(Box::new(hir)), syntax.span.clone()))
            }

            // Symbol or list
            SyntaxKind::Symbol(name) => {
                // Variable reference
                if let Some(var_ref) = self.lookup(name) {
                    Ok(Hir::new(HirKind::Var(var_ref), syntax.span.clone()))
                } else {
                    // Global variable
                    let sym_id = self.symbols.intern(name);
                    Ok(Hir::new(
                        HirKind::Var(VarRef::Global(sym_id)),
                        syntax.span.clone(),
                    ))
                }
            }

            SyntaxKind::List(items) => {
                if items.is_empty() {
                    return Ok(Hir::new(HirKind::Nil, syntax.span.clone()));
                }

                // Check for special forms
                if let Some(name) = items[0].as_symbol() {
                    match name {
                        "if" => self.analyze_if(&items, &syntax.span),
                        "begin" => self.analyze_begin(&items, &syntax.span),
                        "let" => self.analyze_let(&items, &syntax.span),
                        "let*" => self.analyze_let_star(&items, &syntax.span),
                        "letrec" => self.analyze_letrec(&items, &syntax.span),
                        "lambda" | "fn" => self.analyze_lambda(&items, &syntax.span),
                        "define" => self.analyze_define(&items, &syntax.span),
                        "set!" => self.analyze_set(&items, &syntax.span),
                        "while" => self.analyze_while(&items, &syntax.span),
                        "for" => self.analyze_for(&items, &syntax.span),
                        "match" => self.analyze_match(&items, &syntax.span),
                        "try" => self.analyze_try(&items, &syntax.span),
                        "throw" => self.analyze_throw(&items, &syntax.span),
                        "and" => self.analyze_and(&items, &syntax.span),
                        "or" => self.analyze_or(&items, &syntax.span),
                        "yield" => self.analyze_yield(&items, &syntax.span),
                        _ => self.analyze_call(&items, &syntax.span),
                    }
                } else {
                    self.analyze_call(&items, &syntax.span)
                }
            }

            // Quasiquote, unquote, etc. - not yet implemented
            SyntaxKind::Quasiquote(_) | SyntaxKind::Unquote(_) | SyntaxKind::UnquoteSplicing(_) => {
                Err("Quasiquote not yet implemented in HIR analyzer".to_string())
            }
        }
    }

    fn analyze_if(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() < 3 || items.len() > 4 {
            return Err("if requires 2 or 3 arguments".to_string());
        }

        let cond = Box::new(self.analyze_expr(&items[1])?);
        let then_ = Box::new(self.analyze_expr(&items[2])?);
        let else_ = if items.len() == 4 {
            Box::new(self.analyze_expr(&items[3])?)
        } else {
            Box::new(Hir::new(HirKind::Nil, span.clone()))
        };

        Ok(Hir::new(HirKind::If { cond, then_, else_ }, span.clone()))
    }

    fn analyze_begin(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() < 2 {
            return Err("begin requires at least 1 argument".to_string());
        }

        let exprs: Result<Vec<_>, _> = items[1..].iter().map(|s| self.analyze_expr(s)).collect();
        Ok(Hir::new(HirKind::Begin(exprs?), span.clone()))
    }

    fn analyze_let(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() < 2 {
            return Err("let requires at least 1 argument".to_string());
        }

        // Parse bindings
        let bindings_syntax = items[1].as_list().ok_or("let bindings must be a list")?;

        self.push_scope(false);

        let mut bindings = Vec::new();
        for binding_item in bindings_syntax {
            let binding_pair = binding_item.as_list().ok_or("let binding must be a list")?;
            if binding_pair.len() != 2 {
                return Err("let binding must have exactly 2 elements".to_string());
            }

            let name = binding_pair[0]
                .as_symbol()
                .ok_or("let binding name must be a symbol")?
                .to_string();
            let value = self.analyze_expr(&binding_pair[1])?;
            let id = self.bind(name, binding_pair[0].span.clone());
            bindings.push((id, value));
        }

        // Analyze body
        let body_exprs: Result<Vec<_>, _> =
            items[2..].iter().map(|s| self.analyze_expr(s)).collect();
        let body = match body_exprs? {
            exprs if exprs.is_empty() => Box::new(Hir::new(HirKind::Nil, span.clone())),
            exprs if exprs.len() == 1 => Box::new(exprs.into_iter().next().unwrap()),
            exprs => Box::new(Hir::new(HirKind::Begin(exprs), span.clone())),
        };

        self.pop_scope();

        Ok(Hir::new(HirKind::Let { bindings, body }, span.clone()))
    }

    fn analyze_let_star(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() < 2 {
            return Err("let* requires at least 1 argument".to_string());
        }

        // Parse bindings
        let bindings_syntax = items[1].as_list().ok_or("let* bindings must be a list")?;

        self.push_scope(false);

        let mut bindings = Vec::new();
        for binding_item in bindings_syntax {
            let binding_pair = binding_item
                .as_list()
                .ok_or("let* binding must be a list")?;
            if binding_pair.len() != 2 {
                return Err("let* binding must have exactly 2 elements".to_string());
            }

            let name = binding_pair[0]
                .as_symbol()
                .ok_or("let* binding name must be a symbol")?
                .to_string();
            let value = self.analyze_expr(&binding_pair[1])?;
            let id = self.bind(name, binding_pair[0].span.clone());
            bindings.push((id, value));
        }

        // Analyze body
        let body_exprs: Result<Vec<_>, _> =
            items[2..].iter().map(|s| self.analyze_expr(s)).collect();
        let body = match body_exprs? {
            exprs if exprs.is_empty() => Box::new(Hir::new(HirKind::Nil, span.clone())),
            exprs if exprs.len() == 1 => Box::new(exprs.into_iter().next().unwrap()),
            exprs => Box::new(Hir::new(HirKind::Begin(exprs), span.clone())),
        };

        self.pop_scope();

        Ok(Hir::new(HirKind::Let { bindings, body }, span.clone()))
    }

    fn analyze_letrec(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() < 2 {
            return Err("letrec requires at least 1 argument".to_string());
        }

        // Parse bindings
        let bindings_syntax = items[1].as_list().ok_or("letrec bindings must be a list")?;

        self.push_scope(false);

        // First pass: allocate all bindings
        let mut binding_ids = Vec::new();
        for binding_item in bindings_syntax {
            let binding_pair = binding_item
                .as_list()
                .ok_or("letrec binding must be a list")?;
            if binding_pair.len() != 2 {
                return Err("letrec binding must have exactly 2 elements".to_string());
            }

            let name = binding_pair[0]
                .as_symbol()
                .ok_or("letrec binding name must be a symbol")?
                .to_string();
            let id = self.bind(name, binding_pair[0].span.clone());
            binding_ids.push(id);
        }

        // Second pass: analyze values
        let mut bindings = Vec::new();
        for (i, binding_item) in bindings_syntax.iter().enumerate() {
            let binding_pair = binding_item.as_list().unwrap();
            let value = self.analyze_expr(&binding_pair[1])?;
            bindings.push((binding_ids[i], value));
        }

        // Analyze body
        let body_exprs: Result<Vec<_>, _> =
            items[2..].iter().map(|s| self.analyze_expr(s)).collect();
        let body = match body_exprs? {
            exprs if exprs.is_empty() => Box::new(Hir::new(HirKind::Nil, span.clone())),
            exprs if exprs.len() == 1 => Box::new(exprs.into_iter().next().unwrap()),
            exprs => Box::new(Hir::new(HirKind::Begin(exprs), span.clone())),
        };

        self.pop_scope();

        Ok(Hir::new(HirKind::Let { bindings, body }, span.clone()))
    }

    fn analyze_lambda(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() < 3 {
            return Err("lambda requires at least 2 arguments".to_string());
        }

        let params_syntax = items[1]
            .as_list()
            .ok_or("lambda parameters must be a list")?;

        self.push_scope(true);

        let mut params = Vec::new();
        for param in params_syntax {
            let name = param
                .as_symbol()
                .ok_or("lambda parameter must be a symbol")?
                .to_string();
            let id = self.bind(name, param.span.clone());
            params.push(id);
        }

        // Analyze body
        let body_exprs: Result<Vec<_>, _> =
            items[2..].iter().map(|s| self.analyze_expr(s)).collect();
        let body = match body_exprs? {
            exprs if exprs.is_empty() => Box::new(Hir::new(HirKind::Nil, span.clone())),
            exprs if exprs.len() == 1 => Box::new(exprs.into_iter().next().unwrap()),
            exprs => Box::new(Hir::new(HirKind::Begin(exprs), span.clone())),
        };

        let scope = self.pop_scope();
        let num_locals = scope.bindings.len();

        Ok(Hir::new(
            HirKind::Lambda {
                params,
                body,
                captures: vec![], // TODO: capture analysis
                num_locals,
            },
            span.clone(),
        ))
    }

    fn analyze_define(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() != 3 {
            return Err("define requires exactly 2 arguments".to_string());
        }

        let name = items[1]
            .as_symbol()
            .ok_or("define name must be a symbol")?
            .to_string();
        let name_id = self.symbols.intern(&name);

        let value = Box::new(self.analyze_expr(&items[2])?);

        Ok(Hir::new(
            HirKind::Define {
                name: name_id,
                value,
            },
            span.clone(),
        ))
    }

    fn analyze_set(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() != 3 {
            return Err("set! requires exactly 2 arguments".to_string());
        }

        let target_name = items[1].as_symbol().ok_or("set! target must be a symbol")?;

        let target = if let Some(var_ref) = self.lookup(target_name) {
            var_ref
        } else {
            let sym_id = self.symbols.intern(target_name);
            VarRef::Global(sym_id)
        };

        // Mark binding as mutable
        if let VarRef::Local(id) = target {
            if let Some(info) = self.bindings.get_mut(&id) {
                info.mutable = true;
            }
        }

        let value = Box::new(self.analyze_expr(&items[2])?);

        Ok(Hir::new(HirKind::Set { target, value }, span.clone()))
    }

    fn analyze_while(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() != 3 {
            return Err("while requires exactly 2 arguments".to_string());
        }

        let cond = Box::new(self.analyze_expr(&items[1])?);
        let body = Box::new(self.analyze_expr(&items[2])?);

        Ok(Hir::new(HirKind::While { cond, body }, span.clone()))
    }

    fn analyze_for(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() != 4 {
            return Err("for requires exactly 3 arguments".to_string());
        }

        let var_name = items[1]
            .as_symbol()
            .ok_or("for variable must be a symbol")?
            .to_string();

        self.push_scope(false);
        let var = self.bind(var_name, items[1].span.clone());

        let iter = Box::new(self.analyze_expr(&items[2])?);
        let body = Box::new(self.analyze_expr(&items[3])?);

        self.pop_scope();

        Ok(Hir::new(HirKind::For { var, iter, body }, span.clone()))
    }

    fn analyze_match(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() < 3 || items.len() % 2 == 0 {
            return Err("match requires an odd number of arguments (at least 3)".to_string());
        }

        let scrutinee = Box::new(self.analyze_expr(&items[1])?);

        let mut arms = Vec::new();
        for i in (2..items.len()).step_by(2) {
            let pattern = self.analyze_pattern(&items[i])?;
            let expr = self.analyze_expr(&items[i + 1])?;
            arms.push((pattern, expr));
        }

        Ok(Hir::new(HirKind::Match { scrutinee, arms }, span.clone()))
    }

    fn analyze_pattern(&mut self, syntax: &Syntax) -> Result<HirPattern, String> {
        match &syntax.kind {
            SyntaxKind::Nil => Ok(HirPattern::Nil),
            SyntaxKind::Bool(b) => Ok(HirPattern::Bool(*b)),
            SyntaxKind::Int(i) => Ok(HirPattern::Int(*i)),
            SyntaxKind::String(s) => Ok(HirPattern::String(s.clone())),
            SyntaxKind::Symbol(name) => {
                if name == "_" {
                    Ok(HirPattern::Wildcard)
                } else {
                    let id = self.bind(name.clone(), syntax.span.clone());
                    Ok(HirPattern::Var(id))
                }
            }
            SyntaxKind::Keyword(k) => {
                let sym_id = self.symbols.intern(k);
                Ok(HirPattern::Symbol(sym_id))
            }
            SyntaxKind::List(items) => {
                if items.is_empty() {
                    Ok(HirPattern::List(vec![]))
                } else if items.len() == 2 && items[0].is_symbol(".") {
                    // Cons pattern: (. head tail)
                    let head = Box::new(self.analyze_pattern(&items[1])?);
                    let tail = Box::new(HirPattern::Wildcard);
                    Ok(HirPattern::Cons { head, tail })
                } else {
                    let patterns: Result<Vec<_>, _> =
                        items.iter().map(|s| self.analyze_pattern(s)).collect();
                    Ok(HirPattern::List(patterns?))
                }
            }
            _ => Err("Invalid pattern".to_string()),
        }
    }

    fn analyze_try(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() < 2 {
            return Err("try requires at least 1 argument".to_string());
        }

        let body = Box::new(self.analyze_expr(&items[1])?);

        let mut catch = None;
        let mut finally = None;

        let mut i = 2;
        while i < items.len() {
            if let Some(keyword) = items[i].as_symbol() {
                match keyword {
                    "catch" => {
                        if i + 2 >= items.len() {
                            return Err("catch requires a variable and body".to_string());
                        }
                        let var_name = items[i + 1]
                            .as_symbol()
                            .ok_or("catch variable must be a symbol")?
                            .to_string();
                        self.push_scope(false);
                        let var = self.bind(var_name, items[i + 1].span.clone());
                        let catch_body = Box::new(self.analyze_expr(&items[i + 2])?);
                        self.pop_scope();
                        catch = Some((var, catch_body));
                        i += 3;
                    }
                    "finally" => {
                        if i + 1 >= items.len() {
                            return Err("finally requires a body".to_string());
                        }
                        finally = Some(Box::new(self.analyze_expr(&items[i + 1])?));
                        i += 2;
                    }
                    _ => {
                        return Err(format!("Unknown try clause: {}", keyword));
                    }
                }
            } else {
                return Err("try clause must be a keyword".to_string());
            }
        }

        Ok(Hir::new(
            HirKind::Try {
                body,
                catch,
                finally,
            },
            span.clone(),
        ))
    }

    fn analyze_throw(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() != 2 {
            return Err("throw requires exactly 1 argument".to_string());
        }

        let value = Box::new(self.analyze_expr(&items[1])?);
        Ok(Hir::new(HirKind::Throw(value), span.clone()))
    }

    fn analyze_and(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() < 2 {
            return Ok(Hir::new(HirKind::Bool(true), span.clone()));
        }

        let exprs: Result<Vec<_>, _> = items[1..].iter().map(|s| self.analyze_expr(s)).collect();
        Ok(Hir::new(HirKind::And(exprs?), span.clone()))
    }

    fn analyze_or(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() < 2 {
            return Ok(Hir::new(HirKind::Bool(false), span.clone()));
        }

        let exprs: Result<Vec<_>, _> = items[1..].iter().map(|s| self.analyze_expr(s)).collect();
        Ok(Hir::new(HirKind::Or(exprs?), span.clone()))
    }

    fn analyze_yield(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        if items.len() != 2 {
            return Err("yield requires exactly 1 argument".to_string());
        }

        let value = Box::new(self.analyze_expr(&items[1])?);
        Ok(Hir::new(HirKind::Yield(value), span.clone()))
    }

    fn analyze_call(&mut self, items: &[Syntax], span: &Span) -> Result<Hir, String> {
        let func = Box::new(self.analyze_expr(&items[0])?);
        let args: Result<Vec<_>, _> = items[1..].iter().map(|s| self.analyze_expr(s)).collect();

        Ok(Hir::new(
            HirKind::Call {
                func,
                args: args?,
                tail: false,
            },
            span.clone(),
        ))
    }
}

/// Public entry point for analyzing Syntax to HIR
pub fn analyze(syntax: &Syntax, symbols: &mut SymbolTable) -> Result<Hir, String> {
    let mut analyzer = Analyzer::new(symbols);
    analyzer.analyze(syntax)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_span() -> Span {
        Span::new(0, 5, 1, 1)
    }

    fn make_symbol(name: &str) -> Syntax {
        Syntax::new(SyntaxKind::Symbol(name.to_string()), make_span())
    }

    fn make_int(i: i64) -> Syntax {
        Syntax::new(SyntaxKind::Int(i), make_span())
    }

    fn make_list(items: Vec<Syntax>) -> Syntax {
        Syntax::new(SyntaxKind::List(items), make_span())
    }

    #[test]
    fn test_analyze_literal_int() {
        let mut symbols = SymbolTable::new();
        let syntax = make_int(42);
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::Int(42));
    }

    #[test]
    fn test_analyze_literal_bool() {
        let mut symbols = SymbolTable::new();
        let syntax = Syntax::new(SyntaxKind::Bool(true), make_span());
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::Bool(true));
    }

    #[test]
    fn test_analyze_literal_string() {
        let mut symbols = SymbolTable::new();
        let syntax = Syntax::new(SyntaxKind::String("hello".to_string()), make_span());
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::String(ref s) if s == "hello");
    }

    #[test]
    fn test_analyze_global_var() {
        let mut symbols = SymbolTable::new();
        let syntax = make_symbol("+");
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::Var(VarRef::Global(_)));
    }

    #[test]
    fn test_analyze_if() {
        let mut symbols = SymbolTable::new();
        let syntax = make_list(vec![
            make_symbol("if"),
            Syntax::new(SyntaxKind::Bool(true), make_span()),
            make_int(1),
            make_int(2),
        ]);
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::If { .. });
    }

    #[test]
    fn test_analyze_begin() {
        let mut symbols = SymbolTable::new();
        let syntax = make_list(vec![make_symbol("begin"), make_int(1), make_int(2)]);
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::Begin(ref v) if v.len() == 2);
    }

    #[test]
    fn test_analyze_let() {
        let mut symbols = SymbolTable::new();
        let syntax = make_list(vec![
            make_symbol("let"),
            make_list(vec![make_list(vec![make_symbol("x"), make_int(42)])]),
            make_symbol("x"),
        ]);
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::Let { .. });
    }

    #[test]
    fn test_analyze_lambda() {
        let mut symbols = SymbolTable::new();
        let syntax = make_list(vec![
            make_symbol("lambda"),
            make_list(vec![make_symbol("x")]),
            make_symbol("x"),
        ]);
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::Lambda { .. });
    }

    #[test]
    fn test_analyze_define() {
        let mut symbols = SymbolTable::new();
        let syntax = make_list(vec![
            make_symbol("define"),
            make_symbol("foo"),
            make_int(42),
        ]);
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::Define { .. });
    }

    #[test]
    fn test_analyze_set() {
        let mut symbols = SymbolTable::new();
        let syntax = make_list(vec![make_symbol("set!"), make_symbol("x"), make_int(42)]);
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::Set { .. });
    }

    #[test]
    fn test_analyze_call() {
        let mut symbols = SymbolTable::new();
        let syntax = make_list(vec![make_symbol("+"), make_int(1), make_int(2)]);
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::Call { .. });
    }

    #[test]
    fn test_analyze_and() {
        let mut symbols = SymbolTable::new();
        let syntax = make_list(vec![
            make_symbol("and"),
            Syntax::new(SyntaxKind::Bool(true), make_span()),
            Syntax::new(SyntaxKind::Bool(false), make_span()),
        ]);
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::And(_));
    }

    #[test]
    fn test_analyze_or() {
        let mut symbols = SymbolTable::new();
        let syntax = make_list(vec![
            make_symbol("or"),
            Syntax::new(SyntaxKind::Bool(true), make_span()),
            Syntax::new(SyntaxKind::Bool(false), make_span()),
        ]);
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::Or(_));
    }

    #[test]
    fn test_analyze_vector() {
        let mut symbols = SymbolTable::new();
        let syntax = Syntax::new(
            SyntaxKind::Vector(vec![make_int(1), make_int(2)]),
            make_span(),
        );
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::Vector(ref v) if v.len() == 2);
    }

    #[test]
    fn test_analyze_quote() {
        let mut symbols = SymbolTable::new();
        let syntax = Syntax::new(SyntaxKind::Quote(Box::new(make_symbol("x"))), make_span());
        let hir = analyze(&syntax, &mut symbols).unwrap();
        matches!(hir.kind, HirKind::Quote(_));
    }
}
