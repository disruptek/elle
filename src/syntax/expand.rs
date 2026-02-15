//! Hygienic macro expansion

use super::{ScopeId, Span, Syntax, SyntaxKind};
use std::collections::HashMap;

/// Macro definition stored as Syntax
#[derive(Debug, Clone)]
pub struct MacroDef {
    pub name: String,
    pub params: Vec<String>,
    pub template: Syntax,
    pub definition_scope: ScopeId,
}

/// Hygienic macro expander
pub struct Expander {
    macros: HashMap<String, MacroDef>,
    next_scope_id: u32,
}

impl Expander {
    pub fn new() -> Self {
        Expander {
            macros: HashMap::new(),
            next_scope_id: 1, // 0 is reserved for top-level
        }
    }

    /// Register a macro definition
    pub fn define_macro(&mut self, def: MacroDef) {
        self.macros.insert(def.name.clone(), def);
    }

    /// Generate a fresh scope ID
    pub fn fresh_scope(&mut self) -> ScopeId {
        let id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;
        id
    }

    /// Expand all macros in a syntax tree
    pub fn expand(&mut self, syntax: Syntax) -> Result<Syntax, String> {
        match &syntax.kind {
            SyntaxKind::List(items) if !items.is_empty() => {
                // Check if first element is a symbol naming a macro
                if let Some(name) = items[0].as_symbol() {
                    if let Some(macro_def) = self.macros.get(name).cloned() {
                        return self.expand_macro_call(&macro_def, &items[1..], &syntax);
                    }
                }
                // Not a macro call - expand children recursively
                self.expand_list(items, syntax.span, syntax.scopes)
            }
            SyntaxKind::Vector(items) => self.expand_vector(items, syntax.span, syntax.scopes),
            SyntaxKind::Quote(_) => {
                // Don't expand inside quote
                Ok(syntax)
            }
            SyntaxKind::Quasiquote(inner) => {
                // Expand unquotes inside quasiquote
                let expanded = self.expand_quasiquote(inner)?;
                Ok(Syntax::with_scopes(
                    SyntaxKind::Quasiquote(Box::new(expanded)),
                    syntax.span,
                    syntax.scopes,
                ))
            }
            _ => Ok(syntax),
        }
    }

    fn expand_macro_call(
        &mut self,
        macro_def: &MacroDef,
        args: &[Syntax],
        _call_site: &Syntax,
    ) -> Result<Syntax, String> {
        // Check arity
        if args.len() != macro_def.params.len() {
            return Err(format!(
                "Macro '{}' expects {} arguments, got {}",
                macro_def.name,
                macro_def.params.len(),
                args.len()
            ));
        }

        // Generate fresh scope for this macro expansion
        let intro_scope = self.fresh_scope();

        // Substitute parameters with arguments in template
        let substituted = self.substitute(&macro_def.template, &macro_def.params, args);

        // Add intro_scope to all identifiers introduced by the macro
        let hygienized = self.add_scope_recursive(substituted, intro_scope);

        // Recursively expand the result
        self.expand(hygienized)
    }

    fn substitute(&self, template: &Syntax, params: &[String], args: &[Syntax]) -> Syntax {
        match &template.kind {
            SyntaxKind::Symbol(name) => {
                // If this symbol is a parameter, substitute with argument
                if let Some(idx) = params.iter().position(|p| p == name) {
                    args[idx].clone()
                } else {
                    template.clone()
                }
            }
            SyntaxKind::List(items) => {
                let new_items: Vec<Syntax> = items
                    .iter()
                    .map(|item| self.substitute(item, params, args))
                    .collect();
                Syntax::with_scopes(
                    SyntaxKind::List(new_items),
                    template.span.clone(),
                    template.scopes.clone(),
                )
            }
            SyntaxKind::Vector(items) => {
                let new_items: Vec<Syntax> = items
                    .iter()
                    .map(|item| self.substitute(item, params, args))
                    .collect();
                Syntax::with_scopes(
                    SyntaxKind::Vector(new_items),
                    template.span.clone(),
                    template.scopes.clone(),
                )
            }
            SyntaxKind::Quote(_) => {
                // Don't substitute inside quote
                template.clone()
            }
            SyntaxKind::Quasiquote(inner) => {
                let new_inner = self.substitute_quasiquote(inner, params, args);
                Syntax::with_scopes(
                    SyntaxKind::Quasiquote(Box::new(new_inner)),
                    template.span.clone(),
                    template.scopes.clone(),
                )
            }
            // Handle Unquote directly in templates (templates are implicitly quasiquoted)
            SyntaxKind::Unquote(inner) => {
                // Substitute inside the unquote and unwrap
                self.substitute(inner, params, args)
            }
            SyntaxKind::UnquoteSplicing(inner) => {
                // Substitute inside - splicing handled elsewhere
                let substituted = self.substitute(inner, params, args);
                Syntax::with_scopes(
                    SyntaxKind::UnquoteSplicing(Box::new(substituted)),
                    template.span.clone(),
                    template.scopes.clone(),
                )
            }
            _ => template.clone(),
        }
    }

    fn substitute_quasiquote(
        &self,
        template: &Syntax,
        params: &[String],
        args: &[Syntax],
    ) -> Syntax {
        match &template.kind {
            SyntaxKind::Unquote(inner) => {
                // Inside unquote, do substitute
                let substituted = self.substitute(inner, params, args);
                Syntax::with_scopes(
                    SyntaxKind::Unquote(Box::new(substituted)),
                    template.span.clone(),
                    template.scopes.clone(),
                )
            }
            SyntaxKind::UnquoteSplicing(inner) => {
                let substituted = self.substitute(inner, params, args);
                Syntax::with_scopes(
                    SyntaxKind::UnquoteSplicing(Box::new(substituted)),
                    template.span.clone(),
                    template.scopes.clone(),
                )
            }
            SyntaxKind::List(items) => {
                let new_items: Vec<Syntax> = items
                    .iter()
                    .map(|item| self.substitute_quasiquote(item, params, args))
                    .collect();
                Syntax::with_scopes(
                    SyntaxKind::List(new_items),
                    template.span.clone(),
                    template.scopes.clone(),
                )
            }
            _ => template.clone(),
        }
    }

    fn add_scope_recursive(&self, mut syntax: Syntax, scope: ScopeId) -> Syntax {
        // Add scope to this node
        syntax.add_scope(scope);

        // Recurse into children
        syntax.kind = match syntax.kind {
            SyntaxKind::List(items) => SyntaxKind::List(
                items
                    .into_iter()
                    .map(|item| self.add_scope_recursive(item, scope))
                    .collect(),
            ),
            SyntaxKind::Vector(items) => SyntaxKind::Vector(
                items
                    .into_iter()
                    .map(|item| self.add_scope_recursive(item, scope))
                    .collect(),
            ),
            SyntaxKind::Quote(inner) => {
                // Don't add scope inside quote - it's literal data
                SyntaxKind::Quote(inner)
            }
            SyntaxKind::Quasiquote(inner) => {
                SyntaxKind::Quasiquote(Box::new(self.add_scope_recursive(*inner, scope)))
            }
            SyntaxKind::Unquote(inner) => {
                SyntaxKind::Unquote(Box::new(self.add_scope_recursive(*inner, scope)))
            }
            SyntaxKind::UnquoteSplicing(inner) => {
                SyntaxKind::UnquoteSplicing(Box::new(self.add_scope_recursive(*inner, scope)))
            }
            other => other,
        };

        syntax
    }

    fn expand_list(
        &mut self,
        items: &[Syntax],
        span: Span,
        scopes: Vec<ScopeId>,
    ) -> Result<Syntax, String> {
        let expanded: Result<Vec<Syntax>, String> =
            items.iter().map(|item| self.expand(item.clone())).collect();
        Ok(Syntax::with_scopes(
            SyntaxKind::List(expanded?),
            span,
            scopes,
        ))
    }

    fn expand_vector(
        &mut self,
        items: &[Syntax],
        span: Span,
        scopes: Vec<ScopeId>,
    ) -> Result<Syntax, String> {
        let expanded: Result<Vec<Syntax>, String> =
            items.iter().map(|item| self.expand(item.clone())).collect();
        Ok(Syntax::with_scopes(
            SyntaxKind::Vector(expanded?),
            span,
            scopes,
        ))
    }

    fn expand_quasiquote(&mut self, syntax: &Syntax) -> Result<Syntax, String> {
        match &syntax.kind {
            SyntaxKind::Unquote(inner) => {
                // Expand the unquoted expression
                self.expand((**inner).clone())
            }
            SyntaxKind::List(items) => {
                let mut result = Vec::new();
                for item in items {
                    match &item.kind {
                        SyntaxKind::UnquoteSplicing(inner) => {
                            // Expand and mark for splicing (actual splicing at runtime)
                            let expanded = self.expand((**inner).clone())?;
                            result.push(Syntax::with_scopes(
                                SyntaxKind::UnquoteSplicing(Box::new(expanded)),
                                item.span.clone(),
                                item.scopes.clone(),
                            ));
                        }
                        _ => {
                            result.push(self.expand_quasiquote(item)?);
                        }
                    }
                }
                Ok(Syntax::with_scopes(
                    SyntaxKind::List(result),
                    syntax.span.clone(),
                    syntax.scopes.clone(),
                ))
            }
            _ => Ok(syntax.clone()),
        }
    }
}

impl Default for Expander {
    fn default() -> Self {
        Self::new()
    }
}
