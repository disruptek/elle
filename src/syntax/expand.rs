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

    /// Create a symbol syntax node
    fn make_symbol(&self, name: &str, span: Span) -> Syntax {
        Syntax::new(SyntaxKind::Symbol(name.to_string()), span)
    }

    /// Create a list syntax node
    fn make_list(&self, items: Vec<Syntax>, span: Span) -> Syntax {
        Syntax::new(SyntaxKind::List(items), span)
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
                // Convert quasiquote to code that builds the structure
                self.quasiquote_to_code(inner, 1, &syntax.span)
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

    /// Convert quasiquote to code that constructs the value at runtime
    /// depth tracks nesting level for nested quasiquotes
    fn quasiquote_to_code(
        &mut self,
        syntax: &Syntax,
        depth: usize,
        span: &Span,
    ) -> Result<Syntax, String> {
        match &syntax.kind {
            // Unquote at depth 1 - evaluate the expression
            SyntaxKind::Unquote(inner) if depth == 1 => self.expand((**inner).clone()),

            // Nested unquote - decrease depth
            SyntaxKind::Unquote(inner) if depth > 1 => {
                let expanded = self.quasiquote_to_code(inner, depth - 1, span)?;
                // Wrap in (list (quote unquote) expanded)
                Ok(self.make_list(
                    vec![
                        self.make_symbol("list", span.clone()),
                        self.make_list(
                            vec![
                                self.make_symbol("quote", span.clone()),
                                self.make_symbol("unquote", span.clone()),
                            ],
                            span.clone(),
                        ),
                        expanded,
                    ],
                    span.clone(),
                ))
            }

            // Nested quasiquote - increase depth
            SyntaxKind::Quasiquote(inner) => {
                let expanded = self.quasiquote_to_code(inner, depth + 1, span)?;
                Ok(self.make_list(
                    vec![
                        self.make_symbol("list", span.clone()),
                        self.make_list(
                            vec![
                                self.make_symbol("quote", span.clone()),
                                self.make_symbol("quasiquote", span.clone()),
                            ],
                            span.clone(),
                        ),
                        expanded,
                    ],
                    span.clone(),
                ))
            }

            // List - process elements, handling unquote-splicing
            SyntaxKind::List(items) => self.quasiquote_list_to_code(items, depth, span),

            // Everything else gets quoted
            _ => Ok(self.make_list(
                vec![self.make_symbol("quote", span.clone()), syntax.clone()],
                span.clone(),
            )),
        }
    }

    /// Convert a quasiquoted list to code
    fn quasiquote_list_to_code(
        &mut self,
        items: &[Syntax],
        depth: usize,
        span: &Span,
    ) -> Result<Syntax, String> {
        if items.is_empty() {
            return Ok(self.make_list(
                vec![
                    self.make_symbol("quote", span.clone()),
                    self.make_list(vec![], span.clone()),
                ],
                span.clone(),
            ));
        }

        // Check if any element is unquote-splicing
        let has_splice = items
            .iter()
            .any(|item| matches!(item.kind, SyntaxKind::UnquoteSplicing(_)));

        if has_splice {
            // Need to use append for splicing
            let mut segments = Vec::new();
            let mut current_segment = Vec::new();

            for item in items {
                if let SyntaxKind::UnquoteSplicing(inner) = &item.kind {
                    // Flush current segment
                    if !current_segment.is_empty() {
                        let mut list_call = vec![self.make_symbol("list", span.clone())];
                        list_call.append(&mut current_segment);
                        segments.push(self.make_list(list_call, span.clone()));
                    }
                    // Add spliced expression
                    if depth == 1 {
                        segments.push(self.expand((**inner).clone())?);
                    } else {
                        segments.push(self.quasiquote_to_code(inner, depth - 1, span)?);
                    }
                } else {
                    current_segment.push(self.quasiquote_to_code(item, depth, span)?);
                }
            }

            // Flush remaining segment
            if !current_segment.is_empty() {
                let mut list_call = vec![self.make_symbol("list", span.clone())];
                list_call.extend(current_segment);
                segments.push(self.make_list(list_call, span.clone()));
            }

            // Build (append seg1 seg2 ...)
            let mut append_call = vec![self.make_symbol("append", span.clone())];
            append_call.extend(segments);
            Ok(self.make_list(append_call, span.clone()))
        } else {
            // Simple case - just use list
            let mut list_call = vec![self.make_symbol("list", span.clone())];
            for item in items {
                list_call.push(self.quasiquote_to_code(item, depth, span)?);
            }
            Ok(self.make_list(list_call, span.clone()))
        }
    }
}

impl Default for Expander {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quasiquote_simple_list() {
        let mut expander = Expander::new();
        let span = Span::new(0, 10, 1, 1);

        // `(a b c)
        let items = vec![
            Syntax::new(SyntaxKind::Symbol("a".to_string()), span.clone()),
            Syntax::new(SyntaxKind::Symbol("b".to_string()), span.clone()),
            Syntax::new(SyntaxKind::Symbol("c".to_string()), span.clone()),
        ];
        let syntax = Syntax::new(
            SyntaxKind::Quasiquote(Box::new(Syntax::new(SyntaxKind::List(items), span.clone()))),
            span.clone(),
        );

        let result = expander.expand(syntax).unwrap();
        // Should expand to (list (quote a) (quote b) (quote c))
        let result_str = result.to_string();
        assert!(
            result_str.contains("list"),
            "Result should contain 'list': {}",
            result_str
        );
        assert!(
            result_str.contains("quote"),
            "Result should contain 'quote': {}",
            result_str
        );
    }

    #[test]
    fn test_quasiquote_with_unquote() {
        let mut expander = Expander::new();
        let span = Span::new(0, 10, 1, 1);

        // `(a ,x b)
        let items = vec![
            Syntax::new(SyntaxKind::Symbol("a".to_string()), span.clone()),
            Syntax::new(
                SyntaxKind::Unquote(Box::new(Syntax::new(
                    SyntaxKind::Symbol("x".to_string()),
                    span.clone(),
                ))),
                span.clone(),
            ),
            Syntax::new(SyntaxKind::Symbol("b".to_string()), span.clone()),
        ];
        let syntax = Syntax::new(
            SyntaxKind::Quasiquote(Box::new(Syntax::new(SyntaxKind::List(items), span.clone()))),
            span.clone(),
        );

        let result = expander.expand(syntax).unwrap();
        let result_str = result.to_string();
        assert!(
            result_str.contains("list"),
            "Result should contain 'list': {}",
            result_str
        );
        assert!(
            result_str.contains("quote"),
            "Result should contain 'quote': {}",
            result_str
        );
        assert!(
            result_str.contains("x"),
            "Result should contain 'x': {}",
            result_str
        );
    }

    #[test]
    fn test_quasiquote_with_splicing() {
        let mut expander = Expander::new();
        let span = Span::new(0, 10, 1, 1);

        // `(a ,@xs b)
        let items = vec![
            Syntax::new(SyntaxKind::Symbol("a".to_string()), span.clone()),
            Syntax::new(
                SyntaxKind::UnquoteSplicing(Box::new(Syntax::new(
                    SyntaxKind::Symbol("xs".to_string()),
                    span.clone(),
                ))),
                span.clone(),
            ),
            Syntax::new(SyntaxKind::Symbol("b".to_string()), span.clone()),
        ];
        let syntax = Syntax::new(
            SyntaxKind::Quasiquote(Box::new(Syntax::new(SyntaxKind::List(items), span.clone()))),
            span.clone(),
        );

        let result = expander.expand(syntax).unwrap();
        let result_str = result.to_string();
        assert!(
            result_str.contains("append"),
            "Result should contain 'append': {}",
            result_str
        );
        assert!(
            result_str.contains("list"),
            "Result should contain 'list': {}",
            result_str
        );
        assert!(
            result_str.contains("xs"),
            "Result should contain 'xs': {}",
            result_str
        );
    }

    #[test]
    fn test_quasiquote_non_list() {
        let mut expander = Expander::new();
        let span = Span::new(0, 5, 1, 1);

        // `x
        let syntax = Syntax::new(
            SyntaxKind::Quasiquote(Box::new(Syntax::new(
                SyntaxKind::Symbol("x".to_string()),
                span.clone(),
            ))),
            span.clone(),
        );

        let result = expander.expand(syntax).unwrap();
        let result_str = result.to_string();
        // Should expand to (quote x)
        assert!(
            result_str.contains("quote"),
            "Result should contain 'quote': {}",
            result_str
        );
        assert!(
            result_str.contains("x"),
            "Result should contain 'x': {}",
            result_str
        );
    }
}
