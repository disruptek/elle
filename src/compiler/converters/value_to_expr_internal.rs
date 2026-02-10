use super::super::analysis::{analyze_capture_usage, analyze_free_vars};
use super::super::ast::Expr;
use super::super::macros::expand_macro;
use super::patterns::extract_pattern_variables;
use super::quasiquote::expand_quasiquote;
use super::variables::adjust_var_indices;
use crate::symbol::SymbolTable;
use crate::value::{SymbolId, Value};
use std::collections::HashSet;

/// Convert a value to an expression, tracking local variable scopes
/// The scope_stack contains local bindings (as Vec for ordering) at each nesting level
pub fn value_to_expr_with_scope(
    value: &Value,
    symbols: &mut SymbolTable,
    scope_stack: &mut Vec<Vec<SymbolId>>,
) -> Result<Expr, String> {
    match value {
        Value::Nil
        | Value::Bool(_)
        | Value::Int(_)
        | Value::Float(_)
        | Value::String(_)
        | Value::Keyword(_)
        | Value::Vector(_) => Ok(Expr::Literal(value.clone())),

        Value::Symbol(id) => {
            // Check if the symbol is a local binding by walking up the scope stack
            for (reverse_idx, scope) in scope_stack.iter().enumerate().rev() {
                if let Some(local_index) = scope.iter().position(|sym| sym == id) {
                    // Found in local scope - use Var with appropriate depth and index
                    // depth represents how many function scopes up the variable is defined:
                    // 0 = current lambda's parameters
                    // 1 = enclosing lambda's parameters
                    // etc.
                    let actual_depth = scope_stack.len() - 1 - reverse_idx;
                    return Ok(Expr::Var(*id, actual_depth, local_index));
                }
            }
            // Not found in any local scope - treat as global
            Ok(Expr::GlobalVar(*id))
        }

        Value::Cons(_) => {
            let list = value.list_to_vec()?;
            if list.is_empty() {
                return Err("Empty list in expression".to_string());
            }

            let first = &list[0];
            if let Value::Symbol(sym) = first {
                let name = symbols.name(*sym).ok_or("Unknown symbol")?;

                match name {
                    "qualified-ref" => {
                        // Handle module-qualified symbols: (qualified-ref module-name symbol-name)
                        if list.len() != 3 {
                            return Err("qualified-ref requires exactly 2 arguments".to_string());
                        }
                        let module_sym = list[1].as_symbol()?;
                        let name_sym = list[2].as_symbol()?;

                        let module_name =
                            symbols.name(module_sym).ok_or("Unknown module symbol")?;
                        let func_name = symbols.name(name_sym).ok_or("Unknown function symbol")?;

                        // Try to resolve from the specified module's exports
                        if let Some(module_def) = symbols.get_module(module_sym) {
                            // Check if the symbol is exported from the module
                            if module_def.exports.contains(&name_sym) {
                                // Return as a qualified global reference
                                // We use GlobalVar but could add a QualifiedVar variant if needed
                                Ok(Expr::GlobalVar(name_sym))
                            } else {
                                Err(format!(
                                    "Symbol '{}' not exported from module '{}'",
                                    func_name, module_name
                                ))
                            }
                        } else {
                            Err(format!("Unknown module: '{}'", module_name))
                        }
                    }

                    "quote" => {
                        if list.len() != 2 {
                            return Err("quote requires exactly 1 argument".to_string());
                        }
                        Ok(Expr::Literal(list[1].clone()))
                    }

                    "quasiquote" => {
                        if list.len() != 2 {
                            return Err("quasiquote requires exactly 1 argument".to_string());
                        }
                        // Convert the quasiquote form into a proper Expr::Quasiquote
                        // The content is processed to handle unquotes
                        let content = &list[1];
                        expand_quasiquote(content, symbols, scope_stack)
                    }

                    "unquote" | "unquote-splicing" => {
                        // Unquote outside of quasiquote is an error
                        Err(format!("{} can only be used inside quasiquote", name))
                    }

                    "if" => {
                        if list.len() < 3 || list.len() > 4 {
                            return Err("if requires 2 or 3 arguments".to_string());
                        }
                        let cond =
                            Box::new(value_to_expr_with_scope(&list[1], symbols, scope_stack)?);
                        let then =
                            Box::new(value_to_expr_with_scope(&list[2], symbols, scope_stack)?);
                        let else_ = if list.len() == 4 {
                            Box::new(value_to_expr_with_scope(&list[3], symbols, scope_stack)?)
                        } else {
                            Box::new(Expr::Literal(Value::Nil))
                        };
                        Ok(Expr::If { cond, then, else_ })
                    }

                    "begin" => {
                        let exprs: Result<Vec<_>, _> = list[1..]
                            .iter()
                            .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                            .collect();
                        Ok(Expr::Begin(exprs?))
                    }

                    "block" => {
                        let exprs: Result<Vec<_>, _> = list[1..]
                            .iter()
                            .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                            .collect();
                        Ok(Expr::Block(exprs?))
                    }

                    "lambda" => {
                        if list.len() < 3 {
                            return Err("lambda requires at least 2 arguments".to_string());
                        }

                        let params = list[1].list_to_vec()?;
                        let param_syms: Result<Vec<_>, _> =
                            params.iter().map(|p| p.as_symbol()).collect();
                        let param_syms = param_syms?;

                        // Push a new scope with the lambda parameters (as Vec for ordered indices)
                        scope_stack.push(param_syms.clone());

                        let body_exprs: Result<Vec<_>, _> = list[2..]
                            .iter()
                            .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                            .collect();

                        // Pop the lambda's scope
                        scope_stack.pop();

                        let body_exprs = body_exprs?;
                        let body = if body_exprs.len() == 1 {
                            Box::new(body_exprs[0].clone())
                        } else {
                            Box::new(Expr::Begin(body_exprs))
                        };

                        // Analyze free variables that need to be captured
                        let mut local_bindings = std::collections::HashSet::new();
                        for param in &param_syms {
                            local_bindings.insert(*param);
                        }
                        let free_vars = analyze_free_vars(&body, &local_bindings);

                        // Convert free vars to captures, resolving their scope location
                        // We need to distinguish: is this a global, or from an outer scope?
                        let mut sorted_free_vars: Vec<_> = free_vars.iter().copied().collect();
                        sorted_free_vars.sort(); // Deterministic ordering

                        let captures: Vec<_> = sorted_free_vars
                            .iter()
                            .map(|sym| {
                                // Look up in scope stack to determine if global or local
                                for (reverse_idx, scope) in scope_stack.iter().enumerate().rev() {
                                    if let Some(local_index) = scope.iter().position(|s| s == sym) {
                                        let depth = scope_stack.len() - 1 - reverse_idx;
                                        return (*sym, depth, local_index);
                                    }
                                }
                                // If not found in scope stack, it's a global variable
                                (*sym, 0, usize::MAX)
                            })
                            .collect();

                        // Dead capture elimination: filter out captures that aren't actually used in the body
                        let candidates: HashSet<SymbolId> =
                            captures.iter().map(|(sym, _, _)| *sym).collect();
                        let actually_used =
                            analyze_capture_usage(&body, &local_bindings, &candidates);
                        let captures: Vec<_> = captures
                            .into_iter()
                            .filter(|(sym, _, _)| actually_used.contains(sym))
                            .collect();

                        // Adjust variable indices in body to account for closure environment layout
                        // The closure environment is [captures..., parameters...]
                        let mut adjusted_body = body;
                        adjust_var_indices(&mut adjusted_body, &captures, &param_syms);

                        Ok(Expr::Lambda {
                            params: param_syms,
                            body: adjusted_body,
                            captures,
                        })
                    }

                    "define" => {
                        if list.len() != 3 {
                            return Err("define requires exactly 2 arguments".to_string());
                        }
                        let name = list[1].as_symbol()?;
                        let value =
                            Box::new(value_to_expr_with_scope(&list[2], symbols, scope_stack)?);
                        Ok(Expr::Define { name, value })
                    }

                    "let" => {
                        if list.len() < 2 {
                            return Err("let requires at least a binding vector".to_string());
                        }

                        let bindings_vec = list[1].list_to_vec()?;
                        let mut param_syms = Vec::new();
                        let mut binding_exprs = Vec::new();

                        for binding in bindings_vec {
                            let binding_list = binding.list_to_vec()?;
                            if binding_list.len() != 2 {
                                return Err(
                                    "Each let binding must be a [var expr] pair".to_string()
                                );
                            }
                            let var = binding_list[0].as_symbol()?;
                            param_syms.push(var);

                            let expr =
                                value_to_expr_with_scope(&binding_list[1], symbols, scope_stack)?;
                            binding_exprs.push(expr);
                        }

                        scope_stack.push(param_syms.clone());

                        let body_exprs: Result<Vec<_>, _> = list[2..]
                            .iter()
                            .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                            .collect();

                        scope_stack.pop();

                        let body_exprs = body_exprs?;
                        let body = if body_exprs.len() == 1 {
                            Box::new(body_exprs[0].clone())
                        } else if body_exprs.is_empty() {
                            Box::new(Expr::Literal(Value::Nil))
                        } else {
                            Box::new(Expr::Begin(body_exprs))
                        };

                        let mut local_bindings = std::collections::HashSet::new();
                        for param in &param_syms {
                            local_bindings.insert(*param);
                        }
                        let free_vars = analyze_free_vars(&body, &local_bindings);

                        let mut sorted_free_vars: Vec<_> = free_vars.iter().copied().collect();
                        sorted_free_vars.sort();

                        let captures: Vec<_> = sorted_free_vars
                            .iter()
                            .map(|sym| {
                                for (reverse_idx, scope) in scope_stack.iter().enumerate().rev() {
                                    if let Some(local_index) = scope.iter().position(|s| s == sym) {
                                        let depth = scope_stack.len() - 1 - reverse_idx;
                                        return (*sym, depth, local_index);
                                    }
                                }
                                (*sym, 0, usize::MAX)
                            })
                            .collect();

                        let candidates: HashSet<SymbolId> =
                            captures.iter().map(|(sym, _, _)| *sym).collect();
                        let actually_used =
                            analyze_capture_usage(&body, &local_bindings, &candidates);
                        let captures: Vec<_> = captures
                            .into_iter()
                            .filter(|(sym, _, _)| actually_used.contains(sym))
                            .collect();

                        let mut adjusted_body = body;
                        adjust_var_indices(&mut adjusted_body, &captures, &param_syms);

                        let lambda = Expr::Lambda {
                            params: param_syms,
                            body: adjusted_body,
                            captures,
                        };

                        Ok(Expr::Call {
                            func: Box::new(lambda),
                            args: binding_exprs,
                            tail: false,
                        })
                    }

                    "let*" => {
                        if list.len() < 2 {
                            return Err("let* requires at least a binding vector".to_string());
                        }

                        let bindings_vec = list[1].list_to_vec()?;

                        if bindings_vec.is_empty() {
                            let body_exprs: Result<Vec<_>, _> = list[2..]
                                .iter()
                                .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                                .collect();
                            let body_exprs = body_exprs?;
                            if body_exprs.is_empty() {
                                return Ok(Expr::Literal(Value::Nil));
                            } else if body_exprs.len() == 1 {
                                return Ok(body_exprs[0].clone());
                            } else {
                                return Ok(Expr::Begin(body_exprs));
                            }
                        }

                        let mut param_syms = Vec::new();
                        let mut binding_exprs = Vec::new();
                        scope_stack.push(Vec::new());

                        for binding in &bindings_vec {
                            let binding_list = binding.list_to_vec()?;
                            if binding_list.len() != 2 {
                                return Err(
                                    "Each let* binding must be a [var expr] pair".to_string()
                                );
                            }
                            let var = binding_list[0].as_symbol()?;
                            param_syms.push(var);

                            let expr =
                                value_to_expr_with_scope(&binding_list[1], symbols, scope_stack)?;
                            binding_exprs.push(expr);

                            if let Some(current_scope) = scope_stack.last_mut() {
                                current_scope.push(var);
                            }
                        }

                        let body_exprs: Result<Vec<_>, _> = list[2..]
                            .iter()
                            .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                            .collect();

                        scope_stack.pop();

                        let body_exprs = body_exprs?;
                        let body = if body_exprs.len() == 1 {
                            Box::new(body_exprs[0].clone())
                        } else if body_exprs.is_empty() {
                            Box::new(Expr::Literal(Value::Nil))
                        } else {
                            Box::new(Expr::Begin(body_exprs))
                        };

                        let mut local_bindings = std::collections::HashSet::new();
                        for param in &param_syms {
                            local_bindings.insert(*param);
                        }
                        let free_vars = analyze_free_vars(&body, &local_bindings);

                        let mut sorted_free_vars: Vec<_> = free_vars.iter().copied().collect();
                        sorted_free_vars.sort();

                        let captures: Vec<_> = sorted_free_vars
                            .iter()
                            .map(|sym| {
                                for (reverse_idx, scope) in scope_stack.iter().enumerate().rev() {
                                    if let Some(local_index) = scope.iter().position(|s| s == sym) {
                                        let depth = scope_stack.len() - 1 - reverse_idx;
                                        return (*sym, depth, local_index);
                                    }
                                }
                                (*sym, 0, usize::MAX)
                            })
                            .collect();

                        let candidates: HashSet<SymbolId> =
                            captures.iter().map(|(sym, _, _)| *sym).collect();
                        let actually_used =
                            analyze_capture_usage(&body, &local_bindings, &candidates);
                        let captures: Vec<_> = captures
                            .into_iter()
                            .filter(|(sym, _, _)| actually_used.contains(sym))
                            .collect();

                        let mut adjusted_body = body;
                        adjust_var_indices(&mut adjusted_body, &captures, &param_syms);

                        let lambda = Expr::Lambda {
                            params: param_syms,
                            body: adjusted_body,
                            captures,
                        };

                        Ok(Expr::Call {
                            func: Box::new(lambda),
                            args: binding_exprs,
                            tail: false,
                        })
                    }

                    "letrec" => {
                        if list.len() < 2 {
                            return Err("letrec requires at least a binding vector".to_string());
                        }

                        let bindings_vec = list[1].list_to_vec()?;
                        let mut param_syms = Vec::new();
                        let mut binding_exprs = Vec::new();

                        for binding in &bindings_vec {
                            let binding_list = binding.list_to_vec()?;
                            if binding_list.len() != 2 {
                                return Err(
                                    "Each letrec binding must be a [var expr] pair".to_string()
                                );
                            }
                            param_syms.push(binding_list[0].as_symbol()?);
                        }

                        for binding in &bindings_vec {
                            let binding_list = binding.list_to_vec()?;
                            let expr =
                                value_to_expr_with_scope(&binding_list[1], symbols, scope_stack)?;
                            binding_exprs.push(expr);
                        }

                        let body_exprs: Result<Vec<_>, _> = list[2..]
                            .iter()
                            .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                            .collect();

                        let body_exprs = body_exprs?;
                        let body = if body_exprs.len() == 1 {
                            Box::new(body_exprs[0].clone())
                        } else if body_exprs.is_empty() {
                            Box::new(Expr::Literal(Value::Nil))
                        } else {
                            Box::new(Expr::Begin(body_exprs))
                        };

                        let bindings: Vec<_> = param_syms.into_iter().zip(binding_exprs).collect();

                        Ok(Expr::Letrec { bindings, body })
                    }

                    "set!" => {
                        if list.len() != 3 {
                            return Err("set! requires exactly 2 arguments".to_string());
                        }
                        let var = list[1].as_symbol()?;
                        let value =
                            Box::new(value_to_expr_with_scope(&list[2], symbols, scope_stack)?);

                        let mut depth = 0;
                        let mut index = usize::MAX;

                        for (reverse_idx, scope) in scope_stack.iter().enumerate().rev() {
                            if let Some(local_index) = scope.iter().position(|sym| sym == &var) {
                                depth = scope_stack.len() - 1 - reverse_idx;
                                index = local_index;
                                break;
                            }
                        }

                        Ok(Expr::Set {
                            var,
                            depth,
                            index,
                            value,
                        })
                    }

                    "try" => {
                        if list.len() < 2 {
                            return Err("try requires at least a body".to_string());
                        }

                        let body =
                            Box::new(value_to_expr_with_scope(&list[1], symbols, scope_stack)?);
                        let mut catch_clause = None;
                        let mut finally_clause = None;

                        for item in &list[2..] {
                            if item.is_list() {
                                let v = item.list_to_vec()?;
                                if v.is_empty() {
                                    return Err("Empty clause in try expression".to_string());
                                }
                                if let Value::Symbol(keyword) = &v[0] {
                                    let keyword_str = symbols.name(*keyword).unwrap_or("unknown");
                                    match keyword_str {
                                        "catch" => {
                                            if v.len() != 3 {
                                                return Err("catch requires exactly 2 arguments (variable and handler)".to_string());
                                            }
                                            let var = v[1].as_symbol()?;
                                            let handler = Box::new(value_to_expr_with_scope(
                                                &v[2],
                                                symbols,
                                                scope_stack,
                                            )?);
                                            catch_clause = Some((var, handler));
                                        }
                                        "finally" => {
                                            if v.len() != 2 {
                                                return Err("finally requires exactly 1 argument"
                                                    .to_string());
                                            }
                                            finally_clause =
                                                Some(Box::new(value_to_expr_with_scope(
                                                    &v[1],
                                                    symbols,
                                                    scope_stack,
                                                )?));
                                        }
                                        _ => {
                                            return Err(format!(
                                                "Unknown clause in try: {}",
                                                keyword_str
                                            ));
                                        }
                                    }
                                } else {
                                    return Err("Clause keyword must be a symbol".to_string());
                                }
                            } else {
                                return Err("Clauses in try must be lists".to_string());
                            }
                        }

                        Ok(Expr::Try {
                            body,
                            catch: catch_clause,
                            finally: finally_clause,
                        })
                    }

                    "handler-case" => {
                        if list.len() < 2 {
                            return Err("handler-case requires at least a body".to_string());
                        }

                        let body =
                            Box::new(value_to_expr_with_scope(&list[1], symbols, scope_stack)?);
                        let mut handlers = Vec::new();

                        for clause in &list[2..] {
                            if let Ok(clause_vec) = clause.list_to_vec() {
                                if clause_vec.len() != 3 {
                                    return Err(
                                        "handler-case clause requires (exception-id (var) handler)"
                                            .to_string(),
                                    );
                                }

                                let exception_id = match &clause_vec[0] {
                                    Value::Int(id) => *id as u32,
                                    Value::Symbol(sym) => {
                                        let name = symbols.name(*sym).unwrap_or("unknown");
                                        match name {
                                            "condition" => 1,
                                            "error" => 2,
                                            "type-error" => 3,
                                            "division-by-zero" => 4,
                                            "undefined-variable" => 5,
                                            "arity-error" => 6,
                                            "warning" => 7,
                                            "style-warning" => 8,
                                            _ => {
                                                return Err(format!(
                                                    "Unknown exception type: {}",
                                                    name
                                                ))
                                            }
                                        }
                                    }
                                    _ => {
                                        return Err(
                                            "Exception ID must be integer or symbol".to_string()
                                        )
                                    }
                                };

                                let var = clause_vec[1].as_symbol()?;

                                let handler_code = Box::new(value_to_expr_with_scope(
                                    &clause_vec[2],
                                    symbols,
                                    scope_stack,
                                )?);

                                handlers.push((exception_id, var, handler_code));
                            } else {
                                return Err("Handler clauses must be lists".to_string());
                            }
                        }

                        Ok(Expr::HandlerCase { body, handlers })
                    }

                    "handler-bind" => {
                        if list.len() != 3 {
                            return Err("handler-bind requires ((handlers...) body)".to_string());
                        }

                        let handlers_list = list[1].list_to_vec()?;
                        let mut handlers = Vec::new();

                        for handler_spec in handlers_list {
                            let spec_vec = handler_spec.list_to_vec()?;
                            if spec_vec.len() != 2 {
                                return Err(
                                    "Each handler binding must be (exception-id handler-fn)"
                                        .to_string(),
                                );
                            }

                            let exception_id = match &spec_vec[0] {
                                Value::Int(id) => *id as u32,
                                Value::Symbol(sym) => {
                                    let name = symbols.name(*sym).unwrap_or("unknown");
                                    match name {
                                        "condition" => 1,
                                        "error" => 2,
                                        "type-error" => 3,
                                        "division-by-zero" => 4,
                                        "undefined-variable" => 5,
                                        "arity-error" => 6,
                                        "warning" => 7,
                                        "style-warning" => 8,
                                        _ => {
                                            return Err(format!("Unknown exception type: {}", name))
                                        }
                                    }
                                }
                                _ => {
                                    return Err("Exception ID must be integer or symbol".to_string())
                                }
                            };

                            let handler_fn = Box::new(value_to_expr_with_scope(
                                &spec_vec[1],
                                symbols,
                                scope_stack,
                            )?);

                            handlers.push((exception_id, handler_fn));
                        }

                        let body =
                            Box::new(value_to_expr_with_scope(&list[2], symbols, scope_stack)?);

                        Ok(Expr::HandlerBind { handlers, body })
                    }

                    "match" => {
                        if list.len() < 2 {
                            return Err("match requires at least a value".to_string());
                        }

                        let value =
                            Box::new(value_to_expr_with_scope(&list[1], symbols, scope_stack)?);
                        let mut patterns = Vec::new();
                        let mut default = None;

                        for clause in &list[2..] {
                            if let Ok(clause_vec) = clause.list_to_vec() {
                                if clause_vec.is_empty() {
                                    return Err("Empty pattern clause".to_string());
                                }

                                if clause_vec.len() == 1 {
                                    default = Some(Box::new(value_to_expr_with_scope(
                                        &clause_vec[0],
                                        symbols,
                                        scope_stack,
                                    )?));
                                } else if clause_vec.len() == 2 {
                                    let pattern = super::super::patterns::value_to_pattern(
                                        &clause_vec[0],
                                        symbols,
                                    )?;

                                    let pattern_vars = extract_pattern_variables(&pattern);

                                    let result = if !pattern_vars.is_empty() {
                                        let mut new_scope_stack = scope_stack.clone();
                                        new_scope_stack.push(pattern_vars.clone());

                                        let body_expr = value_to_expr_with_scope(
                                            &clause_vec[1],
                                            symbols,
                                            &mut new_scope_stack,
                                        )?;

                                        Expr::Lambda {
                                            params: pattern_vars,
                                            body: Box::new(body_expr),
                                            captures: Vec::new(),
                                        }
                                    } else {
                                        value_to_expr_with_scope(
                                            &clause_vec[1],
                                            symbols,
                                            scope_stack,
                                        )?
                                    };

                                    patterns.push((pattern, result));
                                } else {
                                    return Err(
                                        "Pattern clause must have pattern and result".to_string()
                                    );
                                }
                            } else {
                                return Err("Expected pattern clause to be a list".to_string());
                            }
                        }

                        Ok(Expr::Match {
                            value,
                            patterns,
                            default,
                        })
                    }

                    "throw" => {
                        if list.len() != 2 {
                            return Err("throw requires exactly 1 argument".to_string());
                        }
                        let func = Box::new(Expr::GlobalVar(first.as_symbol()?));
                        let args = vec![value_to_expr_with_scope(&list[1], symbols, scope_stack)?];
                        Ok(Expr::Call {
                            func,
                            args,
                            tail: false,
                        })
                    }

                    "defmacro" | "define-macro" => {
                        if list.len() != 4 {
                            return Err(
                                "defmacro requires exactly 3 arguments (name, parameters, body)"
                                    .to_string(),
                            );
                        }
                        let name = list[1].as_symbol()?;
                        let params_val = &list[2];

                        let params = if params_val.is_list() {
                            let param_vec = params_val.list_to_vec()?;
                            param_vec
                                .iter()
                                .map(|v| v.as_symbol())
                                .collect::<Result<Vec<_>, _>>()?
                        } else {
                            return Err("Macro parameters must be a list".to_string());
                        };

                        let body_str = format!("{}", list[3]);

                        use crate::symbol::MacroDef;
                        symbols.define_macro(MacroDef {
                            name,
                            params: params.clone(),
                            body: body_str,
                        });

                        let body =
                            Box::new(value_to_expr_with_scope(&list[3], symbols, scope_stack)?);

                        Ok(Expr::DefMacro { name, params, body })
                    }

                    "while" => {
                        if list.len() != 3 {
                            return Err(
                                "while requires exactly 2 arguments (condition body)".to_string()
                            );
                        }
                        let cond =
                            Box::new(value_to_expr_with_scope(&list[1], symbols, scope_stack)?);
                        let body =
                            Box::new(value_to_expr_with_scope(&list[2], symbols, scope_stack)?);
                        Ok(Expr::While { cond, body })
                    }

                    "for" => {
                        if list.len() < 4 || list.len() > 5 {
                            return Err(
                                "for requires 3 or 4 arguments (var [in] iter body)".to_string()
                            );
                        }

                        let var = list[1].as_symbol()?;
                        let (iter_expr, body_expr) = if list.len() == 4 {
                            (&list[2], &list[3])
                        } else if let Value::Symbol(in_sym) = &list[2] {
                            if let Some("in") = symbols.name(*in_sym) {
                                (&list[3], &list[4])
                            } else {
                                return Err("for loop syntax: (for var iter body) or (for var in iter body)".to_string());
                            }
                        } else {
                            return Err(
                                "for loop syntax: (for var iter body) or (for var in iter body)"
                                    .to_string(),
                            );
                        };

                        let iter =
                            Box::new(value_to_expr_with_scope(iter_expr, symbols, scope_stack)?);

                        let body =
                            Box::new(value_to_expr_with_scope(body_expr, symbols, scope_stack)?);

                        Ok(Expr::For { var, iter, body })
                    }

                    "and" => {
                        if list.len() < 2 {
                            return Ok(Expr::Literal(Value::Bool(true)));
                        }
                        let exprs: Result<Vec<_>, _> = list[1..]
                            .iter()
                            .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                            .collect();
                        Ok(Expr::And(exprs?))
                    }

                    "or" => {
                        if list.len() < 2 {
                            return Ok(Expr::Literal(Value::Bool(false)));
                        }
                        let exprs: Result<Vec<_>, _> = list[1..]
                            .iter()
                            .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                            .collect();
                        Ok(Expr::Or(exprs?))
                    }

                    "xor" => {
                        if list.len() < 2 {
                            return Ok(Expr::Literal(Value::Bool(false)));
                        }

                        let func = Box::new(Expr::GlobalVar(symbols.intern("xor")));
                        let args: Result<Vec<_>, _> = list[1..]
                            .iter()
                            .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                            .collect();
                        Ok(Expr::Call {
                            func,
                            args: args?,
                            tail: false,
                        })
                    }

                    "->" => {
                        if list.len() < 2 {
                            return Err("-> requires at least a value and one form".to_string());
                        }

                        let mut result = value_to_expr_with_scope(&list[1], symbols, scope_stack)?;

                        for form in &list[2..] {
                            result = if let Ok(form_vec) = form.list_to_vec() {
                                if form_vec.is_empty() {
                                    return Err("Cannot thread through empty form".to_string());
                                }
                                let func =
                                    value_to_expr_with_scope(&form_vec[0], symbols, scope_stack)?;
                                let mut args = vec![result];
                                for arg in &form_vec[1..] {
                                    args.push(value_to_expr_with_scope(arg, symbols, scope_stack)?);
                                }
                                Expr::Call {
                                    func: Box::new(func),
                                    args,
                                    tail: false,
                                }
                            } else {
                                Expr::Call {
                                    func: Box::new(value_to_expr_with_scope(
                                        form,
                                        symbols,
                                        scope_stack,
                                    )?),
                                    args: vec![result],
                                    tail: false,
                                }
                            };
                        }

                        Ok(result)
                    }

                    "->>" => {
                        if list.len() < 2 {
                            return Err("->> requires at least a value and one form".to_string());
                        }

                        let mut result = value_to_expr_with_scope(&list[1], symbols, scope_stack)?;

                        for form in &list[2..] {
                            result = if let Ok(form_vec) = form.list_to_vec() {
                                if form_vec.is_empty() {
                                    return Err("Cannot thread through empty form".to_string());
                                }
                                let func =
                                    value_to_expr_with_scope(&form_vec[0], symbols, scope_stack)?;
                                let mut args: Vec<_> = form_vec[1..]
                                    .iter()
                                    .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                                    .collect::<Result<_, _>>()?;
                                args.push(result);
                                Expr::Call {
                                    func: Box::new(func),
                                    args,
                                    tail: false,
                                }
                            } else {
                                Expr::Call {
                                    func: Box::new(value_to_expr_with_scope(
                                        form,
                                        symbols,
                                        scope_stack,
                                    )?),
                                    args: vec![result],
                                    tail: false,
                                }
                            };
                        }

                        Ok(result)
                    }

                    "cond" => {
                        if list.len() < 2 {
                            return Err("cond requires at least one clause".to_string());
                        }

                        let mut clauses = Vec::new();
                        let mut else_body = None;

                        // Parse clauses
                        for clause in &list[1..] {
                            let clause_vec = clause.list_to_vec()?;
                            if clause_vec.is_empty() {
                                return Err("cond clause cannot be empty".to_string());
                            }

                            // Check if this is the else clause (single symbol 'else' followed by body)
                            if !clause_vec.is_empty() {
                                if let Value::Symbol(test_sym) = &clause_vec[0] {
                                    if let Some("else") = symbols.name(*test_sym) {
                                        // This is the else clause
                                        if else_body.is_some() {
                                            return Err(
                                                "cond can have at most one else clause".to_string()
                                            );
                                        }
                                        // The else clause body can be multiple expressions
                                        let body_exprs: Result<Vec<_>, _> = clause_vec[1..]
                                            .iter()
                                            .map(|v| {
                                                value_to_expr_with_scope(v, symbols, scope_stack)
                                            })
                                            .collect();
                                        let body_exprs = body_exprs?;
                                        let body = if body_exprs.is_empty() {
                                            Expr::Literal(Value::Nil)
                                        } else if body_exprs.len() == 1 {
                                            body_exprs[0].clone()
                                        } else {
                                            Expr::Begin(body_exprs)
                                        };
                                        else_body = Some(Box::new(body));
                                        continue;
                                    }
                                }
                            }

                            // Regular clause: (test body...)
                            if clause_vec.len() < 2 {
                                return Err(
                                    "cond clause must have at least a test and a body".to_string()
                                );
                            }

                            let test =
                                value_to_expr_with_scope(&clause_vec[0], symbols, scope_stack)?;

                            // The body can be multiple expressions
                            let body_exprs: Result<Vec<_>, _> = clause_vec[1..]
                                .iter()
                                .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                                .collect();
                            let body_exprs = body_exprs?;
                            let body = if body_exprs.is_empty() {
                                Expr::Literal(Value::Nil)
                            } else if body_exprs.len() == 1 {
                                body_exprs[0].clone()
                            } else {
                                Expr::Begin(body_exprs)
                            };

                            clauses.push((test, body));
                        }

                        Ok(Expr::Cond { clauses, else_body })
                    }

                    _ => {
                        // Check if it's a macro call
                        if let Value::Symbol(sym_id) = first {
                            if symbols.is_macro(*sym_id) {
                                if let Some(macro_def) = symbols.get_macro(*sym_id) {
                                    let args = list[1..].to_vec();

                                    let expanded =
                                        expand_macro(*sym_id, &macro_def, &args, symbols)?;

                                    return value_to_expr_with_scope(
                                        &expanded,
                                        symbols,
                                        scope_stack,
                                    );
                                }
                            }
                        }

                        // Regular function call
                        let func = Box::new(value_to_expr_with_scope(first, symbols, scope_stack)?);
                        let args: Result<Vec<_>, _> = list[1..]
                            .iter()
                            .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                            .collect();
                        Ok(Expr::Call {
                            func,
                            args: args?,
                            tail: false,
                        })
                    }
                }
            } else {
                // Function call with non-symbol function
                let func = Box::new(value_to_expr_with_scope(first, symbols, scope_stack)?);
                let args: Result<Vec<_>, _> = list[1..]
                    .iter()
                    .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
                    .collect();
                Ok(Expr::Call {
                    func,
                    args: args?,
                    tail: false,
                })
            }
        }

        _ => Err(format!("Cannot convert {:?} to expression", value)),
    }
}
