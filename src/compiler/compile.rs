use super::ast::Expr;
use super::bytecode::{Bytecode, Instruction};
use crate::symbol::SymbolTable;
use crate::value::{Closure, SymbolId, Value};
use std::collections::HashMap;
use std::rc::Rc;

struct Compiler {
    bytecode: Bytecode,
    #[allow(dead_code)]
    symbols: HashMap<SymbolId, usize>,
}

impl Compiler {
    fn new() -> Self {
        Compiler {
            bytecode: Bytecode::new(),
            symbols: HashMap::new(),
        }
    }

    fn compile_expr(&mut self, expr: &Expr, tail: bool) {
        match expr {
            Expr::Literal(val) => match val {
                Value::Nil => self.bytecode.emit(Instruction::Nil),
                Value::Bool(true) => self.bytecode.emit(Instruction::True),
                Value::Bool(false) => self.bytecode.emit(Instruction::False),
                _ => {
                    let idx = self.bytecode.add_constant(val.clone());
                    self.bytecode.emit(Instruction::LoadConst);
                    self.bytecode.emit_u16(idx);
                }
            },

            Expr::Var(_sym, depth, index) => {
                if *depth == 0 {
                    self.bytecode.emit(Instruction::LoadLocal);
                    self.bytecode.emit_byte(*index as u8);
                } else {
                    self.bytecode.emit(Instruction::LoadUpvalue);
                    self.bytecode.emit_byte(*depth as u8);
                    self.bytecode.emit_byte(*index as u8);
                }
            }

            Expr::GlobalVar(sym) => {
                let idx = self.bytecode.add_constant(Value::Symbol(*sym));
                self.bytecode.emit(Instruction::LoadGlobal);
                self.bytecode.emit_u16(idx);
            }

            Expr::If { cond, then, else_ } => {
                self.compile_expr(cond, false);
                self.bytecode.emit(Instruction::JumpIfFalse);
                let else_jump = self.bytecode.current_pos();
                self.bytecode.emit_u16(0); // Placeholder

                self.compile_expr(then, tail);
                self.bytecode.emit(Instruction::Jump);
                let end_jump = self.bytecode.current_pos();
                self.bytecode.emit_u16(0); // Placeholder

                let else_pos = self.bytecode.current_pos();
                self.bytecode
                    .patch_jump(else_jump, (else_pos - else_jump - 2) as i16);

                self.compile_expr(else_, tail);

                let end_pos = self.bytecode.current_pos();
                self.bytecode
                    .patch_jump(end_jump, (end_pos - end_jump - 2) as i16);
            }

            Expr::Begin(exprs) => {
                for (i, expr) in exprs.iter().enumerate() {
                    let is_last = i == exprs.len() - 1;
                    self.compile_expr(expr, tail && is_last);
                    if !is_last {
                        self.bytecode.emit(Instruction::Pop);
                    }
                }
            }

            Expr::Call {
                func,
                args,
                tail: is_tail,
            } => {
                // Compile arguments
                for arg in args {
                    self.compile_expr(arg, false);
                }

                // Compile function
                self.compile_expr(func, false);

                // Emit call
                if tail && *is_tail {
                    self.bytecode.emit(Instruction::TailCall);
                } else {
                    self.bytecode.emit(Instruction::Call);
                }
                self.bytecode.emit_byte(args.len() as u8);
            }

            Expr::Lambda {
                params,
                body,
                captures,
            } => {
                // Create a new compiler for the lambda body
                let mut lambda_compiler = Compiler::new();

                // Compile the body
                lambda_compiler.compile_expr(body, true);
                lambda_compiler.bytecode.emit(Instruction::Return);

                // Create closure value with environment
                // Note: env is empty here, actual capturing happens at runtime via MakeClosure instruction
                let closure = Closure {
                    bytecode: Rc::new(lambda_compiler.bytecode.instructions),
                    arity: crate::value::Arity::Exact(params.len()),
                    env: Rc::new(Vec::new()), // Will be populated by VM when closure is created
                    num_locals: params.len() + captures.len(),
                    constants: Rc::new(lambda_compiler.bytecode.constants),
                };

                let idx = self.bytecode.add_constant(Value::Closure(Rc::new(closure)));

                // Emit captured values onto the stack (in order)
                // These will be stored in the closure's environment by the VM
                for (sym, _depth, _index) in captures {
                    // Load the captured variable
                    let sym_idx = self.bytecode.add_constant(Value::Symbol(*sym));
                    self.bytecode.emit(Instruction::LoadGlobal);
                    self.bytecode.emit_u16(sym_idx);
                }

                // Create closure with captured values
                self.bytecode.emit(Instruction::MakeClosure);
                self.bytecode.emit_u16(idx);
                self.bytecode.emit_byte(captures.len() as u8);
            }

            Expr::Let { bindings, body } => {
                // Compile bindings
                for (_name, value_expr) in bindings {
                    self.compile_expr(value_expr, false);
                }

                // Compile body
                self.compile_expr(body, tail);

                // Pop bindings
                for _ in bindings {
                    self.bytecode.emit(Instruction::Pop);
                }
            }

            Expr::Set {
                var: _,
                depth,
                index,
                value,
            } => {
                self.compile_expr(value, false);
                if *depth == 0 {
                    self.bytecode.emit(Instruction::StoreLocal);
                    self.bytecode.emit_byte(*index as u8);
                } else {
                    self.bytecode.emit(Instruction::LoadUpvalue);
                    self.bytecode.emit_byte(*depth as u8);
                    self.bytecode.emit_byte(*index as u8);
                }
            }

            Expr::Define { name, value } => {
                self.compile_expr(value, false);
                let idx = self.bytecode.add_constant(Value::Symbol(*name));
                self.bytecode.emit(Instruction::StoreGlobal);
                self.bytecode.emit_u16(idx);
            }

            Expr::While { cond, body } => {
                // Implement while loop using conditional jumps
                // Loop label - start of condition check
                let loop_label = self.bytecode.instructions.len();

                // Compile condition
                self.compile_expr(cond, false);

                // Jump to end if condition is false
                self.bytecode.emit(Instruction::JumpIfFalse);
                let exit_jump = self.bytecode.instructions.len();
                self.bytecode.emit_i16(0); // Placeholder for exit offset

                // Compile body
                self.compile_expr(body, false);

                // Pop the body result (we don't care about it)
                self.bytecode.emit(Instruction::Pop);

                // Jump back to loop condition
                self.bytecode.emit(Instruction::Jump);
                let loop_offset =
                    (loop_label as i32) - (self.bytecode.instructions.len() as i32 + 2);
                self.bytecode.emit_i16(loop_offset as i16);

                // Patch the exit jump
                let exit_offset =
                    (self.bytecode.instructions.len() as i32) - (exit_jump as i32 + 2);
                let offset_bytes = (exit_offset as i16).to_le_bytes();
                self.bytecode.instructions[exit_jump] = offset_bytes[0];
                self.bytecode.instructions[exit_jump + 1] = offset_bytes[1];

                // Return nil after loop
                self.bytecode.emit(Instruction::Nil);
            }

            Expr::For { var, iter, body } => {
                // Implement for loop: (for x in lst (do-something-with x))
                // Compile the iterable (list)
                self.compile_expr(iter, false);

                // Store the list in a temporary location and iterate through it
                // We'll use the stack to track: car of list | rest of list | original list
                let loop_label = self.bytecode.instructions.len();

                // Check if list is nil (end of iteration)
                self.bytecode.emit(Instruction::Dup); // Duplicate the list
                self.bytecode.emit(Instruction::IsNil);
                self.bytecode.emit(Instruction::JumpIfFalse);
                let body_jump = self.bytecode.instructions.len();
                self.bytecode.emit_i16(0); // Placeholder for jump to body

                // If nil, exit loop
                self.bytecode.emit(Instruction::Pop);
                self.bytecode.emit(Instruction::Nil);
                self.bytecode.emit(Instruction::Jump);
                let exit_jump = self.bytecode.instructions.len();
                self.bytecode.emit_i16(0); // Placeholder for exit

                // Patch body jump
                let body_offset =
                    (self.bytecode.instructions.len() as i32) - (body_jump as i32 + 2);
                let offset_bytes = (body_offset as i16).to_le_bytes();
                self.bytecode.instructions[body_jump] = offset_bytes[0];
                self.bytecode.instructions[body_jump + 1] = offset_bytes[1];

                // Extract car (current element) and cdr (rest)
                self.bytecode.emit(Instruction::Dup); // Duplicate list
                self.bytecode.emit(Instruction::Car); // Get current element
                                                      // Store in variable for body
                let var_idx = self.bytecode.add_constant(Value::Symbol(*var));
                self.bytecode.emit(Instruction::StoreGlobal);
                self.bytecode.emit_u16(var_idx);

                // Get rest for next iteration
                self.bytecode.emit(Instruction::Cdr);

                // Compile body
                self.compile_expr(body, false);
                self.bytecode.emit(Instruction::Pop); // Pop body result

                // Loop back
                self.bytecode.emit(Instruction::Jump);
                let loop_offset =
                    (loop_label as i32) - (self.bytecode.instructions.len() as i32 + 2);
                self.bytecode.emit_i16(loop_offset as i16);

                // Patch exit jump
                let exit_offset =
                    (self.bytecode.instructions.len() as i32) - (exit_jump as i32 + 2);
                let offset_bytes = (exit_offset as i16).to_le_bytes();
                self.bytecode.instructions[exit_jump] = offset_bytes[0];
                self.bytecode.instructions[exit_jump + 1] = offset_bytes[1];
            }

            Expr::Match {
                value,
                patterns,
                default,
            } => {
                // Compile the value to match against
                self.compile_expr(value, false);
                let mut exit_jumps = Vec::new();

                // Try each pattern in sequence
                for (pattern, body_expr) in patterns {
                    let next_pattern_jumps = self.compile_pattern_check(pattern);

                    // Pattern matched - pop the value and execute body
                    self.bytecode.emit(Instruction::Pop);
                    self.compile_expr(body_expr, tail);

                    // Jump to end of match
                    self.bytecode.emit(Instruction::Jump);
                    exit_jumps.push(self.bytecode.instructions.len());
                    self.bytecode.emit_i16(0);

                    // Patch the next_pattern_jumps to skip to here if pattern doesn't match
                    let skip_to = self.bytecode.instructions.len();
                    for jump_idx in next_pattern_jumps {
                        let offset = (skip_to as i32) - (jump_idx as i32 + 2);
                        self.bytecode.patch_jump(jump_idx, offset as i16);
                    }
                }

                // Default/fallback case
                if let Some(default_expr) = default {
                    self.compile_expr(default_expr, tail);
                } else {
                    self.bytecode.emit(Instruction::Nil);
                }

                // Patch all exit jumps to the end
                let end_pos = self.bytecode.instructions.len();
                for jump_idx in exit_jumps {
                    let offset = (end_pos as i32) - (jump_idx as i32 + 2);
                    self.bytecode.patch_jump(jump_idx, offset as i16);
                }
            }

            Expr::Try {
                body,
                catch: _,
                finally,
            } => {
                // Try-catch implementation
                // For now: compile body, then optionally execute finally
                // Full exception handling requires VM-level support for stack unwinding

                self.compile_expr(body, false);

                // Finally block: always executed after try/catch
                if let Some(finally_expr) = finally {
                    // Save the result
                    self.bytecode.emit(Instruction::Dup);
                    self.compile_expr(finally_expr, false);
                    self.bytecode.emit(Instruction::Pop);
                    // The original result stays on stack
                }

                // NOTE: Catch handlers will need VM support to:
                // 1. Check if body produced an exception
                // 2. Unwind stack to try frame
                // 3. Bind exception to catch variable
                // 4. Execute handler
                // For now, parsing works but catch is not yet functional
            }

            Expr::Quote(expr) => {
                // Quote: return the expression itself without evaluation
                // For Phase 2, we treat quoted expressions as literal data
                // This would require converting AST to Value representation
                self.compile_expr(expr, tail);
            }

            Expr::Quasiquote(expr) => {
                // Quasiquote: quote with unquote support
                // For Phase 2, similar to quote but tracks unquote positions
                self.compile_expr(expr, tail);
            }

            Expr::Unquote(expr) => {
                // Unquote: evaluate inside quasiquote
                self.compile_expr(expr, tail);
            }

            Expr::DefMacro {
                name: _,
                params: _,
                body,
            } => {
                // Define macro: Phase 2 simplified - just compile the body
                // Full macro expansion would happen during parsing
                self.compile_expr(body, tail);
            }

            Expr::Module {
                name: _,
                exports: _,
                body,
            } => {
                // Module definition: compile body in module context
                self.compile_expr(body, tail);
            }

            Expr::Import { module: _ } => {
                // Import: no runtime effect in Phase 2
                // Would load module definitions at compile time
                self.bytecode.emit(Instruction::Nil);
            }

            Expr::ModuleRef { module: _, name: _ } => {
                // Module-qualified reference: resolved during compilation
                // For Phase 2, treat as regular global variable lookup
                self.bytecode.emit(Instruction::Nil);
            }

            Expr::Throw { value: _ } => {
                // Throw is compiled as a function call during value_to_expr
                // This case should never be reached, but we handle it for exhaustiveness
                self.bytecode.emit(Instruction::Nil);
            }
        }
    }

    /// Compile pattern matching check. Returns list of jump positions to patch if pattern fails.
    fn compile_pattern_check(&mut self, pattern: &super::ast::Pattern) -> Vec<usize> {
        use super::ast::Pattern;

        match pattern {
            Pattern::Wildcard => {
                // Wildcard matches anything, no check needed
                Vec::new()
            }
            Pattern::Nil => {
                // Check if value is nil
                self.bytecode.emit(Instruction::Dup);
                self.bytecode.emit(Instruction::Nil);
                self.bytecode.emit(Instruction::Eq);
                self.bytecode.emit(Instruction::JumpIfFalse);
                let fail_jump = self.bytecode.instructions.len();
                self.bytecode.emit_i16(0);
                vec![fail_jump]
            }
            Pattern::Literal(val) => {
                // Check if value equals literal
                self.bytecode.emit(Instruction::Dup);
                let const_idx = self.bytecode.add_constant(val.clone());
                self.bytecode.emit(Instruction::LoadConst);
                self.bytecode.emit_u16(const_idx);
                self.bytecode.emit(Instruction::Eq);
                self.bytecode.emit(Instruction::JumpIfFalse);
                let fail_jump = self.bytecode.instructions.len();
                self.bytecode.emit_i16(0);
                vec![fail_jump]
            }
            Pattern::Var(_var_id) => {
                // Variable pattern always matches - store the value
                // In Phase 2, we'll just accept any value (binding handled later)
                Vec::new()
            }
            Pattern::Cons { head: _, tail: _ } => {
                // Cons pattern: check if it's a pair/cons cell
                self.bytecode.emit(Instruction::Dup);
                self.bytecode.emit(Instruction::IsPair);
                self.bytecode.emit(Instruction::JumpIfFalse);
                let fail_jump = self.bytecode.instructions.len();
                self.bytecode.emit_i16(0);
                // Full cons pattern matching would recursively compile head/tail patterns
                // For Phase 2, just check if it's a pair
                vec![fail_jump]
            }
            Pattern::List(_patterns) => {
                // List pattern: for Phase 2, just check if it's a list
                // Full implementation would check length and match elements
                // For now, accept any value
                Vec::new()
            }
            Pattern::Guard {
                pattern: inner,
                condition: _,
            } => {
                // Guard pattern: check inner pattern first, then condition
                let fails = self.compile_pattern_check(inner);
                // Full guard implementation would evaluate the condition
                // For Phase 2, just check the pattern
                fails
            }
        }
    }

    fn finish(self) -> Bytecode {
        self.bytecode
    }
}

/// Compile an expression to bytecode
pub fn compile(expr: &Expr) -> Bytecode {
    let mut compiler = Compiler::new();
    compiler.compile_expr(expr, true);
    compiler.bytecode.emit(Instruction::Return);
    compiler.finish()
}

/// Analyze free variables in an expression
/// Returns the set of variable symbols that are referenced but not bound locally
fn analyze_free_vars(
    expr: &Expr,
    local_bindings: &std::collections::HashSet<SymbolId>,
) -> std::collections::HashSet<SymbolId> {
    use std::collections::HashSet;

    let mut free_vars = HashSet::new();

    match expr {
        Expr::Literal(_) => {
            // No variables in literals
        }

        Expr::Var(sym, _, _) => {
            // Variable reference - include if not locally bound
            if !local_bindings.contains(sym) {
                free_vars.insert(*sym);
            }
        }

        Expr::GlobalVar(sym) => {
            // Global variable - check if it's not a local binding
            if !local_bindings.contains(sym) {
                free_vars.insert(*sym);
            }
        }

        Expr::If { cond, then, else_ } => {
            free_vars.extend(analyze_free_vars(cond, local_bindings));
            free_vars.extend(analyze_free_vars(then, local_bindings));
            free_vars.extend(analyze_free_vars(else_, local_bindings));
        }

        Expr::Begin(exprs) => {
            for e in exprs {
                free_vars.extend(analyze_free_vars(e, local_bindings));
            }
        }

        Expr::Call { func, args, .. } => {
            free_vars.extend(analyze_free_vars(func, local_bindings));
            for arg in args {
                free_vars.extend(analyze_free_vars(arg, local_bindings));
            }
        }

        Expr::Lambda { params, body, .. } => {
            // Create new local bindings that include lambda parameters
            let mut new_bindings = local_bindings.clone();
            for param in params {
                new_bindings.insert(*param);
            }
            free_vars.extend(analyze_free_vars(body, &new_bindings));
        }

        Expr::Let { bindings, body } => {
            // First, variables in the binding expressions can reference outer scope
            for (_, expr) in bindings {
                free_vars.extend(analyze_free_vars(expr, local_bindings));
            }
            // Then, body can reference let-bound variables
            let mut new_bindings = local_bindings.clone();
            for (name, _) in bindings {
                new_bindings.insert(*name);
            }
            free_vars.extend(analyze_free_vars(body, &new_bindings));
        }

        Expr::Set { var, value, .. } => {
            if !local_bindings.contains(var) {
                free_vars.insert(*var);
            }
            free_vars.extend(analyze_free_vars(value, local_bindings));
        }

        Expr::Define { name: _, value } => {
            free_vars.extend(analyze_free_vars(value, local_bindings));
        }

        Expr::While { cond, body } => {
            free_vars.extend(analyze_free_vars(cond, local_bindings));
            free_vars.extend(analyze_free_vars(body, local_bindings));
        }

        Expr::For { var: _, iter, body } => {
            free_vars.extend(analyze_free_vars(iter, local_bindings));
            // Body can reference the loop variable
            let new_bindings = local_bindings.clone();
            free_vars.extend(analyze_free_vars(body, &new_bindings));
        }

        Expr::Match {
            value,
            patterns,
            default,
        } => {
            free_vars.extend(analyze_free_vars(value, local_bindings));
            for (_, expr) in patterns {
                free_vars.extend(analyze_free_vars(expr, local_bindings));
            }
            if let Some(default_expr) = default {
                free_vars.extend(analyze_free_vars(default_expr, local_bindings));
            }
        }

        Expr::Try {
            body,
            catch,
            finally,
        } => {
            free_vars.extend(analyze_free_vars(body, local_bindings));
            if let Some((var, handler)) = catch {
                let mut new_bindings = local_bindings.clone();
                new_bindings.insert(*var);
                free_vars.extend(analyze_free_vars(handler, &new_bindings));
            }
            if let Some(finally_expr) = finally {
                free_vars.extend(analyze_free_vars(finally_expr, local_bindings));
            }
        }

        _ => {
            // Other expression types (Quote, Quasiquote, Module, etc.) don't affect free vars
        }
    }

    free_vars
}

/// Convert a value to a pattern for pattern matching
fn value_to_pattern(value: &Value, symbols: &SymbolTable) -> Result<super::ast::Pattern, String> {
    use super::ast::Pattern;

    match value {
        Value::Nil => Ok(Pattern::Nil),
        Value::Symbol(id) => {
            // Check if it's a wildcard
            if let Some(name) = symbols.name(*id) {
                if name == "_" {
                    return Ok(Pattern::Wildcard);
                }
            }
            // Otherwise it's a variable binding
            Ok(Pattern::Var(*id))
        }
        _ if matches!(
            value,
            Value::Int(_) | Value::Float(_) | Value::Bool(_) | Value::String(_)
        ) =>
        {
            Ok(Pattern::Literal(value.clone()))
        }
        Value::Cons(_) => {
            let vec = value.list_to_vec()?;
            if vec.is_empty() {
                Ok(Pattern::Nil)
            } else {
                // Convert to List pattern
                let patterns: Result<Vec<_>, _> =
                    vec.iter().map(|v| value_to_pattern(v, symbols)).collect();
                Ok(Pattern::List(patterns?))
            }
        }
        _ => Err(format!("Cannot convert {:?} to pattern", value)),
    }
}

/// Simple value-to-expr conversion for bootstrap
/// This is a simple tree-walking approach before full macro expansion
pub fn value_to_expr(value: &Value, symbols: &SymbolTable) -> Result<Expr, String> {
    match value {
        Value::Nil | Value::Bool(_) | Value::Int(_) | Value::Float(_) | Value::String(_) => {
            Ok(Expr::Literal(value.clone()))
        }

        Value::Symbol(id) => {
            // Treat all symbols as global vars for now
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
                    "quote" => {
                        if list.len() != 2 {
                            return Err("quote requires exactly 1 argument".to_string());
                        }
                        Ok(Expr::Literal(list[1].clone()))
                    }

                    "if" => {
                        if list.len() < 3 || list.len() > 4 {
                            return Err("if requires 2 or 3 arguments".to_string());
                        }
                        let cond = Box::new(value_to_expr(&list[1], symbols)?);
                        let then = Box::new(value_to_expr(&list[2], symbols)?);
                        let else_ = if list.len() == 4 {
                            Box::new(value_to_expr(&list[3], symbols)?)
                        } else {
                            Box::new(Expr::Literal(Value::Nil))
                        };
                        Ok(Expr::If { cond, then, else_ })
                    }

                    "begin" => {
                        let exprs: Result<Vec<_>, _> = list[1..]
                            .iter()
                            .map(|v| value_to_expr(v, symbols))
                            .collect();
                        Ok(Expr::Begin(exprs?))
                    }

                    "lambda" => {
                        if list.len() < 3 {
                            return Err("lambda requires at least 2 arguments".to_string());
                        }

                        let params = list[1].list_to_vec()?;
                        let param_syms: Result<Vec<_>, _> =
                            params.iter().map(|p| p.as_symbol()).collect();
                        let param_syms = param_syms?;

                        let body_exprs: Result<Vec<_>, _> = list[2..]
                            .iter()
                            .map(|v| value_to_expr(v, symbols))
                            .collect();
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

                        // Convert free vars to captures (with placeholder depth/index)
                        // These will be resolved at runtime
                        let captures: Vec<_> = free_vars
                            .iter()
                            .map(|sym| (*sym, 0, 0)) // Depth and index will be resolved later
                            .collect();

                        Ok(Expr::Lambda {
                            params: param_syms,
                            body,
                            captures,
                        })
                    }

                    "define" => {
                        if list.len() != 3 {
                            return Err("define requires exactly 2 arguments".to_string());
                        }
                        let name = list[1].as_symbol()?;
                        let value = Box::new(value_to_expr(&list[2], symbols)?);
                        Ok(Expr::Define { name, value })
                    }

                    "set!" => {
                        if list.len() != 3 {
                            return Err("set! requires exactly 2 arguments".to_string());
                        }
                        let var = list[1].as_symbol()?;
                        let value = Box::new(value_to_expr(&list[2], symbols)?);
                        Ok(Expr::Set {
                            var,
                            depth: 0,
                            index: 0,
                            value,
                        })
                    }

                    "try" => {
                        // Syntax: (try <body> (catch <var> <handler>) (finally <expr>)?)
                        if list.len() < 2 {
                            return Err("try requires at least a body".to_string());
                        }

                        let body = Box::new(value_to_expr(&list[1], symbols)?);
                        let mut catch_clause = None;
                        let mut finally_clause = None;

                        // Parse catch and finally clauses
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
                                            let handler = Box::new(value_to_expr(&v[2], symbols)?);
                                            catch_clause = Some((var, handler));
                                        }
                                        "finally" => {
                                            if v.len() != 2 {
                                                return Err("finally requires exactly 1 argument"
                                                    .to_string());
                                            }
                                            finally_clause =
                                                Some(Box::new(value_to_expr(&v[1], symbols)?));
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

                    "match" => {
                        // Syntax: (match value (pattern1 result1) (pattern2 result2) ... [default])
                        if list.len() < 2 {
                            return Err("match requires at least a value".to_string());
                        }

                        let value = Box::new(value_to_expr(&list[1], symbols)?);
                        let mut patterns = Vec::new();
                        let mut default = None;

                        // Parse pattern clauses
                        for clause in &list[2..] {
                            if let Ok(clause_vec) = clause.list_to_vec() {
                                if clause_vec.is_empty() {
                                    return Err("Empty pattern clause".to_string());
                                }

                                // Check if this is a default clause (symbol, not a list)
                                if clause_vec.len() == 1 {
                                    // Single value - treat as default
                                    default =
                                        Some(Box::new(value_to_expr(&clause_vec[0], symbols)?));
                                } else if clause_vec.len() == 2 {
                                    // Pattern and result
                                    let pattern = value_to_pattern(&clause_vec[0], symbols)?;
                                    let result = value_to_expr(&clause_vec[1], symbols)?;
                                    patterns.push((pattern, result));
                                } else {
                                    return Err(
                                        "Pattern clause must have pattern and result".to_string()
                                    );
                                }
                            } else {
                                // Not a list - treat as default
                                default = Some(Box::new(value_to_expr(clause, symbols)?));
                            }
                        }

                        Ok(Expr::Match {
                            value,
                            patterns,
                            default,
                        })
                    }

                    "throw" => {
                        // Syntax: (throw <exception>)
                        // Throw is a special form that compiles to a function call
                        // The throw primitive will convert the exception to a Rust error
                        if list.len() != 2 {
                            return Err("throw requires exactly 1 argument".to_string());
                        }
                        // Compile as a regular function call to the throw primitive
                        let func = Box::new(Expr::GlobalVar(first.as_symbol()?));
                        let args = vec![value_to_expr(&list[1], symbols)?];
                        Ok(Expr::Call {
                            func,
                            args,
                            tail: false,
                        })
                    }

                    _ => {
                        // Function call
                        let func = Box::new(value_to_expr(first, symbols)?);
                        let args: Result<Vec<_>, _> = list[1..]
                            .iter()
                            .map(|v| value_to_expr(v, symbols))
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
                let func = Box::new(value_to_expr(first, symbols)?);
                let args: Result<Vec<_>, _> = list[1..]
                    .iter()
                    .map(|v| value_to_expr(v, symbols))
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
