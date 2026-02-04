use super::ast::Expr;
use super::bytecode::{Bytecode, Instruction};
use crate::symbol::SymbolTable;
use crate::value::{Closure, SymbolId, Value};
use std::collections::HashMap;
use std::rc::Rc;

struct Compiler {
    bytecode: Bytecode,
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

                // Create closure value
                let closure = Closure {
                    bytecode: Rc::new(lambda_compiler.bytecode.instructions),
                    arity: crate::value::Arity::Exact(params.len()),
                    env: Rc::new(Vec::new()),
                    num_locals: params.len(),
                };

                let idx = self.bytecode.add_constant(Value::Closure(Rc::new(closure)));
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

                        Ok(Expr::Lambda {
                            params: param_syms,
                            body,
                            captures: Vec::new(),
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
