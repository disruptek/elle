// Cranelift code generation for Elle Lisp expressions
//
// This module handles the core logic of translating Elle AST expressions
// into Cranelift IR (CLIF) and compiling to native x86_64 code.

use super::branching::BranchManager;
use super::codegen::IrEmitter;
use super::context::JITContext;
use crate::compiler::ast::Expr;
use crate::symbol::SymbolTable;
use crate::value::Value;
use cranelift::prelude::*;
use cranelift_module::Module;

/// Represents a compiled expression value in CLIF IR
/// Maps Elle values to Cranelift SSA values
#[derive(Debug, Clone, Copy)]
pub enum IrValue {
    /// An i64 SSA value (nil, bool, int, or encoded float)
    I64(cranelift::prelude::Value),
    /// An f64 SSA value (unboxed float)
    F64(cranelift::prelude::Value),
}

/// Expression compiler
pub struct ExprCompiler;

impl ExprCompiler {
    /// Compile a single expression to a function
    pub fn compile_expr(
        ctx: &mut JITContext,
        name: &str,
        expr: &Expr,
        symbols: &SymbolTable,
    ) -> Result<*const u8, String> {
        // Create function signature: fn(args_ptr: i64, args_len: i64) -> i64
        let mut sig = ctx.make_signature();
        sig.params.push(AbiParam::new(types::I64)); // args pointer
        sig.params.push(AbiParam::new(types::I64)); // args length
        sig.returns.push(AbiParam::new(types::I64)); // return value

        let func_id = ctx.declare_function(name, sig)?;

        // Set the signature before building
        ctx.ctx.func.signature = ctx.module.make_signature();
        ctx.ctx
            .func
            .signature
            .params
            .push(AbiParam::new(types::I64));
        ctx.ctx
            .func
            .signature
            .params
            .push(AbiParam::new(types::I64));
        ctx.ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(types::I64));

        let mut builder = FunctionBuilder::new(&mut ctx.ctx.func, &mut ctx.builder_ctx);
        let entry_block = builder.create_block();
        builder.append_block_param(entry_block, types::I64); // args pointer
        builder.append_block_param(entry_block, types::I64); // args length
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // Compile the expression
        let result = Self::compile_expr_block(&mut builder, expr, symbols)?;

        // Convert the compiled value to i64 for return
        let return_val = match result {
            IrValue::I64(v) => v,
            IrValue::F64(_v) => {
                // TODO: Encode float as its bit representation (i64)
                // For now, return 0
                builder.ins().iconst(types::I64, 0)
            }
        };
        builder.ins().return_(&[return_val]);

        builder.finalize();

        ctx.define_function(func_id)?;
        ctx.clear();

        Ok(ctx.get_function(func_id))
    }

    /// Compile an expression within a builder block
    /// Returns an IrValue (Cranelift SSA value)
    pub fn compile_expr_block(
        builder: &mut FunctionBuilder,
        expr: &Expr,
        symbols: &SymbolTable,
    ) -> Result<IrValue, String> {
        match expr {
            Expr::Literal(val) => Self::compile_literal(builder, val),
            Expr::Begin(exprs) => Self::compile_begin(builder, exprs, symbols),
            Expr::If { cond, then, else_ } => Self::compile_if(builder, cond, then, else_, symbols),
            // Try to compile And/Or with shortcircuiting
            Expr::And(exprs) => Self::try_compile_and(builder, exprs, symbols),
            Expr::Or(exprs) => Self::try_compile_or(builder, exprs, symbols),
            // Try to compile Let bindings
            Expr::Let { bindings, body } => Self::try_compile_let(builder, bindings, body, symbols),
            // Try to compile binary operations with integer operands
            Expr::Call { func, args, .. } if args.len() == 2 => {
                Self::try_compile_binop(builder, func, args, symbols)
            }
            // Try to compile unary operations like empty?
            Expr::Call { func, args, .. } if args.len() == 1 => {
                Self::try_compile_unary_op(builder, func, args, symbols)
            }
            _ => Err(format!(
                "Expression type not yet supported in JIT: {:?}",
                expr
            )),
        }
    }

    /// Try to compile a Let expression
    /// Note: This only works for Let that doesn't refer to Var/GlobalVar
    fn try_compile_let(
        builder: &mut FunctionBuilder,
        bindings: &[(crate::value::SymbolId, Expr)],
        body: &Expr,
        symbols: &SymbolTable,
    ) -> Result<IrValue, String> {
        // For now, we only support Let with constant bindings (no Var/GlobalVar references)
        // and where body doesn't reference the bindings via Var indices
        // This is a limitation of the current JIT approach which doesn't track local var bindings

        // We could extend this to track bindings in a map, but for now just reject
        // Let expressions that have non-literal bindings
        for (_sym, binding_expr) in bindings {
            match binding_expr {
                Expr::Literal(_) => {
                    // OK - constant binding
                }
                _ => {
                    // Reject - would need tracking
                    return Err(
                        "Let bindings with non-literal values not yet supported in JIT".to_string(),
                    );
                }
            }
        }

        // For Let with only constant bindings where body doesn't reference them,
        // just compile the body (since the bindings don't affect it in JIT)
        // This is actually a missed optimization, but safe
        Self::compile_expr_block(builder, body, symbols)
    }

    /// Try to compile an And expression with shortcircuiting
    /// (and expr1 expr2 expr3 ...) => returns first falsy value or last value
    fn try_compile_and(
        builder: &mut FunctionBuilder,
        exprs: &[Expr],
        symbols: &SymbolTable,
    ) -> Result<IrValue, String> {
        if exprs.is_empty() {
            return Ok(IrValue::I64(builder.ins().iconst(types::I64, 1))); // (and) => true
        }

        if exprs.len() == 1 {
            return Self::compile_expr_block(builder, &exprs[0], symbols);
        }

        // For multi-argument and, we need control flow for proper short-circuiting
        // Create blocks: one for each expression evaluation + one for the final result
        let mut eval_blocks = Vec::new();
        let mut phi_values = Vec::new();

        for _ in 0..exprs.len() {
            eval_blocks.push(builder.create_block());
        }
        let end_block = builder.create_block();

        let zero = builder.ins().iconst(types::I64, 0);

        // Start with first expression
        let first_val = Self::compile_expr_block(builder, &exprs[0], symbols)?;
        let first_i64 = match first_val {
            IrValue::I64(v) => v,
            _ => return Err("And on non-I64 values not supported".to_string()),
        };
        phi_values.push(first_i64);

        // Check if first is false (equal to 0), if so jump to end with that value
        // Otherwise continue to next expression
        let is_false = builder.ins().icmp(IntCC::Equal, first_i64, zero);
        if exprs.len() > 1 {
            builder
                .ins()
                .brif(is_false, end_block, &[first_i64], eval_blocks[1], &[]);
        } else {
            builder.ins().jump(end_block, &[first_i64]);
        }

        // Evaluate remaining expressions
        for i in 1..exprs.len() {
            builder.switch_to_block(eval_blocks[i]);
            builder.seal_block(eval_blocks[i]);

            let val = Self::compile_expr_block(builder, &exprs[i], symbols)?;
            let val_i64 = match val {
                IrValue::I64(v) => v,
                _ => return Err("And on non-I64 values not supported".to_string()),
            };
            phi_values.push(val_i64);

            // If this is not the last expression, check if value is false
            if i < exprs.len() - 1 {
                let is_false = builder.ins().icmp(IntCC::Equal, val_i64, zero);
                if i + 1 < eval_blocks.len() {
                    builder
                        .ins()
                        .brif(is_false, end_block, &[val_i64], eval_blocks[i + 1], &[]);
                } else {
                    builder.ins().jump(end_block, &[val_i64]);
                }
            } else {
                // Last expression - jump to end with its value
                builder.ins().jump(end_block, &[val_i64]);
            }
        }

        builder.switch_to_block(end_block);
        builder.seal_block(end_block);
        let param = builder.block_params(end_block)[0];

        Ok(IrValue::I64(param))
    }

    /// Try to compile an Or expression with shortcircuiting
    /// (or expr1 expr2 expr3 ...) => returns first truthy value or last value
    fn try_compile_or(
        builder: &mut FunctionBuilder,
        exprs: &[Expr],
        symbols: &SymbolTable,
    ) -> Result<IrValue, String> {
        if exprs.is_empty() {
            return Ok(IrValue::I64(builder.ins().iconst(types::I64, 0))); // (or) => false
        }

        if exprs.len() == 1 {
            return Self::compile_expr_block(builder, &exprs[0], symbols);
        }

        // For multi-argument or, we need control flow for proper short-circuiting
        let mut eval_blocks = Vec::new();
        let mut phi_values = Vec::new();

        for _ in 0..exprs.len() {
            eval_blocks.push(builder.create_block());
        }
        let end_block = builder.create_block();

        let zero = builder.ins().iconst(types::I64, 0);

        // Start with first expression
        let first_val = Self::compile_expr_block(builder, &exprs[0], symbols)?;
        let first_i64 = match first_val {
            IrValue::I64(v) => v,
            _ => return Err("Or on non-I64 values not supported".to_string()),
        };
        phi_values.push(first_i64);

        // Check if first is true (not equal to 0), if so jump to end with that value
        // Otherwise continue to next expression
        let is_true = builder.ins().icmp(IntCC::NotEqual, first_i64, zero);
        if exprs.len() > 1 {
            builder
                .ins()
                .brif(is_true, end_block, &[first_i64], eval_blocks[1], &[]);
        } else {
            builder.ins().jump(end_block, &[first_i64]);
        }

        // Evaluate remaining expressions
        for i in 1..exprs.len() {
            builder.switch_to_block(eval_blocks[i]);
            builder.seal_block(eval_blocks[i]);

            let val = Self::compile_expr_block(builder, &exprs[i], symbols)?;
            let val_i64 = match val {
                IrValue::I64(v) => v,
                _ => return Err("Or on non-I64 values not supported".to_string()),
            };
            phi_values.push(val_i64);

            // If this is not the last expression, check if value is true
            if i < exprs.len() - 1 {
                let is_true = builder.ins().icmp(IntCC::NotEqual, val_i64, zero);
                if i + 1 < eval_blocks.len() {
                    builder
                        .ins()
                        .brif(is_true, end_block, &[val_i64], eval_blocks[i + 1], &[]);
                } else {
                    builder.ins().jump(end_block, &[val_i64]);
                }
            } else {
                // Last expression - jump to end with its value
                builder.ins().jump(end_block, &[val_i64]);
            }
        }

        builder.switch_to_block(end_block);
        builder.seal_block(end_block);
        let param = builder.block_params(end_block)[0];

        Ok(IrValue::I64(param))
    }

    /// Try to compile a unary operation (like empty?, abs)
    fn try_compile_unary_op(
        builder: &mut FunctionBuilder,
        func: &Expr,
        args: &[Expr],
        symbols: &SymbolTable,
    ) -> Result<IrValue, String> {
        // Extract operator name from function
        let op_name = match func {
            Expr::Literal(Value::Symbol(sym_id)) => symbols.name(*sym_id),
            _ => None,
        };

        let op_name = match op_name {
            Some(name) => name,
            None => return Err("Could not resolve operator name".to_string()),
        };

        // Compile the argument
        let arg = Self::compile_expr_block(builder, &args[0], symbols)?;

        // Perform the unary operation
        match op_name {
            "empty?" => {
                // empty? on nil returns true, on cons returns false
                // This is essentially the same as (nil? x)
                match arg {
                    IrValue::I64(val) => {
                        // Check if value is nil (0)
                        let zero = builder.ins().iconst(types::I64, 0);
                        let result =
                            builder
                                .ins()
                                .icmp(cranelift::prelude::IntCC::Equal, val, zero);
                        Ok(IrValue::I64(result))
                    }
                    _ => Err("empty? on non-I64 not supported".to_string()),
                }
            }
            "abs" => {
                // Absolute value: if x < 0 then -x else x
                match arg {
                    IrValue::I64(val) => {
                        let zero = builder.ins().iconst(types::I64, 0);
                        let is_negative = builder.ins().icmp(IntCC::SignedLessThan, val, zero);
                        let negated = builder.ins().ineg(val);
                        let result = builder.ins().select(is_negative, negated, val);
                        Ok(IrValue::I64(result))
                    }
                    _ => Err("abs on non-I64 not supported".to_string()),
                }
            }
            "nil?" => {
                // nil? is same as empty?
                match arg {
                    IrValue::I64(val) => {
                        let zero = builder.ins().iconst(types::I64, 0);
                        let result =
                            builder
                                .ins()
                                .icmp(cranelift::prelude::IntCC::Equal, val, zero);
                        Ok(IrValue::I64(result))
                    }
                    _ => Err("nil? on non-I64 not supported".to_string()),
                }
            }
            "not" => {
                // Logical NOT
                match arg {
                    IrValue::I64(val) => {
                        let zero = builder.ins().iconst(types::I64, 0);
                        let result =
                            builder
                                .ins()
                                .icmp(cranelift::prelude::IntCC::Equal, val, zero);
                        Ok(IrValue::I64(result))
                    }
                    _ => Err("not on non-I64 not supported".to_string()),
                }
            }
            _ => Err(format!("Unknown unary operator: {}", op_name)),
        }
    }

    /// Try to compile a binary operation
    /// Only works for operations on literal integers
    fn try_compile_binop(
        builder: &mut FunctionBuilder,
        func: &Expr,
        args: &[Expr],
        symbols: &SymbolTable,
    ) -> Result<IrValue, String> {
        // Extract operator name from function
        let op_name = match func {
            Expr::Literal(Value::Symbol(sym_id)) => symbols.name(*sym_id),
            _ => None,
        };

        let op_name = match op_name {
            Some(name) => name,
            None => return Err("Could not resolve operator name".to_string()),
        };

        // Compile the arguments
        let left = Self::compile_expr_block(builder, &args[0], symbols)?;
        let right = Self::compile_expr_block(builder, &args[1], symbols)?;

        // Perform the binary operation
        match (left, right) {
            (IrValue::I64(l), IrValue::I64(r)) => {
                let result = match op_name {
                    "+" => IrEmitter::emit_add_int(builder, l, r),
                    "-" => IrEmitter::emit_sub_int(builder, l, r),
                    "*" => IrEmitter::emit_mul_int(builder, l, r),
                    "/" => IrEmitter::emit_sdiv_int(builder, l, r),
                    "=" => IrEmitter::emit_eq_int(builder, l, r),
                    "<" => IrEmitter::emit_lt_int(builder, l, r),
                    ">" => IrEmitter::emit_gt_int(builder, l, r),
                    "<=" => {
                        // <= is (not (>))
                        let gt_result = IrEmitter::emit_gt_int(builder, l, r);
                        let zero = builder.ins().iconst(types::I64, 0);
                        builder.ins().icmp(IntCC::Equal, gt_result, zero)
                    }
                    ">=" => {
                        // >= is (not (<))
                        let lt_result = IrEmitter::emit_lt_int(builder, l, r);
                        let zero = builder.ins().iconst(types::I64, 0);
                        builder.ins().icmp(IntCC::Equal, lt_result, zero)
                    }
                    "!=" | "neq" => {
                        // != is (not (=))
                        let eq_result = IrEmitter::emit_eq_int(builder, l, r);
                        let zero = builder.ins().iconst(types::I64, 0);
                        builder.ins().icmp(IntCC::Equal, eq_result, zero)
                    }
                    _ => return Err(format!("Unknown binary operator: {}", op_name)),
                };
                Ok(IrValue::I64(result))
            }
            _ => Err("Type mismatch in binary operation".to_string()),
        }
    }

    /// Compile a literal value to CLIF IR
    fn compile_literal(builder: &mut FunctionBuilder, val: &Value) -> Result<IrValue, String> {
        match val {
            Value::Nil => {
                // Nil is encoded as 0i64
                let ir_val = IrEmitter::emit_nil(builder);
                Ok(IrValue::I64(ir_val))
            }
            Value::Bool(b) => {
                // Bool is encoded as 0 (false) or 1 (true)
                let ir_val = IrEmitter::emit_bool(builder, *b);
                Ok(IrValue::I64(ir_val))
            }
            Value::Int(i) => {
                // Int is emitted directly
                let ir_val = IrEmitter::emit_int(builder, *i);
                Ok(IrValue::I64(ir_val))
            }
            Value::Float(f) => {
                // Float is emitted as f64
                let ir_val = IrEmitter::emit_float(builder, *f);
                Ok(IrValue::F64(ir_val))
            }
            _ => Err(format!(
                "Cannot compile non-primitive literal in JIT: {:?}",
                val
            )),
        }
    }

    /// Compile a begin (sequence) expression
    fn compile_begin(
        builder: &mut FunctionBuilder,
        exprs: &[Expr],
        symbols: &SymbolTable,
    ) -> Result<IrValue, String> {
        let mut result = IrValue::I64(IrEmitter::emit_nil(builder));
        for expr in exprs {
            result = Self::compile_expr_block(builder, expr, symbols)?;
        }
        Ok(result)
    }

    /// Compile an if expression with proper conditional branching
    fn compile_if(
        builder: &mut FunctionBuilder,
        cond: &Expr,
        then_expr: &Expr,
        else_expr: &Expr,
        symbols: &SymbolTable,
    ) -> Result<IrValue, String> {
        // Compile the condition expression
        let cond_val = Self::compile_expr_block(builder, cond, symbols)?;

        // Extract i64 value from condition (floats would need conversion)
        let cond_i64 = match cond_val {
            IrValue::I64(v) => v,
            IrValue::F64(_v) => {
                // For now, treat any f64 as truthy (non-zero)
                // TODO: Proper float-to-int conversion
                return Err("Float conditions not yet supported".to_string());
            }
        };

        // Create branch blocks
        let (then_block, else_block, join_block) = BranchManager::create_if_blocks(builder);

        // Emit the conditional branch
        BranchManager::emit_if_cond(builder, cond_i64, then_block, else_block);

        // Compile then branch
        builder.switch_to_block(then_block);
        builder.seal_block(then_block);
        let then_val = Self::compile_expr_block(builder, then_expr, symbols)?;
        let then_i64 = Self::ir_value_to_i64(builder, then_val)?;
        BranchManager::jump_to_join(builder, join_block, then_i64);

        // Compile else branch
        builder.switch_to_block(else_block);
        builder.seal_block(else_block);
        let else_val = Self::compile_expr_block(builder, else_expr, symbols)?;
        let else_i64 = Self::ir_value_to_i64(builder, else_val)?;
        BranchManager::jump_to_join(builder, join_block, else_i64);

        // Set up join block and get the result value
        BranchManager::setup_join_block_for_value(join_block, builder);
        builder.switch_to_block(join_block);
        builder.seal_block(join_block);
        let result_i64 = BranchManager::get_join_value(builder, join_block);

        Ok(IrValue::I64(result_i64))
    }

    /// Convert IrValue to i64 for control flow operations
    fn ir_value_to_i64(
        builder: &mut FunctionBuilder,
        val: IrValue,
    ) -> Result<cranelift::prelude::Value, String> {
        match val {
            IrValue::I64(v) => Ok(v),
            IrValue::F64(_v) => {
                // For now, return a placeholder
                // TODO: Proper float-to-i64 encoding
                Ok(builder.ins().iconst(types::I64, 0))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cranelift::codegen::ir;

    #[test]
    fn test_compile_expr_block_literal() {
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut func = ir::Function::new();
        func.signature.params.push(AbiParam::new(types::I64));
        func.signature.returns.push(AbiParam::new(types::I64));
        let mut builder = FunctionBuilder::new(&mut func, &mut builder_ctx);
        let block = builder.create_block();
        builder.switch_to_block(block);
        builder.seal_block(block);

        use crate::symbol::SymbolTable;
        let symbols = SymbolTable::new();
        let result = ExprCompiler::compile_expr_block(
            &mut builder,
            &Expr::Literal(Value::Int(42)),
            &symbols,
        );
        assert!(
            result.is_ok(),
            "Failed to compile integer literal: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_compile_expr_block_bool() {
        use crate::symbol::SymbolTable;
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut func = ir::Function::new();
        func.signature.params.push(AbiParam::new(types::I64));
        func.signature.returns.push(AbiParam::new(types::I64));
        let mut builder = FunctionBuilder::new(&mut func, &mut builder_ctx);
        let block = builder.create_block();
        builder.switch_to_block(block);
        builder.seal_block(block);

        let symbols = SymbolTable::new();
        let result = ExprCompiler::compile_expr_block(
            &mut builder,
            &Expr::Literal(Value::Bool(true)),
            &symbols,
        );
        assert!(
            result.is_ok(),
            "Failed to compile boolean literal: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_compile_expr_block_begin() {
        use crate::symbol::SymbolTable;
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut func = ir::Function::new();
        func.signature.params.push(AbiParam::new(types::I64));
        func.signature.returns.push(AbiParam::new(types::I64));
        let mut builder = FunctionBuilder::new(&mut func, &mut builder_ctx);
        let block = builder.create_block();
        builder.switch_to_block(block);
        builder.seal_block(block);

        let symbols = SymbolTable::new();
        let result = ExprCompiler::compile_expr_block(
            &mut builder,
            &Expr::Begin(vec![
                Expr::Literal(Value::Int(1)),
                Expr::Literal(Value::Int(2)),
            ]),
            &symbols,
        );
        assert!(
            result.is_ok(),
            "Failed to compile begin expression: {:?}",
            result.err()
        );
    }
}
