// Cranelift code generation for Elle Lisp expressions
//
// This module handles the core logic of translating Elle AST expressions
// into Cranelift IR (CLIF) and compiling to native x86_64 code.

use super::context::JITContext;
use crate::compiler::ast::Expr;
use crate::value::Value;
use cranelift::prelude::*;
use cranelift_module::Module;

/// Expression compiler
pub struct ExprCompiler;

impl ExprCompiler {
    /// Compile a single expression to a function
    pub fn compile_expr(
        ctx: &mut JITContext,
        name: &str,
        expr: &Expr,
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
        let _result = Self::compile_expr_block(&mut builder, expr)?;

        // For now, return a constant based on the expression type
        let return_val = builder.ins().iconst(types::I64, 0);
        builder.ins().return_(&[return_val]);

        builder.finalize();

        ctx.define_function(func_id)?;
        ctx.clear();

        Ok(ctx.get_function(func_id))
    }

    /// Compile an expression within a builder block
    pub fn compile_expr_block(builder: &mut FunctionBuilder, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Literal(val) => Self::compile_literal(builder, val),
            Expr::Begin(exprs) => Self::compile_begin(builder, exprs),
            Expr::If { cond, then, else_ } => Self::compile_if(builder, cond, then, else_),
            _ => Err(format!(
                "Expression type not yet supported in JIT: {:?}",
                expr
            )),
        }
    }

    /// Compile a literal value
    fn compile_literal(_builder: &mut FunctionBuilder, val: &Value) -> Result<Value, String> {
        match val {
            Value::Nil => Ok(Value::Nil),
            Value::Bool(b) => Ok(Value::Bool(*b)),
            Value::Int(i) => Ok(Value::Int(*i)),
            Value::Float(f) => Ok(Value::Float(*f)),
            _ => Err(format!(
                "Cannot compile non-primitive literal in JIT: {:?}",
                val
            )),
        }
    }

    /// Compile a begin (sequence) expression
    fn compile_begin(builder: &mut FunctionBuilder, exprs: &[Expr]) -> Result<Value, String> {
        let mut result = Value::Nil;
        for expr in exprs {
            result = Self::compile_expr_block(builder, expr)?;
        }
        Ok(result)
    }

    /// Compile an if expression
    fn compile_if(
        builder: &mut FunctionBuilder,
        cond: &Expr,
        then_expr: &Expr,
        else_expr: &Expr,
    ) -> Result<Value, String> {
        // Create blocks for each branch
        let then_block = builder.create_block();
        let else_block = builder.create_block();
        let join_block = builder.create_block();

        // Compile condition
        let _cond_val = Self::compile_expr_block(builder, cond)?;

        // Branch based on condition
        // Stub: always take then branch for now
        builder.ins().jump(then_block, &[]);

        // Then branch
        builder.switch_to_block(then_block);
        builder.seal_block(then_block);
        let _then_val = Self::compile_expr_block(builder, then_expr)?;
        builder.ins().jump(join_block, &[]);

        // Else branch
        builder.switch_to_block(else_block);
        builder.seal_block(else_block);
        let _else_val = Self::compile_expr_block(builder, else_expr)?;
        builder.ins().jump(join_block, &[]);

        // Join point
        builder.switch_to_block(join_block);
        builder.seal_block(join_block);

        Ok(Value::Nil)
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

        let result = ExprCompiler::compile_expr_block(&mut builder, &Expr::Literal(Value::Int(42)));
        assert!(
            result.is_ok(),
            "Failed to compile integer literal: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_compile_expr_block_bool() {
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut func = ir::Function::new();
        func.signature.params.push(AbiParam::new(types::I64));
        func.signature.returns.push(AbiParam::new(types::I64));
        let mut builder = FunctionBuilder::new(&mut func, &mut builder_ctx);
        let block = builder.create_block();
        builder.switch_to_block(block);
        builder.seal_block(block);

        let result =
            ExprCompiler::compile_expr_block(&mut builder, &Expr::Literal(Value::Bool(true)));
        assert!(
            result.is_ok(),
            "Failed to compile boolean literal: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_compile_expr_block_begin() {
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut func = ir::Function::new();
        func.signature.params.push(AbiParam::new(types::I64));
        func.signature.returns.push(AbiParam::new(types::I64));
        let mut builder = FunctionBuilder::new(&mut func, &mut builder_ctx);
        let block = builder.create_block();
        builder.switch_to_block(block);
        builder.seal_block(block);

        let result = ExprCompiler::compile_expr_block(
            &mut builder,
            &Expr::Begin(vec![
                Expr::Literal(Value::Int(1)),
                Expr::Literal(Value::Int(2)),
            ]),
        );
        assert!(
            result.is_ok(),
            "Failed to compile begin expression: {:?}",
            result.err()
        );
    }
}
