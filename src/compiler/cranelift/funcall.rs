// Function call compilation for Cranelift
//
// Handles compilation of function calls, including:
// - Primitive operations (+, -, *, /, <, >, =, etc.)
// - User-defined functions (deferred to Phase 3)
// - Higher-order functions and closures (Phase 4+)

use crate::compiler::ast::Expr;
use crate::value::Value;

/// Represents the result of trying to compile a call
#[derive(Debug, Clone)]
pub enum CallCompileResult {
    /// Successfully compiled (constant folding or IR generation)
    Compiled,
    /// Could not compile (requires runtime evaluation)
    NotCompilable,
}

/// Compiles function calls.
///
/// In Phase 2, this is primarily a placeholder. Full implementation
/// will come in Phase 3 when symbol table integration is available.
pub struct FunctionCallCompiler;

impl FunctionCallCompiler {
    /// Check if a function call can be compiled
    ///
    /// Phase 2: All calls are deferred to Phase 3
    /// Phase 3: Will support primitive operations with symbol table resolution
    /// Phase 4+: Will support user-defined functions and higher-order calls
    pub fn can_compile_call(_func: &Expr, _args: &[Expr]) -> CallCompileResult {
        // For Phase 2, all function calls require runtime evaluation
        CallCompileResult::NotCompilable
    }

    /// Extract all arguments as constant values (used by Phase 3+)
    pub fn extract_constant_args(args: &[Expr]) -> Result<Vec<Value>, String> {
        let mut values = Vec::new();
        for arg in args {
            match arg {
                Expr::Literal(val) => values.push(val.clone()),
                _ => return Err("Non-literal argument".to_string()),
            }
        }
        Ok(values)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_call_not_compilable_phase2() {
        let result = FunctionCallCompiler::can_compile_call(
            &Expr::Literal(Value::Symbol(crate::value::SymbolId(0))),
            &[Expr::Literal(Value::Int(1))],
        );
        match result {
            CallCompileResult::NotCompilable => (),
            _ => panic!("Expected NotCompilable in Phase 2, got {:?}", result),
        }
    }

    #[test]
    fn test_extract_constant_args() {
        let args = vec![Expr::Literal(Value::Int(1)), Expr::Literal(Value::Int(2))];
        let result = FunctionCallCompiler::extract_constant_args(&args);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 2);
    }

    #[test]
    fn test_extract_args_with_non_literal() {
        let args = vec![
            Expr::Literal(Value::Int(1)),
            Expr::Var(crate::value::SymbolId(0), 0, 0),
        ];
        let result = FunctionCallCompiler::extract_constant_args(&args);
        assert!(result.is_err());
    }
}
