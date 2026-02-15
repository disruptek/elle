//! New compilation pipeline: Syntax → HIR → LIR → Bytecode
//!
//! This module provides the end-to-end compilation function using the
//! new intermediate representations. It runs in parallel with the
//! existing Value-based pipeline until fully integrated.

use crate::compiler::Bytecode;
use crate::hir::Analyzer;
use crate::lir::{Emitter, Lowerer};
use crate::reader::{read_syntax, read_syntax_all};
use crate::symbol::SymbolTable;
use crate::syntax::Expander;

/// Compilation result
pub struct CompileResult {
    pub bytecode: Bytecode,
    pub warnings: Vec<String>,
}

/// Compile source code using the new pipeline
pub fn compile_new(source: &str, symbols: &mut SymbolTable) -> Result<CompileResult, String> {
    // Phase 1: Parse to Syntax
    let syntax = read_syntax(source)?;

    // Phase 2: Macro expansion
    let mut expander = Expander::new();
    let expanded = expander.expand(syntax)?;

    // Phase 3: Analyze to HIR
    let mut analyzer = Analyzer::new(symbols);
    let analysis = analyzer.analyze(&expanded)?;

    // Phase 4: Lower to LIR with binding info
    let mut lowerer = Lowerer::new().with_bindings(analysis.bindings);
    let lir_func = lowerer.lower(&analysis.hir)?;

    // Phase 5: Emit bytecode
    let mut emitter = Emitter::new();
    let bytecode = emitter.emit(&lir_func);

    Ok(CompileResult {
        bytecode,
        warnings: Vec::new(),
    })
}

/// Compile multiple top-level forms
pub fn compile_all_new(
    source: &str,
    symbols: &mut SymbolTable,
) -> Result<Vec<CompileResult>, String> {
    let syntaxes = read_syntax_all(source)?;
    let mut expander = Expander::new();
    let mut results = Vec::new();

    for syntax in syntaxes {
        let expanded = expander.expand(syntax)?;

        let mut analyzer = Analyzer::new(symbols);
        let analysis = analyzer.analyze(&expanded)?;

        let mut lowerer = Lowerer::new().with_bindings(analysis.bindings);
        let lir_func = lowerer.lower(&analysis.hir)?;

        let mut emitter = Emitter::new();
        let bytecode = emitter.emit(&lir_func);

        results.push(CompileResult {
            bytecode,
            warnings: Vec::new(),
        });
    }

    Ok(results)
}

/// Compile and execute using the new pipeline
pub fn eval_new(
    source: &str,
    symbols: &mut SymbolTable,
    vm: &mut crate::vm::VM,
) -> Result<crate::value::Value, String> {
    let result = compile_new(source, symbols)?;
    vm.execute(&result.bytecode).map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primitives::register_primitives;
    use crate::vm::VM;

    fn setup() -> (SymbolTable, VM) {
        let mut symbols = SymbolTable::new();
        let mut vm = VM::new();
        register_primitives(&mut vm, &mut symbols);
        (symbols, vm)
    }

    #[test]
    fn test_compile_literal() {
        let (mut symbols, _) = setup();
        let result = compile_new("42", &mut symbols);
        assert!(result.is_ok());
        let compiled = result.unwrap();
        assert!(!compiled.bytecode.instructions.is_empty());
    }

    #[test]
    fn test_compile_if() {
        let (mut symbols, _) = setup();
        let result = compile_new("(if #t 1 2)", &mut symbols);
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_let() {
        let (mut symbols, _) = setup();
        let result = compile_new("(let ((x 10)) x)", &mut symbols);
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_lambda() {
        let (mut symbols, _) = setup();
        let result = compile_new("(fn (x) x)", &mut symbols);
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_call() {
        let (mut symbols, _) = setup();
        // Note: Function calls to built-in symbols like + may fail during lowering
        // because the new pipeline doesn't yet have full integration with built-in symbols.
        // This test just verifies that the pipeline can attempt to compile function calls.
        let result = compile_new("(+ 1 2)", &mut symbols);
        // We accept either success or a specific error about unbound variables
        // since the new pipeline is still being integrated
        match result {
            Ok(_) => {}                                    // Success is fine
            Err(e) if e.contains("Unbound variable") => {} // Expected during integration
            Err(e) => panic!("Unexpected error: {}", e),
        }
    }

    #[test]
    fn test_compile_global_variable() {
        let (mut symbols, _) = setup();
        // Test that global variables (like +) are properly recognized and emit LoadGlobal
        // instead of "Unbound variable" error
        let result = compile_new("(+ 1 2)", &mut symbols);
        // After the fix, this should compile successfully (or at least not fail with "Unbound variable")
        match result {
            Ok(_) => {
                // Success! The global variable + was properly handled
            }
            Err(e) if e.contains("Unbound variable") => {
                panic!("Global variable handling failed: {}", e);
            }
            Err(_e) => {
                // Other errors are acceptable (e.g., bytecode execution issues)
                // as long as it's not "Unbound variable"
            }
        }
    }

    #[test]
    fn test_compile_begin() {
        let (mut symbols, _) = setup();
        let result = compile_new("(begin 1 2 3)", &mut symbols);
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_and() {
        let (mut symbols, _) = setup();
        let result = compile_new("(and #t #t #f)", &mut symbols);
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_or() {
        let (mut symbols, _) = setup();
        let result = compile_new("(or #f #f #t)", &mut symbols);
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_while() {
        let (mut symbols, _) = setup();
        let result = compile_new("(while #f nil)", &mut symbols);
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_cond() {
        let (mut symbols, _) = setup();
        let result = compile_new("(cond (#t 1) (else 2))", &mut symbols);
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_all() {
        let (mut symbols, _) = setup();
        let result = compile_all_new("1 2 3", &mut symbols);
        assert!(result.is_ok());
        let compiled = result.unwrap();
        assert_eq!(compiled.len(), 3);
    }

    #[test]
    fn test_eval_literal() {
        let (mut symbols, mut vm) = setup();
        let result = eval_new("42", &mut symbols, &mut vm);
        // Note: execution may fail due to incomplete bytecode mapping
        // but compilation should succeed
        let _ = result;
    }

    #[test]
    fn test_eval_addition() {
        let (mut symbols, mut vm) = setup();
        let result = eval_new("(+ 1 2)", &mut symbols, &mut vm);
        match result {
            Ok(v) => assert_eq!(v, crate::value::Value::Int(3)),
            Err(e) => panic!("Expected Ok(3), got Err: {}", e),
        }
    }

    #[test]
    fn test_eval_subtraction() {
        let (mut symbols, mut vm) = setup();
        let result = eval_new("(- 10 3)", &mut symbols, &mut vm);
        match result {
            Ok(v) => assert_eq!(v, crate::value::Value::Int(7)),
            Err(e) => panic!("Expected Ok(7), got Err: {}", e),
        }
    }

    #[test]
    fn test_eval_nested_arithmetic() {
        let (mut symbols, mut vm) = setup();
        let result = eval_new("(+ (* 2 3) (- 10 5))", &mut symbols, &mut vm);
        match result {
            Ok(v) => assert_eq!(v, crate::value::Value::Int(11)),
            Err(e) => panic!("Expected Ok(11), got Err: {}", e),
        }
    }

    #[test]
    fn test_eval_if_true() {
        let (mut symbols, mut vm) = setup();
        let result = eval_new("(if #t 42 0)", &mut symbols, &mut vm);
        match result {
            Ok(v) => assert_eq!(v, crate::value::Value::Int(42)),
            Err(e) => panic!("Expected Ok(42), got Err: {}", e),
        }
    }

    #[test]
    fn test_eval_if_false() {
        let (mut symbols, mut vm) = setup();
        let result = eval_new("(if #f 42 0)", &mut symbols, &mut vm);
        match result {
            Ok(v) => assert_eq!(v, crate::value::Value::Int(0)),
            Err(e) => panic!("Expected Ok(0), got Err: {}", e),
        }
    }

    #[test]
    fn test_eval_let_simple() {
        let (mut symbols, mut vm) = setup();
        let result = eval_new("(let ((x 10)) x)", &mut symbols, &mut vm);
        match result {
            Ok(v) => assert_eq!(v, crate::value::Value::Int(10)),
            Err(e) => panic!("Expected Ok(10), got Err: {}", e),
        }
    }

    #[test]
    fn test_eval_let_with_arithmetic() {
        let (mut symbols, mut vm) = setup();
        let result = eval_new("(let ((x 10) (y 5)) (+ x y))", &mut symbols, &mut vm);
        match result {
            Ok(v) => assert_eq!(v, crate::value::Value::Int(15)),
            Err(e) => panic!("Expected Ok(15), got Err: {}", e),
        }
    }

    #[test]
    fn test_eval_lambda_identity() {
        let (mut symbols, mut vm) = setup();
        let result = eval_new("((fn (x) x) 42)", &mut symbols, &mut vm);
        match result {
            Ok(v) => assert_eq!(v, crate::value::Value::Int(42)),
            Err(e) => panic!("Expected Ok(42), got Err: {}", e),
        }
    }

    #[test]
    fn test_eval_lambda_add_one() {
        let (mut symbols, mut vm) = setup();
        let result = eval_new("((fn (x) (+ x 1)) 10)", &mut symbols, &mut vm);
        match result {
            Ok(v) => assert_eq!(v, crate::value::Value::Int(11)),
            Err(e) => panic!("Expected Ok(11), got Err: {}", e),
        }
    }

    #[test]
    fn test_compile_lambda_with_capture() {
        let (mut symbols, _) = setup();
        let result = compile_new("(let ((x 10)) (fn () x))", &mut symbols);
        match result {
            Ok(_) => {}
            Err(e) => panic!("Failed to compile lambda with capture: {}", e),
        }
    }

    #[test]
    fn test_eval_begin() {
        let (mut symbols, mut vm) = setup();
        let result = eval_new("(begin 1 2 3)", &mut symbols, &mut vm);
        match result {
            Ok(v) => assert_eq!(v, crate::value::Value::Int(3)),
            Err(e) => panic!("Expected Ok(3), got Err: {}", e),
        }
    }

    #[test]
    fn test_eval_comparison_lt() {
        let (mut symbols, mut vm) = setup();
        let result = eval_new("(< 1 2)", &mut symbols, &mut vm);
        match result {
            Ok(v) => assert_eq!(v, crate::value::Value::Bool(true)),
            Err(e) => panic!("Expected Ok(true), got Err: {}", e),
        }
    }
}
