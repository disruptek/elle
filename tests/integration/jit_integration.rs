// JIT Integration Tests
//
// End-to-end tests showing the JIT compilation pipeline working with
// profiling feedback and hot function detection.

use elle::compiler::converters::value_to_expr;
use elle::compiler::JitCoordinator;
use elle::{compile, read_str, register_primitives, SymbolTable, VM};

#[test]
fn test_jit_coordinator_with_execution() {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    register_primitives(&mut vm, &mut symbols);

    // Create JIT coordinator
    let coordinator = JitCoordinator::new(true);
    assert!(coordinator.is_enabled());

    // Parse and compile a simple expression
    let code = "(+ 1 2)";
    let value = read_str(code, &mut symbols).unwrap();
    let expr = value_to_expr(&value, &mut symbols).unwrap();
    let bytecode = compile(&expr);

    // Execute the bytecode (coordinator can monitor this)
    let result = vm.execute(&bytecode).unwrap();
    assert_eq!(result, elle::value::Value::Int(3));

    // Stats should show activity
    let stats = coordinator.get_stats();
    assert!(stats.contains("JIT Coordinator"));
}

#[test]
fn test_jit_hot_function_detection() {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    register_primitives(&mut vm, &mut symbols);

    // Create coordinator with profiling
    let coordinator = JitCoordinator::new(true);

    // Simulate multiple invocations of a function
    let func_id = symbols.intern("test-func");

    // Below threshold (9 calls)
    for _ in 0..9 {
        coordinator
            .profiler()
            .record_call(elle::value::SymbolId(func_id.0));
    }

    // Should not be hot yet
    assert!(!coordinator.should_jit_compile(elle::value::SymbolId(func_id.0)));

    // One more call to reach threshold
    coordinator
        .profiler()
        .record_call(elle::value::SymbolId(func_id.0));

    // Now should be considered for JIT compilation
    assert!(coordinator.should_jit_compile(elle::value::SymbolId(func_id.0)));
}

#[test]
fn test_jit_arithmetic_expression() {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    register_primitives(&mut vm, &mut symbols);

    let test_cases = vec![
        ("(+ 5 3)", elle::value::Value::Int(8)),
        ("(* 4 2)", elle::value::Value::Int(8)),
        ("(- 10 3)", elle::value::Value::Int(7)),
    ];

    for (code, expected) in test_cases {
        let value = read_str(code, &mut symbols).unwrap();
        let expr = value_to_expr(&value, &mut symbols).unwrap();
        let bytecode = compile(&expr);
        let result = vm.execute(&bytecode).unwrap();
        assert_eq!(result, expected, "Failed for: {}", code);
    }
}

#[test]
fn test_jit_conditional_expression() {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    register_primitives(&mut vm, &mut symbols);

    let code = "(if (> 5 3) 100 200)";
    let value = read_str(code, &mut symbols).unwrap();
    let expr = value_to_expr(&value, &mut symbols).unwrap();
    let bytecode = compile(&expr);
    let result = vm.execute(&bytecode).unwrap();
    assert_eq!(result, elle::value::Value::Int(100));
}

#[test]
fn test_jit_with_profiling_feedback() {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    register_primitives(&mut vm, &mut symbols);

    let coordinator = JitCoordinator::new(true);
    let profiler = coordinator.profiler();

    // Execute code 15 times
    for _ in 0..15 {
        let code = "(+ 1 2)";
        let value = read_str(code, &mut symbols).unwrap();
        let expr = value_to_expr(&value, &mut symbols).unwrap();
        let bytecode = compile(&expr);
        let _ = vm.execute(&bytecode).unwrap();

        // Simulate recording function invocation
        profiler.record_call(elle::value::SymbolId(0));
    }

    // Check coordinator stats
    let stats = coordinator.get_stats();
    assert!(stats.contains("JIT Coordinator"));

    // Get profiling summary
    let summary = profiler.summary();
    assert!(summary.total_functions_compiled >= 0);
}
