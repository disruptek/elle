//! Phase 1 Performance Benchmarks

use elle::compiler::compile::value_to_expr;
use elle::{compile, read_str, register_primitives, SymbolTable, VM};

fn benchmark<F>(name: &str, iterations: usize, mut f: F)
where
    F: FnMut(),
{
    let start = std::time::Instant::now();
    for _ in 0..iterations {
        f();
    }
    let elapsed = start.elapsed();
    let per_iter = elapsed.as_nanos() / iterations as u128;
    println!(
        "{}: {} iterations in {:?} ({} ns/iter)",
        name, iterations, elapsed, per_iter
    );
}

fn main() {
    println!("=== Phase 1 Performance Benchmarks ===\n");

    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    register_primitives(&mut vm, &mut symbols);

    // Benchmark 1: Arithmetic
    println!("1. Arithmetic Operations");
    benchmark("Simple addition", 1000, || {
        let code = "(+ 1 2 3 4 5)";
        let value = read_str(code, &mut symbols).unwrap();
        let expr = value_to_expr(&value, &mut symbols).unwrap();
        let bytecode = compile(&expr);
        let _ = vm.execute(&bytecode);
    });

    benchmark("Complex arithmetic", 1000, || {
        let code = "(* (+ 1 2) (- 10 3) (/ 20 4))";
        let value = read_str(code, &mut symbols).unwrap();
        let expr = value_to_expr(&value, &mut symbols).unwrap();
        let bytecode = compile(&expr);
        let _ = vm.execute(&bytecode);
    });

    // Benchmark 2: List operations
    println!("\n2. List Operations");
    benchmark("List construction", 1000, || {
        let code = "(list 1 2 3 4 5)";
        let value = read_str(code, &mut symbols).unwrap();
        let expr = value_to_expr(&value, &mut symbols).unwrap();
        let bytecode = compile(&expr);
        let _ = vm.execute(&bytecode);
    });

    benchmark("List append", 100, || {
        let code = "(append (list 1 2) (list 3 4))";
        let value = read_str(code, &mut symbols).unwrap();
        let expr = value_to_expr(&value, &mut symbols).unwrap();
        let bytecode = compile(&expr);
        let _ = vm.execute(&bytecode);
    });

    // Benchmark 3: String operations
    println!("\n3. String Operations");
    benchmark("String append", 1000, || {
        let code = "(string-append \"hello\" \"world\")";
        let value = read_str(code, &mut symbols).unwrap();
        let expr = value_to_expr(&value, &mut symbols).unwrap();
        let bytecode = compile(&expr);
        let _ = vm.execute(&bytecode);
    });

    // Benchmark 4: Control flow
    println!("\n4. Control Flow");
    benchmark("If-then-else", 1000, || {
        let code = "(if (> 5 3) (+ 1 2) (- 10 5))";
        let value = read_str(code, &mut symbols).unwrap();
        let expr = value_to_expr(&value, &mut symbols).unwrap();
        let bytecode = compile(&expr);
        let _ = vm.execute(&bytecode);
    });

    // Benchmark 5: Map/Filter/Fold
    println!("\n5. Higher-Order Functions");
    benchmark("Map with native function", 100, || {
        let code = "(map abs (list -1 -2 -3 -4 -5))";
        let value = read_str(code, &mut symbols).unwrap();
        let expr = value_to_expr(&value, &mut symbols).unwrap();
        let bytecode = compile(&expr);
        let _ = vm.execute(&bytecode);
    });

    benchmark("Filter with native function", 100, || {
        let code = "(filter even? (list 1 2 3 4 5 6 7 8))";
        let value = read_str(code, &mut symbols).unwrap();
        let expr = value_to_expr(&value, &mut symbols).unwrap();
        let bytecode = compile(&expr);
        let _ = vm.execute(&bytecode);
    });

    println!("\n=== Benchmarks Complete ===");
}
