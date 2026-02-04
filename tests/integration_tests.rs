// DEFENSE: Integration tests ensure the full pipeline works end-to-end
use elle::compiler::compile::value_to_expr;
use elle::{compile, read_str, register_primitives, SymbolTable, Value, VM};

fn eval(input: &str) -> Result<Value, String> {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    register_primitives(&mut vm, &mut symbols);

    let value = read_str(input, &mut symbols)?;
    let expr = value_to_expr(&value, &symbols)?;
    let bytecode = compile(&expr);
    vm.execute(&bytecode)
}

// Basic arithmetic
#[test]
fn test_simple_arithmetic() {
    assert_eq!(eval("(+ 1 2)").unwrap(), Value::Int(3));
    assert_eq!(eval("(- 10 3)").unwrap(), Value::Int(7));
    assert_eq!(eval("(* 4 5)").unwrap(), Value::Int(20));
    assert_eq!(eval("(/ 20 4)").unwrap(), Value::Int(5));
}

#[test]
fn test_nested_arithmetic() {
    assert_eq!(eval("(+ (* 2 3) (- 10 5))").unwrap(), Value::Int(11));
    assert_eq!(eval("(* (+ 1 2) (- 5 2))").unwrap(), Value::Int(9));
}

#[test]
fn test_deeply_nested() {
    assert_eq!(eval("(+ 1 (+ 2 (+ 3 (+ 4 5))))").unwrap(), Value::Int(15));
}

// Comparisons
#[test]
fn test_comparisons() {
    assert_eq!(eval("(= 5 5)").unwrap(), Value::Bool(true));
    assert_eq!(eval("(= 5 6)").unwrap(), Value::Bool(false));
    assert_eq!(eval("(< 3 5)").unwrap(), Value::Bool(true));
    assert_eq!(eval("(< 5 3)").unwrap(), Value::Bool(false));
    assert_eq!(eval("(> 7 5)").unwrap(), Value::Bool(true));
}

// Conditionals
#[test]
fn test_if_true() {
    assert_eq!(eval("(if #t 100 200)").unwrap(), Value::Int(100));
}

#[test]
fn test_if_false() {
    assert_eq!(eval("(if #f 100 200)").unwrap(), Value::Int(200));
}

#[test]
fn test_if_with_condition() {
    assert_eq!(eval("(if (> 5 3) 100 200)").unwrap(), Value::Int(100));
    assert_eq!(eval("(if (< 5 3) 100 200)").unwrap(), Value::Int(200));
}

#[test]
fn test_nested_if() {
    assert_eq!(
        eval("(if (> 5 3) (if (< 2 4) 1 2) 3)").unwrap(),
        Value::Int(1)
    );
}

#[test]
fn test_if_nil_else() {
    // If without else should return nil
    assert_eq!(eval("(if #f 100)").unwrap(), Value::Nil);
}

// Lists
#[test]
fn test_list_construction() {
    let result = eval("(list 1 2 3)").unwrap();
    assert!(result.is_list());
    let vec = result.list_to_vec().unwrap();
    assert_eq!(vec.len(), 3);
}

#[test]
fn test_cons() {
    let result = eval("(cons 1 (cons 2 (cons 3 nil)))").unwrap();
    assert!(result.is_list());
    let vec = result.list_to_vec().unwrap();
    assert_eq!(vec, vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
}

#[test]
fn test_first_rest() {
    assert_eq!(eval("(first (list 10 20 30))").unwrap(), Value::Int(10));

    let result = eval("(rest (list 10 20 30))").unwrap();
    let vec = result.list_to_vec().unwrap();
    assert_eq!(vec, vec![Value::Int(20), Value::Int(30)]);
}

#[test]
fn test_nested_lists() {
    let result = eval("(list (list 1 2) (list 3 4))").unwrap();
    let vec = result.list_to_vec().unwrap();
    assert_eq!(vec.len(), 2);
    assert!(vec[0].is_list());
    assert!(vec[1].is_list());
}

// Quote
#[test]
fn test_quote_symbol() {
    let result = eval("'foo").unwrap();
    assert!(matches!(result, Value::Symbol(_)));
}

#[test]
fn test_quote_list() {
    let result = eval("'(1 2 3)").unwrap();
    assert!(result.is_list());
}

// Type predicates
#[test]
fn test_predicates() {
    assert_eq!(eval("(nil? nil)").unwrap(), Value::Bool(true));
    assert_eq!(eval("(nil? 0)").unwrap(), Value::Bool(false));

    assert_eq!(eval("(number? 42)").unwrap(), Value::Bool(true));
    assert_eq!(eval("(number? nil)").unwrap(), Value::Bool(false));

    assert_eq!(eval("(pair? (cons 1 2))").unwrap(), Value::Bool(true));
    assert_eq!(eval("(pair? nil)").unwrap(), Value::Bool(false));
}

// Global definitions
#[test]
fn test_define_and_use() {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    register_primitives(&mut vm, &mut symbols);

    // Define x
    let def = read_str("(define x 42)", &mut symbols).unwrap();
    let expr = value_to_expr(&def, &symbols).unwrap();
    let bytecode = compile(&expr);
    vm.execute(&bytecode).unwrap();

    // Use x
    let use_x = read_str("(+ x 10)", &mut symbols).unwrap();
    let expr = value_to_expr(&use_x, &symbols).unwrap();
    let bytecode = compile(&expr);
    let result = vm.execute(&bytecode).unwrap();

    assert_eq!(result, Value::Int(52));
}

#[test]
fn test_multiple_defines() {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    register_primitives(&mut vm, &mut symbols);

    // Define multiple variables
    for (name, value) in &[("a", "10"), ("b", "20"), ("c", "30")] {
        let def = read_str(&format!("(define {} {})", name, value), &mut symbols).unwrap();
        let expr = value_to_expr(&def, &symbols).unwrap();
        let bytecode = compile(&expr);
        vm.execute(&bytecode).unwrap();
    }

    // Use them
    let result = read_str("(+ a b c)", &mut symbols).unwrap();
    let expr = value_to_expr(&result, &symbols).unwrap();
    let bytecode = compile(&expr);
    let result = vm.execute(&bytecode).unwrap();

    assert_eq!(result, Value::Int(60));
}

// Begin
#[test]
fn test_begin() {
    let result = eval("(begin 1 2 3)").unwrap();
    assert_eq!(result, Value::Int(3));
}

#[test]
fn test_begin_with_side_effects() {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    register_primitives(&mut vm, &mut symbols);

    // Begin with defines
    let code = "(begin (define x 10) (define y 20) (+ x y))";
    let value = read_str(code, &mut symbols).unwrap();
    let expr = value_to_expr(&value, &symbols).unwrap();
    let bytecode = compile(&expr);
    let result = vm.execute(&bytecode).unwrap();

    assert_eq!(result, Value::Int(30));
}

// Complex expressions
#[test]
fn test_factorial_logic() {
    // Simulate factorial without recursion: (if (<= n 1) 1 (* n ...))
    assert_eq!(eval("(if (<= 1 1) 1 (* 1 1))").unwrap(), Value::Int(1));

    assert_eq!(eval("(if (<= 5 1) 1 (* 5 120))").unwrap(), Value::Int(600));
}

#[test]
fn test_max_logic() {
    assert_eq!(eval("(if (> 10 5) 10 5)").unwrap(), Value::Int(10));

    assert_eq!(eval("(if (> 3 7) 3 7)").unwrap(), Value::Int(7));
}

// Error cases
#[test]
fn test_division_by_zero() {
    assert!(eval("(/ 10 0)").is_err());
}

#[test]
fn test_type_error() {
    assert!(eval("(+ 1 nil)").is_err());
}

#[test]
fn test_undefined_variable() {
    assert!(eval("undefined-var").is_err());
}

#[test]
fn test_arity_error() {
    assert!(eval("(+)").is_ok()); // + accepts 0 args
    assert!(eval("(first)").is_err()); // first requires 1 arg
}

// Stress tests
#[test]
fn test_large_list() {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    register_primitives(&mut vm, &mut symbols);

    // Create list with 100 elements
    let numbers = (0..100)
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    let code = format!("(list {})", numbers);

    let value = read_str(&code, &mut symbols).unwrap();
    let expr = value_to_expr(&value, &symbols).unwrap();
    let bytecode = compile(&expr);
    let result = vm.execute(&bytecode).unwrap();

    assert!(result.is_list());
    assert_eq!(result.list_to_vec().unwrap().len(), 100);
}

#[test]
fn test_deep_arithmetic() {
    // Test with 50 nested additions
    let mut expr = "1".to_string();
    for _ in 0..50 {
        expr = format!("(+ {} 1)", expr);
    }

    assert_eq!(eval(&expr).unwrap(), Value::Int(51));
}

#[test]
fn test_many_operations() {
    // Chain many operations
    assert_eq!(eval("(+ 1 2 3 4 5 6 7 8 9 10)").unwrap(), Value::Int(55));

    assert_eq!(eval("(* 1 2 3 4 5)").unwrap(), Value::Int(120));
}

// Mixed types
#[test]
fn test_int_float_mixing() {
    match eval("(+ 1 2.5)").unwrap() {
        Value::Float(f) => assert!((f - 3.5).abs() < 1e-10),
        _ => panic!("Expected float"),
    }

    match eval("(* 2 3.5)").unwrap() {
        Value::Float(f) => assert!((f - 7.0).abs() < 1e-10),
        _ => panic!("Expected float"),
    }
}

// Logic combinations
#[test]
fn test_not() {
    assert_eq!(eval("(not #t)").unwrap(), Value::Bool(false));
    assert_eq!(eval("(not #f)").unwrap(), Value::Bool(true));
    assert_eq!(eval("(not nil)").unwrap(), Value::Bool(true));
    assert_eq!(eval("(not 0)").unwrap(), Value::Bool(false)); // 0 is truthy
}

#[test]
fn test_complex_conditionals() {
    assert_eq!(eval("(if (not (< 5 3)) 100 200)").unwrap(), Value::Int(100));

    assert_eq!(
        eval("(if (= (+ 2 3) 5) 'yes 'no)")
            .unwrap()
            .as_symbol()
            .is_ok(),
        true
    );
}
