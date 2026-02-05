// DEFENSE: Primitives are the building blocks - must be correct
use elle::primitives::register_primitives;
use elle::symbol::SymbolTable;
use elle::value::{list, Value};
use elle::vm::VM;

fn setup() -> (VM, SymbolTable) {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();
    register_primitives(&mut vm, &mut symbols);
    (vm, symbols)
}

fn get_primitive(vm: &VM, symbols: &mut SymbolTable, name: &str) -> Value {
    let id = symbols.intern(name);
    vm.get_global(id.0).unwrap().clone()
}

fn call_primitive(prim: &Value, args: &[Value]) -> Result<Value, String> {
    match prim {
        Value::NativeFn(f) => f(args),
        _ => panic!("Not a function"),
    }
}

// Arithmetic tests
#[test]
fn test_addition() {
    let (vm, mut symbols) = setup();
    let add = get_primitive(&vm, &mut symbols, "+");

    // No args
    assert_eq!(call_primitive(&add, &[]).unwrap(), Value::Int(0));

    // Single arg
    assert_eq!(
        call_primitive(&add, &[Value::Int(5)]).unwrap(),
        Value::Int(5)
    );

    // Multiple args
    assert_eq!(
        call_primitive(&add, &[Value::Int(1), Value::Int(2), Value::Int(3)]).unwrap(),
        Value::Int(6)
    );

    // Mixed int/float
    match call_primitive(&add, &[Value::Int(1), Value::Float(2.5)]).unwrap() {
        Value::Float(f) => assert!((f - 3.5).abs() < 1e-10),
        _ => panic!("Expected float"),
    }
}

#[test]
fn test_subtraction() {
    let (vm, mut symbols) = setup();
    let sub = get_primitive(&vm, &mut symbols, "-");

    // Negate
    assert_eq!(
        call_primitive(&sub, &[Value::Int(5)]).unwrap(),
        Value::Int(-5)
    );

    // Subtract
    assert_eq!(
        call_primitive(&sub, &[Value::Int(10), Value::Int(3)]).unwrap(),
        Value::Int(7)
    );

    // Multiple args
    assert_eq!(
        call_primitive(&sub, &[Value::Int(100), Value::Int(25), Value::Int(25)]).unwrap(),
        Value::Int(50)
    );
}

#[test]
fn test_multiplication() {
    let (vm, mut symbols) = setup();
    let mul = get_primitive(&vm, &mut symbols, "*");

    // Identity
    assert_eq!(call_primitive(&mul, &[]).unwrap(), Value::Int(1));

    // Multiply
    assert_eq!(
        call_primitive(&mul, &[Value::Int(2), Value::Int(3), Value::Int(4)]).unwrap(),
        Value::Int(24)
    );

    // Zero
    assert_eq!(
        call_primitive(&mul, &[Value::Int(5), Value::Int(0)]).unwrap(),
        Value::Int(0)
    );
}

#[test]
fn test_division() {
    let (vm, mut symbols) = setup();
    let div = get_primitive(&vm, &mut symbols, "/");

    // Division
    assert_eq!(
        call_primitive(&div, &[Value::Int(10), Value::Int(2)]).unwrap(),
        Value::Int(5)
    );

    // Integer division
    assert_eq!(
        call_primitive(&div, &[Value::Int(7), Value::Int(2)]).unwrap(),
        Value::Int(3)
    );

    // Division by zero
    assert!(call_primitive(&div, &[Value::Int(10), Value::Int(0)]).is_err());
}

// Comparison tests
#[test]
fn test_equality() {
    let (vm, mut symbols) = setup();
    let eq = get_primitive(&vm, &mut symbols, "=");

    assert_eq!(
        call_primitive(&eq, &[Value::Int(5), Value::Int(5)]).unwrap(),
        Value::Bool(true)
    );

    assert_eq!(
        call_primitive(&eq, &[Value::Int(5), Value::Int(6)]).unwrap(),
        Value::Bool(false)
    );

    // Float equality
    assert_eq!(
        call_primitive(&eq, &[Value::Float(3.14), Value::Float(3.14)]).unwrap(),
        Value::Bool(true)
    );
}

#[test]
fn test_less_than() {
    let (vm, mut symbols) = setup();
    let lt = get_primitive(&vm, &mut symbols, "<");

    assert_eq!(
        call_primitive(&lt, &[Value::Int(3), Value::Int(5)]).unwrap(),
        Value::Bool(true)
    );

    assert_eq!(
        call_primitive(&lt, &[Value::Int(5), Value::Int(5)]).unwrap(),
        Value::Bool(false)
    );

    assert_eq!(
        call_primitive(&lt, &[Value::Int(7), Value::Int(5)]).unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn test_greater_than() {
    let (vm, mut symbols) = setup();
    let gt = get_primitive(&vm, &mut symbols, ">");

    assert_eq!(
        call_primitive(&gt, &[Value::Int(7), Value::Int(5)]).unwrap(),
        Value::Bool(true)
    );

    assert_eq!(
        call_primitive(&gt, &[Value::Int(5), Value::Int(5)]).unwrap(),
        Value::Bool(false)
    );
}

// List operation tests
#[test]
fn test_cons() {
    let (vm, mut symbols) = setup();
    let cons = get_primitive(&vm, &mut symbols, "cons");

    let result = call_primitive(&cons, &[Value::Int(1), Value::Int(2)]).unwrap();
    let cons_cell = result.as_cons().unwrap();

    assert_eq!(cons_cell.first, Value::Int(1));
    assert_eq!(cons_cell.rest, Value::Int(2));
}

#[test]
fn test_first() {
    let (vm, mut symbols) = setup();
    let first = get_primitive(&vm, &mut symbols, "first");

    let l = list(vec![Value::Int(10), Value::Int(20), Value::Int(30)]);
    let result = call_primitive(&first, &[l]).unwrap();

    assert_eq!(result, Value::Int(10));
}

#[test]
fn test_rest() {
    let (vm, mut symbols) = setup();
    let rest = get_primitive(&vm, &mut symbols, "rest");

    let l = list(vec![Value::Int(10), Value::Int(20), Value::Int(30)]);
    let result = call_primitive(&rest, &[l]).unwrap();

    assert!(result.is_list());
    let vec = result.list_to_vec().unwrap();
    assert_eq!(vec.len(), 2);
    assert_eq!(vec[0], Value::Int(20));
    assert_eq!(vec[1], Value::Int(30));
}

#[test]
fn test_list() {
    let (vm, mut symbols) = setup();
    let list_fn = get_primitive(&vm, &mut symbols, "list");

    let result = call_primitive(&list_fn, &[Value::Int(1), Value::Int(2), Value::Int(3)]).unwrap();

    assert!(result.is_list());
    let vec = result.list_to_vec().unwrap();
    assert_eq!(vec.len(), 3);
}

// Type predicate tests
#[test]
fn test_nil_predicate() {
    let (vm, mut symbols) = setup();
    let nil_pred = get_primitive(&vm, &mut symbols, "nil?");

    assert_eq!(
        call_primitive(&nil_pred, &[Value::Nil]).unwrap(),
        Value::Bool(true)
    );

    assert_eq!(
        call_primitive(&nil_pred, &[Value::Int(0)]).unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn test_pair_predicate() {
    let (vm, mut symbols) = setup();
    let pair_pred = get_primitive(&vm, &mut symbols, "pair?");

    let l = list(vec![Value::Int(1)]);
    assert_eq!(call_primitive(&pair_pred, &[l]).unwrap(), Value::Bool(true));

    assert_eq!(
        call_primitive(&pair_pred, &[Value::Nil]).unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn test_number_predicate() {
    let (vm, mut symbols) = setup();
    let num_pred = get_primitive(&vm, &mut symbols, "number?");

    assert_eq!(
        call_primitive(&num_pred, &[Value::Int(42)]).unwrap(),
        Value::Bool(true)
    );

    assert_eq!(
        call_primitive(&num_pred, &[Value::Float(3.14)]).unwrap(),
        Value::Bool(true)
    );

    assert_eq!(
        call_primitive(&num_pred, &[Value::Nil]).unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn test_symbol_predicate() {
    let (vm, mut symbols) = setup();
    let sym_pred = get_primitive(&vm, &mut symbols, "symbol?");

    let sym_id = symbols.intern("foo");
    assert_eq!(
        call_primitive(&sym_pred, &[Value::Symbol(sym_id)]).unwrap(),
        Value::Bool(true)
    );

    assert_eq!(
        call_primitive(&sym_pred, &[Value::Int(42)]).unwrap(),
        Value::Bool(false)
    );
}

// Logic tests
#[test]
fn test_not() {
    let (vm, mut symbols) = setup();
    let not = get_primitive(&vm, &mut symbols, "not");

    assert_eq!(
        call_primitive(&not, &[Value::Bool(false)]).unwrap(),
        Value::Bool(true)
    );

    assert_eq!(
        call_primitive(&not, &[Value::Bool(true)]).unwrap(),
        Value::Bool(false)
    );

    assert_eq!(
        call_primitive(&not, &[Value::Nil]).unwrap(),
        Value::Bool(true)
    );

    // Truthy values
    assert_eq!(
        call_primitive(&not, &[Value::Int(0)]).unwrap(),
        Value::Bool(false)
    );
}

// Error handling tests
#[test]
fn test_arithmetic_type_errors() {
    let (vm, mut symbols) = setup();
    let add = get_primitive(&vm, &mut symbols, "+");

    // Adding non-numbers
    assert!(call_primitive(&add, &[Value::Nil]).is_err());
    assert!(call_primitive(&add, &[Value::Bool(true)]).is_err());
}

#[test]
fn test_comparison_type_errors() {
    let (vm, mut symbols) = setup();
    let lt = get_primitive(&vm, &mut symbols, "<");

    // Comparing non-numbers
    assert!(call_primitive(&lt, &[Value::Nil, Value::Int(5)]).is_err());
}

#[test]
fn test_list_operation_errors() {
    let (vm, mut symbols) = setup();
    let first = get_primitive(&vm, &mut symbols, "first");

    // First of non-list
    assert!(call_primitive(&first, &[Value::Int(42)]).is_err());
    assert!(call_primitive(&first, &[Value::Nil]).is_err());
}

#[test]
fn test_arity_errors() {
    let (vm, mut symbols) = setup();

    // first requires exactly 1 argument
    let first = get_primitive(&vm, &mut symbols, "first");
    assert!(call_primitive(&first, &[]).is_err());
    assert!(call_primitive(&first, &[Value::Int(1), Value::Int(2)]).is_err());

    // = requires exactly 2 arguments
    let eq = get_primitive(&vm, &mut symbols, "=");
    assert!(call_primitive(&eq, &[Value::Int(1)]).is_err());
}

// Exception handling tests
#[test]
fn test_exception_creation() {
    let (vm, mut symbols) = setup();
    let exception_fn = get_primitive(&vm, &mut symbols, "exception");

    // Create exception with message
    let exc = call_primitive(&exception_fn, &[Value::String("Error message".into())]).unwrap();
    assert_eq!(exc.type_name(), "exception");
}

#[test]
fn test_exception_message() {
    let (vm, mut symbols) = setup();
    let exception_fn = get_primitive(&vm, &mut symbols, "exception");
    let message_fn = get_primitive(&vm, &mut symbols, "exception-message");

    // Create exception and extract message
    let exc = call_primitive(&exception_fn, &[Value::String("Test error".into())]).unwrap();
    let msg = call_primitive(&message_fn, &[exc]).unwrap();

    match msg {
        Value::String(s) => assert_eq!(s.as_ref(), "Test error"),
        _ => panic!("Expected string"),
    }
}

#[test]
fn test_exception_data() {
    let (vm, mut symbols) = setup();
    let exception_fn = get_primitive(&vm, &mut symbols, "exception");
    let data_fn = get_primitive(&vm, &mut symbols, "exception-data");

    // Exception without data
    let exc1 = call_primitive(&exception_fn, &[Value::String("Error".into())]).unwrap();
    let data1 = call_primitive(&data_fn, &[exc1]).unwrap();
    assert_eq!(data1, Value::Nil);

    // Exception with data
    let exc2 = call_primitive(
        &exception_fn,
        &[Value::String("Error".into()), Value::Int(42)],
    )
    .unwrap();
    let data2 = call_primitive(&data_fn, &[exc2]).unwrap();
    assert_eq!(data2, Value::Int(42));
}

#[test]
fn test_throw() {
    let (vm, mut symbols) = setup();
    let throw_fn = get_primitive(&vm, &mut symbols, "throw");

    // throw with string message should produce error
    let result = call_primitive(&throw_fn, &[Value::String("Test error".into())]);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "Test error");
}

#[test]
fn test_exception_is_value() {
    let (vm, mut symbols) = setup();
    let exception_fn = get_primitive(&vm, &mut symbols, "exception");
    let type_fn = get_primitive(&vm, &mut symbols, "type");

    // Exception should be a value with type "exception"
    let exc = call_primitive(&exception_fn, &[Value::String("Error".into())]).unwrap();
    let type_val = call_primitive(&type_fn, &[exc]).unwrap();

    match type_val {
        Value::String(s) => assert_eq!(s.as_ref(), "exception"),
        _ => panic!("Expected string type name"),
    }
}

// Macro and meta-programming tests
#[test]
fn test_gensym_generation() {
    let (vm, mut symbols) = setup();
    let gensym = get_primitive(&vm, &mut symbols, "gensym");

    // Generate unique symbols
    let sym1 = call_primitive(&gensym, &[]).unwrap();
    let sym2 = call_primitive(&gensym, &[]).unwrap();

    // Should generate strings (symbol names)
    match (&sym1, &sym2) {
        (Value::String(s1), Value::String(s2)) => {
            // Symbols should be unique
            assert_ne!(s1.as_ref(), s2.as_ref());
            // Should start with G (default prefix)
            assert!(s1.starts_with('G'));
            assert!(s2.starts_with('G'));
        }
        _ => panic!("gensym should return strings"),
    }
}

#[test]
fn test_gensym_with_prefix() {
    let (vm, mut symbols) = setup();
    let gensym = get_primitive(&vm, &mut symbols, "gensym");

    // Generate symbol with custom prefix
    let sym = call_primitive(&gensym, &[Value::String("VAR".into())]).unwrap();

    match sym {
        Value::String(s) => {
            assert!(s.starts_with("VAR"));
        }
        _ => panic!("gensym should return string"),
    }
}

// Module system tests
#[test]
fn test_symbol_table_macro_support() {
    use elle::symbol::{MacroDef, SymbolTable};
    use elle::value::SymbolId;

    let mut table = SymbolTable::new();
    let name = table.intern("when");
    let cond = table.intern("cond");
    let body = table.intern("body");

    // Define a macro
    let macro_def = MacroDef {
        name,
        params: vec![cond, body],
        body: "(if cond body nil)".to_string(),
    };

    table.define_macro(macro_def);

    // Check macro exists
    assert!(table.is_macro(name));
    assert!(table.get_macro(name).is_some());
}

#[test]
fn test_symbol_table_module_support() {
    use elle::symbol::{ModuleDef, SymbolTable};

    let mut table = SymbolTable::new();
    let math = table.intern("math");
    let add = table.intern("add");
    let sub = table.intern("sub");

    // Define a module
    let module_def = ModuleDef {
        name: math,
        exports: vec![add, sub],
    };

    table.define_module(module_def);

    // Check module exists
    assert!(table.is_module(math));
    assert!(table.get_module(math).is_some());

    // Check exports
    if let Some(module) = table.get_module(math) {
        assert_eq!(module.exports.len(), 2);
        assert!(module.exports.contains(&add));
        assert!(module.exports.contains(&sub));
    }
}

#[test]
fn test_module_tracking() {
    use elle::symbol::SymbolTable;

    let mut table = SymbolTable::new();
    let math = table.intern("math");

    // Initially no current module
    assert_eq!(table.current_module(), None);

    // Set current module
    table.set_current_module(Some(math));
    assert_eq!(table.current_module(), Some(math));

    // Clear current module
    table.set_current_module(None);
    assert_eq!(table.current_module(), None);
}
