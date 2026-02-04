// DEFENSE: Reader must handle all valid Lisp syntax correctly
use elle::reader::read_str;
use elle::symbol::SymbolTable;
use elle::value::Value;

#[test]
fn test_read_integers() {
    let mut symbols = SymbolTable::new();

    assert_eq!(read_str("0", &mut symbols).unwrap(), Value::Int(0));
    assert_eq!(read_str("42", &mut symbols).unwrap(), Value::Int(42));
    assert_eq!(read_str("-123", &mut symbols).unwrap(), Value::Int(-123));
    assert_eq!(read_str("+456", &mut symbols).unwrap(), Value::Int(456));
}

#[test]
fn test_read_floats() {
    let mut symbols = SymbolTable::new();

    assert_eq!(read_str("3.14", &mut symbols).unwrap(), Value::Float(3.14));
    assert_eq!(read_str("-2.5", &mut symbols).unwrap(), Value::Float(-2.5));
    assert_eq!(read_str("0.0", &mut symbols).unwrap(), Value::Float(0.0));
}

#[test]
fn test_read_booleans() {
    let mut symbols = SymbolTable::new();

    assert_eq!(read_str("#t", &mut symbols).unwrap(), Value::Bool(true));
    assert_eq!(read_str("#f", &mut symbols).unwrap(), Value::Bool(false));
}

#[test]
fn test_read_nil() {
    let mut symbols = SymbolTable::new();
    assert_eq!(read_str("nil", &mut symbols).unwrap(), Value::Nil);
}

#[test]
fn test_read_symbols() {
    let mut symbols = SymbolTable::new();

    let sym = read_str("foo", &mut symbols).unwrap();
    assert!(matches!(sym, Value::Symbol(_)));

    let sym2 = read_str("bar-baz", &mut symbols).unwrap();
    assert!(matches!(sym2, Value::Symbol(_)));

    let sym3 = read_str("+", &mut symbols).unwrap();
    assert!(matches!(sym3, Value::Symbol(_)));
}

#[test]
fn test_read_strings() {
    let mut symbols = SymbolTable::new();

    match read_str("\"hello\"", &mut symbols).unwrap() {
        Value::String(s) => assert_eq!(&*s, "hello"),
        _ => panic!("Expected string"),
    }

    match read_str("\"\"", &mut symbols).unwrap() {
        Value::String(s) => assert_eq!(&*s, ""),
        _ => panic!("Expected empty string"),
    }
}

#[test]
fn test_read_string_escapes() {
    let mut symbols = SymbolTable::new();

    match read_str(r#""hello\nworld""#, &mut symbols).unwrap() {
        Value::String(s) => assert_eq!(&*s, "hello\nworld"),
        _ => panic!("Expected string"),
    }

    match read_str(r#""quote: \"test\"""#, &mut symbols).unwrap() {
        Value::String(s) => assert_eq!(&*s, "quote: \"test\""),
        _ => panic!("Expected string"),
    }
}

#[test]
fn test_read_empty_list() {
    let mut symbols = SymbolTable::new();

    let result = read_str("()", &mut symbols).unwrap();
    assert_eq!(result, Value::Nil);
}

#[test]
fn test_read_simple_list() {
    let mut symbols = SymbolTable::new();

    let result = read_str("(1 2 3)", &mut symbols).unwrap();
    assert!(result.is_list());

    let vec = result.list_to_vec().unwrap();
    assert_eq!(vec.len(), 3);
    assert_eq!(vec[0], Value::Int(1));
    assert_eq!(vec[1], Value::Int(2));
    assert_eq!(vec[2], Value::Int(3));
}

#[test]
fn test_read_nested_lists() {
    let mut symbols = SymbolTable::new();

    let result = read_str("((1 2) (3 4))", &mut symbols).unwrap();
    assert!(result.is_list());

    let vec = result.list_to_vec().unwrap();
    assert_eq!(vec.len(), 2);
    assert!(vec[0].is_list());
    assert!(vec[1].is_list());
}

#[test]
fn test_read_quote() {
    let mut symbols = SymbolTable::new();

    let result = read_str("'foo", &mut symbols).unwrap();
    assert!(result.is_list());

    let vec = result.list_to_vec().unwrap();
    assert_eq!(vec.len(), 2);
    // First element should be 'quote symbol
    assert!(matches!(vec[0], Value::Symbol(_)));
}

#[test]
fn test_read_quasiquote() {
    let mut symbols = SymbolTable::new();

    let result = read_str("`foo", &mut symbols).unwrap();
    assert!(result.is_list());

    let vec = result.list_to_vec().unwrap();
    assert_eq!(vec.len(), 2);
}

#[test]
fn test_read_unquote() {
    let mut symbols = SymbolTable::new();

    let result = read_str(",foo", &mut symbols).unwrap();
    assert!(result.is_list());
}

#[test]
fn test_read_vector() {
    let mut symbols = SymbolTable::new();

    let result = read_str("[1 2 3]", &mut symbols).unwrap();
    match result {
        Value::Vector(v) => {
            assert_eq!(v.len(), 3);
            assert_eq!(v[0], Value::Int(1));
            assert_eq!(v[1], Value::Int(2));
            assert_eq!(v[2], Value::Int(3));
        }
        _ => panic!("Expected vector"),
    }
}

#[test]
fn test_read_empty_vector() {
    let mut symbols = SymbolTable::new();

    let result = read_str("[]", &mut symbols).unwrap();
    match result {
        Value::Vector(v) => assert_eq!(v.len(), 0),
        _ => panic!("Expected vector"),
    }
}

#[test]
fn test_read_comments() {
    let mut symbols = SymbolTable::new();

    // Comment should be ignored
    let result = read_str("; this is a comment\n42", &mut symbols).unwrap();
    assert_eq!(result, Value::Int(42));
}

#[test]
fn test_read_whitespace() {
    let mut symbols = SymbolTable::new();

    // Various whitespace should be handled
    let result = read_str("  \t\n  42  \t\n  ", &mut symbols).unwrap();
    assert_eq!(result, Value::Int(42));
}

#[test]
fn test_read_complex_expression() {
    let mut symbols = SymbolTable::new();

    let result = read_str("(+ (* 2 3) (- 10 5))", &mut symbols).unwrap();
    assert!(result.is_list());

    let vec = result.list_to_vec().unwrap();
    assert_eq!(vec.len(), 3); // +, first arg, second arg
}

#[test]
fn test_read_errors() {
    let mut symbols = SymbolTable::new();

    // Unterminated string
    assert!(read_str("\"hello", &mut symbols).is_err());

    // Unterminated list
    assert!(read_str("(1 2 3", &mut symbols).is_err());

    // Unexpected closing paren
    assert!(read_str(")", &mut symbols).is_err());

    // Note: "12.34.56" is currently parsed as a symbol, not rejected as invalid
    // This is a known limitation - the lexer doesn't validate number format strictly
}

#[test]
fn test_read_special_symbols() {
    let mut symbols = SymbolTable::new();

    // Arithmetic operators
    for op in &["+", "-", "*", "/", "=", "<", ">"] {
        let result = read_str(op, &mut symbols).unwrap();
        assert!(matches!(result, Value::Symbol(_)));
    }

    // Hyphenated names (common in Lisp)
    let result = read_str("some-func-name", &mut symbols).unwrap();
    assert!(matches!(result, Value::Symbol(_)));
}

#[test]
fn test_read_deep_nesting() {
    let mut symbols = SymbolTable::new();

    // Test deep nesting doesn't cause stack overflow
    let result = read_str("((((((((((42))))))))))", &mut symbols).unwrap();
    assert!(result.is_list());
}

#[test]
fn test_read_large_list() {
    let mut symbols = SymbolTable::new();

    // Generate a list with 100 elements
    let input = format!(
        "({})",
        (0..100)
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    );
    let result = read_str(&input, &mut symbols).unwrap();

    assert!(result.is_list());
    let vec = result.list_to_vec().unwrap();
    assert_eq!(vec.len(), 100);
}

#[test]
fn test_symbol_interning() {
    let mut symbols = SymbolTable::new();

    // Same symbol should have same ID
    let sym1 = read_str("foo", &mut symbols).unwrap();
    let sym2 = read_str("foo", &mut symbols).unwrap();
    assert_eq!(sym1, sym2);

    // Different symbols should have different IDs
    let sym3 = read_str("bar", &mut symbols).unwrap();
    assert_ne!(sym1, sym3);
}
