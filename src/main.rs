use elle::compiler::compile::value_to_expr;
use elle::{compile, read_str, register_primitives, SymbolTable, VM};
use std::io::{self, Write};

fn print_welcome() {
    println!("╔═══════════════════════════════════════╗");
    println!("║        Elle v0.1.0 - Lisp Interpreter║");
    println!("╚═══════════════════════════════════════╝");
    println!();
    println!("Quick commands:");
    println!("  (exit)          - Exit the REPL");
    println!("  (help)          - Show this help");
    println!("  (+ 1 2)         - Simple arithmetic");
    println!("  (list 1 2 3)    - Create a list");
    println!("  (type 42)       - Get type name");
    println!();
}

fn print_error_context(input: &str, _msg: &str, line: usize, col: usize) {
    let lines: Vec<&str> = input.lines().collect();

    if line > 0 && line <= lines.len() {
        let line_str = lines[line - 1];
        eprintln!("  {}", line_str);

        // Print caret pointing to error location
        if col > 0 {
            eprintln!("  {}{}", " ".repeat(col - 1), "^");
        }
    }
}

fn print_help() {
    println!();
    println!("Elle - Fast Lisp Interpreter");
    println!();
    println!("Primitives:");
    println!("  Arithmetic:  +, -, *, /");
    println!("  Comparison:  =, <, >, <=, >=");
    println!("  Lists:       cons, first, rest, list, length, append, reverse");
    println!("  List utils:  nth, last, take, drop");
    println!("  Math:        min, max, abs, sqrt, sin, cos, tan, log, exp, pow");
    println!("  Constants:   pi, e");
    println!("  Rounding:    floor, ceil, round");
    println!("  Integer ops: mod, remainder, even?, odd?");
    println!("  Strings:     string-length, string-append, string-upcase, string-downcase,");
    println!("               substring, string-index, char-at");
    println!("  Vectors:     vector, vector-length, vector-ref, vector-set!");
    println!("  Types:       type, int, float, string");
    println!("  Logic:       not, if");
    println!("  I/O:         display, newline");
    println!();
    println!("Special forms:");
    println!("  (if cond then else)  - Conditional");
    println!("  (quote x)            - Quote literal");
    println!("  (define x 10)        - Define variable");
    println!("  (begin ...)          - Sequence");
    println!();
}

fn main() {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();

    // Register primitive functions
    register_primitives(&mut vm, &mut symbols);

    print_welcome();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => break, // EOF
            Err(_) => break,
            Ok(_) => {}
        }

        let input = input.trim();
        if input.is_empty() {
            continue;
        }

        // Check for built-in REPL commands
        match input {
            "(exit)" | "exit" => break,
            "(help)" | "help" => {
                print_help();
                continue;
            }
            _ => {}
        }

        // Read
        let value = match read_str(input, &mut symbols) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("✗ Parse error: {}", e);
                print_error_context(input, "parse error", 1, 1);
                continue;
            }
        };

        // Compile
        let expr = match value_to_expr(&value, &symbols) {
            Ok(e) => e,
            Err(e) => {
                eprintln!("✗ Compilation error: {}", e);
                continue;
            }
        };

        let bytecode = compile(&expr);

        // Execute
        match vm.execute(&bytecode) {
            Ok(result) => {
                if !result.is_nil() {
                    println!("⟹ {:?}", result);
                }
            }
            Err(e) => {
                eprintln!("✗ Runtime error: {}", e);
            }
        }
    }

    println!();
    println!("Goodbye!");
}
