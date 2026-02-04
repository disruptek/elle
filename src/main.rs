use elle::compiler::compile::value_to_expr;
use elle::{compile, read_str, register_primitives, SymbolTable, VM};
use std::io::{self, Write};

fn main() {
    let mut vm = VM::new();
    let mut symbols = SymbolTable::new();

    // Register primitive functions
    register_primitives(&mut vm, &mut symbols);

    println!("Elle v0.1.0");
    println!("Type (exit) to quit");
    println!();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            break;
        }

        let input = input.trim();
        if input.is_empty() {
            continue;
        }

        // Check for exit
        if input == "(exit)" || input == "exit" {
            break;
        }

        // Read
        let value = match read_str(input, &mut symbols) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("Parse error: {}", e);
                continue;
            }
        };

        // Compile
        let expr = match value_to_expr(&value, &symbols) {
            Ok(e) => e,
            Err(e) => {
                eprintln!("Compilation error: {}", e);
                continue;
            }
        };

        let bytecode = compile(&expr);

        // Execute
        match vm.execute(&bytecode) {
            Ok(result) => {
                if !result.is_nil() {
                    println!("{:?}", result);
                }
            }
            Err(e) => {
                eprintln!("Runtime error: {}", e);
            }
        }
    }

    println!("Goodbye!");
}
