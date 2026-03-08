use crate::pipeline::compile_file;
use crate::symbol::SymbolTable;
use crate::vm::VM;

/// Standard library source, embedded at compile time.
const STDLIB: &str = include_str!("../../stdlib.lisp");

/// Initialize the standard library by evaluating stdlib.lisp.
///
/// The stdlib is compiled as a single synthetic letrec so that
/// definitions are visible to subsequent forms (mutual recursion).
pub fn init_stdlib(vm: &mut VM, symbols: &mut SymbolTable) {
    let result = match compile_file(STDLIB, symbols) {
        Ok(r) => r,
        Err(e) => panic!("stdlib compilation failed: {}", e),
    };
    if let Err(e) = vm.execute(&result.bytecode) {
        panic!("stdlib execution failed: {}", e);
    }
}
