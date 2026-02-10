# Issue #106 Analysis: set! Does Not Work Inside Lambda Bodies

## Overview
The issue is that `set!` (mutation) fails with "Undefined global variable" when used inside lambda bodies, even when the variable is defined locally with `define`. This prevents imperative patterns (while loops + set!) inside functions.

**Works at top level:**
```lisp
(define x 0)
(set! x 42)
(display x) ; prints 42
```

**Fails inside lambda:**
```lisp
(define test (lambda ()
  (begin
    (define x 0)
    (set! x 42)
    x)))
(display (test)) ; Runtime error: Undefined global variable
```

## Codebase Structure

The Elle Lisp implementation is a bytecode interpreter with a multi-phase compilation pipeline:

1. **Reader** (`src/reader.rs`) - Parses text into S-expressions
2. **Converter** (`src/compiler/converters.rs`) - Converts S-expressions to AST (Expr enum)
3. **Analysis** (`src/compiler/analysis.rs`) - Analyzes free variables and captures
4. **Capture Resolution** (`src/compiler/capture_resolution.rs`) - Fixes capture indices
5. **Compilation** (`src/compiler/compile.rs`) - Converts AST to bytecode
6. **Runtime** (`src/vm/`) - Executes bytecode with scope management

Key directories:
- `/home/adavidoff/git/elle4/src/compiler/` - Compiler infrastructure
- `/home/adavidoff/git/elle4/src/vm/` - Runtime VM and scope management
- `/home/adavidoff/git/elle4/src/primitives/` - Built-in functions
- `/home/adavidoff/git/elle4/tests/integration/` - Integration tests

## Finding #1: set! Implementation

### Location
- **AST Definition**: `/home/adavidoff/git/elle4/src/compiler/ast.rs`, lines 84-90

```rust
/// Set! (mutation)
Set {
    var: SymbolId,
    depth: usize,
    index: usize,
    value: Box<Expr>,
},
```

### Conversion Phase
- **File**: `/home/adavidoff/git/elle4/src/compiler/converters.rs`, lines 831-859
- **Logic**: When `set!` is encountered, the converter:
  1. Extracts the variable name and value expression
  2. **Looks up the variable in scope_stack** to determine depth and index
  3. Sets `index = usize::MAX` if variable is NOT found (signals global variable)

```rust
"set!" => {
    if list.len() != 3 {
        return Err("set! requires exactly 2 arguments".to_string());
    }
    let var = list[1].as_symbol()?;
    let value = Box::new(value_to_expr_with_scope(&list[2], symbols, scope_stack)?);

    // Look up the variable in the scope stack to determine depth and index
    let mut depth = 0;
    let mut index = usize::MAX; // Use MAX to signal global variable

    for (reverse_idx, scope) in scope_stack.iter().enumerate().rev() {
        if let Some(local_index) = scope.iter().position(|sym| sym == &var) {
            depth = scope_stack.len() - 1 - reverse_idx;
            index = local_index;
            break;
        }
    }

    Ok(Expr::Set {
        var,
        depth,
        index,
        value,
    })
}
```

### Compilation Phase
- **File**: `/home/adavidoff/git/elle4/src/compiler/compile.rs`, lines 314-337
- **Critical Issue**: When compiling `Set`, if depth > 0, it's treated as an **upvalue** (variable from outer scope)
- **Problem**: The code emits `StoreGlobal` for upvalues instead of `StoreUpvalue` (which doesn't exist)

```rust
Expr::Set {
    var,
    depth,
    index,
    value,
} => {
    self.compile_expr(value, false);
    if *index == usize::MAX {
        // Global variable set
        let idx = self.bytecode.add_constant(Value::Symbol(*var));
        self.bytecode.emit(Instruction::StoreGlobal);
        self.bytecode.emit_u16(idx);
    } else if *depth == 0 {
        // Local variable set
        self.bytecode.emit(Instruction::StoreLocal);
        self.bytecode.emit_byte(*index as u8);
    } else {
        // Upvalue variable set (not supported yet - treat as error or global)
        // For now, treat as global to avoid corruption
        let idx = self.bytecode.add_constant(Value::Symbol(*var));
        self.bytecode.emit(Instruction::StoreGlobal);
        self.bytecode.emit_u16(idx);
    }
}
```

This is the ROOT CAUSE: upvalue sets are falling back to global, which fails at runtime.

## Finding #2: Variable Scoping Architecture

### Runtime Scope Stack
- **File**: `/home/adavidoff/git/elle4/src/vm/scope/scope_stack.rs`
- **Implementation**: `ScopeStack` maintains a stack of `RuntimeScope`s
- **Methods**:
  - `get(sym_id)` - searches all scopes from current to global
  - `get_at_depth(depth, sym_id)` - gets from specific scope depth
  - `set(sym_id, value)` - finds and updates variable in any scope
  - `set_at_depth(depth, sym_id, value)` - sets at specific scope depth
  - `define_local(sym_id, value)` - adds new variable to current scope

### Runtime Variable Handler
- **File**: `/home/adavidoff/git/elle4/src/vm/variables.rs`
- **handle_load_global**: Tries scope_stack first, then globals (line 13)
- **handle_store_global**: Updates in scope_stack if found, else in globals (line 40)
- **handle_store_local**: Updates stack position directly
- **handle_load_upvalue**: Loads from closure environment

The error "Undefined global variable" comes from line 22:
```rust
if let Some(val) = vm.globals.get(&sym_id.0) {
    vm.stack.push(val.clone());
} else {
    return Err(format!("Undefined global variable: {:?}", sym_id));
}
```

When `StoreGlobal` is emitted for an upvalue set!, at runtime `handle_store_global` is called, which tries to update globals or the current scope_stack. But the variable was never added to the current scope at runtime (it only exists in the closure environment). Hence the lookup fails when trying to read it back with `LoadGlobal`.

## Finding #3: Variable Analysis and Capture Resolution

### Capture Detection
- **File**: `/home/adavidoff/git/elle4/src/compiler/analysis.rs`, lines 175-300
- **Function**: `analyze_free_vars()` - identifies which variables need to be captured

When a lambda is defined:
1. Free variables are identified (variables not bound by parameters)
2. These are converted to captures with their scope location info
3. Closure environment layout: `[capture_0, capture_1, ..., param_0, param_1, ...]`

### Capture Resolution Phase
- **File**: `/home/adavidoff/git/elle4/src/compiler/capture_resolution.rs`
- **Functions**: `resolve_captures()`, `remap_body_vars()`
- **Problem**: In `remap_body_vars()`, lines 183-195, `Set` remapping exists but...

```rust
Expr::Set {
    var,
    depth,
    index,
    value,
} => {
    if *depth > 0 {
        if let Some(&env_idx) = env_info.symbol_to_env_index.get(var) {
            *index = env_idx;  // Remap to closure env index
        }
    }
    remap_body_vars(value, env_info);
}
```

The remapping correctly adjusts the index to point into the closure environment. However, the bytecode generation in compile.rs doesn't handle this case!

## Finding #4: Existing Tests for set!

### Closure Tests
- **File**: `/home/adavidoff/git/elle4/tests/integration/closures_and_lambdas.rs`, lines 713-725
- **Test**: `test_set_in_nested_closure()` - Tests setting captured variables

```rust
#[test]
fn test_set_in_nested_closure() {
    let code = r#"
        (begin
          (define counter 0)
          (define inc (lambda () (begin (set! counter (+ counter 1)) counter)))
          (inc)
          (inc)
          (inc))
    "#;
    assert_eq!(eval(code).unwrap(), Value::Int(3));
}
```

This test uses captured global variables (defined outside the lambda), not locally-defined variables.

### Loop Tests  
- **File**: `/home/adavidoff/git/elle4/tests/integration/loops.rs`, lines 32+
- Heavy use of `set!` in while loops at top level
- No tests for `set!` inside lambdas

### Scope Tests
- **File**: `/home/adavidoff/git/elle4/tests/integration/scoping.rs`
- Tests variable scoping with `for`, `while` loops
- No tests for `set!` inside lambda-defined local variables

## The Bug: Complete Analysis

### Problem Chain

1. **Parse Phase** (converters.rs line 831-859):
   - When parsing `(set! x 42)` inside a lambda, the scope_stack contains `[x]` from the `(define x 0)`
   - So `set! x` is correctly marked with `depth=0, index=0` (local variable)
   - ✓ This is correct

2. **Capture Resolution Phase** (capture_resolution.rs line 183-195):
   - When remapping body variables inside the lambda, if the variable is in the closure environment:
   - The index is correctly adjusted to point to its position in `[captures..., params...]`
   - ✓ This is correct

3. **Compile Phase** (compile.rs line 314-337):
   - ✗ **BUG HERE**: When compiling `Set { depth=0, index=?, ... }`:
   - The code checks: `if *depth == 0` then emit `StoreLocal`
   - BUT: At this point, the index has been adjusted by capture_resolution
   - `StoreLocal` assumes the index is a stack frame offset, not a closure environment index
   - This is wrong!

4. **Runtime Phase** (variables.rs):
   - When `StoreLocal` tries to write to stack[index], it works
   - But the variable is actually in the closure environment, not the stack
   - When later trying to read with `LoadGlobal`, it fails because the variable was never in globals

### Root Cause

The compiler doesn't distinguish between:
- **Local variables in the current scope** (created by `define` inside lambda) - should use `StoreLocal` with stack offset
- **Captured variables from outer scopes** (accessed via closure environment) - should use `StoreUpvalue` with closure env offset

After capture resolution, `depth=0` is ambiguous: it could mean either!

### Why the test passes

The `test_set_in_nested_closure()` test works because:
```lisp
(define counter 0)  ; Global
(define inc (lambda () (set! counter (+ counter 1)) counter))
```
- `counter` is NOT a local variable (depth > 0 or global)
- At parse time, counter is not in scope_stack yet (it's global)
- So it's marked with `depth=0, index=MAX` (global)
- This correctly emits `StoreGlobal`

## The Missing Piece: StoreUpvalue Instruction

The compiler has `LoadUpvalue` (variables.rs line 73) but NO `StoreUpvalue` instruction!

This is needed to:
- Read from closure environment: `LoadUpvalue`
- **Write to closure environment: `StoreUpvalue`** (missing!)

## Summary of Files to Fix

1. **`src/compiler/bytecode.rs`** - Add `StoreUpvalue` instruction
2. **`src/compiler/compile.rs`** - Emit `StoreUpvalue` for captured variable sets
3. **`src/vm/variables.rs`** - Implement `handle_store_upvalue`
4. **`tests/integration/closures_and_lambdas.rs`** - Add test case for issue #106

## Recommended Fix Strategy

1. Add `StoreUpvalue` bytecode instruction
2. In `compile.rs`, for `Set` expressions with captured variables:
   - If variable is in closure environment: emit `StoreUpvalue`
   - If variable is local (created by define in lambda): emit `StoreLocal`
   - If variable is global: emit `StoreGlobal`
3. Implement `handle_store_upvalue` in variables.rs
4. Add comprehensive tests for set! inside lambdas
