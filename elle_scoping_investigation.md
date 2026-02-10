# Elle Lisp Compiler Scoping Investigation Report

## Problem Statement

When evaluating code like:
```lisp
(define test (lambda ()
  (begin
    (define x 0)
    (set! x 42)
    x)))
```

The `set!` expression treats `x` as a **global variable** instead of the **local variable** defined in the same `begin` block.

---

## Root Cause Analysis

### 1. How `scope_stack` Works During Parsing

The `scope_stack` in `src/compiler/converters.rs` is a `Vec<Vec<SymbolId>>` that tracks variable bindings at each nesting level during AST conversion.

**Key mechanism (lines 331-360):**
```rust
/// The scope_stack contains local bindings (as Vec for ordering) at each nesting level
fn value_to_expr_with_scope(
    value: &Value,
    symbols: &mut SymbolTable,
    scope_stack: &mut Vec<Vec<SymbolId>>,
) -> Result<Expr, String> {
    // ... for symbols ...
    Value::Symbol(id) => {
        // Check if the symbol is a local binding by walking up the scope stack
        for (reverse_idx, scope) in scope_stack.iter().enumerate().rev() {
            if let Some(local_index) = scope.iter().position(|sym| sym == id) {
                let actual_depth = scope_stack.len() - 1 - reverse_idx;
                return Ok(Expr::Var(*id, actual_depth, local_index));
            }
        }
        // Not found in any local scope - treat as global
        Ok(Expr::GlobalVar(*id))
    }
}
```

**How it works:**
- When entering a `lambda`, a new scope is **pushed** with lambda parameters
- When exiting a `lambda`, the scope is **popped**
- Symbols are looked up by walking the scope_stack in reverse (innermost to outermost)
- If a symbol is found, it returns `Expr::Var(id, depth, index)` with calculated depth
- If NOT found, it returns `Expr::GlobalVar(id)`

---

### 2. The Critical Problem: `begin` Does NOT Push a Scope

**Lines 442-448 (begin handling):**
```rust
"begin" => {
    let exprs: Result<Vec<_>, _> = list[1..]
        .iter()
        .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
        .collect();
    Ok(Expr::Begin(exprs?))
}
```

**Key observation:**
- `begin` does **NOT** push/pop a scope on the `scope_stack`
- It just directly processes all expressions with the CURRENT `scope_stack`
- This is fundamentally different from `lambda`

**Contrast with `lambda` (lines 468-477):**
```rust
"lambda" => {
    let param_syms: Result<Vec<_>, _> = params.iter().map(|p| p.as_symbol()).collect();
    let param_syms = param_syms?;
    
    // Push a new scope with the lambda parameters
    scope_stack.push(param_syms.clone());
    
    let body_exprs: Result<Vec<_>, _> = list[2..]
        .iter()
        .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
        .collect();
    
    // Pop the lambda's scope
    scope_stack.pop();
```

Lambda explicitly:
1. Pushes a new scope with parameters
2. Processes body expressions
3. Pops the scope

But `begin` does neither!

---

### 3. The `define` Problem Within `begin`

When we have:
```lisp
(begin
  (define x 0)      ; <- Line 1
  (set! x 42)       ; <- Line 2
  x)                ; <- Line 3
```

**At parse time:**

**When processing `(define x 0)` (Line 1):**
- Lines 535-542:
```rust
"define" => {
    if list.len() != 3 {
        return Err("define requires exactly 2 arguments".to_string());
    }
    let name = list[1].as_symbol()?;
    let value = Box::new(value_to_expr_with_scope(&list[2], symbols, scope_stack)?);
    Ok(Expr::Define { name, value })
}
```

The `define` expression is **created** but `x` is **NOT added to `scope_stack`** at this point. The `Define` variant just creates an AST node; it doesn't update the `scope_stack`.

**When processing `(set! x 42)` (Line 2):**
- Lines 831-859:
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
    
    // If not found in local scopes (index == usize::MAX), it's a global variable set
    
    Ok(Expr::Set {
        var,
        depth,
        index,
        value,
    })
}
```

**The problem:**
When `set! x` is processed, it searches `scope_stack` for `x`. But since:
1. `define x 0` didn't add `x` to `scope_stack`
2. `begin` doesn't maintain its own scope
3. We're still inside the lambda's scope (if there is one)

The variable `x` is **NOT FOUND** in any scope, so:
- `index` remains `usize::MAX`
- This signals a **global variable set**!

---

## The Scope Stack During Parsing - Detailed Flow

For the problematic code:
```lisp
(define test (lambda ()
  (begin
    (define x 0)
    (set! x 42)
    x)))
```

**Step-by-step scope_stack changes:**

```
1. Start: scope_stack = []

2. Process lambda:
   - scope_stack.push([])  ; empty params
   scope_stack = [[]]
   
3. Process begin (inside lambda):
   - NO scope_stack changes!
   scope_stack = [[]]
   
4. Process (define x 0):
   - Creates Expr::Define { name: x, ... }
   - Does NOT add 'x' to scope_stack
   scope_stack = [[]]
   
5. Process (set! x 42):
   - Searches scope_stack for 'x'
   - 'x' is NOT FOUND
   - index = usize::MAX (GLOBAL!)
   scope_stack = [[]]
   
6. Process x reference:
   - Searches scope_stack for 'x'
   - 'x' is NOT FOUND
   - Returns Expr::GlobalVar(x) (GLOBAL!)
   scope_stack = [[]]
   
7. Pop lambda scope:
   scope_stack.pop()
   scope_stack = []
```

**Result:** Both `set! x` and the `x` reference are treated as global!

---

## AST Structure: The `Expr::Set` Type

From `src/compiler/ast.rs` (lines 84-90):
```rust
/// Set! (mutation)
Set {
    var: SymbolId,
    depth: usize,
    index: usize,
    value: Box<Expr>,
},
```

- `var`: The symbol being set
- `depth`: How many function scopes up (0 = current lambda parameters)
- `index`: Position within that scope's environment
- `value`: The expression being assigned

When `index == usize::MAX`, it signals a **global variable** mutation.

---

## The Fundamental Issue: No Block Scoping at Parse Time

The compiler has **two different scope concepts**:

1. **Parse-time scope tracking (`scope_stack`)**: Only tracks lambda parameters
   - Used by `value_to_expr_with_scope` to determine variable depth/index
   - Used to generate `Expr::Var` nodes with correct depth
   - Used to generate `Expr::Set` nodes with correct depth/index

2. **Runtime scope tracking (VM execution)**: Tracks all local variables
   - Implemented in the VM during execution
   - Handles block scope, let scope, etc.
   - Can dynamically look up variables

**The problem:** 
- `define` creates a runtime binding that doesn't exist in the parse-time `scope_stack`
- `set!` at parse time can't find variables defined by `define` because they're not in `scope_stack`
- The compiler assumes all local variables are either:
  - Lambda parameters (pushed into scope_stack)
  - Or globals

There's no mechanism for `define` to register variables in `scope_stack` so that subsequent `set!` expressions can find them.

---

## Why `lambda` Works Differently

When you have a lambda, the parameters are **known at compile time**, so they're pushed into `scope_stack`:

```lisp
((lambda (x)
  (begin
    (set! x 42)
    x))
 10)
```

This works because:
1. `lambda` pushes `[x]` into scope_stack
2. `set! x` finds `x` in scope_stack → creates `Expr::Set { var: x, depth: 0, index: 0, ... }`
3. Variable reference `x` finds it → creates `Expr::Var(x, 0, 0)`

---

## Current Test Cases Involving This Issue

From `tests/integration/scoping.rs`:

**Line 62-68:**
```rust
#[test]
fn test_define_in_while_loop_is_local() {
    let mut eval = ScopeEval::new();
    eval.eval("(define i 0)").unwrap();
    eval.eval("(while (< i 3) (begin (define temp (* i 2)) (set! i (+ i 1))))")
        .unwrap();
    let result = eval.eval("temp");
    // Expects 'temp' to NOT leak to global scope
}
```

This test:
- Defines `i` globally (so it's findable)
- Uses `set! i` in the while loop (sets global `i`)
- Defines `temp` locally in the loop
- Expects `temp` to not leak

**But there's a bug here**: If you tried to do `set! temp` inside the while loop after the `(define temp ...)`, it would incorrectly treat `temp` as global!

---

## Summary: The Root Cause Chain

1. **`begin` doesn't push a scope** on `scope_stack`
   - It directly processes expressions with current scope_stack
   - No mechanism to track variables defined inside it

2. **`define` doesn't register in `scope_stack`**
   - Only creates an `Expr::Define` AST node
   - Doesn't add the variable to scope_stack for later lookup
   
3. **`set!` relies on `scope_stack` to find variables**
   - If variable not in scope_stack, treats it as global (index = usize::MAX)
   - This is correct for lambda parameters but WRONG for locally-defined variables

4. **Mismatch between parse-time and runtime semantics**
   - Parse time only tracks lambda parameters in scope_stack
   - Runtime can have arbitrary local variables from define/let/etc.
   - `set!` and variable references use parse-time scope_stack to determine type

5. **The fix would require**:
   - Either making `begin` push a scope and having `define` update scope_stack
   - Or deferring variable resolution to runtime for local variables
   - Or implementing a two-pass analysis to collect all local definitions first

---

## Specific Line Numbers Reference

| Element | File | Lines | Description |
|---------|------|-------|-------------|
| `value_to_expr_with_scope` signature | converters.rs | 332-336 | Main conversion function with scope_stack |
| Symbol lookup logic | converters.rs | 346-360 | How variables are resolved in scope_stack |
| `begin` handling | converters.rs | 442-448 | NO scope push/pop! |
| `lambda` handling | converters.rs | 458-532 | Proper scope push/pop |
| `define` handling | converters.rs | 535-542 | No scope_stack update |
| `set!` handling | converters.rs | 831-859 | Variable lookup and Set node creation |
| Expr::Set definition | ast.rs | 84-90 | Set AST node structure |
| Scope module | scope.rs | 1-285 | CompileScope structure (not currently used in converters.rs) |

---

## Implications and Next Steps

This bug would manifest in code like:

```lisp
; This WON'T work correctly
(lambda ()
  (begin
    (define x 0)
    (set! x 42)      ; Treats x as global, not local!
    x))              ; Correctly returns x, but as global ref
```

Whereas this WILL work:

```lisp
; This WORKS
((lambda (x)
  (begin
    (set! x 42)      ; Finds x in lambda params
    x))
 0)
```

And this WORKS because `i` is pre-defined globally:

```lisp
; This WORKS
(define i 0)
(while (< i 3)
  (begin
    (define temp (* i 2))
    (set! i (+ i 1))))  ; i is found globally
```

The solution would need to either:
1. Make `begin` create a block scope in `scope_stack` and have `define` register variables there
2. Implement a pre-analysis pass to collect all `define`d variables before main conversion
3. Track define'd variables separately and resolve them during runtime
