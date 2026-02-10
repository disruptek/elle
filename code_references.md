# Code References: Exact Locations of the Scoping Bug

## File Structure
```
/home/adavidoff/git/elle4/
├── src/
│   └── compiler/
│       ├── converters.rs      ← MAIN FILE (1506 lines)
│       ├── ast.rs             ← AST definitions
│       ├── scope.rs           ← Scope structures (not used)
│       ├── analysis.rs        ← Variable analysis
│       └── capture_resolution.rs
└── tests/
    └── integration/
        └── scoping.rs         ← Existing tests
```

---

## Key Code Sections

### 1. Main Entry Point: `value_to_expr_with_scope`

**File:** `src/compiler/converters.rs`  
**Lines:** 332-391

```rust
/// Convert a value to an expression, tracking local variable scopes
/// The scope_stack contains local bindings (as Vec for ordering) at each nesting level
fn value_to_expr_with_scope(
    value: &Value,
    symbols: &mut SymbolTable,
    scope_stack: &mut Vec<Vec<SymbolId>>,
) -> Result<Expr, String> {
    match value {
        Value::Symbol(id) => {
            // Check if the symbol is a local binding by walking up the scope stack
            for (reverse_idx, scope) in scope_stack.iter().enumerate().rev() {
                if let Some(local_index) = scope.iter().position(|sym| sym == id) {
                    // Found in local scope - use Var with appropriate depth and index
                    let actual_depth = scope_stack.len() - 1 - reverse_idx;
                    return Ok(Expr::Var(*id, actual_depth, local_index));
                }
            }
            // Not found in any local scope - treat as global
            Ok(Expr::GlobalVar(*id))
        }
        // ... other cases ...
    }
}
```

**Key insight:** Symbol resolution is entirely dependent on `scope_stack`. If a symbol isn't in `scope_stack`, it's treated as global.

---

### 2. The `begin` Expression (THE PROBLEM)

**File:** `src/compiler/converters.rs`  
**Lines:** 442-448

```rust
"begin" => {
    let exprs: Result<Vec<_>, _> = list[1..]
        .iter()
        .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
        .collect();
    Ok(Expr::Begin(exprs?))
}
```

**The bug:** 
- **NO `scope_stack.push(...)` call**
- **NO `scope_stack.pop()` call**
- Processes all expressions with the CURRENT scope_stack
- No mechanism to register local definitions

---

### 3. Compare With `lambda` Expression (THE CONTRAST)

**File:** `src/compiler/converters.rs`  
**Lines:** 458-532

```rust
"lambda" => {
    if list.len() < 3 {
        return Err("lambda requires at least 2 arguments".to_string());
    }

    let params = list[1].list_to_vec()?;
    let param_syms: Result<Vec<_>, _> =
        params.iter().map(|p| p.as_symbol()).collect();
    let param_syms = param_syms?;

    // ✓ Push a new scope with the lambda parameters
    scope_stack.push(param_syms.clone());

    let body_exprs: Result<Vec<_>, _> = list[2..]
        .iter()
        .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
        .collect();

    // ✓ Pop the lambda's scope
    scope_stack.pop();

    // ... rest of lambda processing ...
}
```

**The difference:**
- **Line 469:** `scope_stack.push(param_syms.clone());` - PUSHES params
- **Line 473:** Uses the new scope for body processing
- **Line 477:** `scope_stack.pop();` - POPS scope

This is why lambda parameters are found but locally-defined variables aren't!

---

### 4. The `define` Expression

**File:** `src/compiler/converters.rs`  
**Lines:** 535-543

```rust
"define" => {
    if list.len() != 3 {
        return Err("define requires exactly 2 arguments".to_string());
    }
    let name = list[1].as_symbol()?;
    let value =
        Box::new(value_to_expr_with_scope(&list[2], symbols, scope_stack)?);
    Ok(Expr::Define { name, value })
}
```

**The problem:**
- Just creates an `Expr::Define` AST node
- **Does NOT update `scope_stack`**
- Later references to this variable won't find it

---

### 5. The `set!` Expression (WHERE THE BUG MANIFESTS)

**File:** `src/compiler/converters.rs`  
**Lines:** 831-859

```rust
"set!" => {
    if list.len() != 3 {
        return Err("set! requires exactly 2 arguments".to_string());
    }
    let var = list[1].as_symbol()?;
    let value =
        Box::new(value_to_expr_with_scope(&list[2], symbols, scope_stack)?);

    // Look up the variable in the scope stack to determine depth and index
    let mut depth = 0;
    let mut index = usize::MAX; // ← Use MAX to signal global variable

    for (reverse_idx, scope) in scope_stack.iter().enumerate().rev() {
        if let Some(local_index) = scope.iter().position(|sym| sym == &var) {
            depth = scope_stack.len() - 1 - reverse_idx;
            index = local_index;
            break;
        }
    }

    // If not found in local scopes (index == usize::MAX), it's a global variable set
    // ← ⚠️ THIS IS THE BUG: doesn't distinguish between "truly global" and "defined locally"

    Ok(Expr::Set {
        var,
        depth,
        index,
        value,
    })
}
```

**The bug trigger:**
- **Line 840-841:** Initialize `index = usize::MAX` (global signal)
- **Lines 843-849:** Search `scope_stack` for the variable
- If NOT found (because `define` didn't add it), `index` stays `usize::MAX`
- **Lines 851-858:** Creates `Expr::Set` with `index = usize::MAX` → interpreted as global mutation!

---

### 6. The `let*` Expression (BETTER SCOPING)

**File:** `src/compiler/converters.rs`  
**Lines:** 649-776

`let*` demonstrates better scoping practice:

```rust
"let*" => {
    // ... validation ...
    
    let bindings_vec = list[1].list_to_vec()?;
    
    if bindings_vec.is_empty() {
        // Handle empty case
    }

    let mut param_syms = Vec::new();
    let mut binding_exprs = Vec::new();
    
    // ✓ Push a new scope for let* bindings
    scope_stack.push(Vec::new());

    for binding in &bindings_vec {
        let binding_list = binding.list_to_vec()?;
        let var = binding_list[0].as_symbol()?;
        param_syms.push(var);

        // Parse binding expression WITH PREVIOUS BINDINGS IN SCOPE
        let expr = value_to_expr_with_scope(&binding_list[1], symbols, scope_stack)?;
        binding_exprs.push(expr);

        // ✓ Add this variable to scope for next binding
        if let Some(current_scope) = scope_stack.last_mut() {
            current_scope.push(var);
        }
    }

    let body_exprs: Result<Vec<_>, _> = list[2..]
        .iter()
        .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
        .collect();

    // ✓ Pop the let* scope
    scope_stack.pop();

    // ... rest of let* processing ...
}
```

**Better practice here:**
- **Line 682:** `scope_stack.push(Vec::new());` - PUSHES new scope
- **Line 701-703:** `scope_stack.last_mut().push(var);` - Updates scope with variables
- **Line 712:** `scope_stack.pop();` - POPS scope

`let*` correctly adds variables to scope as they're encountered!

---

## AST Structure Definitions

### The `Expr::Set` Variant

**File:** `src/compiler/ast.rs`  
**Lines:** 84-90

```rust
/// Set! (mutation)
Set {
    var: SymbolId,
    depth: usize,
    index: usize,
    value: Box<Expr>,
},
```

**Semantics:**
- `var`: Symbol being mutated
- `depth`: How many function scopes up (0 = current lambda)
- `index`: Position in that scope's environment
- When `index == usize::MAX`: Treated as global variable mutation

### The `Expr::Var` Variant

**File:** `src/compiler/ast.rs`  
**Lines:** 31-32

```rust
/// Variable reference (symbol, depth, index)
Var(SymbolId, usize, usize),
```

**Semantics:**
- First `usize`: depth (0 = current scope)
- Second `usize`: index in that scope

### The `Expr::GlobalVar` Variant

**File:** `src/compiler/ast.rs`  
**Lines:** 34-35

```rust
/// Global variable reference
GlobalVar(SymbolId),
```

No depth or index needed - just the symbol.

---

## Type Definitions

### Scope Stack Type

**File:** `src/compiler/converters.rs`  
**Documented at Line 331:**

```rust
/// The scope_stack contains local bindings (as Vec for ordering) at each nesting level
scope_stack: &mut Vec<Vec<SymbolId>>
```

Structure:
```
Vec<Vec<SymbolId>>
  ↓
  Outer Vec: Stack of scope levels
  Inner Vec: Variables in each scope (ordered for consistent indexing)

Example:
[[x, y], [a, b, c]]
  ↑      ↑
  |      └─ Parameters/bindings at level 1 (inner)
  └─────── Parameters at level 0 (outer)
```

---

## Tests That Would Fail With This Bug

**File:** `tests/integration/scoping.rs`

### Existing test (MIGHT FAIL):
**Lines 62-68:**
```rust
#[test]
fn test_define_in_while_loop_is_local() {
    let mut eval = ScopeEval::new();
    eval.eval("(define i 0)").unwrap();
    eval.eval("(while (< i 3) (begin (define temp (* i 2)) (set! i (+ i 1))))")
        .unwrap();
    let result = eval.eval("temp");
    assert!(result.is_err(), "Variable defined inside while loop should not leak");
}
```

Works because `i` is pre-defined globally, but would FAIL if you tried:
```lisp
(while (< i 3) (begin (define temp 0) (set! temp 42)))
```

### Test that SHOULD exist but doesn't:
```rust
#[test]
fn test_set_bang_on_locally_defined_variable() {
    let mut eval = ScopeEval::new();
    let result = eval.eval("((lambda () (begin (define x 0) (set! x 42) x)))").unwrap();
    assert_eq!(result, Value::Int(42), "Should set local x to 42");
}
```

This test would FAIL with the current implementation!

---

## Function Call Graph

```
value_to_expr() [line 323]
  └─> value_to_expr_with_scope() [line 332]
      ├─> [Symbol] → lookup in scope_stack [lines 346-360]
      ├─> [begin] → process all exprs [lines 442-448] ⚠️ NO SCOPE PUSH
      ├─> [lambda] → push scope, process, pop [lines 458-532] ✓
      ├─> [define] → create Define node [lines 535-542] ⚠️ NO SCOPE UPDATE
      ├─> [set!] → lookup var in scope_stack [lines 831-859] ← WHERE BUG MANIFESTS
      ├─> [let*] → push scope, add vars, process [lines 649-776] ✓
      └─> [... other special forms ...]
```

---

## Summary Table

| Element | File | Lines | Status | Issue |
|---------|------|-------|--------|-------|
| Entry function | converters.rs | 332-391 | OK | Uses scope_stack for all lookups |
| Symbol lookup | converters.rs | 346-360 | OK | Searches scope_stack correctly |
| `begin` handling | converters.rs | 442-448 | ⚠️ BUG | No scope management |
| `lambda` handling | converters.rs | 458-532 | ✓ OK | Pushes/pops scope correctly |
| `define` handling | converters.rs | 535-542 | ⚠️ BUG | Doesn't update scope_stack |
| `set!` handling | converters.rs | 831-859 | ⚠️ BUG | Treats unfound vars as global |
| `let*` handling | converters.rs | 649-776 | ✓ OK | Updates scope_stack correctly |
| `Expr::Set` type | ast.rs | 84-90 | OK | Correct structure |
| `Expr::Var` type | ast.rs | 31-32 | OK | Correct structure |
| Scope structures | scope.rs | 1-285 | OK | Structures exist but not used in converters.rs |

---

## The Fix Locations

To fix this bug, you would need to modify:

1. **`begin` handling** (lines 442-448)
   - Add `scope_stack.push(Vec::new());` at start
   - Make `define` statements add variables to the scope
   - Add `scope_stack.pop();` at end

2. **`define` handling** (lines 535-542)
   - If inside a block scope, add the variable to that scope
   - Only create `Expr::Define` for global defines

3. **OR: Implement a pre-analysis pass**
   - Scan `begin` blocks before processing
   - Collect all local definitions
   - Add them to scope_stack before processing expressions

4. **OR: Use the existing `scope.rs` module**
   - Replace the simple `Vec<Vec<SymbolId>>` with the richer `CompileScope` structure
   - Track definition types and binding information
