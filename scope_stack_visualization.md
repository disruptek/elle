# Visual Breakdown: How scope_stack Fails with begin/define/set!

## The Problematic Code
```lisp
(define test (lambda ()
  (begin
    (define x 0)    ; ← Creates local x
    (set! x 42)     ; ← Should mutate the x above
    x)))            ; ← Should reference the x above
```

---

## Parse Timeline with scope_stack State

### Initial State
```
scope_stack = []
```

### Step 1: Process `lambda`
```rust
// Lines 468-469 in converters.rs
scope_stack.push(param_syms.clone());
// param_syms = [] (no parameters)

scope_stack = [[]]
              ↑ Lambda scope (empty, no params)
```

### Step 2: Process `begin`
```rust
// Lines 442-448 in converters.rs
// "begin" => {
//   let exprs: Result<Vec<_>, _> = list[1..]
//       .iter()
//       .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
//       .collect();
//   Ok(Expr::Begin(exprs?))
// }

// ⚠️ NO SCOPE PUSH!

scope_stack = [[]]
              ↑ Still just lambda scope (no begin scope)
```

### Step 3a: Process `(define x 0)`
```rust
// Lines 535-542 in converters.rs
// "define" => {
//     let name = list[1].as_symbol()?;
//     let value = Box::new(value_to_expr_with_scope(&list[2], symbols, scope_stack)?);
//     Ok(Expr::Define { name, value })
// }

// The Define node is created with name='x'
// BUT 'x' is NOT added to scope_stack!

scope_stack = [[]]
              ↑ UNCHANGED - no tracking of 'x'!

Result: Expr::Define { name: x, value: Expr::Literal(0) }
```

### Step 3b: Process `(set! x 42)`

**Critical moment!** Let's trace the `set!` lookup:

```rust
// Lines 831-859 in converters.rs
"set!" => {
    let var = list[1].as_symbol()?;  // var = 'x'
    let value = Box::new(value_to_expr_with_scope(&list[2], symbols, scope_stack)?);
    
    // Look up the variable in the scope stack to determine depth and index
    let mut depth = 0;
    let mut index = usize::MAX;  // ← Defaults to MAX (global signal)
    
    for (reverse_idx, scope) in scope_stack.iter().enumerate().rev() {
        //                       ↑ scope_stack = [[]]
        //                       ↑ First iteration: reverse_idx=0, scope=[]
        
        if let Some(local_index) = scope.iter().position(|sym| sym == &var) {
            //                                         ↑ []
            //                                         ↑ Searching for 'x' in empty scope!
            //                                         ↑ NOT FOUND!
            depth = scope_stack.len() - 1 - reverse_idx;
            index = local_index;
            break;
        }
    }
    
    // Loop exits without finding 'x'
    // index is still usize::MAX ← ⚠️ GLOBAL SIGNAL!
    
    Ok(Expr::Set {
        var: x,
        depth: 0,          // (but index=MAX overrides this!)
        index: usize::MAX, // ← ⚠️ INTERPRETED AS GLOBAL!
        value: Expr::Literal(42),
    })
}
```

**Result:** `set! x 42` creates `Expr::Set { var: x, index: usize::MAX }` 
→ **Treated as global variable mutation!**

### Step 3c: Process `x` reference

Same problem:

```rust
// Lines 346-360 in converters.rs
Value::Symbol(id) => {  // id = 'x'
    for (reverse_idx, scope) in scope_stack.iter().enumerate().rev() {
        //                       ↑ scope_stack = [[]]
        if let Some(local_index) = scope.iter().position(|sym| sym == id) {
            //                    ↑ Searching for 'x' in []
            //                    ↑ NOT FOUND!
            let actual_depth = scope_stack.len() - 1 - reverse_idx;
            return Ok(Expr::Var(*id, actual_depth, local_index));
        }
    }
    Ok(Expr::GlobalVar(*id))  // ← ⚠️ TREATED AS GLOBAL!
}
```

**Result:** `x` creates `Expr::GlobalVar(x)` → **Treated as global variable reference!**

### Step 4: Exit `lambda`
```rust
// Lines 476-477 in converters.rs
scope_stack.pop();

scope_stack = []
```

---

## Comparison: Why Lambda Parameters Work

```lisp
((lambda (x) (set! x 42)) 10)
```

### Processing the lambda body:
```
scope_stack.push([x])  // ← Adds parameter 'x'
scope_stack = [[x]]
             ↑ Now 'x' is in the parameter scope!

When processing (set! x 42):
  for scope in scope_stack.iter().rev():
    scope = [x]
    position of 'x' in [x] = Some(0)  ← FOUND!
    index = 0
    
Result: Expr::Set { var: x, depth: 0, index: 0, ... }
→ Correctly identifies as LOCAL variable!
```

---

## What SHOULD Happen (The Fix)

For proper scoping, `begin` should work like `lambda`:

```rust
// Hypothetical fix
"begin" => {
    // Push a block scope to track locally-defined variables
    scope_stack.push(Vec::new());
    
    let mut block_scope = Vec::new();
    
    // First pass: collect all define statements
    for expr in &list[1..] {
        if let Value::Cons(_) = expr {
            if let Ok(expr_list) = expr.list_to_vec() {
                if !expr_list.is_empty() {
                    if let Value::Symbol(sym) = &expr_list[0] {
                        let name = symbols.name(*sym).ok_or("Unknown symbol")?;
                        if name == "define" && expr_list.len() == 3 {
                            let var = expr_list[1].as_symbol()?;
                            let index = block_scope.len();
                            block_scope.push(var);
                            // Also update scope_stack
                            if let Some(scope) = scope_stack.last_mut() {
                                scope.push(var);
                            }
                        }
                    }
                }
            }
        }
    }
    
    // Second pass: process expressions with updated scope_stack
    let exprs: Result<Vec<_>, _> = list[1..]
        .iter()
        .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
        .collect();
    
    scope_stack.pop();
    Ok(Expr::Begin(exprs?))
}
```

With this fix, the state would be:

```
Step 2 (modified): Process begin
  scope_stack.push(Vec::new())
  scope_stack = [[], []]  ← New block scope!

Step 3a: Process (define x 0)
  // First pass collects 'x'
  scope_stack.last_mut().push(x)
  scope_stack = [[], [x]]  ← 'x' is now in the block scope!

Step 3b: Process (set! x 42)
  // Search scope_stack
  for scope in [[], [x]].iter().rev():
    scope = [x]  ← First iteration finds 'x'!
    position = 0
  Result: Expr::Set { var: x, depth: 0, index: 0, ... }
  → Correctly identified as LOCAL!
```

---

## The Core Issue Summary

```
┌─────────────────────────────────────────────────────────────┐
│                    SCOPING MISMATCH                         │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  PARSE TIME (scope_stack):           RUNTIME (VM):          │
│  ─────────────────────────           ──────────────         │
│  Tracks:                             Tracks:                │
│  • Lambda parameters    ✓            • Lambda parameters ✓  │
│  • Let bindings         ✓            • Let bindings      ✓  │
│  • Block-local defines  ✗            • Block-local defs  ✓  │
│                                                              │
│  When set! looks up variables:                              │
│  "If not in scope_stack → treat as global" ✗               │
│  But runtime has local variable from define!               │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

The compiler doesn't know about variables defined with `define` at parse time, so it treats them as globals for `set!` and variable references.

But at runtime, those variables ARE local to the block where they're defined!

This creates a semantic mismatch between what the compiler thinks and what the runtime actually does.
