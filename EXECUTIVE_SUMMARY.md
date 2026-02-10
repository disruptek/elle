# Executive Summary: Elle Lisp Scoping Bug Investigation

## The Problem

In the Elle Lisp compiler, the following code **incorrectly treats `x` as a global variable**:

```lisp
(define test (lambda ()
  (begin
    (define x 0)
    (set! x 42)
    x)))
```

**Expected behavior:** `set! x 42` should mutate the local variable `x` defined on the previous line.

**Actual behavior:** `set! x 42` treats `x` as a **global variable**, not the locally-defined one.

---

## Root Cause (In One Sentence)

The compiler's `scope_stack` (which tracks local variables at parse-time) **does not include variables defined with `define`** because `define` doesn't update `scope_stack`, and `begin` doesn't create a scope to track them.

---

## The Three-Part Bug

### 1. **`begin` Doesn't Push a Scope** (Line 442-448 in converters.rs)

```rust
"begin" => {
    let exprs: Result<Vec<_>, _> = list[1..]
        .iter()
        .map(|v| value_to_expr_with_scope(v, symbols, scope_stack))
        .collect();
    Ok(Expr::Begin(exprs?))
}
```

**The problem:** Unlike `lambda` (which does `scope_stack.push()` on line 469), `begin` doesn't create a new scope. All expressions are processed with the current `scope_stack`.

### 2. **`define` Doesn't Register Variables** (Line 535-542 in converters.rs)

```rust
"define" => {
    let name = list[1].as_symbol()?;
    let value = Box::new(value_to_expr_with_scope(&list[2], symbols, scope_stack)?);
    Ok(Expr::Define { name, value })
}
```

**The problem:** `define` just creates an AST node. It **never updates `scope_stack`** to record that the variable exists. This is why subsequent `set!` and variable references can't find it.

### 3. **`set!` Assumes Unfound Variables Are Global** (Line 831-859 in converters.rs)

```rust
"set!" => {
    let var = list[1].as_symbol()?;
    let value = Box::new(value_to_expr_with_scope(&list[2], symbols, scope_stack)?);
    
    let mut depth = 0;
    let mut index = usize::MAX;  // ← MAX = "treat as global"
    
    for (reverse_idx, scope) in scope_stack.iter().enumerate().rev() {
        if let Some(local_index) = scope.iter().position(|sym| sym == &var) {
            depth = scope_stack.len() - 1 - reverse_idx;
            index = local_index;
            break;
        }
    }
    
    // If not found (index still == usize::MAX), it's a global variable
    Ok(Expr::Set { var, depth, index, value })
}
```

**The problem:** When `set! x` searches `scope_stack` and `x` is not found (because `define x` didn't register it), the code sets `index = usize::MAX`, which is the compiler's signal for "this is a global variable". But we're not looking at a global variable—we're looking at a local variable that `define` didn't register!

---

## Visual Flow: Where It Breaks

```
┌─────────────────────────────────────────┐
│ Processing (begin (define x 0) ...)     │
├─────────────────────────────────────────┤
│                                         │
│ scope_stack = [[]]  (just lambda scope) │
│                                         │
├─────────────────────────────────────────┤
│ 1. Process (define x 0)                 │
│    • Creates Expr::Define node          │
│    • Does NOT update scope_stack        │
│    • scope_stack = [[]]  (UNCHANGED!)   │
│                                         │
├─────────────────────────────────────────┤
│ 2. Process (set! x 42)                  │
│    • Searches scope_stack for 'x'       │
│    • scope_stack = [[]]  (empty!)       │
│    • 'x' NOT FOUND                      │
│    • Sets index = usize::MAX            │
│    • ⚠️ Treats x as GLOBAL!             │
│                                         │
├─────────────────────────────────────────┤
│ 3. Process x reference                  │
│    • Searches scope_stack for 'x'       │
│    • scope_stack = [[]]  (empty!)       │
│    • 'x' NOT FOUND                      │
│    • Returns GlobalVar(x)               │
│    • ⚠️ Treats x as GLOBAL!             │
│                                         │
└─────────────────────────────────────────┘
```

---

## Why Lambda Parameters Work

When you use lambda parameters instead:

```lisp
((lambda (x) (set! x 42)) 0)
```

This works because:

```
1. Process lambda (x)
   scope_stack.push([x])
   scope_stack = [[x]]

2. Process (set! x 42)
   Searches scope_stack
   scope_stack = [[x]]
   'x' FOUND at index 0
   Creates Set { var: x, index: 0, ... }  ← LOCAL!
   ✓ Works correctly
```

---

## The Conceptual Issue

There's a fundamental mismatch:

```
PARSE-TIME scope_stack:          RUNTIME Execution:
─────────────────────────         ──────────────────
Tracks:                           Has access to:
• Lambda parameters ✓             • Lambda parameters ✓
• (nothing else)                  • Variables from (define) ✓
                                  • Variables from (let) ✓
                                  • Block-scoped locals ✓

When set! processes:
"If it's not in scope_stack, it must be global"
← WRONG for variables from (define)!
```

At runtime, the VM correctly handles locally-defined variables. But the compiler doesn't know about them, so it doesn't generate the right instructions.

---

## The Fix (High Level)

There are three possible approaches:

### Option A: Make `begin` Track Definitions
1. Have `begin` push a new scope on `scope_stack`
2. When processing `define x val`, add `x` to the current scope
3. When processing `set! x val` or referencing `x`, it will be found in the scope
4. Pop the scope when exiting `begin`

**Advantage:** Matches how `let*` already works (lines 649-776)  
**Disadvantage:** Requires tracking which scope a `define` belongs to

### Option B: Two-Pass Analysis
1. First pass: Scan all `define` statements in the block and collect variable names
2. Add those variables to `scope_stack`
3. Second pass: Process expressions normally, variables will be found
4. Pop the scope when done

**Advantage:** Cleaner separation of concerns  
**Disadvantage:** Requires scanning twice

### Option C: Use Pre-Existing Scope Infrastructure
1. The codebase has a complete `scope.rs` module (lines 1-285) with `CompileScope` structure
2. Replace the simple `Vec<Vec<SymbolId>>` with the richer `CompileScope` type
3. Use `define_local()` to register variables properly
4. The bug would be fixed by this infrastructure if it were used

**Advantage:** Leverages existing infrastructure  
**Disadvantage:** Requires refactoring `converters.rs` to use a different scope structure

---

## Specific Code Locations

| What | File | Lines | Issue |
|------|------|-------|-------|
| Main function | converters.rs | 332-391 | Uses scope_stack correctly but it's incomplete |
| `begin` bug | converters.rs | 442-448 | No scope push/pop |
| `lambda` correct | converters.rs | 458-532 | Properly pushes/pops |
| `define` bug | converters.rs | 535-542 | Doesn't update scope_stack |
| `set!` consequence | converters.rs | 831-859 | Finds nothing, treats as global |
| `let*` reference | converters.rs | 649-776 | Shows how to do it right |
| Scope structures | scope.rs | 1-285 | Infrastructure not used in converters.rs |
| Set AST node | ast.rs | 84-90 | index=usize::MAX means global |

---

## Example That Fails

```lisp
(define test (lambda ()
  (begin
    (define x 0)
    (set! x 42)
    x)))

(test)  ; Returns x as a global reference, not 42 as a local variable
```

This would fail because:
1. `(define x 0)` creates a local variable
2. `(set! x 42)` can't find `x` in scope_stack, treats it as global
3. Runtime error: trying to set a global variable that wasn't previously defined

---

## Example That Works (Correctly)

```lisp
((lambda (x)
  (begin
    (set! x 42)
    x))
 0)

; Returns 42
; Works because x is a lambda parameter, so it's in scope_stack
```

---

## Impact

This bug would affect any code that:
1. Uses `define` to create local variables in a `begin` block
2. Then uses `set!` to mutate those variables
3. Or references those variables later

Common patterns that would break:
- Initializing local state in a lambda with `begin` + `define`
- Loops that use local variables defined with `define`
- Any block-scoped mutable state

---

## Documents Generated

Three detailed investigation documents have been created:

1. **elle_scoping_investigation.md** - Full technical analysis
   - Detailed explanation of the bug
   - Root cause analysis
   - Test cases that demonstrate the issue
   - Scope stack timing diagram

2. **scope_stack_visualization.md** - Visual breakdown
   - Step-by-step trace through scope_stack state
   - Comparisons showing why lambda works
   - Diagram of the proposed fix
   - Core issue summary

3. **code_references.md** - Exact code locations
   - All relevant code sections quoted
   - Line numbers and file paths
   - Type definitions and structures
   - Function call graph
   - Summary table of all components

---

## Conclusion

The Elle Lisp compiler's variable scoping system works correctly for **lambda parameters** because they're registered in the `scope_stack` at parse time. However, it fails for **locally-defined variables** from `define` statements because:

1. `begin` doesn't create a scope to track them
2. `define` doesn't register variables in `scope_stack`
3. `set!` interprets unfound variables as globals

The fix requires making `begin` and `define` properly update `scope_stack` to match the runtime behavior where local variables are correctly tracked and mutated.
