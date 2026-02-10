# Elle's `for` Loop: Naming and Convention Analysis

## Executive Summary

After comprehensive research comparing iterator constructs across Janet, Clojure, Scheme, and Common Lisp, **we recommend KEEPING Elle's current `for` naming with NO CHANGES required**.

Elle's `for` is semantically equivalent to Janet's `for` (modern Lisp) and Clojure's `doseq` (eager side-effect iteration), making the current naming convention both modern and appropriate.

---

## Quick Reference Table

| Language | Iterator Name | Type | Eagerness | Returns | Multi-Seq Support |
|----------|--------------|------|-----------|---------|-------------------|
| **Janet** | **`for`** | Eager Loop | Eager | iterable | Nesting only |
| **Clojure** | **`for`** (lazy) | Comprehension | **Lazy** | Lazy seq | YES (native) |
| **Clojure** | **`doseq`** (eager) | Side-effect | Eager | nil | YES (native) |
| **Scheme** | **`for-each`** | Functional | Eager | Unspecified | YES (via args) |
| **Scheme** | **named-let** | Recursive | Eager | Result | Single + recursion |
| **Common Lisp** | **`dolist`** | Eager Loop | Eager | nil/result | Nesting only |
| **Common Lisp** | **`loop`** | Powerful DSL | Eager | Optional | YES (native) |
| **Elle** | **`for`** | Eager Loop | Eager | nil | Nesting only |

---

## Semantic Classification

Elle's `for` belongs to Category: **EAGER SIDE-EFFECT LOOP**

This category includes:
- ✓ Janet `for`
- ✓ Clojure `doseq`
- ✓ Common Lisp `dolist`
- ✓ Scheme `for-each` (when used for side effects)

Elle's `for` does NOT match:
- ✗ Clojure `for` (which is LAZY)
- ✗ Scheme approach (which uses lambda functions)

---

## Code Examples Across Languages

### Basic Iteration (Printing Numbers)

**Janet:**
```janet
(for i in [1 2 3]
  (print i))
```

**Elle (RECOMMENDED: Same pattern as Janet):**
```elle
(for i [1 2 3]
  (print i))
```

**Clojure (using `doseq` for side effects):**
```clojure
(doseq [i [1 2 3]]
  (println i))
```

**Clojure (using `for` - generates LAZY sequence, different semantics):**
```clojure
(for [i [1 2 3]]
  (inc i))  ; creates lazy sequence, doesn't print
```

**Scheme:**
```scheme
(for-each (lambda (i) (display i)) '(1 2 3))
```

**Common Lisp:**
```lisp
(dolist (i '(1 2 3))
  (print i))
```

### Multiple Sequence Iteration

**Janet:**
```janet
(for i in [1 2 3]
  (for j in [a b]
    (print [i j])))
```

**Elle (same nesting pattern):**
```elle
(for i [1 2 3]
  (for j [a b]
    (print [i j])))
```

**Clojure (native multi-seq support):**
```clojure
(doseq [i [1 2 3] j ['a 'b]]
  (println [i j]))
```

**Scheme:**
```scheme
(for-each (lambda (i j) (display [i j]))
          '(1 2 3) '(a b))
```

**Common Lisp:**
```lisp
(dolist (i '(1 2 3))
  (dolist (j '(a b))
    (print [i j])))
```

---

## Why NOT Rename to `doseq`?

While `doseq` would align with Clojure community expectations, it's **not recommended** because:

1. **Cognitive load:** Different from what users expect in modern Lisps
2. **Premature optimization:** We don't (yet) have a lazy `for` to differentiate from
3. **Community alignment:** Janet (most modern active Lisp) uses `for`
4. **User intuition:** "for item in list" is universally understood

### Example of Why Early Naming Matters

If we rename to `doseq` now:
```elle
(doseq i lst (print i))  ; confusing to Janet users
```

Then later if we add lazy sequences, we'd need:
```elle
(for [i lst] (+ i 1))      ; new meaning!
(doseq [i lst] (print i))  ; already means side-effects
```

By keeping `for` now, we preserve this path naturally.

---

## Why NOT Rename to `for-each`?

Traditional Lisp naming, but:

1. **Verbosity:** More characters without added clarity in Elle's context
2. **Modern convention:** Modern Lisps (Janet) trend toward `for`
3. **Consistency:** Elle is positioning as modern, not traditional
4. **Unpopular:** Scheme still exists but Janet is more actively developed

---

## Why NOT Rename to `dolist`?

Common Lisp tradition, but:

1. **Unintuitive:** Name doesn't clearly indicate "iterate"
2. **Not widely used:** Neither modern (Janet) nor increasingly popular (Clojure) Lisps use this
3. **Niche appeal:** Only relevant to Common Lisp experts

---

## Current Elle `for` Implementation Details

From the Elle codebase:

```rust
// src/compiler/converters.rs
"for" => {
    // Syntax: (for var iter body)
    // Also supports: (for var in iter body) for clarity
    if list.len() < 4 || list.len() > 5 {
        return Err(
            "for requires 3 or 4 arguments (var [in] iter body)".to_string()
        );
    }
    
    let var = list[1].as_symbol()?;
    let (iter_expr, body_expr) = if list.len() == 4 {
        // (for var iter body)
        (&list[2], &list[3])
    } else {
        // (for var in iter body) - optional 'in' keyword for clarity
        // ...
    };
    
    Ok(Expr::For { var, iter, body })
}
```

**Characteristics:**
- Supports both `(for item list body)` and `(for item in list body)` syntax
- Returns `nil` (side-effect iteration)
- Simple and clear semantics
- Exactly matches Janet's approach

---

## Recommendation Summary

### Primary Recommendation: **KEEP `for`**

**Score: 8/10**

**Rationale:**
1. ✓ Aligns with modern Lisp convention (Janet)
2. ✓ Intuitive and concise
3. ✓ No confusion in Elle's current context (no lazy sequences)
4. ✓ Natural growth path if lazy sequences added later
5. ✗ Slightly different from Clojure's `for` (which is lazy)

### Optional Future Enhancements (Not Required Now):

1. **Add `doseq` as alias** - if/when lazy `for` is introduced
2. **Documentation note** - explicitly state Elle's `for` is eager
3. **Comparison table in docs** - show how Elle aligns with other Lisps
4. **Consider `for-each` alias** - only if significant community feedback

---

## Implementation Notes

### Current Behavior (Correct):

```elle
(define result 0)
(for item [1 2 3]
  (set! result (+ result item)))
; result now equals 6
```

### Variable Scope (Current Elle Implementation):

Elle sets loop variables globally (implementation detail):
- Loop variable `item` is accessible in outer scopes
- This differs from Janet/Clojure (which use local bindings)
- Consider this in future scope management improvements

### What Works Well:

- Simple, readable syntax
- Single sequence iteration with clean semantics
- Works with side effects naturally
- Matches user expectations from other languages

---

## Hypothetical Future Path (Informational)

**IF Elle adds lazy sequences:**

```elle
; Current (eager, side-effects):
(for i [1 2 3]
  (print i))
; Returns: nil

; Hypothetical future (lazy, generates sequence):
(for [i [1 2 3]]
  (+ i 10))
; Returns: lazy sequence of [11, 12, 13]

; If we need to clarify side-effects:
(doseq [i [1 2 3]]
  (print i))
; Returns: nil
```

This would follow Clojure's model naturally, which is why keeping `for` now preserves this path.

---

## Conclusion

**Elle's `for` construct is already well-named and properly positioned within the Lisp family.**

No rename is necessary. The current naming:
1. Aligns with modern Lisp conventions (Janet)
2. Is intuitive and user-friendly
3. Provides a clear growth path for future enhancements
4. Matches community expectations for a modern Lisp

**RECOMMENDATION: Keep `for`, continue as-is, add documentation if needed.**

