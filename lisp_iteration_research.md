# Lisp Family Iterator Constructs - Comprehensive Comparison

## 1. JANET

**Primary Iterator Construct: `(for pattern in iterable body)`**

- **Also supports:** `do`, `loop`, `each`
- **Eagerness:** Eager execution with side effects
- **Multiple Sequences:** Single sequence iterator (can nest multiple `for` loops)
- **Returns:** The `iterable` value (not the result of body)
- **Typical Use Case:** Side-effect-based iteration over sequences
- **Modern/Idiomatic:** YES - `for` is the primary modern construct
- **Syntax Example:**
  ```janet
  (for i in (range 10)
    (print i))
  ```

---

## 2. CLOJURE

**Primary Iterator Constructs: `(for ...)` and `(doseq ...)` (distinct purposes)**

### For Comprehensions (`for`)
- **Eagerness:** LAZY - Returns a lazy sequence
- **Multiple Sequences:** YES - Strong support for multiple sequences
- **Returns:** Lazy sequence of results
- **Typical Use Case:** Declarative sequence generation
- **Syntax Example:**
  ```clojure
  (for [i (range 10) j (range 3)] [i j])  ; generates [i,j] pairs
  ```

### Doseq (`doseq`)
- **Eagerness:** EAGER - Executes for side effects only
- **Multiple Sequences:** YES - Supports multiple sequences
- **Returns:** `nil`
- **Typical Use Case:** Side-effect iteration
- **Syntax Example:**
  ```clojure
  (doseq [i (range 10)] (println i))
  ```

### Also supports:
- `map`, `reduce`, `filter` (functional style)
- `dotimes` (numeric iteration)

**Idiomatic:** YES - Both are standard; `for` for results, `doseq` for effects

---

## 3. SCHEME

**Primary Iterator Constructs: `(for-each ...)`, `(do ...)`, named-let**

### For-Each (`for-each`)
- **Eagerness:** EAGER execution
- **Multiple Sequences:** YES - Can iterate over multiple sequences
- **Returns:** Unspecified (typically `#t` or void)
- **Typical Use Case:** Side-effect iteration
- **Syntax Example:**
  ```scheme
  (for-each (lambda (x) (display x)) '(1 2 3))
  ```

### Named-Let
- **Eagerness:** EAGER (recursive)
- **Multiple Sequences:** Single variable with recursion
- **Returns:** Result of final expression
- **Typical Use Case:** Recursive iteration with accumulation
- **Syntax Example:**
  ```scheme
  (let loop ((i 0))
    (if (< i 10)
      (begin (display i) (loop (+ i 1)))))
  ```

### Also supports:
- `do` - General iteration construct
- `map` (functional style)

**Idiomatic:** YES - Both `for-each` and named-let are standard idioms

---

## 4. COMMON LISP

**Primary Iterator Constructs: `(dolist ...)`, `(loop ...)`, `(iterate ...)`**

### DOLIST
- **Eagerness:** EAGER execution
- **Multiple Sequences:** Single sequence (nested for multiple)
- **Returns:** Result of optional final form or `nil`
- **Typical Use Case:** Simple iteration over lists
- **Syntax Example:**
  ```lisp
  (dolist (item '(1 2 3))
    (print item))
  ```

### LOOP
- **Eagerness:** EAGER execution
- **Multiple Sequences:** YES - Full support
- **Returns:** Optional result value
- **Typical Use Case:** Complex iteration with multiple clauses
- **Syntax Example:**
  ```lisp
  (loop for i from 0 below 10 do (print i))
  (loop for i from 0 below 10 collect i)  ; accumulates results
  ```

### ITERATE (library-based, CL-ITERATE)
- **Eagerness:** EAGER or LAZY depending on driver
- **Multiple Sequences:** YES - Strong support
- **Returns:** Varies (collect, accumulate, etc.)
- **Typical Use Case:** Powerful DSL-like iteration

### Note: NO "for" in standard Common Lisp!
- Common Lisp eschewed `for` in favor of `loop` and `dolist`
- `for` might exist in libraries but is non-standard

**Idiomatic:** YES - `dolist` and `loop` are both idiomatic

---

## Comparison Matrix

| Language | Primary Name | Eagerness | Multi-Seq | Returns | Standard? |
|----------|-------------|-----------|-----------|---------|-----------|
| **Janet** | `for` | Eager | Single (nesting) | iterable | ✓ |
| **Clojure** | `for` (lazy) | **Lazy** | **YES** | Lazy seq | ✓ |
| **Clojure** | `doseq` (effects) | Eager | **YES** | nil | ✓ |
| **Scheme** | `for-each` | Eager | YES | Unspec | ✓ |
| **Scheme** | named-let | Eager | Single (recursion) | Result | ✓ |
| **Common Lisp** | `dolist` | Eager | Single (nesting) | Result | ✓ |
| **Common Lisp** | `loop` | Eager | YES | Optional | ✓ |

---

## Key Observations

### Naming Conventions
1. **NOT typically named `for`:**
   - Common Lisp: Uses `dolist` and `loop` (NOT `for`)
   - Scheme: Uses `for-each` (NOT `for`)
   - Janet: DOES use `for` (modern approach)

2. **Uses `for`:**
   - Clojure: `for` (lazy comprehension) - but also has `doseq` for side effects
   - Janet: `for` (eager iteration)

### Semantic Differences
- **Clojure is unique:** `for` is LAZY (generates sequences), `doseq` is EAGER (effects)
- **Janet/Scheme/CL:** Iterator constructs are EAGER with side effects
- **Common Lisp:** Avoids `for` entirely, uses `dolist` for simplicity and `loop` for power

### Community Conventions
- **Prefix naming:** `for-each`, `do-*`, `dolist` indicate procedural/side-effect iteration
- **No prefix:** `for` in Clojure (lazy), Janet (eager)
- **Loop keyword:** Common Lisp's `loop` is a powerful macro with domain-specific syntax

---

## What About Elle's `for`?

**Current Elle Implementation:**
```elle
(for item list
  (+ item 1))
```

**Analysis:**
- Pattern matches **Janet** and **Clojure's `doseq`** most closely
- Eager execution for side effects ✓
- Single sequence iteration ✓
- Returns `nil` ✓
- Simple, readable syntax ✓

**Comparison to Standards:**
- ✗ Common Lisp would use: `(dolist (item list) ...)` or `(loop for item in list do ...)`
- ✗ Scheme would use: `(for-each (lambda (item) ...) list)`
- ✓ Janet uses exactly: `(for item in list ...)`
- ✓ Clojure uses `doseq` for this pattern: `(doseq [item list] ...)`

---

## Recommendation for Elle

### Option 1: Keep `for` (RECOMMENDED)
**Pros:**
- Aligns with modern Lisp (Janet)
- Concise and readable
- Similar to Clojure's `doseq` semantics
- Simple one-liner like other Lisps

**Cons:**
- Technically breaks from Common Lisp tradition
- Scheme tradition uses `for-each`

### Option 2: Rename to `doseq` (Good, following Clojure)
**Pros:**
- Aligns with Clojure community
- Explicitly indicates "side effects" semantics
- If Elle later adds lazy `for`, can maintain Clojure-like distinction

**Cons:**
- Less familiar to non-Clojure developers
- Longer name for a simple construct

### Option 3: Rename to `for-each` (Following Scheme)
**Pros:**
- Aligns with Scheme/standard Lisp traditions
- Explicitly descriptive

**Cons:**
- Longer naming
- Less common in modern lisps (Janet doesn't use it)

### Option 4: Rename to `dolist` (Following Common Lisp)
**Pros:**
- Aligns with most "traditional" Lisp

**Cons:**
- Less intuitive naming
- Doesn't align with modern Lisps

---

## FINAL RECOMMENDATION

### **KEEP `for` - NO RENAME NEEDED**

**Rationale:**
1. **Modern alignment:** Janet (the most modern Lisp in active development) uses `for`
2. **Simplicity:** Matches user expectations better than `dolist` or `for-each`
3. **Consistency with other Lisps:** Functionally equivalent to Clojure's `doseq` and Janet's `for`
4. **Clear semantics in context:** In Elle's current design, without lazy sequences, `for` for eager iteration is unambiguous
5. **Growth path:** If Elle adds lazy sequences later, can add `for` (lazy) and `doseq` (eager) like Clojure

**However, consider adding:**
- Documentation explicitly stating: "Elle's `for` is eager and executes for side effects"
- Parallel support for `foreach` or `for-each` as aliases if community feedback suggests it
- Future `for` with lazy semantics IF lazy sequences are added

---

## Code Example Comparison

```janet
; Janet
(for i in (range 10)
  (print i))
```

```clojure
; Clojure (side effects)
(doseq [i (range 10)]
  (println i))

; Clojure (lazy, creates sequence)
(for [i (range 10)]
  (+ i 1))
```

```scheme
; Scheme
(for-each (lambda (i) (display i)) (range 10))
```

```commonlisp
; Common Lisp
(dolist (i (list 0 1 2 3 4 5 6 7 8 9))
  (print i))
```

```elle
; Elle (RECOMMENDED: keep as-is)
(for i (range 10)
  (print i))
```

