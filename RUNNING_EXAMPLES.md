# Running Elle Examples

## ✅ Working Examples

Yes! Several examples **DO work** in the current Elle interpreter:

### 1. Arithmetic Operations
```bash
cd /home/adavidoff/git/elle
./target/debug/elle
```

Then in the REPL:
```lisp
> (+ 10 20)
⟹ 30

> (* 4 5)
⟹ 20

> (/ 20 4)
⟹ 5

> (exit)
```

### 2. List Operations
```lisp
> (list 1 2 3 4 5)
⟹ (1 2 3 4 5)

> (first (list 10 20 30))
⟹ 10

> (rest (list 10 20 30))
⟹ (20 30)

> (cons 5 (list 1 2 3))
⟹ (5 1 2 3)

> (length (list 1 2 3 4 5))
⟹ 5
```

### 3. Comparison Operations
```lisp
> (< 5 10)
⟹ true

> (> 10 5)
⟹ true

> (= 5 5)
⟹ true
```

## ⏳ Not Yet Working

The following features are **partially implemented** or **not yet ready**:

### ❌ Lambda/Closure Definitions
```lisp
> (define square (lambda (x) (* x x)))
✗ Runtime error: Closures not yet fully implemented
```

### ❌ Fibonacci Example
Requires `define` + `lambda` which aren't fully working yet:
```lisp
> (define fib (lambda (n) ...))
✗ Not yet implemented
```

### ❌ FFI Examples (Phase 3-5)
These require FFI primitives to be registered:
- `examples/gtk4-app.l` - Needs GUI infrastructure
- `examples/sdl2-game.l` - Needs SDL2 integration
- `examples/llvm-compiler.l` - Needs LLVM integration
- `examples/gtk4-bindings.l` - Needs header parsing
- `examples/sdl2-bindings.l` - Needs header parsing

## How to Run Elle

### Start the Interactive REPL
```bash
cd /home/adavidoff/git/elle
./target/debug/elle
```

### Send Commands via Stdin
```bash
(
echo "(+ 2 3)"
echo "(list 1 2 3)"
echo "(exit)"
) | ./target/debug/elle
```

### Automated Testing
```bash
# Run all tests (254 passing)
cargo test

# Run FFI tests specifically
cargo test --lib ffi

# Run individual test file
cargo test --test ffi_tests
```

## Working Primitives

✅ **Arithmetic:**
- `+` - Addition
- `-` - Subtraction
- `*` - Multiplication
- `/` - Division
- `%` - Modulo

✅ **Comparison:**
- `<` - Less than
- `>` - Greater than
- `<=` - Less than or equal
- `>=` - Greater than or equal
- `=` - Equal

✅ **List Operations:**
- `list` - Create list
- `cons` - Prepend to list
- `first` - Get first element
- `rest` - Get rest of list
- `length` - Get list length

✅ **I/O:**
- `display` - Print value
- `newline` - Print newline
- `print` - Print with formatting

✅ **Type Operations:**
- `type` - Get type name
- `vector` - Create vector

✅ **Control:**
- `if` - Conditional
- `and` - Logical AND
- `or` - Logical OR
- `not` - Logical NOT

## Partially Working

⏳ **Closures/Lambdas:**
- `lambda` - Function definition (basic parsing works, evaluation not complete)
- `define` - Variable definition (works for primitives, not for closures)

## Not Yet Implemented

❌ **Pattern Matching:**
- `match` - Pattern matching
- `cond` - Conditional branches

❌ **Loops:**
- `while` - While loop
- `for` - For loop

❌ **FFI (Phase 3-5):**
- `load-library` - Load .so files
- `call-c-function` - Call C functions
- `make-c-callback` - Create C callbacks
- `load-header-with-lib` - Parse C headers

## Running the Examples Interactively

### Test Arithmetic
```bash
./target/debug/elle
> (+ 1 2 3 4 5)
⟹ 15
> (exit)
```

### Test List Operations
```bash
./target/debug/elle
> (cons 0 (list 1 2 3))
⟹ (0 1 2 3)
> (exit)
```

### Test Comparisons
```bash
./target/debug/elle
> (and (< 5 10) (> 5 3))
⟹ true
> (exit)
```

## Next Steps for Full Example Support

To make all examples runnable:

1. **Complete Lambda Implementation** (needed for fibonacci.l)
   - Evaluate lambda expressions
   - Closure variable binding
   - Recursive function calls

2. **Integrate FFI Primitives** (needed for GTK4, SDL2, LLVM)
   - Register primitives with VM
   - Implement callback registry
   - Handle special forms

3. **Add Pattern Matching** (needed for some examples)
   - Implement `match` form
   - Support cond branches

4. **Add Loop Support** (needed for game loops)
   - Implement `while` loops
   - Implement `for` loops

## Current Implementation Status

| Feature | Status | Example |
|---------|--------|---------|
| Arithmetic | ✅ Working | `(+ 2 3)` → 5 |
| Lists | ✅ Working | `(list 1 2 3)` → (1 2 3) |
| Comparisons | ✅ Working | `(< 5 10)` → true |
| Logic | ✅ Working | `(and true false)` → false |
| I/O | ✅ Working | `(display "hi")` |
| Lambda | ⏳ Partial | `(lambda (x) x)` parses but doesn't evaluate |
| Define | ⏳ Partial | `(define x 5)` works, `(define f ...)` doesn't |
| Closures | ❌ Not working | Needs implementation |
| Recursion | ❌ Not working | Needs closures |
| Pattern match | ❌ Not working | Needs implementation |
| Loops | ❌ Not working | Needs implementation |
| FFI | ❌ Not working | Needs VM integration |

## Summary

**Right now you can:**
- ✅ Run simple arithmetic and list operations
- ✅ Use built-in primitives like `+`, `-`, `list`, `first`, etc.
- ✅ Use the interactive REPL

**You cannot yet:**
- ❌ Define and call custom functions (closures not ready)
- ❌ Run fibonacci or other recursive examples
- ❌ Load and call C libraries (FFI not integrated)
- ❌ Use pattern matching or advanced control flow

**To enable more examples**, the Lambda/Closure implementation and FFI integration would need to be completed.
