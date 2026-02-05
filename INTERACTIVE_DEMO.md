# Interactive Elle Demo

## Quick Start

Run Elle and try these commands:

```bash
cd /home/adavidoff/git/elle
./target/debug/elle
```

## Commands to Try

### Arithmetic
```lisp
> (+ 5 3)
⟹ 8

> (* 10 4)
⟹ 40

> (/ 20 4)
⟹ 5

> (- 100 25)
⟹ 75
```

### Lists
```lisp
> (list 1 2 3 4 5)
⟹ (1 2 3 4 5)

> (first (list 10 20 30))
⟹ 10

> (rest (list 10 20 30))
⟹ (20 30)

> (cons 0 (list 1 2 3))
⟹ (0 1 2 3)

> (length (list 1 2 3 4 5))
⟹ 5
```

### Comparisons
```lisp
> (< 5 10)
⟹ true

> (> 10 5)
⟹ true

> (= 7 7)
⟹ true

> (<= 5 5)
⟹ true
```

### Logic
```lisp
> (and true true)
⟹ true

> (or true false)
⟹ true

> (not false)
⟹ true

> (and (< 5 10) (> 10 5))
⟹ true
```

### Conditionals
```lisp
> (if true 1 2)
⟹ 1

> (if (< 5 10) "yes" "no")
⟹ "yes"

> (if (and true false) 100 200)
⟹ 200
```

## Demo Session

Here's a complete demo you can copy-paste:

```lisp
(display "=== Elle Interpreter Demo ===")
(newline)
(newline)

(display "Arithmetic: (+ 5 3) = ")
(display (+ 5 3))
(newline)

(display "Multiply: (10 * 4) = ")
(display (* 10 4))
(newline)
(newline)

(display "Create a list: ")
(display (list 1 2 3 4 5))
(newline)

(display "Get first element: ")
(display (first (list 10 20 30)))
(newline)

(display "Get rest of list: ")
(display (rest (list 10 20 30)))
(newline)
(newline)

(display "Test comparison: (< 5 10)? ")
(display (< 5 10))
(newline)

(display "Test logic: (and true false)? ")
(display (and true false))
(newline)
(newline)

(display "Test conditional: ")
(display (if (> 10 5) "10 is greater" "5 is greater"))
(newline)

(display "Done!")
(newline)
(exit)
```

## What Works ✅

- Basic arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparisons: `<`, `>`, `<=`, `>=`, `=`
- Logic: `and`, `or`, `not`
- Lists: `list`, `cons`, `first`, `rest`, `length`
- Conditionals: `if`
- I/O: `display`, `newline`, `print`
- Types: `type`, `vector`

## What Doesn't Work Yet ❌

- `define` with lambdas: `(define f (lambda (x) ...))`
- Recursive functions
- Pattern matching with `match`
- Loops (`while`, `for`)
- FFI functions (C library integration)

## Running in Batch Mode

To run multiple commands without the interactive prompt:

```bash
(
echo "(+ 1 2)"
echo "(list 1 2 3)"
echo "(exit)"
) | ./target/debug/elle 2>&1 | grep "⟹"
```

Output:
```
> ⟹ 3
> ⟹ (1 2 3)
```

## Tips

1. **Display output:** Use `(display value)` to print results
2. **Newlines:** Use `(newline)` after display to add line breaks
3. **Nested operations:** You can nest expressions: `(+ (* 2 3) 4)` → 10
4. **Type checking:** Use `(type value)` to get the type of a value

Example:
```lisp
> (type 42)
⟹ "integer"

> (type (list 1 2 3))
⟹ "list"

> (type "hello")
⟹ "string"
```

## Summary

Elle currently works great for:
- ✅ Interactive calculation
- ✅ List processing
- ✅ Logical operations
- ✅ Basic data manipulation

It's perfect for learning Lisp basics and practicing functional programming!

Enjoy! 🎉
