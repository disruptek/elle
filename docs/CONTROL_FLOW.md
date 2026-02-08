# Control Flow in Elle

A comprehensive guide to control flow constructs, exception handling, and the condition system in Elle.

## Table of Contents

1. [Conditionals](#conditionals)
2. [Loops](#loops)
3. [Exception Handling](#exception-handling)
4. [The Condition System](#the-condition-system)
5. [Control Flow Best Practices](#control-flow-best-practices)

---

## Conditionals

### if - The Basic Conditional

`if` is the fundamental conditional expression:

```lisp
(if condition then-branch else-branch)
```

The else branch is optional:

```lisp
(if condition then-branch)
```

**Examples:**

```lisp
(if (> 10 5)
  "10 is greater"
  "5 is greater")
⟹ "10 is greater"

(if #t
  (display "This prints"))
⟹ nil (else branch omitted, returns nil)

(if #f
  "not shown"
  "shown")
⟹ "shown"
```

### cond - Multiple Conditions

`cond` evaluates a series of conditions and returns the value of the first true branch:

```lisp
(cond
  (condition1 expression1)
  (condition2 expression2)
  ...
  (#t fallback-expression))
```

**Examples:**

```lisp
(define x 15)
(cond
  ((> x 20) "x is large")
  ((> x 10) "x is medium")
  ((> x 5)  "x is small")
  (#t       "x is tiny"))
⟹ "x is medium"
```

The final `(#t ...)` clause acts as a catch-all (equivalent to `else`):

```lisp
(cond
  ((number? x) "It's a number")
  ((string? x) "It's a string")
  (#t          "It's something else"))
```

### when - Execute if True

`when` executes a body if and only if a condition is true:

```lisp
(when condition
  expression1
  expression2
  ...)
```

Returns `nil` if condition is false, otherwise returns the value of the last expression.

**Examples:**

```lisp
(when (> 10 5)
  (display "10 is greater")
  (newline))

(define result (when #t 42))
result ⟹ 42

(define result (when #f 42))
result ⟹ nil
```

### unless - Execute if False

`unless` is the opposite of `when` - executes if condition is false:

```lisp
(unless condition
  expression1
  expression2
  ...)
```

**Examples:**

```lisp
(unless (< 5 10)
  (display "5 is not less than 10"))
⟹ nil (condition is true, so body doesn't run)

(unless (< 10 5)
  (display "10 is not less than 5")
  (newline))
⟹ prints message, returns nil
```

### do/begin - Compound Expressions

`do` (or `begin`) groups multiple expressions together, returning the value of the last:

```lisp
(do
  expression1
  expression2
  ...
  final-expression)
```

**Examples:**

```lisp
(do
  (display "First")
  (newline)
  (display "Second")
  (newline)
  (+ 2 2))
⟹ 4 (returns value of last expression)

(define result (do
  (set! x 10)
  (set! y 20)
  (+ x y)))
result ⟹ 30
```

---

## Loops

### loop - Infinite Loop with break

`loop` creates an infinite loop that continues until explicitly broken:

```lisp
(loop
  body...
  (when condition (break)))
```

**Examples:**

```lisp
(define count 0)
(loop
  (display count)
  (newline)
  (set! count (+ count 1))
  (when (>= count 3)
    (break)))
```

Output:
```
0
1
2
```

### while - Conditional Loop

`while` repeats a body while a condition remains true:

```lisp
(while condition
  body...)
```

**Examples:**

```lisp
(define x 0)
(while (< x 3)
  (display x)
  (newline)
  (set! x (+ x 1)))
```

Output:
```
0
1
2
```

More complex example:

```lisp
(define lst (list 1 2 3 4 5))
(define sum 0)
(define remaining lst)

(while (not (nil? remaining))
  (set! sum (+ sum (first remaining)))
  (set! remaining (rest remaining)))

sum ⟹ 15
```

### for - List Iteration

`for` iterates over elements of a list:

```lisp
(for (item list)
  body...)
```

**Examples:**

```lisp
(for (item (list 'a 'b 'c))
  (display item)
  (newline))
```

Output:
```
a
b
c
```

With accumulation:

```lisp
(define sum 0)
(for (n (list 1 2 3 4 5))
  (set! sum (+ sum n)))

sum ⟹ 15
```

### break and continue

`break` exits a loop immediately:

```lisp
(loop
  (when (> x 10) (break))
  (display x)
  (set! x (+ x 1)))
```

`continue` skips to the next iteration:

```lisp
(for (n (list 1 2 3 4 5))
  (when (= n 3)
    (continue))
  (display n)
  (newline))
```

Output:
```
1
2
4
5
```

---

## Exception Handling

### try-catch - Basic Error Handling

Use `try` to wrap potentially failing code and `catch` to handle exceptions:

```lisp
(try
  risky-expression
  (catch (e)
    handle-error))
```

**Examples:**

```lisp
(try
  (/ 10 0)
  (catch (e)
    (display "Division by zero!")
    (newline)))
```

The catch block receives the exception value:

```lisp
(define result (try
  (throw (exception "Bad input" (table "type" "validation")))
  (catch (e)
    (exception-message e))))

result ⟹ "Bad input"
```

### finally - Cleanup Code

`finally` executes regardless of success or failure:

```lisp
(try
  body
  (catch (e)
    handle-error)
  (finally
    cleanup-code))
```

Execution order:
1. body executes
2. If exception, catch block runs
3. finally always runs
4. Result from body (or catch) is returned

**Examples:**

```lisp
(define result (try
  (display "Body")
  (newline)
  42
  (catch (e)
    (display "Caught: ")
    (display e)
    (newline)
    0)
  (finally
    (display "Finally")
    (newline))))

result ⟹ 42
```

Output:
```
Body
Finally
```

With an exception:

```lisp
(try
  (throw "Error!")
  (catch (e)
    (display "Caught: ")
    (display e))
  (finally
    (display " - Cleanup"))))
```

Output:
```
Caught: Error! - Cleanup
```

### Creating and Throwing Exceptions

Use `exception` to create exception values:

```lisp
(define e (exception "Error message" data))
```

The data parameter can be any value (typically a table with context):

```lisp
(define e (exception "Database error"
  (table
    "code" 500
    "query" "SELECT * FROM users"
    "retry" #t)))
```

Use `throw` to raise an exception:

```lisp
(throw e)
(throw (exception "Simple error" nil))
(throw "String exceptions also work")
```

Extract information from exceptions:

```lisp
(define e (exception "Test" (table "x" 42)))
(exception-message e)  ⟹ "Test"
(exception-data e)     ⟹ #<table String("x")=42>
```

---

## The Condition System

The condition system is a more sophisticated approach to error handling than simple exceptions. It allows defining custom signal types with handlers that can respond gracefully.

### Defining Conditions

Define a condition type with `define-condition`:

```lisp
(define-condition :condition-name
  (field1 "default-value-1")
  (field2 "default-value-2")
  ...)
```

**Examples:**

```lisp
(define-condition :validation-error
  (message "Validation failed")
  (field "unknown")
  (value nil))

(define-condition :network-error
  (message "Network failed")
  (url "")
  (status-code 0)
  (retry-count 0))
```

### Registering Handlers

Register a handler for a condition with `define-handler`:

```lisp
(define-handler :condition-name
  (lambda (condition)
    handler-body))
```

Multiple handlers can be registered for the same condition - they're called in order:

```lisp
(define-handler :validation-error
  (lambda (c)
    (display "Handler 1: ")
    (display (condition-get c 'message))
    (newline)))

(define-handler :validation-error
  (lambda (c)
    (display "Handler 2: ")
    (display (condition-get c 'field))
    (newline)))
```

### Signaling Conditions

Use `signal` to trigger a condition:

```lisp
(signal :condition-name
  :field1 value1
  :field2 value2
  ...)
```

All registered handlers are called in the order they were defined:

```lisp
(signal :validation-error
  :message "Email format is invalid"
  :field "email"
  :value "not-an-email")
```

Output (if handlers are registered):
```
Handler 1: Email format is invalid
Handler 2: email
```

### Catching Conditions

Use `catch-condition` to intercept a specific condition:

```lisp
(catch-condition :condition-name
  (signal-body)
  (lambda (condition)
    handler-body))
```

**Examples:**

```lisp
(catch-condition :validation-error
  (signal :validation-error
    :message "Invalid input"
    :field "username")
  (lambda (c)
    (display "Caught and handled: ")
    (display (condition-get c 'message'))
    (newline)))
```

### Generic Condition Catching

Use `condition-catch` to handle any condition:

```lisp
(condition-catch
  (signal-body)
  (lambda (condition-type condition-data)
    handler-body))
```

**Examples:**

```lisp
(condition-catch
  (begin
    (signal :validation-error :message "Bad email")
    (signal :network-error :message "No connection"))
  (lambda (type data)
    (display "Caught ")
    (display type)
    (display ": ")
    (display (condition-get data 'message))
    (newline)))
```

### Condition Objects

Access condition fields with `condition-get`:

```lisp
(define c (signal :validation-error
  :message "Invalid"
  :field "email"))

(condition-get c 'message) ⟹ "Invalid"
(condition-get c 'field)   ⟹ "email"
```

### Practical Example: Input Validation

```lisp
; Define validation error condition
(define-condition :validation-error
  (message "Validation failed")
  (field "unknown")
  (constraint "unknown"))

; Register handlers
(define-handler :validation-error
  (lambda (c)
    (display "VALIDATION ERROR: ")
    (display (condition-get c 'field))
    (display " - ")
    (display (condition-get c 'message))
    (newline)))

(define-handler :validation-error
  (lambda (c)
    (display "  (constraint: ")
    (display (condition-get c 'constraint))
    (display ")")
    (newline)))

; Validation function
(define (validate-email email)
  (unless (string-contains? email "@")
    (signal :validation-error
      :message "Missing @ symbol"
      :field "email"
      :constraint "must contain @")))

; Use it
(validate-email "invalid-email")
```

Output:
```
VALIDATION ERROR: email - Missing @ symbol
  (constraint: must contain @)
```

---

## Control Flow Best Practices

### 1. Prefer `cond` for Multiple Conditions

Instead of nested `if`:

```lisp
; Good
(cond
  ((null? x) "empty")
  ((> x 0) "positive")
  ((< x 0) "negative")
  (#t "zero"))

; Avoid
(if (null? x)
  "empty"
  (if (> x 0)
    "positive"
    (if (< x 0)
      "negative"
      "zero")))
```

### 2. Use `when` and `unless` for Single-Branch Conditions

```lisp
; Good
(when (valid? input)
  (process input))

; Less clear
(if (valid? input)
  (process input)
  nil)
```

### 3. Use Loops for Side Effects, Higher-Order Functions for Data

```lisp
; For iteration with side effects
(for (item items)
  (display item)
  (newline))

; For data transformation
(define doubled (map (lambda (x) (* x 2)) items))
```

### 4. Use the Condition System for Expected Errors

Use exceptions for truly exceptional cases, conditions for expected error scenarios:

```lisp
; Good: Expected validation error
(define-condition :invalid-input
  (message "Input validation failed")
  (field "unknown"))

; Less good: Using exceptions for validation
(throw (exception "Input validation failed" nil))
```

### 5. Always Use try-finally for Resource Cleanup

If you open resources, ensure they're closed:

```lisp
(define file (slurp "data.txt"))
(try
  (process file)
  (finally
    (close file)))
```

### 6. Separate Concerns with catch-condition

```lisp
; Good: Handle specific conditions
(catch-condition :network-error
  (fetch-data)
  (lambda (c)
    (retry (condition-get c 'url))))

(catch-condition :validation-error
  (validate-input)
  (lambda (c)
    (prompt-user-to-fix (condition-get c 'field))))

; Less good: Generic exception handling
(try
  (do
    (fetch-data)
    (validate-input))
  (catch (e)
    ; Now we have to parse the error ourselves
    ...))
```

---

## Summary

| Construct | Use Case | Returns |
|-----------|----------|---------|
| `if` | Simple true/false choice | Any value |
| `cond` | Multiple conditions | First true branch value |
| `when` | Execute if true | Value or nil |
| `unless` | Execute if false | Value or nil |
| `do/begin` | Multiple expressions | Last expression value |
| `loop` | Infinite loop | Value after break |
| `while` | Conditional loop | nil |
| `for` | List iteration | nil |
| `try-catch` | Exception handling | Caught value or exception |
| `finally` | Cleanup code | Always executes |
| Condition system | Sophisticated error handling | Handler result |
