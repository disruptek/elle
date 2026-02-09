#!/usr/bin/env elle
;; Mutable Closures: Using set! on locally-defined variables inside lambda bodies
;; 
;; This example demonstrates the fix for issue #106: set! now works correctly
;; on variables defined with (define ...) inside lambda bodies.
;;
;; Key patterns:
;; 1. Basic mutation of local variables
;; 2. Using set! with loops for imperative iteration
;; 3. Accumulator patterns with mutation
;; 4. Stateful functions (closures with internal state)

;; ============================================================================
;; Pattern 1: Basic Local Mutation
;; ============================================================================

(display "=== Pattern 1: Basic Local Mutation ===\n")

;; Simple counter that increments a local variable
(define simple-counter (lambda ()
  (begin
    (define count 0)
    (set! count (+ count 1))
    (set! count (+ count 1))
    (set! count (+ count 10))
    count)))

(display "simple-counter result: ")
(display (simple-counter))
(display "\n")

;; ============================================================================
;; Pattern 2: While Loop with Mutation (Imperative Style)
;; ============================================================================

(display "\n=== Pattern 2: While Loop with Mutation ===\n")

;; Count from 1 to n using a while loop
(define count-to (lambda (n)
  (begin
    (define i 0)
    (define result (list))
    (while (< i n)
      (begin
        (set! i (+ i 1))
        (set! result (append result (list i)))))
    result)))

(display "count-to 5: ")
(display (count-to 5))
(display "\n")

;; ============================================================================
;; Pattern 3: Accumulator with Multiple Variables
;; ============================================================================

(display "\n=== Pattern 3: Accumulator Pattern ===\n")

;; Sum and count in one pass using mutation
(define sum-and-count (lambda (numbers)
  (begin
    (define sum 0)
    (define count 0)
    (while (> (length numbers) 0)
      (begin
        (set! sum (+ sum (car numbers)))
        (set! count (+ count 1))
        (set! numbers (cdr numbers))))
    (list sum count))))

(define nums (list 1 2 3 4 5))
(display "sum-and-count [1,2,3,4,5]: ")
(display (sum-and-count nums))
(display "\n")

;; ============================================================================
;; Pattern 4: Stateful Functions (Classic Closure Pattern)
;; ============================================================================

(display "\n=== Pattern 4: Stateful Closures ===\n")

;; Create a "bank account" with balance mutation
(define make-account (lambda (initial-balance)
  (begin
    (define balance initial-balance)
    (lambda (amount)
      (begin
        (set! balance (+ balance amount))
        balance)))))

(define account1 (make-account 1000))
(display "Created account with $1000\n")
(display "Deposit $100: ")
(display (account1 100))
(display "\n")
(display "Withdraw $250: ")
(display (account1 -250))
(display "\n")
(display "Check balance (deposit $50): ")
(display (account1 50))
(display "\n")

;; ============================================================================
;; Pattern 5: Nested Lambdas with Local Mutation
;; ============================================================================

(display "\n=== Pattern 5: Nested Lambdas ===\n")

;; Outer function has mutable state, inner function modifies it
(define make-incrementer (lambda (step)
  (begin
    (define current 0)
    (lambda ()
      (begin
        (set! current (+ current step))
        current)))))

(define inc-by-2 (make-incrementer 2))
(define inc-by-5 (make-incrementer 5))

(display "Incrementer by 2: ")
(display (inc-by-2))
(display " ")
(display (inc-by-2))
(display " ")
(display (inc-by-2))
(display "\n")

(display "Incrementer by 5: ")
(display (inc-by-5))
(display " ")
(display (inc-by-5))
(display "\n")

;; ============================================================================
;; Pattern 6: String Building with Mutation
;; ============================================================================

(display "\n=== Pattern 6: String Building ===\n")

;; Build a string character by character
(define build-string (lambda (chars)
  (begin
    (define result "")
    (while (> (length chars) 0)
      (begin
        (set! result (string-append result (car chars)))
        (set! chars (cdr chars))))
    result)))

(define letters (list "H" "e" "l" "l" "o"))
(display "Built string: ")
(display (build-string letters))
(display "\n")

;; ============================================================================
;; Summary
;; ============================================================================

(display "\n=== Summary ===\n")
(display "All patterns above now work correctly thanks to the fix for issue #106.\n")
(display "Key points:\n")
(display "- (define x 0) inside a lambda creates a mutable local variable\n")
(display "- (set! x value) modifies that variable in subsequent code\n")
(display "- This enables imperative patterns (while loops with mutation)\n")
(display "- Closures can maintain internal state using this pattern\n")
