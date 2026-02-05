;; Mutual Recursion Patterns
;; Demonstrates functions that call each other in cycles
;; Elle supports mutual recursion through top-level define pre-declaration

(display "=== Mutual Recursion Patterns ===")
(newline)
(newline)

;; ============================================================================
;; Part 1: Even/Odd Predicates - The Classic Example
;; ============================================================================

(display "Part 1: Even/Odd Predicates")
(newline)
(display "Classic mutual recursion example")
(newline)
(newline)

(define is-even
  (lambda (n)
    (if (= n 0)
      #t
      (is-odd (- n 1)))))

(define is-odd
  (lambda (n)
    (if (= n 0)
      #f
      (is-even (- n 1)))))

(display "Testing even/odd predicates:")
(newline)
(display "is-even(0): ")
(display (is-even 0))
(newline)
(display "is-even(4): ")
(display (is-even 4))
(newline)
(display "is-even(7): ")
(display (is-even 7))
(newline)
(display "is-odd(1): ")
(display (is-odd 1))
(newline)
(display "is-odd(5): ")
(display (is-odd 5))
(newline)
(display "is-odd(8): ")
(display (is-odd 8))
(newline)
(newline)

;; ============================================================================
;; Part 2: Countdown with Two Functions
;; ============================================================================

(display "Part 2: Countdown with Two Functions")
(newline)
(display "Two mutually recursive counters")
(newline)
(newline)

(define count-down-a
  (lambda (n)
    (if (= n 0)
      (display "A: Done!")
      (begin
        (display "A: ")
        (display n)
        (newline)
        (count-down-b (- n 1))))))

(define count-down-b
  (lambda (n)
    (if (= n 0)
      (display "B: Done!")
      (begin
        (display "B: ")
        (display n)
        (newline)
        (count-down-a (- n 1))))))

(count-down-a 6)
(newline)
(newline)

;; ============================================================================
;; Part 3: Tree Traversal with Multiple Functions
;; ============================================================================

(display "Part 3: Tree Traversal")
(newline)
(display "Process different node types with mutual recursion")
(newline)
(newline)

;; Helper to check if something is a list
(define is-list? (lambda (x) (not (= 0 0))))  ; Always true for lists in this example

(define process-tree
  (lambda (tree)
    (if (= tree 0)
      0
      (+ (process-value tree) (process-children (rest tree))))))

(define process-value
  (lambda (tree)
    (if (= tree 0)
      0
      (first tree))))

(define process-children
  (lambda (children)
    (if (= (length children) 0)
      0
      (+ (process-tree (first children))
         (process-children (rest children))))))

;; Test with a simple structure: (1 (2 (3)))
(display "Simple tree sum: ")
(display (process-tree (list 1 (list 2 (list 3)))))
(newline)
(newline)

;; ============================================================================
;; Part 4: String Processing with Mutual Recursion
;; ============================================================================

(display "Part 4: String Processing")
(newline)
(display "Process words and phrases with mutual recursion")
(newline)
(newline)

(define process-words
  (lambda (words)
    (if (= (length words) 0)
      ""
      (string-append
        (string-upcase (first words))
        " "
        (process-separators (rest words))))))

(define process-separators
  (lambda (words)
    (if (= (length words) 0)
      ""
      (string-append
        "-"
        (process-words words)))))

(display "Processing: ")
(display (process-words (list "hello" "world" "elle")))
(newline)
(newline)

;; ============================================================================
;; Part 5: Factorial with Helper - Mutual Style
;; ============================================================================

(display "Part 5: Factorial with Helper Function")
(newline)
(display "Mutual recursion pattern for factorial computation")
(newline)
(newline)

(define factorial
  (lambda (n)
    (factorial-helper n 1)))

(define factorial-helper
  (lambda (n acc)
    (if (= n 0)
      acc
      (factorial-helper (- n 1) (* acc n)))))

(display "factorial(5): ")
(display (factorial 5))
(newline)
(display "factorial(7): ")
(display (factorial 7))
(newline)
(newline)

;; ============================================================================
;; Part 6: Three-Way Mutual Recursion
;; ============================================================================

(display "Part 6: Three-Way Mutual Recursion")
(newline)
(display "Three functions calling each other")
(newline)
(newline)

(define func-a
  (lambda (n)
    (if (= n 0)
      "A-done"
      (func-b (- n 1)))))

(define func-b
  (lambda (n)
    (if (= n 0)
      "B-done"
      (func-c (- n 1)))))

(define func-c
  (lambda (n)
    (if (= n 0)
      "C-done"
      (func-a (- n 1)))))

(display "func-a(5): ")
(display (func-a 5))
(newline)
(newline)

;; ============================================================================
;; Part 7: Listing Functions (Even/Odd List)
;; ============================================================================

(display "Part 7: Filtering with Mutual Recursion")
(newline)
(display "Separate even and odd numbers from a list")
(newline)
(newline)

(define separate-numbers
  (lambda (nums)
    (separate-helper nums (list) (list))))

(define separate-helper
  (lambda (nums evens odds)
    (if (= (length nums) 0)
      (list evens odds)
      (if (is-even (first nums))
        (separate-helper (rest nums) (append evens (list (first nums))) odds)
        (separate-helper (rest nums) evens (append odds (list (first nums))))))))

(display "Input: (1 2 3 4 5 6)")
(newline)
(define separated (separate-numbers (list 1 2 3 4 5 6)))
(display "Evens: ")
(display (first separated))
(newline)
(display "Odds: ")
(display (first (rest separated)))
(newline)
(newline)

;; ============================================================================
;; Part 8: Mutual Reference with Counting
;; ============================================================================

(display "Part 8: Counting Pattern")
(newline)
(display "Mutual recursion with accumulated results")
(newline)
(newline)

(define count-up
  (lambda (n max)
    (if (> n max)
      0
      (+ 1 (count-down (- n 1) max)))))

(define count-down
  (lambda (n max)
    (if (> n max)
      0
      (+ 1 (count-up (+ n 1) max)))))

(display "count-up(1, 5): ")
(display (count-up 1 5))
(newline)
(newline)

;; ============================================================================
;; Summary
;; ============================================================================

(display "=== Summary ===")
(newline)
(display "Mutual recursion patterns demonstrated:")
(newline)
(display "1. Even/odd predicates - classic example")
(newline)
(display "2. Alternating countdown - back and forth calls")
(newline)
(display "3. Tree traversal - processing nested structures")
(newline)
(display "4. String processing - manipulating text data")
(newline)
(display "5. Factorial with helper - accumulation pattern")
(newline)
(display "6. Three-way recursion - circular function groups")
(newline)
(display "7. List filtering - conditional logic in recursion")
(newline)
(display "8. Counting patterns - mutual accumulation")
(newline)
(newline)

(display "=== Mutual Recursion Patterns Complete ===")
(newline)
