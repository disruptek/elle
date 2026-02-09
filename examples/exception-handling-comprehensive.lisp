;;;; Exception Handling in Elle Lisp
;;;; Comprehensive demonstration of try/catch and handler-case
;;;; 
;;;; Elle provides two levels of exception handling:
;;;; 1. try/catch/finally - High-level, user-friendly (Phase 10)
;;;; 2. handler-case     - Low-level, fine-grained control (Phase 9a)

;;;; ============================================================================
;;;; Part 1: Try/Catch/Finally (User-Friendly)
;;;; ============================================================================

(display "=== Try Without Catch ===\n")
(display (try (+ 10 20)))
(display "\n")

(display "\n=== Try With Catch ===\n")
(display (try (/ 10 0) (catch e 99)))
(display "\n")

(display "\n=== Try With Catch and Finally ===\n")
(display "Running: (try (/ 10 0) (catch e 99) (finally ...))\n")
(display (try (/ 10 0) (catch e 99) (finally 0)))
(display "\n")

(display "\n=== Try Success Ignores Catch ===\n")
(display "Result: ")
(display (try (+ 5 3) (catch e 999)))
(display " (should be 8, not 999)\n")

(display "\n=== Finally Always Executes ===\n")
(display "Success path - finally executes:\n")
(try 
  (display "  Body executed\n")
  (finally (display "  Finally executed\n")))

(display "\nException path - finally executes:\n")
(try 
  (/ 10 0)
  (catch e (display "  Exception caught\n"))
  (finally (display "  Finally executed\n")))

(display "\n=== Try/Catch Returns Different Types ===\n")
(display "Number: ")
(display (try 42 (catch e 0)))
(display "\n")

(display "String: ")
(display (try (/ 10 0) (catch e "error")))
(display "\n")

(display "Boolean: ")
(display (try #f (catch e #t)))
(display "\n")

(display "\n=== Nested Try/Catch ===\n")
(display (try 
  (try 
    (/ 10 0)
    (catch e1 50))
  (catch e2 100)))
(display "\n")

;;;; ============================================================================
;;;; Part 2: Handler-Case (Low-Level Control)
;;;; ============================================================================

(display "\n=== Handler-Case Basics ===\n")
(display "Basic handler-case: ")
(display (handler-case (/ 10 0) (4 e 99)))
(display "\n")

(display "No exception: ")
(display (handler-case 42 (4 e 0)))
(display "\n")

(display "\n=== Handler Arithmetic ===\n")
(display "Handler with computation: ")
(display (handler-case (/ 10 0) (4 e (+ 50 49))))
(display "\n")

(display "\n=== Nested Handler-Case ===\n")
(display (handler-case 
  (handler-case 
    (/ 10 0)
    (4 e 50))
  (4 e 100)))
(display "\n")

(display "\n=== Handler-Case With Complex Body ===\n")
(display (handler-case 
  (+ (* 2 5) (* 3 4))
  (4 e 0)))
(display "\n")

;;;; ============================================================================
;;;; Part 3: Exception ID Reference
;;;; ============================================================================

(display "\n=== Exception ID 4 (Arithmetic Errors) ===\n")
(display "Division by zero caught: ")
(display (try (/ 10 0) (catch e 0)))
(display "\n")

(display "Safe division: ")
(display (try (/ 100 10) (catch e 0)))
(display "\n")

(display "Multiplication (no exception): ")
(display (try (* 5 6) (catch e 0)))
(display "\n")

;;;; ============================================================================
;;;; Part 4: Exception Handling Patterns
;;;; ============================================================================

(display "\n=== Pattern: Default Value Fallback ===\n")
(display "10 / 2 = ")
(display (try (/ 10 2) (catch e 0)))
(display "\n")

(display "10 / 0 = ")
(display (try (/ 10 0) (catch e -1)))
(display "\n")

(display "\n=== Pattern: Cleanup Code ===\n")
(display "With exception:\n")
(try
  (/ 10 0)
  (catch e (display "  Error caught\n"))
  (finally (display "  Cleanup code executed\n")))

(display "\nWithout exception:\n")
(try
  (display "  Normal execution\n")
  (finally (display "  Cleanup code executed\n")))

(display "\n=== Pattern: Optional Value ===\n")
(display "Success case (10 / 2): ")
(display (try (/ 10 2) (catch e nil)))
(display "\n")

(display "Error case (10 / 0): ")
(display (try (/ 10 0) (catch e nil)))
(display "\n")

;;;; ============================================================================
;;;; Part 5: Composite Operations
;;;; ============================================================================

(display "\n=== Arithmetic Sequences ===\n")
(display "Sequential operations: ")
(display (+ 
  (try (/ 20 4) (catch e 0))
  (try (+ 10 5) (catch e 0))))
(display "\n")

(display "\n=== Conditional with Try/Catch ===\n")
(if (= (try (/ 10 0) (catch e 99)) 99)
  (display "Exception was caught\n")
  (display "No exception occurred\n"))

(display "\n=== Composite Let Binding ===\n")
(let ((a (try (/ 20 4) (catch e 0)))
      (b (try (+ 10 5) (catch e 0))))
  (display "a = ")
  (display a)
  (display ", b = ")
  (display b)
  (display ", a + b = ")
  (display (+ a b))
  (display "\n"))

;;;; ============================================================================
;;;; Part 6: Summary and Key Points
;;;; ============================================================================

(display "\n=== Exception Handling Summary ===\n")
(display "Elle provides two levels:\n")
(display "  1. try/catch/finally - High-level, user-friendly\n")
(display "  2. handler-case      - Low-level, direct control\n")
(display "\nKey features:\n")
(display "  - Both catch arithmetic exceptions (ID 4)\n")
(display "  - Nested exception handling works correctly\n")
(display "  - Stack integrity is maintained\n")
(display "  - finally blocks always execute\n")
(display "  - Exception handling preserves values\n")
(display "  - Can use catch variables (e) in handlers\n")
