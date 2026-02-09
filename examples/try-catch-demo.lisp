;;;; Exception Handling Demo - Try/Catch/Finally
;;;; 
;;;; This demonstrates the user-friendly try/catch mechanism
;;;; for handling exceptions in Elle Lisp

(display "=== Try/Catch Basic Example ===\n")

;; Simple division that works
(display "Safe division: 10 / 2 = ")
(display (try (/ 10 2) (catch e 0)))
(display "\n")

;; Division by zero caught
(display "Division by zero: 10 / 0 = ")
(display (try (/ 10 0) (catch e -1)))
(display "\n")

;; Try without catch
(display "\nNo exception, no catch needed:\n")
(display "Result: ")
(display (try (+ 5 3)))
(display "\n")

;; Catch not executed when no exception
(display "\nCatch clause is skipped when no exception:\n")
(display "Try (+ 5 3) with catch e 999: ")
(display (try (+ 5 3) (catch e 999)))
(display "\n")

;; Different return types
(display "\nException handlers can return different types:\n")
(display "Number on error: ")
(display (try (/ 10 0) (catch e 99)))
(display "\n")

(display "String on error: ")
(display (try (/ 10 0) (catch e "error")))
(display "\n")

;; Finally block
(display "\nFinally blocks always execute:\n")
(display "With exception:\n")
(try 
  (/ 10 0)
  (catch e (display "  Caught!\n"))
  (finally (display "  Cleanup\n")))

(display "\nWithout exception:\n")
(try 
  (display "  Running\n")
  (finally (display "  Cleanup\n")))

;; Nested try/catch
(display "\n=== Nested Try/Catch ===\n")
(display "Inner handles exception, outer doesn't run:\n")
(display (try 
  (try (/ 10 0) (catch inner 50))
  (catch outer 100)))
(display "\n")

;; Multiple operations
(display "\n=== Sequence with Error Handling ===\n")
(let ((a (try (+ 5 3) (catch e 0)))
      (b (try (/ 20 4) (catch e 0)))
      (c (try (/ 10 0) (catch e -1))))
  (display "a = ")
  (display a)
  (display ", b = ")
  (display b)
  (display ", c = ")
  (display c)
  (display "\n")
  (display "sum = ")
  (display (+ a b c))
  (display "\n"))
