#!/usr/bin/env elle

; Threading Operators Demo
; ========================
; This example demonstrates the -> and ->> threading operators
; inspired by Clojure and Janet Lisp.

; -> (thread-first): Inserts the value as the FIRST argument to each form
; Example: (-> 5 (+ 10) (* 2)) expands to (* (+ 5 10) 2) = 30

; ->> (thread-last): Inserts the value as the LAST argument to each form
; Example: (->> 5 (+ 10) (* 2)) expands to (* 2 (+ 10 5)) = 30

(define (demo-thread-first)
  (prn "=== Thread-First (->) Examples ===")
  
  ; Simple arithmetic chain
  (prn "Simple chain: (-> 5 (+ 10) (* 2))")
  (prn "Expected: 30, Got: " (-> 5 (+ 10) (* 2)))
  
  ; Multiple arguments
  (prn "\nWith multiple args: (-> 5 (+ 10 2) (* 3))")
  (prn "Expected: 51, Got: " (-> 5 (+ 10 2) (* 3)))
  
  ; Longer chain
  (prn "\nLonger chain: (-> 1 (+ 1) (+ 1) (+ 1))")
  (prn "Expected: 4, Got: " (-> 1 (+ 1) (+ 1) (+ 1)))
  
  ; With list operations
  (prn "\nWith lists: (-> (list 1 2 3) (length))")
  (prn "Expected: 3, Got: " (-> (list 1 2 3) (length)))
  
  ; Nested operations
  (prn "\nNested: (-> 10 (- 3) (+ 5))")
  (prn "Evaluation: (+ (- 10 3) 5) = (+ 7 5) = 12")
  (prn "Got: " (-> 10 (- 3) (+ 5)))
)

(define (demo-thread-last)
  (prn "\n=== Thread-Last (->>) Examples ===")
  
  ; Simple arithmetic chain
  (prn "Simple chain: (->> 5 (+ 10) (* 2))")
  (prn "Expected: 30, Got: " (->> 5 (+ 10) (* 2)))
  
  ; Multiple arguments
  (prn "\nWith multiple args: (->> 2 (+ 10) (* 3))")
  (prn "Expected: 36, Got: " (->> 2 (+ 10) (* 3)))
  
  ; Longer chain
  (prn "\nLonger chain: (->> 1 (+ 1) (+ 1) (+ 1))")
  (prn "Expected: 4, Got: " (->> 1 (+ 1) (+ 1) (+ 1)))
  
  ; With list operations
  (prn "\nWith lists: (->> (list 1 2 3) (length))")
  (prn "Expected: 3, Got: " (->> (list 1 2 3) (length)))
  
  ; Nested operations with order difference
  (prn "\nNested: (->> 10 (- 3) (+ 5))")
  (prn "Evaluation: (+ 5 (- 3 10)) = (+ 5 -7) = -2")
  (prn "Got: " (->> 10 (- 3) (+ 5)))
)

(define (demo-comparison)
  (prn "\n=== Thread-First vs Thread-Last Comparison ===")
  
  ; Show how the same threading path gives different results
  ; with different operators
  
  (prn "Value: 3")
  (prn "Operations: (- 1), (+ 2)")
  
  (prn "\nThread-first: (-> 3 (- 1) (+ 2))")
  (prn "  = (+ (- 3 1) 2)")
  (prn "  = (+ 2 2)")
  (prn "  = 4")
  (prn "Result: " (-> 3 (- 1) (+ 2)))
  
  (prn "\nThread-last: (->> 3 (- 1) (+ 2))")
  (prn "  = (+ 2 (- 1 3))")
  (prn "  = (+ 2 -2)")
  (prn "  = 0")
  (prn "Result: " (->> 3 (- 1) (+ 2)))
)

(define (main)
  (demo-thread-first)
  (demo-thread-last)
  (demo-comparison)
  (prn "\n=== Demo Complete ===")
)

(main)
