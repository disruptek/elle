;; Elle Assertions Library
;;
;; Shared assertion helpers for all examples. Load this file with:
;;   ; Load shared assertions library
;;
;; Functions:
;;   - assert-eq(actual, expected, msg)
;;     Assert that actual equals expected (using = for numbers, eq? for symbols)
;;   - assert-true(val, msg)
;;     Assert that val is #t
;;   - assert-false(val, msg)
;;     Assert that val is #f
;;   - assert-list-eq(actual, expected, msg)
;;     Assert that two lists are equal (same length and elements)
;;
;; All assertions crash with exit code 1 on failure, making examples
;; act as contracts for the implementation.

(define assert-eq (fn (actual expected msg)
  "Assert that actual equals expected (using = for numbers, eq? for symbols)"
  (let ((matches
    (if (symbol? expected)
        (eq? actual expected)
        (= actual expected))))
    (if matches
        #t
        (begin
          (display "FAIL: ")
          (display msg)
          (display "\n  Expected: ")
          (display expected)
          (display "\n  Actual: ")
          (display actual)
          (display "\n")
          (exit 1))))))

(define assert-true (fn (val msg)
  "Assert that val is #t"
  (assert-eq val #t msg)))

(define assert-false (fn (val msg)
  "Assert that val is #f"
  (assert-eq val #f msg)))

(define assert-list-eq (fn (actual expected msg)
  "Assert that two lists are equal (same length and elements)"
  (if (= (length actual) (length expected))
      ; Check each element - use a simple loop approach
      (let ((check-all (fn (index)
        (if (>= index (length actual))
            #t
            (if (if (symbol? (nth index expected))
                    (eq? (nth index actual) (nth index expected))
                    (= (nth index actual) (nth index expected)))
                (check-all (+ index 1))
                (begin
                  (display "FAIL: ")
                  (display msg)
                  (display "\n  Element at index ")
                  (display index)
                  (display " differs\n  Expected: ")
                  (display (nth index expected))
                  (display "\n  Actual: ")
                  (display (nth index actual))
                  (display "\n")
                  (exit 1)))))))
        (check-all 0))
      (begin
        (display "FAIL: ")
        (display msg)
        (display "\n  Expected length: ")
        (display (length expected))
        (display "\n  Actual length: ")
        (display (length actual))
        (display "\n")
        (exit 1)))))
