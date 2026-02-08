;; N-Queens Problem Solver in Chez Scheme
;; Solves the classic N-Queens chess problem using backtracking
;; Tests: recursion, list building, functional predicates

;; Helper: Check if placing a queen at 'col' is safe given previously placed queens
;; queens is a list of column positions from previous rows (most recent first)
(define safe?
  (lambda (col queens)
    (define check-placement
      (lambda (row-offset placed-cols)
        (cond
          ((null? placed-cols) #t)  ;; All checked, no conflicts
          ((or (= col (car placed-cols))
               (= row-offset (abs (- col (car placed-cols)))))
           #f)  ;; Conflict found
          (else
           (check-placement (+ row-offset 1) (cdr placed-cols))))))
    (check-placement 1 queens)))

;; Solve N-Queens: return list of solutions
;; Each solution is a list of column positions for each row
(define solve-nqueens
  (lambda (n)
    (let ((solve-from #f))
      (set! solve-from
        (lambda (row queens)
          (cond
            ((= row n)
             ;; Found a complete solution - reverse to get rows in order
             (list (reverse queens)))
            (else
             ;; Try placing a queen in each column of current row
             (let ((try-cols #f))
               (set! try-cols
                 (lambda (col)
                   (cond
                     ((= col n) '())  ;; No more columns to try
                     ((safe? col queens)
                      ;; Safe to place here - combine solutions
                      (append (solve-from (+ row 1) (cons col queens))
                              (try-cols (+ col 1))))
                     (else
                      ;; Not safe - skip this column
                      (try-cols (+ col 1))))))
               (try-cols 0))))))
      (solve-from 0 '()))))

;; Benchmark: solve for various board sizes
(define benchmark
  (lambda (n)
    (display "Solving N-Queens for N=")
    (display n)
    (display "... ")
    (let ((solutions (solve-nqueens n)))
      (display "Found ")
      (display (length solutions))
      (display " solution(s)")
      (newline))))

;; Main
(display "=== N-Queens Solver (Chez Scheme) ===")
(newline)
(newline)

(benchmark 4)
(benchmark 8)
(benchmark 10)
(benchmark 12)

(display "=== Complete ===")
(newline)
