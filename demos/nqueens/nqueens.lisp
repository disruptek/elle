;; N-Queens Problem Solver in Elle
;; Solves the classic N-Queens chess problem using backtracking
;; Tests: recursion, list building, functional predicates

(begin

(display "=== N-Queens Solver (Elle) ===")
(newline)
(newline)

;; Helper: Check if placing a queen at 'col' is safe given previously placed queens
;; queens is a list of column positions from previous rows (most recent first)
(define safe?
  (lambda (col queens)
    (define check-placement
      (lambda (row-offset placed-cols)
        (cond
          ((= (length placed-cols) 0) #t)  ;; All checked, no conflicts
          ((or (= col (first placed-cols))
               (= row-offset (abs (- col (first placed-cols)))))
           #f)  ;; Conflict found
          (#t
           (check-placement (+ row-offset 1) (rest placed-cols))))))
    (check-placement 1 queens)))

;; Solve N-Queens: return list of solutions
;; Each solution is a list of column positions for each row
(define solve-nqueens
  (lambda (n)
    (define solve-from
      (lambda (row queens)
        (cond
          ((= row n)
           ;; Found a complete solution - reverse to get rows in order
           (list (reverse queens)))
          (#t
           ;; Try placing a queen in each column of current row
           (begin
             (define try-cols
               (lambda (col)
                 (cond
                   ((= col n) (list))  ;; No more columns to try
                   ((safe? col queens)
                    ;; Safe to place here - combine solutions from this and other columns
                    (append (solve-from (+ row 1) (cons col queens))
                            (try-cols (+ col 1))))
                   (#t
                    ;; Not safe - skip this column
                    (try-cols (+ col 1))))))
             (try-cols 0))))))
    (solve-from 0 (list))))

;; Benchmark: solve for various board sizes
(define benchmark
  (lambda (n)
    (display "Solving N-Queens for N=")
    (display n)
    (display "... ")
    (define solutions (solve-nqueens n))
    (display "Found ")
    (display (length solutions))
    (display " solution(s)")
    (newline)))

;; Main
(benchmark 4)
(benchmark 8)
(benchmark 10)
(benchmark 12)

(display "=== Complete ===")
(newline)

) ;; End begin
