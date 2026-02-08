;; N-Queens Problem Solver in Common Lisp (SBCL)
;; Solves the classic N-Queens chess problem using backtracking
;; Tests: recursion, list building, functional predicates

;; Helper: Check if placing a queen at 'col' is safe given previously placed queens
;; queens is a list of column positions from previous rows (most recent first)
(defun safe? (col queens)
  (labels ((check-placement (row-offset placed-cols)
             (cond
               ((null placed-cols) t)  ;; All checked, no conflicts
               ((or (= col (car placed-cols))
                    (= row-offset (abs (- col (car placed-cols)))))
                nil)  ;; Conflict found
               (t
                (check-placement (+ row-offset 1) (cdr placed-cols))))))
    (check-placement 1 queens)))

;; Solve N-Queens: return list of solutions
;; Each solution is a list of column positions for each row
(defun solve-nqueens (n)
  (labels ((solve-from (row queens)
             (cond
               ((= row n)
                ;; Found a complete solution - reverse to get rows in order
                (list (reverse queens)))
               (t
                ;; Try placing a queen in each column of current row
                (labels ((try-cols (col)
                           (cond
                             ((= col n) nil)  ;; No more columns to try
                             ((safe? col queens)
                              ;; Safe to place here - combine solutions
                              (append (solve-from (+ row 1) (cons col queens))
                                      (try-cols (+ col 1))))
                             (t
                              ;; Not safe - skip this column
                              (try-cols (+ col 1))))))
                  (try-cols 0))))))
    (solve-from 0 nil)))

;; Benchmark: solve for various board sizes
(defun benchmark (n)
  (format t "Solving N-Queens for N=~A... " n)
  (let ((solutions (solve-nqueens n)))
    (format t "Found ~A solution(s)~%" (length solutions))))

;; Main
(format t "=== N-Queens Solver (SBCL) ===~%~%")

(benchmark 4)
(benchmark 8)
(benchmark 10)
(benchmark 12)

(format t "=== Complete ===~%")
