# N-Queens Problem Solver in Janet

(defn safe? [col queens]
  (var row-offset 1)
  (var idx 0)
  (while (< idx (length queens))
    (let [placed-col (queens idx)]
      (if (or (= col placed-col)
              (= row-offset (math/abs (- col placed-col))))
        (break false)))
    (set row-offset (+ row-offset 1))
    (set idx (+ idx 1)))
  true)

(defn solve-nqueens [n]
  (defn solve [row queens]
    (if (= row n)
      (let [result @[]]
        (array/push result (array/slice queens))
        result)
      (let [result @[]]
        (for col 0 n
          (when (safe? col queens)
            (let [new-queens (array/push (array/slice queens) col)]
              (array/concat result (solve (+ row 1) new-queens)))))
        result)))
  (solve 0 @[]))

(defn benchmark [n]
  (printf "Solving N-Queens for N=%d... " n)
  (let [solutions (solve-nqueens n)]
    (printf "Found %d solution(s)\n" (length solutions))))

(print "=== N-Queens Solver (Janet) ===\n\n")
(benchmark 4)
(benchmark 8)
(benchmark 10)
(benchmark 12)
(print "=== Complete ===\n")
