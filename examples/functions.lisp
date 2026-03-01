#!/usr/bin/env elle

# Functions in Elle — from basics to composition
#
# Demonstrates:
#   defn / fn          — named and anonymous functions
#   Lexical scope      — let, let*, letrec, shadowing
#   Closures           — capturing environment, mutable state
#   Higher-order fns   — map, filter, fold via recursion
#   Composition        — compose, ->, ->> pipelines
#   Variadic functions — & rest parameters
#   Mutual recursion   — letrec with even?/odd?
#   block / break      — named blocks with early exit
#   Mutable captures   — shared cells between closures

(import-file "./examples/assertions.lisp")


# ========================================
# 1. defn and fn
# ========================================

(print "=== defn and fn ===")

# defn is sugar for (def name (fn ...))
(defn square [x]
  "Return x squared."
  (* x x))

(assert-eq (square 5) 25 "defn: square")

# Anonymous function — fn without a name
(def cube (fn [x] (* x x x)))
(assert-eq (cube 3) 27 "fn: cube")

# Docstrings are the first string literal in a function body.
# (doc "name") retrieves it.
(assert-eq (doc "square") "Return x squared." "doc retrieves docstring")


# ========================================
# 2. Lexical scope
# ========================================

(print "=== lexical scope ===")

# let — parallel bindings (can't reference each other)
(let ((a 10) (b 20))
  (assert-eq (+ a b) 30 "let: parallel bindings"))

# let* — sequential bindings (each sees the previous)
(let* ((a 10) (b (+ a 5)))
  (assert-eq b 15 "let*: sequential binding"))

# letrec — bindings can reference each other (for mutual recursion)
(letrec ((f (fn [n] (if (= n 0) 1 (* n (f (- n 1)))))))
  (assert-eq (f 5) 120 "letrec: self-recursive factorial"))

# Shadowing — inner binding hides outer
(def x 100)
(let ((x 42))
  (assert-eq x 42 "shadowing: inner x"))
(assert-eq x 100 "shadowing: outer x unchanged")

# let* shadowing within the same form
(let* ((z 10) (z (+ z 5)))
  (assert-eq z 15 "let*: sequential shadow"))


# ========================================
# 3. Closures
# ========================================

(print "=== closures ===")

# Pure closure — captures immutable value
(defn make-adder [n]
  "Return a function that adds n to its argument."
  (fn [x] (+ x n)))

(def add5 (make-adder 5))
(def add10 (make-adder 10))
(assert-eq (add5 3) 8 "closure: add5")
(assert-eq (add10 3) 13 "closure: add10")

# Mutable closure — counter over a captured var
(defn make-counter []
  "Return a function that increments and returns a count."
  (var count 0)
  (fn []
    (set count (+ count 1))
    count))

(def c1 (make-counter))
(def c2 (make-counter))
(assert-eq (c1) 1 "counter c1: first call")
(assert-eq (c1) 2 "counter c1: second call")
(assert-eq (c2) 1 "counter c2: independent state")


# ========================================
# 4. Higher-order functions
# ========================================

(print "=== higher-order functions ===")

# my-map: apply f to each element of a list
(defn my-map [f lst]
  "Apply f to each element of lst, returning a new list."
  (if (empty? lst)
    (list)
    (cons (f (first lst)) (my-map f (rest lst)))))

(assert-list-eq (my-map square (list 1 2 3 4)) (list 1 4 9 16) "my-map: square")
(assert-list-eq (my-map (fn [x] (+ x 1)) (list 10 20)) (list 11 21) "my-map: anon fn")

# my-filter: keep elements where pred returns true
(defn my-filter [pred lst]
  "Keep elements of lst for which pred returns true."
  (if (empty? lst)
    (list)
    (if (pred (first lst))
      (cons (first lst) (my-filter pred (rest lst)))
      (my-filter pred (rest lst)))))

(assert-list-eq (my-filter (fn [x] (> x 3)) (list 1 5 2 8 3))
                (list 5 8) "my-filter: > 3")

# my-fold: left fold
(defn my-fold [f acc lst]
  "Left fold: apply f to acc and each element of lst."
  (if (empty? lst)
    acc
    (my-fold f (f acc (first lst)) (rest lst))))

(assert-eq (my-fold + 0 (list 1 2 3 4 5)) 15 "my-fold: sum")
(assert-eq (my-fold * 1 (list 1 2 3 4 5)) 120 "my-fold: product")

# Reverse via fold
(assert-list-eq (my-fold (fn [acc x] (cons x acc)) (list) (list 1 2 3))
                (list 3 2 1) "my-fold: reverse")


# ========================================
# 5. Composition and pipelines
# ========================================

(print "=== composition and pipelines ===")

# compose: (compose f g)(x) = f(g(x))
(defn compose [f g]
  "Return a function that applies g then f."
  (fn [x] (f (g x))))

(def double-then-square (compose square (fn [x] (* x 2))))
(assert-eq (double-then-square 3) 36 "compose: square(double(3))")

# -> thread-first: insert value as first argument
(assert-eq (-> 5 (+ 3) (* 2)) 16 "->: (5+3)*2")

# ->> thread-last: insert value as last argument
(assert-list-eq (->> (list 1 2 3 4 5)
                     (my-filter (fn [x] (= (mod x 2) 0)))
                     (my-map square))
                (list 4 16) "->>: filter even then square")

# Data pipeline: sum the squares of odd numbers 1-5
(def pipeline-result
  (->> (list 1 2 3 4 5)
       (my-filter (fn [x] (= (mod x 2) 1)))
       (my-map square)
       (my-fold + 0)))
(assert-eq pipeline-result 35 "pipeline: sum of squares of odds")


# ========================================
# 6. Variadic functions
# ========================================

(print "=== variadic functions ===")

(defn my-sum [& args]
  "Sum all arguments."
  (my-fold + 0 args))

(assert-eq (my-sum 1 2 3) 6 "variadic: sum 3 args")
(assert-eq (my-sum 10 20 30 40) 100 "variadic: sum 4 args")
(assert-eq (my-sum) 0 "variadic: no args")

# Fixed head + rest
(defn head-and-rest [x & xs]
  "Return a list of x and the rest list."
  (list x xs))

(let* ((result (head-and-rest 1 2 3))
       (h (first result))
       (r (first (rest result))))
  (assert-eq h 1 "variadic: fixed head")
  (assert-list-eq r (list 2 3) "variadic: rest list"))


# ========================================
# 7. Mutual recursion
# ========================================

(print "=== mutual recursion ===")

(letrec ((my-even? (fn [n] (if (= n 0) true  (my-odd?  (- n 1)))))
         (my-odd?  (fn [n] (if (= n 0) false (my-even? (- n 1))))))
  (assert-true  (my-even? 0) "mutual: 0 is even")
  (assert-true  (my-even? 4) "mutual: 4 is even")
  (assert-false (my-even? 3) "mutual: 3 is not even")
  (assert-true  (my-odd? 5)  "mutual: 5 is odd")
  (assert-false (my-odd? 6)  "mutual: 6 is not odd"))


# ========================================
# 8. block and break
# ========================================

(print "=== block and break ===")

# Named block with early exit via break
(def found (block :search
  (each x in (list 1 2 3 42 5)
    (when (= x 42)
      (break :search x)))
  nil))
(assert-eq found 42 "block: found 42 via break")

# Block without break returns the last value
(def no-break (block :blk
  (+ 1 2)))
(assert-eq no-break 3 "block: no break, returns last value")

# Nested blocks
(def outer-val (block :outer
  (block :inner
    (break :outer "escaped"))
  "never reached"))
(assert-eq outer-val "escaped" "block: break crosses inner block")


# ========================================
# 9. Mutable captures
# ========================================

(print "=== mutable captures ===")

# Two closures sharing the same mutable cell
(defn make-box [initial]
  "Return (getter, setter) pair over shared state."
  (var val initial)
  (list
    (fn [] val)
    (fn [new-val] (set val new-val))))

(let* ((box (make-box 0))
       (getter (first box))
       (setter (first (rest box))))
  (assert-eq (getter) 0 "box: initial value")
  (setter 42)
  (assert-eq (getter) 42 "box: after set"))

# The counter from section 3 demonstrates this too:
# the closure and the factory share the same cell for 'count'.
(def c3 (make-counter))
(c3)
(c3)
(c3)
(assert-eq (c3) 4 "mutable capture: counter reaches 4")

# Accumulator — another shared-cell pattern
(defn make-accumulator [initial]
  "Return a function that adds to a running total."
  (var total initial)
  (fn [amount]
    (set total (+ total amount))
    total))

(def acc (make-accumulator 100))
(assert-eq (acc 10) 110 "accumulator: +10")
(assert-eq (acc 20) 130 "accumulator: +20")
(assert-eq (acc 5) 135 "accumulator: +5")


(print "=== All function tests passed ===")
