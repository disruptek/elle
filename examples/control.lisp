#!/usr/bin/env elle

# Control flow in Elle — an expression evaluator
#
# Demonstrates:
#   if              — three-valued conditional, optional else, truthiness rules
#   cond            — multi-branch conditional
#   case            — equality dispatch (prelude macro)
#   when / unless   — conditional execution without else
#   if-let / when-let — conditional binding
#   while           — basic loop with set mutation
#   forever / break — infinite loop with early exit
#   block / break   — named blocks with early exit
#   match           — pattern matching: literals, wildcards, binding,
#                     list/tuple/struct patterns, nested patterns, guards
#   each            — iteration macro (brief — see collections.lisp)
#   -> / ->>        — threading macros
#
# The running example is a small calculator that evaluates arithmetic
# expressions represented as tuples:  [:lit n], [:add a b], [:mul a b],
# [:neg a], [:sub a b], [:div a b].

(import-file "./examples/assertions.lisp")


# ========================================
# 1. if — basic conditional
# ========================================
#
# (if test then else) — else is optional (returns nil when omitted).
# Only nil and false are falsy; everything else is truthy.

(assert-eq (if true :yes :no) :yes "if: true branch")
(assert-eq (if false :yes :no) :no "if: false branch")
(assert-eq (if nil :yes :no) :no "if: nil is falsy")
(assert-eq (if true :yes) :yes "if: no else, true")
(assert-eq (if false :yes) nil "if: no else, false returns nil")

# Truthiness: 0, empty string, empty list are all truthy
(assert-eq (if 0 :yes :no) :yes "if: 0 is truthy")
(assert-eq (if "" :yes :no) :yes "if: empty string is truthy")
(assert-eq (if (list) :yes :no) :yes "if: empty list is truthy")
(assert-eq (if :keyword :yes :no) :yes "if: keywords are truthy")


# ========================================
# 2. cond — multi-branch conditional
# ========================================
#
# (cond (test1 expr1) (test2 expr2) ... (true default))
# Evaluates tests in order, returns the body of the first truthy test.

(defn classify-number [n]
  "Classify a number as negative, zero, or positive."
  (cond
    ((< n 0) :negative)
    ((= n 0) :zero)
    ((> n 0) :positive)))

(assert-eq (classify-number -5) :negative "cond: negative")
(assert-eq (classify-number 0) :zero "cond: zero")
(assert-eq (classify-number 42) :positive "cond: positive")

# cond is cleaner than nested ifs for dispatch on keywords
(defn op-symbol [op]
  "Return the printable symbol for an operator keyword."
  (cond
    ((= op :add) "+")
    ((= op :sub) "-")
    ((= op :mul) "*")
    ((= op :div) "/")
    ((= op :neg) "-")
    (true "?")))

(assert-eq (op-symbol :add) "+" "cond dispatch: add")
(assert-eq (op-symbol :neg) "-" "cond dispatch: neg")
(assert-eq (op-symbol :unknown) "?" "cond dispatch: default")


# ========================================
# 3. case — equality dispatch
# ========================================
#
# (case expr val1 body1 val2 body2 ... default)
# Sugar for chained (if (= g val) ...). Flat pairs, optional default.

(defn binary-op [op a b]
  "Apply a binary arithmetic operator."
  (case op
    :add (+ a b)
    :sub (- a b)
    :mul (* a b)
    :div (/ a b)
    (error :unknown-op (string/join (list "unknown op: " (string op)) ""))))

(assert-eq (binary-op :add 3 4) 7 "case: add")
(assert-eq (binary-op :sub 10 3) 7 "case: sub")
(assert-eq (binary-op :mul 6 7) 42 "case: mul")
(assert-eq (binary-op :div 15 3) 5 "case: div")


# ========================================
# 4. when and unless — conditional execution
# ========================================
#
# (when test body...) — runs body if test is truthy, returns nil otherwise.
# (unless test body...) — inverse: runs body if test is falsy.
# Useful for side effects (no else branch).

(var log @[])

(defn check-bounds [n lo hi]
  "Push warnings to the log array for out-of-range values."
  (when (< n lo) (push log :too-low))
  (when (> n hi) (push log :too-high))
  (unless (or (< n lo) (> n hi)) (push log :ok)))

(check-bounds 5 0 10)
(assert-eq (pop log) :ok "when/unless: in range")

(check-bounds -1 0 10)
(assert-eq (pop log) :too-low "when: below range")

(check-bounds 99 0 10)
(assert-eq (pop log) :too-high "when: above range")

# when returns nil when test is falsy
(assert-eq (when false :never) nil "when: returns nil on false")
(assert-eq (unless true :never) nil "unless: returns nil on true")


# ========================================
# 5. if-let and when-let — conditional binding
# ========================================
#
# (if-let ((x expr)) then else)
# Binds x to expr; if x is falsy, runs else branch instead.

(defn safe-div [a b]
  "Divide a by b, returning nil if b is zero."
  (if (= b 0) nil (/ a b)))

(defn describe-quotient [a b]
  "Describe the result of dividing a by b."
  (if-let ((q (safe-div a b)))
    (string/join (list "result: " (string q)) "")
    "division by zero"))

(assert-eq (describe-quotient 10 2) "result: 5" "if-let: truthy binding")
(assert-eq (describe-quotient 10 0) "division by zero" "if-let: falsy binding")

# when-let — no else variant
(var results @[])
(defn maybe-push-quotient [a b]
  "Push the quotient to results if division succeeds."
  (when-let ((q (safe-div a b)))
    (push results q)))

(maybe-push-quotient 20 4)
(maybe-push-quotient 10 0)
(maybe-push-quotient 15 3)
(assert-eq (length results) 2 "when-let: only truthy values pushed")
(assert-eq (get results 0) 5 "when-let: first quotient")
(assert-eq (get results 1) 5 "when-let: second quotient")


# ========================================
# 6. while — basic loop
# ========================================
#
# (while test body...) — loops while test is truthy.
# Use (set var val) to mutate bindings within the loop.

# Sum integers 1..10
(var total 0)
(var i 1)
(while (<= i 10)
  (set total (+ total i))
  (set i (+ i 1)))
(assert-eq total 55 "while: sum 1..10")

# Factorial via while
(defn factorial [n]
  "Compute n! iteratively."
  (var acc 1)
  (var k n)
  (while (> k 1)
    (set acc (* acc k))
    (set k (- k 1)))
  acc)

(assert-eq (factorial 0) 1 "factorial: 0")
(assert-eq (factorial 5) 120 "factorial: 5")
(assert-eq (factorial 10) 3628800 "factorial: 10")


# ========================================
# 7. forever and break
# ========================================
#
# (forever body...) expands to (while true body...).
# while wraps an implicit block named :while, so (break :while val) exits.
# Plain (break) also works inside while/forever.

# Collatz sequence: count steps to reach 1
(defn collatz-steps [n]
  "Count steps in the Collatz sequence from n to 1."
  (var x n)
  (var steps 0)
  (forever
    (if (= x 1)
      (break :while steps))
    (set x (if (= (% x 2) 0) (/ x 2) (+ (* 3 x) 1)))
    (set steps (+ steps 1))))

(assert-eq (collatz-steps 1) 0 "collatz: 1 -> 0 steps")
(assert-eq (collatz-steps 6) 8 "collatz: 6 -> 8 steps")
(assert-eq (collatz-steps 27) 111 "collatz: 27 -> 111 steps")


# ========================================
# 8. block and break — named early exit
# ========================================
#
# (block :name body...) creates a named scope.
# (break :name value) exits the block, returning value.
# break is compile-time validated: must be inside the named block,
# cannot cross function boundaries.

# Find the first element satisfying a predicate
(defn find-first [arr pred]
  "Return the first element of arr where (pred elem) is truthy, or nil."
  (block :search
    (var idx 0)
    (while (< idx (length arr))
      (let ((elem (get arr idx)))
        (when (pred elem)
          (break :search elem)))
      (set idx (+ idx 1)))
    nil))

(assert-eq (find-first @[1 4 9 16 25] (fn [x] (> x 10))) 16
  "block/break: find first > 10")
(assert-eq (find-first @[1 2 3] (fn [x] (> x 100))) nil
  "block/break: not found returns nil")

# Nested blocks
(def nested-result
  (block :outer
    (block :inner
      (break :outer :escaped-both))
    :never-reached))
(assert-eq nested-result :escaped-both "block: break targets outer")


# ========================================
# 9. match — pattern matching
# ========================================
#
# (match value (pattern body) ...)
#
# Patterns:
#   42, "hello", :kw, true   — literal match
#   nil                       — matches nil
#   _                         — wildcard (matches anything)
#   x                         — variable binding
#   (a b c)                   — list pattern (matches cons cells/lists)
#   (h . t)                   — cons pattern (head . tail)
#   [a b c]                   — tuple pattern (matches tuples [immutable])
#   @[a b c]                  — array pattern (matches arrays [mutable])
#   {:key var}                — struct pattern
#   @{:key var}               — table pattern
#   (pattern when guard body) — guarded arm

# --- Literal patterns ---
(var m-int (match 42 (42 :found) (_ :nope)))
(assert-eq m-int :found "match: literal int")

(var m-str (match "hi" ("hi" :found) (_ :nope)))
(assert-eq m-str :found "match: literal string")

(var m-kw (match :foo (:foo :found) (_ :nope)))
(assert-eq m-kw :found "match: literal keyword")

(var m-bool (match true (true :yes) (false :no)))
(assert-eq m-bool :yes "match: literal bool")

# --- Wildcard ---
(var m-wild (match 999 (_ :anything)))
(assert-eq m-wild :anything "match: wildcard")

# --- Variable binding ---
(var m-bind (match 7 (x (* x x))))
(assert-eq m-bind 49 "match: variable binding")

# --- nil pattern ---
(var m-nil (match nil (nil :got-nil) (_ :other)))
(assert-eq m-nil :got-nil "match: nil")

# --- List patterns ---
(var m-list (match (list 1 2 3) ((1 2 3) :exact) (_ :no)))
(assert-eq m-list :exact "match: exact list")

(var m-lbind (match (list 1 2) ((a b) (+ a b))))
(assert-eq m-lbind 3 "match: list binding")

(var m-empty (match (list) (() :empty) (_ :no)))
(assert-eq m-empty :empty "match: empty list")

# Cons pattern: (head . tail)
(var m-head (match (list 1 2 3) ((h . t) h)))
(assert-eq m-head 1 "match: cons head")

(var m-tail (match (list 1 2 3) ((h . t) (length t))))
(assert-eq m-tail 2 "match: cons tail length")

# --- Tuple patterns ---
# [...] in expression position creates tuples.
# [...] in match patterns matches tuples.
(var m-tup (match [10 20] ([a b] (+ a b))))
(assert-eq m-tup 30 "match: tuple pattern")

(var m-tkw (match [:neg 5] ([:neg n] (- 0 n)) (_ 0)))
(assert-eq m-tkw -5 "match: tuple with keyword")

# --- Struct patterns ---
(var m-struct (match {:x 1 :y 2} ({:x x :y y} (+ x y))))
(assert-eq m-struct 3 "match: struct pattern")

# Struct pattern with literal value matching
(var m-dispatch
  (match {:type :circle :r 5}
    ({:type :square :s s} (* s s))
    ({:type :circle :r r} (* r r))
    (_ 0)))
(assert-eq m-dispatch 25 "match: struct dispatch on keyword value")

# --- Guarded arms ---
(defn abs-val [n]
  "Absolute value via guarded match."
  (match n
    (x when (< x 0) (- 0 x))
    (x x)))

(assert-eq (abs-val -7) 7 "match guard: negative")
(assert-eq (abs-val 5) 5 "match guard: positive")
(assert-eq (abs-val 0) 0 "match guard: zero")

# --- The expression evaluator ---
#
# Arithmetic expressions as tuples:
#   [:lit n]     — literal number
#   [:add a b]   — addition
#   [:sub a b]   — subtraction
#   [:mul a b]   — multiplication
#   [:div a b]   — division
#   [:neg a]     — negation

(defn eval-expr [expr]
  "Evaluate an arithmetic expression tree."
  (match expr
    ([:lit n] n)
    ([:add a b] (+ (eval-expr a) (eval-expr b)))
    ([:sub a b] (- (eval-expr a) (eval-expr b)))
    ([:mul a b] (* (eval-expr a) (eval-expr b)))
    ([:div a b] (/ (eval-expr a) (eval-expr b)))
    ([:neg a]   (- 0 (eval-expr a)))))

# Simple expressions
(assert-eq (eval-expr [:lit 42]) 42 "eval: literal")
(assert-eq (eval-expr [:neg [:lit 7]]) -7 "eval: negation")
(assert-eq (eval-expr [:add [:lit 3] [:lit 4]]) 7 "eval: addition")
(assert-eq (eval-expr [:sub [:lit 10] [:lit 3]]) 7 "eval: subtraction")
(assert-eq (eval-expr [:mul [:lit 6] [:lit 7]]) 42 "eval: multiplication")
(assert-eq (eval-expr [:div [:lit 15] [:lit 3]]) 5 "eval: division")

# Nested: (3 + 4) * (10 - 3) = 7 * 7 = 49
(def complex-expr
  [:mul [:add [:lit 3] [:lit 4]]
        [:sub [:lit 10] [:lit 3]]])
(assert-eq (eval-expr complex-expr) 49 "eval: nested expression")

# Deeper nesting: -(2 * (5 + 3)) = -(2 * 8) = -16
(def deep-expr
  [:neg [:mul [:lit 2] [:add [:lit 5] [:lit 3]]]])
(assert-eq (eval-expr deep-expr) -16 "eval: deep nesting")

# --- Nested match patterns ---
(def pair-of-pairs (list (list 1 2) (list 3 4)))
(var m-nested
  (match pair-of-pairs
    (((a b) (c d)) (+ a (* b (+ c d))))))
(assert-eq m-nested 15 "match: nested list pattern")

# --- Rest patterns in match ---
(var m-rest
  (match (list 1 2 3 4 5)
    ((a b & rest) (+ a b (length rest)))))
(assert-eq m-rest 6 "match: list rest pattern")


# ========================================
# 10. each — iteration
# ========================================
#
# (each var in collection body...)
# Dispatches on type. See collections.lisp for full coverage.

(var squares @[])
(each n in (list 1 2 3 4 5)
  (push squares (* n n)))
(assert-eq (length squares) 5 "each: iterated list")
(assert-eq (get squares 0) 1 "each: first square")
(assert-eq (get squares 4) 25 "each: last square")

# each over a tuple
(var tuple-sum 0)
(each x in [10 20 30]
  (set tuple-sum (+ tuple-sum x)))
(assert-eq tuple-sum 60 "each: tuple sum")


# ========================================
# 11. Threading macros
# ========================================
#
# (-> val (f a) (g b))  = (g (f val a) b)    — thread-first
# (->> val (f a) (g b)) = (g b (f a val))    — thread-last
# Bare symbols work too: (-> x f g) = (g (f x))

# Thread-first: value becomes first argument
(assert-eq (-> 5 (+ 10) (* 2)) 30
  "->: (5+10)*2 = 30")
(assert-eq (-> 10 (- 3) (+ 5)) 12
  "->: (10-3)+5 = 12")

# Thread-last: value becomes last argument
(assert-eq (->> 5 (+ 10) (* 2)) 30
  "->>: (10+5)*2 = 30")
(assert-eq (->> 10 (- 3) (+ 5)) -2
  "->>: (3-10) = -7, (5+(-7)) = -2")

# Bare symbol threading: (-> x f g) = (g (f x))
(assert-eq (-> -7 abs-val string) "7"
  "->: bare symbol threading")

# Thread-first for nested data access
(def config {:db {:host "localhost" :port 5432}})
(assert-eq (-> config (get :db) (get :port)) 5432
  "->: nested struct access")

# Thread-last for pipeline processing
(assert-eq (->> "  hello  " string/trim string/upcase) "HELLO"
  "->>: trim then upcase")


(print "=== All control flow tests passed ===")
