#!/usr/bin/env elle

# Effects — user-defined signals and effect restrictions
#
# Demonstrates:
#   effect                  — declaring user-defined effects
#   restrict                — constraining a function's effects
#   restrict (parameter)    — constraining a parameter's effects
#   Effect composition      — multiple restrictions on same function
#   Effect bounds           — last restriction wins for same target
#   Runtime enforcement     — violations caught at call time

(def {:assert-eq assert-eq :assert-equal assert-equal :assert-true assert-true :assert-false assert-false :assert-list-eq assert-list-eq :assert-not-nil assert-not-nil :assert-string-eq assert-string-eq :assert-err assert-err :assert-err-kind assert-err-kind} ((import-file "./examples/assertions.lisp")))


# ========================================
# 1. Declaring user-defined effects
# ========================================

# effect declares a new user-defined effect that functions can signal.
# Each effect is a distinct signal type that can be restricted or allowed.

(effect :heartbeat)
(effect :rate-limit)
(effect :audit)

# :heartbeat — periodic status signals (e.g., "still working")
# :rate-limit — signals when rate limits are approached
# :audit — signals for audit logging of sensitive operations


# ========================================
# 2. Inert higher-order functions
# ========================================

# A function can restrict its parameters to be inert (emit no signals).
# This ensures callbacks don't have side effects.

(defn transform [f xs]
  "Map f over xs, requiring f to emit no signals."
  (restrict f)
  (map f xs))

# Test with inert functions: + and string/length both emit no signals
(def result1 (transform + [1 2 3]))
(display "  transform + [1 2 3]: ") (print result1)
(assert-list-eq result1 [1 2 3] "transform: + is inert")

(def result2 (transform string/length ["a" "bb" "ccc"]))
(display "  transform string/length: ") (print result2)
(assert-list-eq result2 [1 2 3] "transform: string/length is inert")

# Test failure: passing a yielding function violates the restriction
# We use protect to catch the runtime error
(defn yielding-fn [x]
  "A function that yields (not inert)."
  (yield :signal)
  x)

(def [ok? err] (protect (transform yielding-fn [1 2 3])))
(display "  transform with yielding function: ") (print err)
(assert-false ok? "transform: yielding function rejected at runtime")
(assert-eq (get err :error) :effect-violation "transform: error kind is :effect-violation")


# ========================================
# 3. Restricting a function's own effects
# ========================================

# A function can restrict itself to emit only specific effects.
# This is a contract: the function promises not to signal other effects.

(defn validate-positive [x]
  "Validate that x is positive. May only signal :error."
  (restrict :error)
  (if (< x 0)
    (error {:error :negative :message "expected positive number"})
    x))

# Test success: positive input returns the value
(def valid (validate-positive 42))
(display "  validate-positive 42: ") (print valid)
(assert-eq valid 42 "validate-positive: accepts positive")

# Test failure: negative input signals :error
(def [ok? err] (protect (validate-positive -5)))
(display "  validate-positive -5: ") (print err)
(assert-false ok? "validate-positive: rejects negative")
(assert-eq (get err :error) :negative "validate-positive: error kind is :negative")


# ========================================
# 4. Parameter bounds with user-defined effects
# ========================================

# A function can restrict a parameter to allow specific effects.
# This lets the callback emit only certain signals.

(defn monitored-run [f]
  "Run f, allowing it to emit :heartbeat signals."
  (restrict f :heartbeat)
  (f))

# Test success: a function that emits :heartbeat is allowed
(defn heartbeat-fn []
  "Emit a heartbeat signal."
  (fiber/signal :heartbeat :ping)
  :done)

(def result3 (monitored-run heartbeat-fn))
(display "  monitored-run with heartbeat: ") (print result3)
(assert-eq result3 :done "monitored-run: heartbeat allowed")

# Test failure: a function that yields (not :heartbeat) violates the restriction
(defn other-signal-fn []
  "Emit a different signal."
  (yield :other)
  :done)

(def [ok? err] (protect (monitored-run other-signal-fn)))
(display "  monitored-run with other signal: ") (print err)
(assert-false ok? "monitored-run: other signal rejected")
(assert-eq (get err :error) :effect-violation "monitored-run: error kind is :effect-violation")


# ========================================
# 5. Composed restrictions
# ========================================

# A function can have both its own restrictions and parameter restrictions.
# The function restricts itself to :error; it also restricts its parameter f to be inert.

(defn safe-transform [f xs]
  "Transform xs with f. f must be inert; this function may only error."
  (restrict :error)
  (restrict f)
  (map f xs))

# Test success: inert function, no errors
(def result4 (safe-transform + [10 20 30]))
(display "  safe-transform + [10 20 30]: ") (print result4)
(assert-list-eq result4 [10 20 30] "safe-transform: inert callback succeeds")

# Test failure: callback is not inert
(def [ok? err] (protect (safe-transform yielding-fn [1 2 3])))
(display "  safe-transform with yielding callback: ") (print err)
(assert-false ok? "safe-transform: non-inert callback rejected")
(assert-eq (get err :error) :effect-violation "safe-transform: error kind is :effect-violation")


# ========================================
# 6. Duplicate restrict — last one wins
# ========================================

# Multiple restrict forms for the same parameter are allowed.
# The last one wins: it replaces the previous bound.

(defn flexible [f]
  "Demonstrate that last restrict wins."
  (restrict f :heartbeat :error)
  (restrict f :error)
  (f))

# Test: f is now restricted to :error only (the second restrict overrides the first)
(defn error-only-fn []
  "Emit only :error."
  (error {:error :demo :message "demo error"})
  :done)

(def [ok? err] (protect (flexible error-only-fn)))
(display "  flexible with error-only function: ") (print err)
(assert-false ok? "flexible: error-only function allowed")
(assert-eq (get err :error) :demo "flexible: error kind is :demo")

# Test: if we try to emit :heartbeat, it should fail (because last restrict is :error only)
(defn heartbeat-only-fn []
  "Emit only :heartbeat."
  (fiber/signal :heartbeat :ping)
  :done)

(def [ok? err] (protect (flexible heartbeat-only-fn)))
(display "  flexible with heartbeat-only function: ") (print err)
(assert-false ok? "flexible: heartbeat-only function rejected (last restrict wins)")
(assert-eq (get err :error) :effect-violation "flexible: error kind is :effect-violation")


(print "")
(print "all effects tests passed.")
