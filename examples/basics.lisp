#!/usr/bin/env elle

# Basics — Elle's type system and value representations
#
# Demonstrates:
#   Immediates       — nil, booleans, integers, floats, symbols, keywords
#   Truthiness       — only nil and false are falsy
#   Arithmetic       — +, -, *, /, mod, abs, min, max, even?, odd?
#   Math             — math/sqrt, math/sin, math/cos, math/pow, math/pi, ...
#   Comparison/logic — =, <, >, not, and, or (short-circuiting)
#   Bitwise          — bit/and, bit/or, bit/xor, bit/not, bit/shl, bit/shr
#   Type conversions — number->string, string->integer, integer, float, ...
#   Mutability split — [tuple] vs @[array], {struct} vs @{table}, "str" vs @"buf"
#   Bytes and blobs  — immutable/mutable binary data
#   Boxes            — first-class mutable cells
#   Equality         — = does structural equality on data, reference on functions

(import-file "./examples/assertions.lisp")


# ========================================
# 1. Immediates
# ========================================

(print "=== Immediates ===")

# nil — the absence of a value
(assert-true (nil? nil) "nil? on nil")
(assert-false (nil? false) "nil? on false")
(assert-false (nil? 0) "nil? on 0")

# Booleans
(assert-true (boolean? true) "boolean? on true")
(assert-true (boolean? false) "boolean? on false")
(assert-false (boolean? 1) "boolean? not on int")

# Integers and floats
(assert-true (number? 42) "number? on int")
(assert-true (number? 3.14) "number? on float")
(assert-eq (type 42) :integer "42 is integer")
(assert-eq (type 3.14) :float "3.14 is float")
(assert-false (number? "42") "number? not on string")

# Symbols and keywords
(assert-true (symbol? 'hello) "symbol? on quoted symbol")
(assert-true (keyword? :hello) "keyword? on keyword")
(assert-false (= 'name :name) "symbol != keyword with same name")

# Empty list is its own thing — not nil
(assert-true (empty? (list)) "empty? on empty list")
(assert-false (nil? (list)) "empty list is NOT nil")

# type returns a keyword
(assert-eq (type 42) :integer "type of int")
(assert-eq (type 3.14) :float "type of float")
(assert-eq (type "hi") :string "type of string")
(assert-eq (type true) :boolean "type of bool")
(assert-eq (type nil) :nil "type of nil")
(assert-eq (type :foo) :keyword "type of keyword")
(assert-eq (type 'foo) :symbol "type of symbol")


# ========================================
# 2. Truthiness
# ========================================

(print "=== Truthiness ===")

# Only nil and false are falsy — everything else is truthy.
# This differs from C/Python/JS where 0, "", [] are falsy.
(assert-false (if nil true false) "nil is falsy")
(assert-false (if false true false) "false is falsy")

(assert-true (if 0 true false) "0 is truthy")
(assert-true (if "" true false) "empty string is truthy")
(assert-true (if (list) true false) "empty list () is truthy")
(assert-true (if [] true false) "empty tuple is truthy")
(assert-true (if @[] true false) "empty array is truthy")
(assert-true (if :keyword true false) "keyword is truthy")
(assert-true (if 'symbol true false) "symbol is truthy")


# ========================================
# 3. Arithmetic
# ========================================

(print "=== Arithmetic ===")

# Basic ops
(assert-eq (+ 10 5) 15 "addition")
(assert-eq (- 10 3) 7 "subtraction")
(assert-eq (* 6 7) 42 "multiplication")
(assert-eq (/ 10 2) 5 "division")
(assert-eq (mod 10 3) 1 "modulo")
(assert-eq (% 10 3) 1 "% is mod alias")

# Variadic + and *
(assert-eq (+ 1 2 3 4) 10 "+ is variadic")
(assert-eq (* 1 2 3 4) 24 "* is variadic")

# Integer vs float
(assert-eq (/ 7 2) 3 "int / int = int (truncates)")
(assert-eq (/ 7.0 2) 3.5 "float / int = float")

# Utility
(assert-eq (abs -5) 5 "abs")
(assert-eq (min 3 1 4 1 5) 1 "min is variadic")
(assert-eq (max 3 1 4 1 5) 5 "max is variadic")
(assert-true (even? 4) "even?")
(assert-true (odd? 3) "odd?")
(assert-true (even? 0) "0 is even")


# ========================================
# 4. Math
# ========================================

(print "=== Math ===")

(assert-eq (math/sqrt 16) 4.0 "sqrt returns float")
(assert-eq (math/floor 3.7) 3 "floor returns integer")
(assert-eq (math/ceil 3.2) 4 "ceil returns integer")
(assert-eq (math/round 3.5) 4 "round returns integer")
(assert-eq (math/pow 2 10) 1024 "pow")
(assert-eq (math/sin 0) 0.0 "sin returns float")
(assert-eq (math/cos 0) 1.0 "cos returns float")
(assert-true (> (math/pi) 3.14) "pi > 3.14")
(assert-true (< (math/pi) 3.15) "pi < 3.15")
(assert-true (> (math/e) 2.71) "e > 2.71")
(assert-true (< (math/e) 2.72) "e < 2.72")


# ========================================
# 5. Comparison and logic
# ========================================

(print "=== Comparison and logic ===")

(assert-true (= 1 1) "= on equal ints")
(assert-false (= 1 2) "= on unequal ints")
(assert-true (< 1 2) "<")
(assert-true (> 2 1) ">")
(assert-true (<= 1 1) "<= equal")
(assert-true (<= 1 2) "<= less")
(assert-true (>= 2 2) ">= equal")
(assert-true (>= 3 2) ">= greater")

(assert-true (not false) "not false")
(assert-false (not true) "not true")
(assert-false (not 0) "not 0 (0 is truthy)")

# and/or are short-circuiting and return the deciding value
(assert-eq (and 1 2 3) 3 "and: returns last if all truthy")
(assert-eq (and 1 false 3) false "and: returns first falsy")
(assert-eq (and 1 nil 3) nil "and: nil is falsy")
(assert-eq (or nil false 42) 42 "or: returns first truthy")
(assert-eq (or nil false) false "or: returns last if all falsy")
(assert-eq (or 0 1) 0 "or: 0 is truthy, returned first")


# ========================================
# 6. Bitwise
# ========================================

(print "=== Bitwise ===")

(assert-eq (bit/and 12 10) 8 "bit/and")
(assert-eq (bit/or 12 10) 14 "bit/or")
(assert-eq (bit/xor 12 10) 6 "bit/xor")
(assert-eq (bit/not 0) -1 "bit/not 0")
(assert-eq (bit/shl 1 3) 8 "bit/shl")
(assert-eq (bit/shr 16 2) 4 "bit/shr")

# Build a byte from nibbles: 0xA5 = (10 << 4) | 5 = 165
(assert-eq (bit/or (bit/shl 10 4) 5) 165 "nibble assembly")


# ========================================
# 7. Type conversions
# ========================================

(print "=== Type conversions ===")

# number->string and back
(assert-eq (number->string 42) "42" "number->string int")
(assert-eq (string->integer "42") 42 "string->integer")
(assert-eq (string->float "3.14") 3.14 "string->float")

# Generic converters
(assert-eq (integer 3.7) 3 "integer truncates float")
(assert-eq (float 42) 42.0 "float from int")
(assert-true (string? (string 42)) "string from int")
(assert-true (string? (string true)) "string from bool")

# Symbol/keyword to string
(assert-eq (symbol->string 'hello) "hello" "symbol->string")
(assert-eq (keyword->string :hello) "hello" "keyword->string (no colon)")

# any->string works on everything
(assert-true (string? (any->string (list 1 2))) "any->string on list")
(assert-true (string? (any->string nil)) "any->string on nil")

# Round-trip
(assert-eq (string->integer (number->string 99)) 99 "round-trip int")


# ========================================
# 8. The @ mutability split
# ========================================

(print "=== The @ mutability split ===")

# @ is the universal mutability prefix:
#   [...]  tuple  (immutable)    @[...]  array  (mutable)
#   {...}  struct (immutable)    @{...}  table  (mutable)
#   "..."  string (immutable)    @"..."  buffer (mutable)

(assert-eq (type [1 2 3]) :tuple "[] is tuple")
(assert-eq (type @[1 2 3]) :array "@[] is array")
(assert-eq (type {:a 1}) :struct "{} is struct")
(assert-eq (type @{:a 1}) :table "@{} is table")
(assert-eq (type "hello") :string "\"\" is string")
(assert-eq (type @"hello") :buffer "@\"\" is buffer")

# Type predicates
(assert-true (tuple? [1 2]) "tuple?")
(assert-true (array? @[1 2]) "array?")
(assert-true (struct? {:a 1}) "struct?")
(assert-true (table? @{:a 1}) "table?")
(assert-true (string? "hi") "string?")
(assert-true (buffer? @"hi") "buffer?")

# Mutable types are not immutable types and vice versa
(assert-false (tuple? @[1 2]) "array is not tuple")
(assert-false (array? [1 2]) "tuple is not array")


# ========================================
# 9. Bytes and blobs
# ========================================

(print "=== Bytes and blobs ===")

# bytes (immutable) and blob (mutable) — raw binary data
(def b (bytes 72 101 108 108 111))
(assert-eq (type b) :bytes "bytes type")
(assert-eq (length b) 5 "bytes length")
(assert-eq (get b 0) 72 "get returns integer byte value")
(assert-eq (bytes->string b) "Hello" "bytes->string (UTF-8)")
(assert-eq (bytes->hex b) "48656c6c6f" "bytes->hex")

(def bl (blob 72 101 108))
(assert-eq (type bl) :blob "blob type")
(assert-eq (get bl 0) 72 "get on blob")

# each iterates byte values (integers)
(var byte-sum 0)
(each v in (bytes 1 2 3)
  (set byte-sum (+ byte-sum v)))
(assert-eq byte-sum 6 "each over bytes sums integers")

# Conversions between bytes, blobs, strings, buffers
(def b2 (string->bytes "hi"))
(assert-eq (type b2) :bytes "string->bytes")
(assert-eq (bytes->string b2) "hi" "round-trip string->bytes->string")

(def bl2 (bytes->blob b2))
(assert-eq (type bl2) :blob "bytes->blob")
(def b3 (blob->bytes bl2))
(assert-eq (type b3) :bytes "blob->bytes")

(def buf-bytes (buffer->bytes @"world"))
(assert-eq (type buf-bytes) :bytes "buffer->bytes")
(assert-eq (bytes->string buf-bytes) "world" "buffer->bytes preserves content")

(def buf (bytes->buffer (string->bytes "test")))
(assert-eq (type buf) :buffer "bytes->buffer")

# Slice
(def sliced (slice (bytes 10 20 30 40 50) 1 3))
(assert-eq (length sliced) 2 "slice bytes length")
(assert-eq (get sliced 0) 20 "slice bytes content")


# ========================================
# 10. Boxes
# ========================================

(print "=== Boxes ===")

# box/unbox/rebox — explicit first-class mutable cells
(def b (box 0))
(assert-true (box? b) "box? on box")
(assert-false (box? 42) "box? on non-box")
(assert-eq (unbox b) 0 "unbox initial value")

(rebox b 42)
(assert-eq (unbox b) 42 "rebox updates value")

# Boxes are different from var/set — they're first-class values.
# You can pass them around, store them in collections, share between closures.
(def counter (box 0))

(defn inc! []
  "Increment the shared counter."
  (rebox counter (+ (unbox counter) 1)))

(defn get-count []
  "Read the shared counter."
  (unbox counter))

(inc!)
(inc!)
(inc!)
(assert-eq (get-count) 3 "shared counter via box")


# ========================================
# 11. Equality
# ========================================

(print "=== Equality ===")

# = does structural equality on data types
(assert-true (= [1 2 3] [1 2 3]) "= on equal tuples")
(assert-true (= @[1 2 3] @[1 2 3]) "= on equal arrays (structural)")
(assert-true (= {:a 1 :b 2} {:a 1 :b 2}) "= on equal structs")
(assert-true (= "hello" "hello") "= on equal strings")
(assert-true (= (bytes 1 2 3) (bytes 1 2 3)) "= on equal bytes")

# = returns false for different values
(assert-false (= [1 2] [1 3]) "= on different tuples")
(assert-false (= "hello" "world") "= on different strings")

# Numbers compare by value
(assert-true (= 42 42) "= on ints")
(assert-false (= 42 43) "= on different ints")

# Symbols compare by identity (interned)
(assert-true (= 'foo 'foo) "= on same symbol")
(assert-false (= 'foo 'bar) "= on different symbols")

# Keywords compare by identity (interned)
(assert-true (= :foo :foo) "= on same keyword")
(assert-false (= :foo :bar) "= on different keywords")

# Closures compare by reference — two identical lambdas are NOT equal
(def f (fn [x] x))
(def g (fn [x] x))
(assert-false (= f g) "identical lambdas are different objects")
(assert-true (= f f) "same closure is equal to itself")


(print "=== All basics tests passed ===")
