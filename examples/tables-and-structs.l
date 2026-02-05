; Tables and Structs Example
; Demonstrates mutable tables and immutable structs with basic operations

(display "=== Testing Tables (Mutable Hash Maps) ===")
(newline)

; Test 1: Create and type-check empty table
(let ((t (table)))
  (display "Created empty table, type is: ")
  (display (type t))
  (newline))

; Test 2: Create table with numeric keys
(let ((t (table 1 "value1" 2 "value2")))
  (display "Created table with integer keys, length: ")
  (display (table-length t))
  (newline))

; Test 3: Get values from table
(let ((t (table 42 "answer")))
  (display "Retrieved value from table: ")
  (display (get t 42))
  (newline))

; Test 4: Get with default
(let ((t (table 1 "exists")))
  (display "Non-existent key with default: ")
  (display (get t 999 "not-found"))
  (newline))

; Test 5: Has? predicate
(let ((t (table 5 "five")))
  (display "Table has key 5? ")
  (display (has? t 5))
  (newline)
  (display "Table has key 10? ")
  (display (has? t 10))
  (newline))

; Test 6: Put and verify
(let ((t (table)))
  (put t 100 "hundred")
  (display "After put, has key? ")
  (display (has? t 100))
  (newline))

; Test 7: Del and verify
(let ((t (table 7 "seven" 8 "eight")))
  (del t 7)
  (display "After delete, has key 7? ")
  (display (has? t 7))
  (newline))

(newline)
(display "=== Testing Structs (Immutable Hash Maps) ===")
(newline)

; Test 8: Create and type-check empty struct
(let ((s (struct)))
  (display "Created empty struct, type is: ")
  (display (type s))
  (newline))

; Test 9: Create struct with numeric keys
(let ((s (struct 10 "ten" 20 "twenty")))
  (display "Created struct with integer keys, length: ")
  (display (struct-length s))
  (newline))

; Test 10: Get values from struct
(let ((s (struct 99 "ninetymine")))
  (display "Retrieved value from struct: ")
  (display (struct-get s 99))
  (newline))

; Test 11: Get with default
(let ((s (struct 3 "three")))
  (display "Non-existent key with default: ")
  (display (struct-get s 777 "default-val"))
  (newline))

; Test 12: Struct-has? predicate
(let ((s (struct 2 "two")))
  (display "Struct has key 2? ")
  (display (struct-has? s 2))
  (newline)
  (display "Struct has key 3? ")
  (display (struct-has? s 3))
  (newline))

; Test 13: Immutability - original unchanged after put
(let ((s1 (struct 4 "four")))
  (let ((s2 (struct-put s1 5 "five")))
    (display "Original struct has key 5? ")
    (display (struct-has? s1 5))
    (newline)
    (display "New struct has key 5? ")
    (display (struct-has? s2 5))
    (newline)))

; Test 14: Immutability - original unchanged after del
(let ((s1 (struct 6 "six" 7 "seven")))
  (let ((s2 (struct-del s1 6)))
    (display "Original struct has key 6? ")
    (display (struct-has? s1 6))
    (newline)
    (display "New struct has key 6? ")
    (display (struct-has? s2 6))
    (newline)))

(display (newline))
(display "=== All Tests Completed Successfully ===")
(newline)
