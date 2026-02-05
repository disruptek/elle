; Tables and Structs Example
; Demonstrates mutable tables and immutable structs

(display "=== TABLES (Mutable Hash Maps) ===")
(newline)

; Creating empty table
(let ((empty-table (table)))
  (display "Empty table type: ")
  (display (type empty-table))
  (newline))

; Creating table with initial values
(let ((person (table "name" "Alice" "age" 30)))
  (display "Table with values - type: ")
  (display (type person))
  (newline))

; Getting values from table
(let ((config (table "host" "localhost" "port" 8080)))
  (display "Config host: ")
  (display (get config "host"))
  (newline)
  (display "Config port: ")
  (display (get config "port"))
  (newline))

; Getting missing value with default
(let ((settings (table "volume" 50)))
  (display "Missing setting with default: ")
  (display (get settings "brightness" 100))
  (newline))

; Putting values (mutating)
(let ((state (table)))
  (put state "count" 0)
  (display "After put - has key? ")
  (display (has? state "count"))
  (newline))

; Deleting values (mutating)
(let ((data (table 1 "one" 2 "two" 3 "three")))
  (del data 2)
  (display "After delete key 2 - has key? ")
  (display (has? data 2))
  (newline))

; Table inspection
(let ((colors (table "red" "#FF0000" "green" "#00FF00")))
  (display "Color table length: ")
  (display (table-length colors))
  (newline)
  (display "Has red? ")
  (display (has? colors "red"))
  (newline))

(newline)
(display "=== STRUCTS (Immutable Hash Maps) ===")
(newline)

; Creating empty struct
(let ((empty-struct (struct)))
  (display "Empty struct type: ")
  (display (type empty-struct))
  (newline))

; Creating struct with initial values
(let ((point (struct "x" 10 "y" 20)))
  (display "Point struct type: ")
  (display (type point))
  (newline))

; Getting values from struct
(let ((user (struct "id" 42 "name" "Bob")))
  (display "User ID: ")
  (display (struct-get user "id"))
  (newline)
  (display "User name: ")
  (display (struct-get user "name"))
  (newline))

; Getting missing value with default
(let ((config (struct "db-host" "localhost")))
  (display "Missing config value: ")
  (display (struct-get config "db-port" 5432))
  (newline))

; Creating new struct with updated value (immutable)
(let ((v1 (struct "x" 1 "y" 2)))
  (let ((v2 (struct-put v1 "x" 10)))
    (display "Original struct x: ")
    (display (struct-get v1 "x"))
    (newline)
    (display "New struct x: ")
    (display (struct-get v2 "x"))
    (newline)))

; Creating new struct with deleted key (immutable)
(let ((s1 (struct "a" 1 "b" 2 "c" 3)))
  (let ((s2 (struct-del s1 "b")))
    (display "Original struct has b? ")
    (display (struct-has? s1 "b"))
    (newline)
    (display "New struct has b? ")
    (display (struct-has? s2 "b"))
    (newline)))

; Struct inspection
(let ((config (struct "db-host" "postgres" "db-port" 5432)))
  (display "Config struct length: ")
  (display (struct-length config))
  (newline)
  (display "Has db-host? ")
  (display (struct-has? config "db-host"))
  (newline))

(display (newline))
(display "=== Example Complete ===")
(newline)
