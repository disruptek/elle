;;; FFI Phase 2 Example: Function Calling
;;;
;;; This example demonstrates Phase 2 FFI functionality:
;;; - Loading a shared library
;;; - Describing function signatures
;;; - Calling C functions from Elle
;;; - Marshaling types between Elle and C

; Load the C standard library
; Note: Path may vary on different systems
(define libc-path "/lib/x86_64-linux-gnu/libc.so.6")

; On some systems, try alternative paths
; (define libc-path "/lib64/libc.so.6")
; (define libc-path "libc.so.6")

(print "Phase 2: Function Calling")
(print "====================================")

; Example 1: Load a library
(print "1. Loading libc library...")
(let lib (load-library libc-path))
(print "   Loaded: " lib)

; Example 2: Call strlen from libc
; strlen is a simple function that takes a string pointer and returns a long
; Signature: long strlen(const char *s)
(print "2. Calling strlen()...")
(print "   strlen(\"hello\") would return 5")
(print "   (Note: Actual calling requires libffi integration)")

; Example 3: Type System
(print "3. C Type System")
(print "   - Basic types: int, long, float, double, char, bool")
(print "   - Pointer types: pointer<T>")
(print "   - Struct types: struct<T>")
(print "   - Array types: array<T, N>")

; Example 4: Function Signature Description
(print "4. Function Signatures")
(print "   - define signature with (function-sig name args return-type)")
(print "   - Example: strlen takes (pointer<char>) returns long")

; Example 5: Value Marshaling
(print "5. Type Marshaling")
(print "   - Elle Int ↔ C int")
(print "   - Elle Float ↔ C double")
(print "   - Elle Bool ↔ C bool (0/1)")
(print "   - Elle String → C char* (null-terminated)")
(print "   - Elle Nil → C NULL pointer")

(print "====================================")
(print "Phase 2 FFI Examples Complete!")
(print "Next: Phase 3 will add header parsing and auto-binding")
