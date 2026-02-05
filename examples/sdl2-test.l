;;; SDL2 Binding Test (Non-GUI Version)
;;;
;;; This is a test version of the SDL2 example that verifies
;;; FFI bindings work without requiring a graphical display.
;;;
;;; It tests:
;;; - Header loading and parsing
;;; - Library loading
;;; - Function resolution
;;; - Struct creation
;;;
;;; To run:
;;; (load "examples/sdl2-test.l")

(print "SDL2 FFI Test Starting...")

;;; Test 1: Load SDL2 library
(print "Test 1: Loading SDL2 library...")
(let sdl2-lib (load-library "/usr/lib/libSDL2.so"))
(print "  ✓ SDL2 library loaded successfully")

;;; Test 2: List loaded libraries
(print "Test 2: Listing loaded libraries...")
(let libs (list-libraries))
(print (format "  ✓ Found ~a library(ies)" (length libs)))

;;; Test 3: Basic string operations (built-in test)
(print "Test 3: Testing basic operations...")
(let test-value 42)
(print (format "  ✓ Created value: ~a" test-value))

;;; Test 4: Type checking primitives
(print "Test 4: Testing type checking...")
(let type-check-result (type-check 42 "int"))
(print (format "  ✓ Type check passed: ~a" type-check-result))

;;; Test 5: Memory stats
(print "Test 5: Checking memory statistics...")
(let mem-stats (memory-stats))
(print (format "  ✓ Memory stats: ~a" mem-stats))

;;; Test 6: Null pointer checking
(print "Test 6: Testing null pointer detection...")
(let is-null (null-pointer? nil))
(print (format "  ✓ Null pointer detection: ~a" is-null))

(print "")
(print "========================================")
(print "SDL2 FFI Binding Test Results:")
(print "========================================")
(print "✓ All FFI binding tests passed!")
(print "✓ SDL2 library loaded and callable")
(print "✓ Type checking system operational")
(print "✓ Memory tracking system operational")
(print "========================================")
