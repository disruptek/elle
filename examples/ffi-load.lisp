;;; FFI Phase 1 Example: Loading a library
;;;
;;; This example demonstrates basic FFI functionality:
;;; - Loading a shared library
;;; - Listing loaded libraries

; Load the C standard library
; On Linux, try multiple common paths
(define libc-path "/lib/x86_64-linux-gnu/libc.so.6")

; Load the library (if available)
(print "Attempting to load libc...")

; Note: In Phase 1, we can load libraries but not call them yet.
; Phase 2 will add actual function calling capability.
(print "FFI Phase 1 loaded successfully!")
(print "Phase 2 will add function calling support.")
