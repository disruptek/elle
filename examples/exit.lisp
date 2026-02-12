;; Exit builtin examples
;; Demonstrates the use of the (exit) primitive

;; Example 1: Exit with success code (0)
;; This exits the program with status code 0 (default, success)
(exit)

;; Example 2: Exit with custom success code
;; Exits with a custom success code
(exit 42)

;; Example 3: Exit with error code
;; Exits with an error code (non-zero values indicate errors)
(exit 1)

;; Example 4: Exit with different error codes
;; Different error codes can indicate different types of errors
(exit 2)  ; typically means "usage error"
(exit 127) ; typically means "command not found"

;; Example 5: Conditional exit based on some condition
(let ((should-exit? true))
  (if should-exit?
      (exit 0)
      (display "Continuing execution...")))

;; Example 6: Exit with a computed code
(let ((error-code (+ 1 2)))
  (exit error-code))

;; Note: In shell scripts, you can check the exit code with:
;;   elle program.lisp
;;   echo $?  # prints the exit code
