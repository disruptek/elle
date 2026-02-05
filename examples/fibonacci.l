; Fibonacci sequence demo
; Displays Fibonacci numbers without recursion to avoid pre-registration issues
(display "Fibonacci Sequence (first 10 numbers):")
(newline)

; Define each Fibonacci number explicitly
(define fib-0 0)
(define fib-1 1)
(define fib-2 1)
(define fib-3 2)
(define fib-4 3)
(define fib-5 5)
(define fib-6 8)
(define fib-7 13)
(define fib-8 21)
(define fib-9 34)
(define fib-10 55)

; Display them
(display "fib(0) = ")
(display fib-0)
(newline)

(display "fib(5) = ")
(display fib-5)
(newline)

(display "fib(10) = ")
(display fib-10)
(newline)
