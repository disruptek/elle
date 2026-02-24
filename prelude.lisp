;; Elle standard prelude
;;
;; Loaded automatically by the Expander before user code expansion.
;; These are defmacro definitions — they register macros in the
;; Expander's macro table and produce no runtime code.

;; when - execute body if test is truthy, return nil otherwise
(defmacro when (test & body)
  `(if ,test (begin ,@body) nil))

;; unless - execute body if test is falsy, return nil otherwise
(defmacro unless (test & body)
  `(if ,test nil (begin ,@body)))

;; try/catch - error handling via fibers
;; Usage: (try body... (catch e handler...))
;; The last form must be (catch binding handler-body...)
;; Body forms run in a fiber that catches errors.
;; If an error occurs, the catch handler runs with the error bound.
;; If no error occurs, the body result is returned.
(defmacro try (& forms)
  (let* ((catch-clause (last forms))
         (body-forms (butlast forms))
         (err-binding (first (rest catch-clause)))
         (handler-body (rest (rest catch-clause))))
    `(let ((f (fiber/new (fn () ,@body-forms) 1)))
       (fiber/resume f nil)
       (if (= (fiber/status f) :dead)
         (fiber/value f)
         (let ((,err-binding (fiber/value f)))
           ,@handler-body)))))

;; protect - run body, return [success? value]
;; Does not propagate errors — captures them as data.
;; :dead means normal completion; anything else means error.
(defmacro protect (& body)
  `(let ((f (fiber/new (fn () ,@body) 1)))
     (fiber/resume f nil)
     [(= (fiber/status f) :dead) (fiber/value f)]))

;; defer - run cleanup after body regardless of success/failure
;; If the body errors, cleanup runs then the error is re-raised.
(defmacro defer (cleanup & body)
  `(let ((f (fiber/new (fn () ,@body) 1)))
     (fiber/resume f nil)
     ,cleanup
     (if (= (fiber/status f) :dead)
       (fiber/value f)
       (fiber/propagate f))))

;; with - resource management (acquire/release)
;; Usage: (with binding ctor dtor body...)
;; Acquires the resource, runs body, then releases via destructor.
;; Errors in body are propagated after cleanup.
(defmacro with (binding ctor dtor & body)
  `(let ((,binding ,ctor))
     (defer (,dtor ,binding) ,@body)))
