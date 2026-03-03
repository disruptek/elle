## Elle standard library
##
## Loaded at startup after primitives are registered.
## Unlike the prelude (which is macro-only), these define
## runtime functions that need the full pipeline.

## ── Higher-order functions ──────────────────────────────────────────

(def map (fn (f coll)
  (cond
    ((or (array? coll) (tuple? coll) (bytes? coll) (blob? coll))
     (letrec ((loop (fn (i acc)
                      (if (>= i (length coll))
                        (reverse acc)
                        (loop (+ i 1) (cons (f (get coll i)) acc))))))
       (loop 0 ())))
    ((or (string? coll) (buffer? coll))
     (letrec ((loop (fn (i acc)
                      (if (>= i (length coll))
                        (reverse acc)
                        (loop (+ i 1) (cons (f (string/char-at coll i)) acc))))))
       (loop 0 ())))
    ((or (pair? coll) (empty? coll))
     (if (empty? coll)
       ()
       (cons (f (first coll)) (map f (rest coll)))))
    (true (error [:type-error "map: not a sequence"])))))


(def filter (fn (p lst)
  (if (empty? lst)
    ()
    (if (p (first lst))
      (cons (first lst) (filter p (rest lst)))
      (filter p (rest lst))))))

(def fold (fn (f init lst)
  (if (empty? lst)
    init
    (fold f (f init (first lst)) (rest lst)))))

## ── Time utilities ──────────────────────────────────────────────────

(def time/stopwatch (fn ()
  (coro/new (fn ()
    (let ((start (clock/monotonic)))
      (while true
        (yield (- (clock/monotonic) start))))))))

(def time/elapsed (fn (thunk)
  (let ((start (clock/monotonic)))
    (let ((result (thunk)))
      (list result (- (clock/monotonic) start))))))

## ── VM query wrappers ───────────────────────────────────────────────

(def call-count (fn (f) (vm/query "call-count" f)))
(def global? (fn (sym) (vm/query "global?" sym)))
(def fiber/self (fn () (vm/query "fiber/self" nil)))

## ── Arena introspection ─────────────────────────────────────────────

(def arena/allocs (fn (thunk)
  "Run thunk, return (result alloc-count) where alloc-count is net heap objects allocated."
  (let* ((before (arena-count))
         (result (thunk))
         (after (arena-count)))
    (list result (- after before 1)))))

## ── Control flow graph rendering ────────────────────────────────────

(defn fn/cfg (target & opts)
  "Render a closure or fiber's control flow graph as text.
   Optional format keyword: :mermaid (default) or :dot.
   (fn/cfg my-fn)          => Mermaid flowchart string
   (fn/cfg my-fn :dot)     => DOT digraph string
   (fn/cfg my-fn :mermaid) => Mermaid flowchart string"
  (let* ((fmt (if (empty? opts)
                :mermaid
                (if (> (length opts) 1)
                  (error [:arity-error "fn/cfg: expected at most 1 format keyword"])
                  (first opts))))
         (cfg (fn/flow target)))
    (when (nil? cfg)
      (error [:type-error "fn/cfg: target has no LIR"]))
    (cond
      ((= fmt :mermaid) (fn/cfg-mermaid cfg))
      ((= fmt :dot)     (fn/cfg-dot cfg))
      (true (error [:type-error (-> "fn/cfg: unknown format "
                                  (append (string fmt))
                                  (append ", expected :mermaid or :dot"))])))))

(defn fn/cfg-label (cfg)
  "Build the label string from a CFG struct's metadata."
  (let* ((name (get cfg :name))
         (doc (get cfg :doc)))
    (if (nil? name)
      (if (nil? doc) "anonymous" doc)
      name)))

(defn fn/cfg-dot (cfg)
  "Render a CFG struct as a DOT digraph string."
  (letrec ((dot-escape (fn (s)
             (-> s
               (string/replace "{" "\\{")
               (string/replace "}" "\\}")
               (string/replace "|" "\\|")
               (string/replace "<" "\\<")
               (string/replace ">" "\\>")))))
    (let ((result (-> "digraph {\n  label=\""
                    (append (fn/cfg-label cfg))
                    (append " arity:")
                    (append (get cfg :arity))
                    (append " regs:")
                    (append (string (get cfg :regs)))
                    (append " locals:")
                    (append (string (get cfg :locals)))
                    (append "\";\n  node [shape=record];\n"))))
      (each block (get cfg :blocks)
        (let* ((lbl (string (get block :label)))
               (instrs (get block :instrs))
               (term (get block :term))
               (edges (get block :edges)))
          (set result (-> result
                        (append "  block")
                        (append lbl)
                        (append " [label=\"{block")
                        (append lbl)))
          (set result (append result "|"))
          (each instr instrs
            (set result (-> result
                          (append (dot-escape instr))
                          (append "\\l"))))
          (set result (-> result
                        (append "|")
                        (append (dot-escape term))
                        (append "}\"];\n")))
          (each edge edges
            (set result (-> result
                          (append "  block")
                          (append lbl)
                          (append " -> block")
                          (append (string edge))
                          (append ";\n"))))))
      (append result "}\n"))))

(defn fn/cfg-mermaid (cfg)
  "Render a CFG struct as a Mermaid flowchart string."
  (letrec ((mmd-escape (fn (s)
             (-> s
               (string/replace "&" "&amp;")
               (string/replace "\"" "&quot;")))))
    (let ((result (-> "flowchart TD\n"
                    (append "  %% ")
                    (append (fn/cfg-label cfg))
                    (append " arity:")
                    (append (get cfg :arity))
                    (append " regs:")
                    (append (string (get cfg :regs)))
                    (append " locals:")
                    (append (string (get cfg :locals)))
                    (append "\n"))))
      (each block (get cfg :blocks)
        (let* ((lbl (string (get block :label)))
               (instrs (get block :instrs))
               (term (get block :term))
               (edges (get block :edges)))
          (set result (-> result
                        (append "  block")
                        (append lbl)
                        (append "[\"block")
                        (append lbl)))
          (each instr instrs
            (set result (-> result
                          (append "<br/>")
                          (append (mmd-escape instr)))))
          (set result (-> result
                        (append "<br/>---<br/>")
                        (append (mmd-escape term))
                        (append "\"]\n")))
          (each edge edges
            (set result (-> result
                          (append "  block")
                          (append lbl)
                          (append " --> block")
                          (append (string edge))
                          (append "\n"))))))
      result)))
