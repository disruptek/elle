# CFG Visualizer Demo
#
# Renders control flow graphs of Elle functions to SVG using
# fn/cfg (Mermaid output) and selkie (Mermaid-to-SVG rendering).
#
# Usage:
#   cargo build --release -p elle-selkie
#   cargo run --release -- demos/cfgviz/cfgviz.lisp
#
# Produces: identity.svg, factorial.svg, fizzbuzz.svg, make-adder.svg,
#           eval-expr.svg

(import-file "target/release/libelle_selkie.so")

# ── Functions to visualize ───────────────────────────────────────────

(defn identity [x]
  "The simplest function — one block, one return."
  x)

(defn factorial [n]
  "Recursive factorial — branching and self-call."
  (if (< n 2)
    1
    (* n (factorial (- n 1)))))

(defn fizzbuzz [n]
  "Classic fizzbuzz — nested branching."
  (cond
    ((= (mod n 15) 0) "fizzbuzz")
    ((= (mod n 3) 0)  "fizz")
    ((= (mod n 5) 0)  "buzz")
    (true              n)))

(defn make-adder [x]
  "Returns a closure — shows captured variable in LIR."
  (fn [y] (+ x y)))

(defn eval-expr [expr]
  "Evaluate an arithmetic expression tree.
   Match dispatch, recursion, let-binding, conditional error —
   produces a complex CFG with many blocks and cross-edges."
  (match expr
    ([:lit n]   n)
    ([:neg a]   (- 0 (eval-expr a)))
    ([:add a b] (+ (eval-expr a) (eval-expr b)))
    ([:sub a b] (- (eval-expr a) (eval-expr b)))
    ([:mul a b] (* (eval-expr a) (eval-expr b)))
    ([:div a b]
      (let* ([divisor (eval-expr b)]
             [dividend (eval-expr a)])
        (if (= divisor 0)
          (error [:division-by-zero "division by zero in expression"])
          (/ dividend divisor))))))

# ── Render each function to SVG ─────────────────────────────────────

(defn render-cfg [f name]
  "Render a function's CFG to an SVG file."
  (let* ((mmd (fn/cfg f :mermaid))
         (svg (selkie/render mmd))
         (path (append name ".svg")))
    (file/write path svg)
    (display "  wrote ")
    (display path)
    (display "\n")))

(display "Rendering control flow graphs...\n")
(render-cfg identity "identity")
(render-cfg factorial "factorial")
(render-cfg fizzbuzz "fizzbuzz")
(render-cfg make-adder "make-adder")
(render-cfg eval-expr "eval-expr")
(display "Done.\n")
