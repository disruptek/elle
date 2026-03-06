# Elle

[![CI](https://github.com/elle-lisp/elle/actions/workflows/ci.yml/badge.svg)](https://github.com/elle-lisp/elle/actions/workflows/ci.yml)

Elle is a Lisp that compiles to bytecode and runs on a register-based VM. Source locations flow through the entire pipeline for precise error reporting. Memory is managed deterministically without a garbage collector.

## Contents

- [Language](#language)
- [Runtime](#runtime)
- [Tooling](#tooling)
- [Getting Started](#getting-started)
- [License](#license)

## Language

- **Lisp syntax with no parser ambiguity.** Macros operate on syntax trees, not text. See [`prelude.lisp`](prelude.lisp) for hygienic macros and standard forms.

- **Hygienic macros prevent accidental name capture.** Scope sets (Racket-style) protect macro-introduced bindings.
  <details><summary>Example</summary>

  ```lisp
  (defmacro my-swap (a b)
    `(let ((tmp ,a)) (set ,a ,b) (set ,b tmp)))

  (let ([tmp 100] [x 1] [y 2])
    (my-swap x y)
    tmp)  # => 100, not 1
  ```
  </details>

- **Destructuring works in all binding positions.** `def`, `let`, `fn` parameters, `match` patterns — missing values become `nil`, wrong types become `nil`.
  <details><summary>Examples</summary>

  ```lisp
  (def (head & tail) (list 1 2 3 4))
  (def [x _ z] [10 20 30])
  (def {:name n :age a} {:name "Bob" :age 25})
  ```
  </details>

- **Effects are inferred, not declared.** The compiler automatically determines whether a function is pure, yields, or polymorphic. See [`docs/effects.md`](docs/effects.md).

- **Colorless functions, colored fibers.** Any function can run inside a fiber. The fiber's signal mask (set at creation) decides what to catch — not the function. No `async`/`await` coloring.
  <details><summary>Why this matters</summary>

  A pure function and a yielding function have the same type, calling convention, and syntax. The difference is only visible to the compiler's effect analysis, which uses it to optimize, not restrict.
  </details>

## Runtime

- **NaN-boxed values: 8 bytes per value.** Integers, floats, booleans, nil, symbols, keywords, and short strings (≤6 bytes) fit inline. Everything else is a pointer.

- **No garbage collector.** Memory is reclaimed through three mechanisms:
  - **Per-fiber heaps:** Each fiber allocates into a bump arena. When it finishes, the entire heap is freed in O(1).
  - **Zero-copy inter-fiber sharing:** Yielding fibers route allocations to a shared arena; parents read directly from shared memory.
  - **Escape-analysis-driven scope reclamation:** The compiler frees scope allocations at exit when it can prove nothing escapes.

- **Fibers are the primitive; scheduling is user-space.** Elle provides no built-in scheduler. [`examples/processes.lisp`](examples/processes.lisp) demonstrates Erlang-style cooperative scheduling in ~200 lines.
  <details><summary>What this enables</summary>

  Crash isolation (each fiber owns its heap), link-based supervision (signal propagation), and composable concurrency patterns — all built on top of fibers, not baked into the runtime.
  </details>

- **FFI without ceremony.** Load a library, bind a symbol, call it. Struct marshalling, variadic calls, callbacks, and manual memory management all work.
  <details><summary>Example</summary>

  ```lisp
  (def libc (ffi/native nil))
  (ffi/defbind sqrt libc "sqrt" :double @[:double])
  (sqrt 2.0)  # => 1.4142135623730951
  ```
  </details>

- **JIT compiles non-suspending functions to native x86_64.** Effect inference decides what qualifies. Self-tail-calls become native loops. Integer arithmetic gets inline fast paths.

## Tooling

- **Module system is minimal by design.** `import-file` loads Elle source or native `.so` plugins, compiles and executes them, returns the last expression's value. No module declarations, no export lists.
  <details><summary>Parametric modules</summary>

  ```lisp
  # math.lisp
  (fn (precision)
    {:add (fn (a b) (round (+ a b) precision))
     :mul (fn (a b) (round (* a b) precision))})

  # Usage
  (def {:add add :mul mul} ((import-file "math.lisp") 3))
  (add 1.1111 2.2222)  # => 3.333
  ```
  </details>

- **Source-to-source rewriting tool.** The `rewrite` subcommand applies pattern-based rules to Elle source files for refactoring and code generation.

- **Compilation pipeline is fully documented.** See [`docs/pipeline.md`](docs/pipeline.md) for data flow across boundaries and [`AGENTS.md`](AGENTS.md) for architecture details.

## Getting Started

```bash
make                                      # build elle + plugins + docs
./target/release/elle examples/hello.lisp # run a file
./target/release/elle                     # REPL
./target/release/elle lint <file|dir>    # static analysis
./target/release/elle lsp                 # language server
./target/release/elle rewrite <file>     # source-to-source rewriting
```

The `examples/` directory is executable documentation. Each file demonstrates a feature and asserts its own correctness — they run as part of CI.

## License

MIT
