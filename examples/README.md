# Elle Examples

Each file is a self-verifying program that exits 0 on success, 1 on failure.
CI runs every `.lisp` file here with a 10-second timeout. These are integration
tests, not tutorials — they exercise the full compilation pipeline (reader →
expander → analyzer → lowerer → emitter → VM).

## Running

```bash
cargo run -- examples/basics.lisp     # single file
cargo test --test '*'                  # all examples as part of the test suite
```

## Assertions

`assertions.lisp` defines shared assertion helpers. All other files load it:

```lisp
(import-file "./examples/assertions.lisp")
```

Available assertions: `assert-eq`, `assert-true`, `assert-false`,
`assert-list-eq`, `assert-not-nil`, `assert-string-eq`. Each prints
expected vs actual on failure and exits with code 1.

## Files

### Refreshed (idiomatic, consolidated)

| File | Theme | Covers |
|------|-------|--------|
| `basics.lisp` | Type system tour | Immediates, truthiness, arithmetic, math, comparison, bitwise, conversions, `@` mutability split, bytes/blobs, boxes, equality |
| `functions.lisp` | Functional toolkit | `defn`/`fn`, lexical scope, closures, HOFs, composition, pipelines, variadic, mutual recursion, `block`/`break`, mutable captures |
| `control.lisp` | Expression evaluator | `if`, `cond`, `case`, `when`/`unless`, `if-let`/`when-let`, `while`, `forever`, `block`/`break`, `match` (full pattern coverage), `each`, `->` / `->>` |
| `collections.lisp` | Contact book app | Literal syntax, mutability split, polymorphic `get`/`put`, destructuring, `each`, threading, splice, string ops, grapheme clusters |
| `destructuring.lisp` | Unpacking data | Silent nil semantics, wildcards, `& rest`, nested patterns, `var`+`set`, `let`/`let*`, struct/table by-key, match dispatch on struct tags |
| `errors.lisp` | Error handling | `error`, `try`/`catch`, `protect`, `defer`, `with`, error propagation, safe wrappers, validation patterns |
| `coroutines.lisp` | Cooperative sequences | `coro/new`, `yield`, lifecycle tracking, Fibonacci generator, closure captures, interleaving, nesting, `yield*` delegation |
| `meta.lisp` | Macros and hygiene | `defmacro`, quasiquote/unquote, macro composition, `gensym`, macro hygiene, `datum->syntax`, `syntax->datum` |
| `concurrency.lisp` | Parallel threads | `spawn`, `join`, closure captures across threads, `current-thread-id`, parallel computation |
| `processes.lisp` | Erlang-style actors | Fiber-based scheduler, message passing, `spawn`/`recv`/`!`, links, `trap-exit`, crash propagation |
| `io.lisp` | Files, JSON, modules | `slurp`/`spit`, paths, directories, `json-parse`/`json-serialize`, `import-file` |
| `introspection.lisp` | Looking inside | Clock primitives, `time/elapsed`, closure introspection, `disbit`/`disjit`, `debug-print`, `trace`, benchmarking |
| `ffi.lisp` | C interop | `ffi/native`, `ffi/defbind`, memory management, structs, variadic calls, callbacks (`qsort`) |

### To be removed (superseded by refreshed files)

`types.lisp`, `math-and-logic.lisp`, `closures.lisp`,
`higher-order-functions.lisp`, `scope-and-binding.lisp`,
`control-flow.lisp`, `syntax-sugar.lisp`, `lists-and-arrays.lisp`,
`tables-and-structs.lisp`, `string-operations.lisp`, `json.lisp`,
`modules.lisp`, `debugging.lisp`, `debugging-profiling.lisp`,
`benchmarks.lisp`, `time.lisp`, `meta-programming.lisp`

### Other

| File | Purpose |
|------|---------|
| `assertions.lisp` | Shared assertion library |
| `hello.lisp` | Smoke test (no assertions) |
