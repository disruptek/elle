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

### Pending refresh

| File | Status |
|------|--------|
| `concurrency.lisp` | Needs idiom update |
| `processes.lisp` | Needs idiom update |
| `meta-programming.lisp` | Needs idiom update |
| `ffi.lisp` | Needs idiom update |
| `io.lisp` | Will absorb `json.lisp` and `modules.lisp` |

### To be removed (superseded by refreshed files)

`types.lisp`, `math-and-logic.lisp`, `closures.lisp`,
`higher-order-functions.lisp`, `scope-and-binding.lisp`,
`control-flow.lisp`, `syntax-sugar.lisp`, `lists-and-arrays.lisp`,
`tables-and-structs.lisp`, `string-operations.lisp`, `json.lisp`,
`modules.lisp`, `debugging.lisp`, `debugging-profiling.lisp`,
`benchmarks.lisp`, `time.lisp`

### Other

| File | Purpose |
|------|---------|
| `assertions.lisp` | Shared assertion library |
| `hello.lisp` | Smoke test (no assertions) |
