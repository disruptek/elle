# Elle Roadmap

> Last updated: February 2026

## Where we are

Elle has a working new compilation pipeline:

```
Source → Reader → Syntax → Expander → Syntax → Analyzer → HIR → Lowerer → LIR → Emitter → Bytecode → VM
```

The pipeline handles: lexical scoping with `BindingId`, closure capture
analysis with `LocalCell` for mutable captures, effect inference (`Pure`,
`Yields`, `Polymorphic`), tail call optimization, `handler-case` exception
handling, and coroutines with first-class continuations.

The CPS tree-walking interpreter is gone (~4,400 lines deleted). Coroutines
run on bytecode with continuation frames that capture and restore full VM
state across yield boundaries, including exception handler state.

Yield is a proper LIR terminator that splits functions into multiple basic
blocks. The emitter carries stack simulation state across yield boundaries.

### What still exists but shouldn't

- **`value_old/`** — runtime types (`Closure`, `Coroutine`, `Condition`,
  `Arity`, `SymbolId`, etc.) still live here and are re-exported through
  `value/mod.rs`. The old `Value` enum also lives here, used only by
  bridge code and the JIT.

- **Cranelift JIT** — `compiler/cranelift/` (~5,300 lines), plus
  `jit_coordinator.rs`, `jit_executor.rs`, `jit_wrapper.rs`,
  `primitives/jit.rs`. Compiles from the old `Expr` AST, not LIR.
  The new pipeline never populates `source_ast` on closures, so the JIT
  is effectively inert for all new-pipeline code.

- **Old compiler** — `compiler/compile/` (~1,935 lines), zero production
  callers. `compiler/converters/` (~2,020 lines), `compiler/ast.rs` (the
  `Expr` type), `compiler/analysis.rs`, `compiler/optimize.rs`,
  `compiler/patterns.rs`, `compiler/macros.rs`, `compiler/linter/`,
  `compiler/symbol_index.rs`. All replaced by HIR/LIR equivalents.

- **Old effect inference** — `effects/inference.rs` (~616 lines) operates
  on `Expr`. The new pipeline does effect inference inline in
  `hir/analyze.rs`.

- **Empty LocationMap** — the infrastructure for instruction-offset to
  source-location mapping exists (`LocationMap`, `VM.location_map`,
  `capture_stack_trace`), but it's never populated. Error messages lack
  file:line:col information.

- **8 ignored tests** — yield-from delegation (1), defmacro persistence
  (3), macro? primitive (1), expand-macro (1), module-qualified names (2).

### What works well

- Full compilation pipeline with property tests
- TCO (tail call optimization) — handles 50,000+ depth
- First-class continuations for coroutines across call boundaries
- Exception handlers preserved across yield/resume
- NaN-boxed 8-byte Value (Copy semantics)
- elle-lint and elle-lsp use the new pipeline exclusively
- elle-doc generates the documentation site from Elle code
- Clean clippy, all tests pass

## What we're doing next

### Phase B: Hammer time

Remove dead code, migrate types, implement source location tracking.

#### PR 1: Unwire JIT, delete old pipeline

Delete:
- `src/compiler/cranelift/` (14 files)
- `src/compiler/compile/` (2 files)
- `src/compiler/converters/` (8 files)
- `src/compiler/ast.rs`, `analysis.rs`, `optimize.rs`, `patterns.rs`,
  `capture_resolution.rs`, `macros.rs`, `linter/`, `symbol_index.rs`
- `src/compiler/jit_coordinator.rs`, `jit_executor.rs`, `jit_wrapper.rs`
- `src/effects/inference.rs`
- `src/primitives/jit.rs`
- `src/value/closure.rs` (unused unified type)
- `benches/cranelift_jit_benchmarks.rs`, `jit_vs_bytecode.rs`

Remove from types: `JitLambda`, `JitClosure`, `source_ast` on Closure.
Remove from VM: JitClosure dispatch paths, JIT encoding functions.
Remove from Cargo.toml: cranelift dependencies.
Remove from main.rs: `--jit` flag, JIT context init/cleanup.

Estimated removal: ~12,500 lines, 4 crate dependencies.

#### PR 2: Migrate value_old into value/

Move all types from `value_old/mod.rs` into `value/` submodules:
- `SymbolId`, `Arity`, `TableKey`, `NativeFn`, `VmAwareFn` → `value/types.rs`
- `Closure` (minus `source_ast`) → `value/closure.rs`
- `Coroutine`, `CoroutineState` → `value/coroutine.rs`
- `LibHandle`, `CHandle` → `value/ffi.rs`
- Wire in the new `Condition` (already in `value/condition.rs`)
- `ThreadHandle` data already in `value/heap.rs`

Eliminate: old `Value` enum, `old_value_to_new`/`new_value_to_old` bridges,
`is_value_sendable_old`. Delete `value_old/`.

#### PR 3: Implement LocationMap

Source locations flow: Syntax Span → HIR span → LIR span → bytecode offset.

- Add `span: Option<Span>` to LIR instructions (via wrapper type)
- Lowerer propagates HIR spans to LIR
- Emitter builds `LocationMap` during emission
- `Closure` gains `location_map: Rc<LocationMap>`
- `CompileResult` carries location map
- VM uses per-closure location map in `capture_stack_trace`
- Thread transfer clones location map alongside bytecode

#### PR 4: Thread transfer tests

Property tests and integration tests confirming closures transfer correctly
between threads with source location data intact.

### Phase C: Macros and modules

Un-ignore the 7 macro/module tests by implementing:

- **defmacro persistence** — the Expander needs to persist across
  compilations within a session. Currently created fresh per `compile_new`
  call, so macros defined in one form aren't visible in the next.

- **macro? primitive** — requires runtime access to the macro registry.

- **expand-macro** — requires runtime macro expansion on quoted forms.

- **Module-qualified names** — `string/upcase`, `math/abs` syntax in the
  new pipeline. The HIR analyzer needs to resolve `module/name` references.

### Phase D: Hammer time #2

Final cleanup pass:
- Update all AGENTS.md and README.md files
- Update `docs/CPS_REWORK.md` to reflect completed state
- Audit file sizes (300-line target)
- Remove stale documentation (`docs/CPS_DESIGN.md`,
  `docs/LEXICAL_SCOPE_REFACTOR.md` — completed work)
- Update `refactor/` docs or remove if no longer needed

### Phase E: JIT (future)

Rewrite Cranelift JIT to consume LIR instead of Expr. This is a from-scratch
implementation using the preserved git history as reference. Prerequisites:
all of the above is done, LIR is stable, LocationMap works.

## Decisions made

| Decision | Rationale |
|----------|-----------|
| Delete JIT code, not feature-flag it | Git preserves history. Dead code has maintenance cost. |
| Full first-class continuations | More work than simple coroutine support, but composable and future-proof. |
| Yield as LIR terminator | Proper control flow modeling; prerequisite for future JIT. |
| Single execution path (bytecode) | CPS interpreter deleted. Simpler, fewer bugs, one thing to optimize. |
| `handler-case` not try/catch | Condition system is the exception mechanism. No Java-style try/catch. |
| Nil ≠ empty list | `nil` is falsy (absence), `()` is truthy (empty list). Lists terminate with `()`. |
| New pipeline skips Expr | Syntax → HIR directly. Expr was the old AST; no reason to generate it. |
| TCO via trampoline | `pending_tail_call` on VM, loop in `execute_bytecode`. Works for mutual recursion. |

## Known defects

- LocationMap is unpopulated — errors lack source locations
- `handler-bind` is a stub (parsed, codegen ignores handlers)
- `InvokeRestart` opcode allocated but VM handler is no-op
- `signal`/`warn`/`error` are constructors, not signaling primitives
- yield-from delegation not implemented
- defmacro doesn't persist across compilation units
- Module-qualified names not supported in new pipeline
