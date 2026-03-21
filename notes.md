# Async-First Runtime: Session 2 Notes

## What was done

### Root cause: JIT signal annotations

The "breaks after ~10 operations" bug across stream I/O, protect+ev/run,
and coroutine control-passing was a single root cause: `fiber/resume` and
`coro/resume` had signal `SIG_ERROR | SIG_RESUME` but were missing
`SIG_YIELD`. Since `SIG_RESUME` is VM-internal (not in `may_suspend()`'s
check for `SIG_YIELD | SIG_DEBUG`), functions calling `coro/resume` were
inferred as non-suspending. The JIT compiled them without
yield-through-call checks. When the resumed fiber did I/O (SIG_IO), the
raw `#<io-request>` leaked through as a return value instead of being
dispatched to the scheduler.

Fix: added `SIG_YIELD` to both `fiber/resume` and `coro/resume` signal
bits (`src/primitives/fibers.rs`, `src/primitives/coroutines.rs`).

### JIT side-exit disabled

With correct signal annotations, more functions are recognized as
yielding. JIT side-exit (yield-through-call frame reconstruction) is
buggy — it corrupts locals on resume (e.g., `acc` in `stream/collect`
becomes invalid). Disabled in `src/jit/compiler.rs` by rejecting
`may_suspend()` functions. `JitError::Yielding` is now handled
gracefully in `jit_entry.rs` (falls back to interpreter, no panic).

### Async ReadLine EOF fix

`src/io/completion.rs`: when the async backend gets EOF (result_code=0)
for a ReadLine, it now checks the per-fd buffer for a partial last line
before returning nil. Previously it returned nil immediately, losing the
last line of files without trailing newline. The sync backend already
handled this correctly.

### ev/gather

`stdlib.lisp`: new function `ev/gather` — like `ev/run` but returns
thunk results. Single thunk returns a single value; multiple thunks
return a list. Uses `fiber/value` after pump completes.

### Stream test cleanup

`tests/elle/streams.lisp`: replaced `ev/spawn` (returns fiber handle)
with `ev/gather` (returns result) for all port-to-stream tests. Each
test gets its own async scheduler for I/O isolation.

### ELLE_JIT_THRESHOLD

`src/vm/core.rs`: JIT hotness threshold configurable via
`ELLE_JIT_THRESHOLD` env var (default 10). Useful for debugging:
- `ELLE_JIT_THRESHOLD=1` — JIT everything immediately (exposes JIT bugs)
- `ELLE_JIT_THRESHOLD=999999` — effectively disable JIT

### Test suite additions

- `tests/elle/fiber_stress.lisp` — sustained resume loops,
  yield-through-call, deep call chains, resume values, interleaved
  coroutines, SIG_IO inside coroutine body, JIT hotness boundary
- `tests/elle/fiber_io_stress.lisp` — sustained port/lines, nested
  ev/run, protect+ev/run, ReadLine EOF handling

## What's broken

### 1. subprocess.lisp regression

`subprocess/system "echo" ["hello"]` returns non-empty stderr. The
`subprocess/system` stdlib function reads stdout/stderr via
`stream/read-line` inside fibers. With the signal fix, these functions
are now recognized as yielding, which changes how the scheduler
dispatches their I/O. The deep stack trace (19+ frames at `<stdlib>:1106`
= pump loop's `process-completions`) suggests the subprocess I/O isn't
completing correctly.

Likely cause: `subprocess/system` uses `sync-scheduler` internally to
run its I/O fibers. But the sync-scheduler calls `fiber/resume` which
now has `SIG_YIELD`. In the context of `execute_scheduled` (which wraps
everything in `ev/run`), the sync-scheduler's `fiber/resume` call is
inside an async fiber. The `SIG_YIELD` from `fiber/resume` propagates
up through the async scheduler, causing the I/O dispatch path to change
from sync to async mid-operation. The subprocess's pipe reads get
routed to the async backend instead of the sync backend.

### 2. JIT side-exit frame reconstruction

Disabled but not fixed. When a JIT-compiled function yields through a
call (callee does I/O), `elle_jit_yield_through_call` spills locals and
operands to build a `SuspendedFrame`. On resume, the interpreter
restores from these frames. Something in the spill/restore corrupts
locals — `acc` in `stream/collect` becomes an invalid value after
resume. The `num_locals` / `num_spilled` split or the env reconstruction
(`captures + locals`) may be wrong.

### 3. ev/run state leak (may be resolved)

Session notes bug #3 (protect stops catching errors after ~10 ev/run
calls) was confirmed as JIT-specific during debugging. With the signal
fix and JIT side-exit disabled, the standalone reproduction passes.
Needs re-verification once subprocess is fixed to confirm it's fully
resolved.

## What to investigate next

### subprocess/system I/O path

The subprocess/system function in stdlib.lisp uses sync-scheduler to
run fiber/resume loops for reading stdout/stderr pipes. With
fiber/resume now carrying SIG_YIELD, the signal propagation through
the async scheduler context may be wrong. Two possible fixes:

1. **Make subprocess/system use the current scheduler** instead of
   always using sync-scheduler. If we're already in an async context,
   I/O should go through the async backend.

2. **Ensure sync-scheduler works inside async context.** The
   sync-scheduler creates its own `(io/backend :sync)` and calls
   `io/execute` directly. If the SIG_YIELD from fiber/resume
   propagates past the sync-scheduler's loop, the async scheduler
   catches it and tries to dispatch it — but the sync backend already
   handled it.

### JIT side-exit (future)

The long-term fix is CPS transform, which eliminates SuspendedFrame
replay entirely. Short-term, the spill/restore could be debugged by:
- Adding assertions on env length after reconstruction
- Comparing interpreter vs JIT SuspendedFrame contents for the same
  yield point
- Checking `lbox_locals_mask` interaction with yield spilling
