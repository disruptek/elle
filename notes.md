# Async runtime unification — session notes

## Branch

`feature/fiber-trampoline-switch-v2` based on `origin/main` (7a0075d1).

## What was fixed this session

### JIT yield frame env reconstruction (`src/jit/suspend.rs`)

`elle_jit_yield` and `elle_jit_yield_through_call` saved `SuspendedFrame`
with `closure.env` (captures only). The interpreter's `LoadUpvalue` indexes
env for ALL variables — captures, params, and locally-defined vars. On
resume, upvalue indices beyond the capture count panicked.

**Fix:** split spilled buffer using `num_locals` from yield/call-site
metadata. First `num_locals` values are appended to `closure.env` to
reconstruct the full interpreter env; remaining values become the operand
stack. Applied to both helpers.

**Trigger found:** stdlib `map` (bc_len=544) gets JIT-compiled after
enough calls during stream tests. Its `closure.env` has 1 entry
(self-capture), but the full interpreter env needs 15. Signal inference
reports `propagates: 0` for `map` — this is suspicious and may be a
separate signal analysis bug.

### SIG_SWITCH trampoline reverted (`src/vm/fiber.rs`)

The WIP commit changed `handle_fiber_resume_signal` from calling
`do_fiber_resume` inline to returning SIG_SWITCH (deferred resume via
`handle_sig_switch`). This broke `println` inside `parameterize` — the
scheduler calls `fiber/resume` and expects the result synchronously.
Reverting to the inline `do_fiber_resume` fixed it.

## The problem we need to solve

The redis client needs a manager fiber that resumes client fibers, which
do I/O, which involves more fiber resumes. The current architecture has
**three separate execution modes** for fiber/resume, and **two scheduler
implementations** (Rust and Elle), creating a fractured control flow.

### Current execution modes

1. **`execute_scheduled` (Rust, mod.rs:209)** — top-level event loop.
   Handles `SIG_IO` by executing I/O synchronously via `SyncBackend`,
   then calling `resume_suspended`. Handles `SIG_SWITCH` via
   `handle_sig_switch`. This is the Rust-side scheduler for the root
   fiber.

2. **`sync-scheduler` (Elle, stdlib.lisp:945)** — Elle-level scheduler.
   Calls `fiber/resume`, checks `fiber/status`, dispatches I/O via
   `io/execute`. This is what `println` uses (wraps the write in a
   fiber, runs it through `sync-scheduler`). Also what `ev/spawn` uses
   via `*scheduler*` parameter.

3. **`make-async-scheduler` (Elle, stdlib.lisp:1004)** — async scheduler
   for `ev/run`. Manages a runnable queue and pending I/O map. Pumps
   until all fibers complete. Uses `fiber/resume` and `fiber/bits` to
   route fibers.

### Why this fractures

- `execute_scheduled` handles SIG_IO in Rust. The Elle schedulers handle
  SIG_IO in Elle (via `io/execute`). Same work, different codepaths.
- `handle_sig_switch` at the Rust level trampolines fiber resumes. The
  Elle schedulers call `fiber/resume` which does inline `do_fiber_resume`.
  Same operation, different stack behavior.
- `println` uses `sync-scheduler` to wrap I/O in a fiber. This means
  every print statement creates a fiber, runs a scheduler loop, does I/O,
  tears down. Heavy for something that should be a simple write.

### Rust stack growth

Each `fiber/resume` in the interpreter grows the Rust stack:
```
call_inner → handle_primitive_signal → handle_fiber_resume_signal
  → do_fiber_resume → with_child_fiber → execute_bytecode_saving_stack
    → dispatch loop → [child calls fiber/resume] → (recurse)
```

The `do_fiber_resume` trampoline handles the nested case (fiber A resumes
B resumes C) iteratively. But the initial entry from
`handle_fiber_resume_signal` still adds a full interpreter frame to the
Rust stack. For the redis manager pattern (manager → client → I/O fiber),
this is 3 levels minimum, more with nested streams.

## The case for SIG_SWITCH everywhere

**Pros:**
- Flat Rust stack: fiber/resume never recurses through the interpreter.
  All execution is driven from one top-level loop.
- One codepath: the scheduler IS the top-level loop. No separate
  `sync-scheduler`, no `execute_scheduled` I/O handling. Everything
  goes through the same dispatch.
- Composable: nested fiber/resume just produces more SIG_SWITCH frames
  to unwind. The trampoline handles any depth.
- JIT-friendly: the JIT already handles SIG_SWITCH (yield-through-call
  saves the frame). Unifying on SIG_SWITCH means the JIT and interpreter
  use the same suspension protocol.

**Cons:**
- Every fiber/resume becomes two operations (save frame + resume later)
  instead of one (inline resume). Overhead for the common case where the
  child completes synchronously.
- Parameter frames, call stack, and other fiber state must survive the
  round-trip through SIG_SWITCH. We proved this breaks `parameterize`
  when the scheduler is not SIG_SWITCH-aware.
- The Elle schedulers (`sync-scheduler`, `make-async-scheduler`) call
  `fiber/resume` and expect a result. With SIG_SWITCH, they'd need to
  handle the case where fiber/resume doesn't return a result but instead
  signals "I need to be trampolined." This changes the scheduler API.
- Debugging: the control flow becomes indirect. A fiber/resume call
  doesn't complete at the call site — it bounces through the event loop.

## The case against SIG_SWITCH (keep inline do_fiber_resume)

**Pros:**
- Simple: fiber/resume calls do_fiber_resume, gets a result, continues.
  The scheduler can reason locally about fiber state.
- Works today: the print test, stream combinators, ev/run all work with
  inline resume. Only the Rust stack depth is a concern.
- No scheduler changes needed: the Elle schedulers don't need to handle
  SIG_SWITCH.

**Cons:**
- Rust stack growth is O(fiber nesting depth). For redis with manager +
  client + I/O, that's 3 interpreter frames on the Rust stack per
  operation. Deeper nesting (streams of streams) makes it worse.
- Two I/O codepaths remain: `execute_scheduled` (Rust) and
  `sync-scheduler` (Elle) both handle SIG_IO independently.
- Can't easily move to a true event loop model where all execution is
  cooperative and driven from one place.

## The third option: 100% async from the start

Instead of retrofitting SIG_SWITCH onto the existing architecture, make
ALL user code run inside a scheduler fiber from the outset.

### Design sketch

1. `main` creates a root scheduler (the event loop).
2. User code always runs in a fiber spawned by the root scheduler.
3. `fiber/resume` returns the result inline (via `do_fiber_resume`) when
   the child completes synchronously. No change for the fast path.
4. When the child yields SIG_IO, the child fiber is parked. The
   scheduler performs the I/O and reschedules the child. The PARENT fiber
   (which called fiber/resume) is also parked — it's waiting for the
   child's result.
5. When the child completes (after I/O), the scheduler resumes the parent
   with the child's result. The parent continues from after fiber/resume.

This eliminates `execute_scheduled` (Rust-level I/O handling). All I/O
goes through the scheduler. `sync-scheduler` becomes the default
execution mode, not an opt-in wrapper.

### What changes

- `execute_bytecode` / `execute_scheduled` merge into one entry point
  that runs a scheduler loop.
- `println` no longer needs to create a fiber + scheduler — it just does
  `stream/write` which yields SIG_IO, handled by the always-present
  scheduler.
- `fiber/resume` stays inline for the synchronous case. For the async
  case (child yields SIG_IO), it yields SIG_IO itself (the parent is
  waiting for the child, so the parent also suspends).
- The JIT env reconstruction fix in `suspend.rs` applies regardless of
  which model we use — it's about how JIT frames are saved, not about
  the execution model.

### Open questions

1. **Startup cost.** If all code runs in a fiber, there's a fiber
   allocation for every `elle script.lisp` invocation. Is this
   measurable?
2. **Prelude/stdlib loading.** These run during startup and must not do
   I/O. Can they run outside the scheduler, or does the scheduler need a
   "no I/O" mode?
3. **Parameter inheritance.** When the scheduler resumes a parent fiber
   after a child's I/O completes, do the parent's parameter bindings
   survive correctly? (This is the same issue that broke `parameterize`
   with SIG_SWITCH, but the scheduler-driven model handles it because
   the parent fiber's state is preserved on the fiber, not on the Rust
   call stack.)
4. **Error propagation.** If a child fiber errors during I/O, the
   scheduler needs to propagate the error to the parent. The current
   `do_fiber_resume` handles this inline. A scheduler-driven model needs
   equivalent error routing.
5. **Backward compatibility.** Code that calls `fiber/resume` today
   expects synchronous semantics. If fiber/resume can now suspend the
   caller, does existing code break?

## Files that matter

| File | What it does | What needs to change |
|------|-------------|---------------------|
| `src/vm/mod.rs` | `execute_scheduled`, `handle_sig_switch` | Merge into unified scheduler loop |
| `src/vm/fiber.rs` | `handle_fiber_resume_signal`, `do_fiber_resume`, `with_child_fiber` | Decide inline vs. trampoline |
| `src/vm/execute.rs` | `execute_bytecode_saving_stack`, `execute_bytecode_from_ip` | Unchanged (execution primitives) |
| `src/vm/call.rs` | `call_inner` suspend paths (JIT + interpreter) | Frame saving stays the same |
| `src/vm/core.rs` | `resume_suspended` | Frame replay stays the same |
| `src/jit/suspend.rs` | `elle_jit_yield`, `elle_jit_yield_through_call` | ✅ Fixed (env reconstruction) |
| `src/jit/calls.rs` | `jit_handle_primitive_signal`, `elle_jit_call` | Signal checks already fixed by #661 |
| `stdlib.lisp` | `sync-scheduler`, `ev/run`, `println`, `ev/spawn` | Simplify if scheduler is built-in |
| `lib/redis.lisp` | Not yet written | Needs working async fiber resume |

## Redis client status

See `~/git/elle/reddy.md` for the architectural decisions. The library is
at `lib/redis.lisp` (not yet created). It needs:

- TCP connect (`tcp/connect`) — async I/O
- RESP2 encode/decode — pure Elle, no I/O
- Manager fiber pattern — fiber/resume heavy
- Pub/sub — long-lived subscription fibers
- Pipelining — batch send + batch read

All of these depend on reliable async fiber/resume. The current inline
`do_fiber_resume` works but grows the Rust stack. The redis manager
pattern (manager → client → I/O) adds 3+ interpreter frames per
operation. With streams on top (pub/sub → stream/map → ...), the depth
increases further.

## Test commands

```bash
cargo run -- tests/elle/streams.lisp      # stream/zip (JIT env fix)
cargo run -- tests/elle/print.lisp        # println + parameterize
make smoke                                # full smoke (~15s)
make test                                 # build + examples + scripts + unit tests (~2min)
```

## CFG

`fiber-resume-cfg.dot` / `fiber-resume-cfg.svg` — control flow graph of
the current fiber/resume paths. View with `feh fiber-resume-cfg.svg`.
