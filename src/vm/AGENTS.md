# vm

Bytecode execution. Stack-based operand handling with register-addressed locals.

## Responsibility

Execute bytecode instructions. Manage:
- Operand stack
- Global bindings
- Call frames and stack traces
- Closure environments
- Exception handlers
- Coroutine state

Does NOT:
- Compile code (that's `compiler/`, `hir/`, `lir/`)
- Parse source (that's `reader/`)
- Define primitives (that's `primitives/`)

## Interface

| Type | Purpose |
|------|---------|
| `VM` | Global state + root Fiber. Per-execution state lives on `vm.fiber` |
| `SignalBits` | Internal return type: `SIG_OK`, `SIG_ERROR`, `SIG_YIELD`, `SIG_DEBUG`, `SIG_RESUME`, `SIG_FFI` |
| `CallFrame` | Function name, IP, frame base |
| `ExceptionHandler` | Handler offset, finally offset, stack depth |

## Data flow

```
Bytecode + Constants
    │
    ▼
execute_bytecode()  ← public API, returns Result<Value, String>
    │
    ├─► execute_bytecode_inner() → SignalBits
    │       │
    │       ├─► fetch instruction
    │       ├─► dispatch by opcode
    │       ├─► modify stack/locals/globals
    │       ├─► check for exceptions
    │       └─► loop until Return/Yield/Error
    │       │
    │       ▼
    │   SignalBits (result in fiber.signal)
    │
    ▼
Result<Value, String>  ← translation boundary
```

## Signal-based returns

Internal VM methods return `SignalBits` instead of `Result<VmResult, String>`:
- `SIG_OK` (0): Normal completion. Value in `fiber.signal`.
- `SIG_ERROR` (1): Exception. Exception in `fiber.current_exception`.
- `SIG_YIELD` (2): Coroutine yield. Value in `fiber.signal`, continuation in `fiber.continuation`.
- `SIG_RESUME` (8): VM-internal. Coroutine primitive requests VM-side execution.

The public `execute_bytecode` method is the translation boundary — it converts
`SignalBits` to `Result<Value, String>` for external callers.

Instruction handlers no longer return `Result<(), String>`. VM bugs panic
immediately. User errors set `fiber.current_exception` and return normally.

## Primitive dispatch (NativeFn)

All primitives are `NativeFn`: `fn(&[Value]) -> (SignalBits, Value)`. The VM
dispatches the return signal in `handle_primitive_signal()` (`call.rs`):
- `SIG_OK` → push value to stack
- `SIG_ERROR` → extract `Condition` from value, set `fiber.current_exception`
- `SIG_YIELD` → store in `fiber.signal`, return yield
- `SIG_RESUME` → execute coroutine via `handle_coroutine_resume_signal()`

Coroutine primitives (`coroutine-resume`, `yield-from`, `coroutine-next`) set a
`ResumeOp` on the coroutine and return `(SIG_RESUME, coroutine_value)`. The VM
reads and clears the `ResumeOp` in the SIG_RESUME handler, then performs the
actual bytecode execution.

## Dependents

- `primitives/` - NativeFn primitives; SIG_RESUME signals trigger VM-side coroutine execution
- `repl.rs` - runs compiled code
- `main.rs` - file execution

## Invariants

1. **Stack underflow is a VM bug.** Every pop must have a preceding push.
   If you see "Stack underflow," the bytecode or emitter is broken. Handlers
   panic on stack underflow.

2. **Closure environments are immutable Rc<Vec>.** The vec is created at
   closure call time; mutations go through cells, not env modification.

3. **`LocalCell` auto-unwraps on `LoadUpvalue`.** `Cell` (user's `box`) does
   NOT auto-unwrap. This distinction matters.

4. **Tail calls don't grow call_depth.** `TailCall` stores pending call info
   and returns; the outer loop executes it. Stack overflow = tail call bug.

5. **Exception handlers are a stack.** `PushHandler` adds, `PopHandler` removes.
   On exception, unwind to handler's stack_depth and jump to handler_offset.

6. **Coroutines use first-class continuations.** On yield, a `ContinuationFrame`
   captures bytecode, constants, env, IP, stack, and exception handler state.
   When yield propagates through Call instructions, each caller's frame is
   appended to form a chain. `resume_continuation` replays frames from
   innermost to outermost, restoring handler state for each frame.

   **yield-from delegation**: The `Coroutine` struct has a `delegate` field
   (`Option<Rc<RefCell<Coroutine>>>`). When set, `coroutine-resume` forwards
   resume values to the delegate coroutine instead of the outer one. This
   enables transparent delegation where an outer coroutine can yield all
   values from an inner coroutine. When the delegate completes, control
   returns to the outer coroutine.

7. **VM bugs panic, user errors set exceptions.** Instruction handlers return
   `()` (not `Result`). VM bugs (stack underflow, bad bytecode) panic
   immediately. User errors (type mismatch, division by zero) set
   `fiber.current_exception`, push `Value::NIL` to keep the stack consistent,
   and return normally. The interrupt mechanism at the bottom of the
   instruction loop handles the exception. See `handle_div_int` and
   `handle_load_global` for the canonical pattern.

## Key VM fields

| Field | Type | Purpose |
|-------|-------|---------|
| `fiber` | `Fiber` | Root fiber: stack, call frames, exception state, coroutine state |
| `globals` | `Vec<Value>` | Global bindings by SymbolId |
| `jit_cache` | `HashMap<*const u8, Rc<JitCode>>` | JIT code cache |
| `scope_stack` | `ScopeStack` | Runtime scope stack |

### Key Fiber fields (on `vm.fiber`)

| Field | Type | Purpose |
|-------|-------|---------|
| `stack` | `SmallVec<[Value; 256]>` | Operand stack |
| `call_stack` | `Vec<CallFrame>` | For stack traces |
| `call_depth` | `usize` | Stack overflow detection |
| `exception_handlers` | `SmallVec<[ExceptionHandler; 2]>` | Active handlers |
| `current_exception` | `Option<Rc<Condition>>` | Exception being handled |
| `handling_exception` | `bool` | In exception handler code |
| `coroutine_stack` | `Vec<Rc<RefCell<Coroutine>>>` | Active coroutines |
| `pending_yield` | `Option<Value>` | Pending yield from delegation |
| `pending_tail_call` | `Option<(bytecode, constants, env)>` | Deferred tail call |
| `signal` | `Option<(SignalBits, Value)>` | Signal value from execution |
| `continuation` | `Option<Value>` | Continuation on yield (temporary, Step 8 removes) |

## Exception hierarchy

```
condition (1)
├── error (2)
│   ├── type-error (3)
│   ├── division-by-zero (4)
│   ├── undefined-variable (5)
│   └── arity-error (6)
└── warning (7)
    └── style-warning (8)
```

Hierarchy data and `is_exception_subclass(child, parent)` live in
`value/condition.rs` — the single source of truth. Re-exported from `vm/mod.rs`.

## Continuation mechanism

When a coroutine yields:

1. **Yield instruction** captures innermost frame: bytecode, constants, env,
   IP (after yield), stack, exception_handlers, handling_exception
2. **Call handler** (if yield propagates through a call) appends caller's
   frame to the continuation chain
3. **Frame ordering**: innermost (yielder) first, outermost (caller) last
4. **Resume** iterates frames forward, calling `execute_bytecode_from_ip_with_state`
   for each, which restores exception handler state before execution

Key methods:
- `execute_bytecode_from_ip_with_state`: Executes with pre-set handler state
- `resume_continuation`: Replays frame chain, handles re-yields and exceptions

Exception handling across resume:
- Exception check at START of instruction loop catches exceptions from inner frames
- Each frame's handlers are restored before execution
- Exceptions propagate to outer frames if no local handler

## Files

| File | Lines | Content |
|------|-------|---------|
| `mod.rs` | ~350 | VM struct, VmResult, public interface |
| `dispatch.rs` | ~556 | Main execution loop, instruction dispatch |
| `call.rs` | ~550 | Call, TailCall, Return, exception handling, SIG_RESUME handler |
| `execute.rs` | ~300 | Helper functions for instruction execution |
| `core.rs` | ~590 | `resume_continuation`, continuation replay |
| `stack.rs` | ~100 | Stack operations: LoadConst, Pop, Dup |
| `variables.rs` | ~150 | LoadGlobal, StoreGlobal, LoadUpvalue, etc. |
| `control.rs` | ~100 | Jump, JumpIfFalse, Return |
| `closure.rs` | ~100 | MakeClosure |
| `arithmetic.rs` | ~150 | Add, Sub, Mul, Div |
| `comparison.rs` | ~100 | Eq, Lt, Gt, Le, Ge |
| `types.rs` | ~50 | IsNil, IsEmptyList, IsPair, Not |
| `data.rs` | ~100 | Cons, Car, Cdr, MakeVector |
| `scope/` | ~200 | Runtime scope stack (legacy) |

## Truthiness

The VM evaluates truthiness via `Value::is_truthy()`:
- `Value::NIL` → falsy
- `Value::FALSE` → falsy  
- Everything else (including `Value::EMPTY_LIST`, `Value::int(0)`) → truthy

The `Instruction::Nil` pushes `Value::NIL` (falsy).
The `Instruction::EmptyList` pushes `Value::EMPTY_LIST` (truthy).
```
