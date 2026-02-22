# vm

Bytecode execution. Stack-based operand handling with register-addressed locals.

## Responsibility

Execute bytecode instructions. Manage:
- Operand stack
- Global bindings
- Call frames and stack traces
- Closure environments
- Fiber state and signals

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
- `SIG_ERROR` (1): Error. Error tuple in `fiber.signal` as `[:keyword "message"]`.
- `SIG_YIELD` (2): Fiber yield. Value in `fiber.signal`, continuation in `fiber.continuation`.
- `SIG_RESUME` (8): VM-internal. Fiber primitive requests VM-side execution.

The public `execute_bytecode` method is the translation boundary — it converts
`SignalBits` to `Result<Value, String>` for external callers. On `SIG_ERROR`,
it extracts the condition from `fiber.signal` and formats the error message.

Instruction handlers no longer return `Result<(), String>`. VM bugs panic
immediately. User errors set `fiber.signal` to `(SIG_ERROR, Value::condition(...))`
and push `Value::NIL` to keep the stack consistent.

## Primitive dispatch (NativeFn)

All primitives are `NativeFn`: `fn(&[Value]) -> (SignalBits, Value)`. The VM
dispatches the return signal in `handle_primitive_signal()` (`call.rs`):
- `SIG_OK` → push value to stack
- `SIG_ERROR` → store `(SIG_ERROR, value)` in `fiber.signal`, push NIL
- `SIG_YIELD` → store in `fiber.signal`, return yield
- `SIG_RESUME` → dispatch to fiber handler

All SIG_RESUME primitives (including coroutine wrappers) return
`(SIG_RESUME, fiber_value)`. The VM
swaps the child fiber into `vm.fiber` via `std::mem::swap`, executes the child,
then swaps back. The child's `saved_context` stores bytecode/constants/env/IP
for resumption after signals. The dispatch loop saves the IP into
`fiber.suspended_ip` before returning on signal paths.

## Dependents

- `primitives/` - NativeFn primitives; SIG_RESUME signals trigger VM-side execution
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

5. **Yield uses first-class continuations.** On yield, a `ContinuationFrame`
   captures bytecode, constants, env, IP, and stack. When yield propagates
   through Call instructions, each caller's frame is appended to form a chain.
   `resume_continuation` replays frames from innermost to outermost.

6. **VM bugs panic, user errors set `fiber.signal`.** Instruction handlers
   return `()` (not `Result`). VM bugs (stack underflow, bad bytecode) panic
   immediately. User errors (type mismatch, division by zero) set
   `fiber.signal` to `(SIG_ERROR, Value::condition(...))`, push `Value::NIL`
   to keep the stack consistent, and return normally. The dispatch loop checks
   `fiber.signal` for `SIG_ERROR` after each instruction and returns
   immediately. See `set_error()` in `call.rs` for the helper.

## Key VM fields

| Field | Type | Purpose |
|-------|-------|---------|
| `fiber` | `Fiber` | Root fiber: stack, call frames, exception state |
| `globals` | `Vec<Value>` | Global bindings by SymbolId |
| `jit_cache` | `HashMap<*const u8, Rc<JitCode>>` | JIT code cache |
| `scope_stack` | `ScopeStack` | Runtime scope stack |

### Key Fiber fields (on `vm.fiber`)

| Field | Type | Purpose |
|-------|-------|---------|
| `stack` | `SmallVec<[Value; 256]>` | Operand stack |
| `call_stack` | `Vec<CallFrame>` | For stack traces |
| `call_depth` | `usize` | Stack overflow detection |
| `signal` | `Option<(SignalBits, Value)>` | Signal from execution (errors, yields) |
| `continuation` | `Option<Value>` | Continuation on yield (for yield-through-calls) |
| `signal_mask` | `SignalBits` | Which signals this fiber catches |

## Continuation mechanism

When a fiber yields (via the yield instruction):

1. **Yield instruction** captures innermost frame: bytecode, constants, env,
   IP (after yield), stack
2. **Call handler** (if yield propagates through a call) appends caller's
   frame to the continuation chain
3. **Frame ordering**: innermost (yielder) first, outermost (caller) last
4. **Resume** iterates frames forward, calling `execute_bytecode_from_ip`
   for each

Key methods:
- `execute_bytecode_from_ip`: Executes from a given IP
- `resume_continuation`: Replays frame chain, handles re-yields and errors

## Files

| File | Lines | Content |
|------|-------|---------|
| `mod.rs` | ~350 | VM struct, VmResult, public interface |
| `dispatch.rs` | ~556 | Main execution loop, instruction dispatch |
| `call.rs` | ~880 | Call, TailCall, Return, error handling, SIG_RESUME fiber handler |
| `execute.rs` | ~160 | Helper functions for instruction execution |
| `core.rs` | ~460 | `resume_continuation`, continuation replay |
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
