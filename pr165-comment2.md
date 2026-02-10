## Addendum: Unify Variable Storage — Eliminate the Scope Stack

After deeper analysis, the right fix is not to patch around the three-storage-system problem but to **eliminate the scope_stack entirely** and unify all local/captured variable storage into the closure environment. Here's why and how.

### Why Unify

The scope_stack is not just mismatched with the compiler — it's fundamentally broken:

1. **It leaks across call frames.** The `scope_stack` is a single VM-wide `Vec<RuntimeScope>` that is never saved/restored around `Call` or `TailCall`. A callee's `PushScope` frames are visible to the caller after return. A callee can read and write the caller's locally-defined variables. An early `Return` leaves orphaned scope frames.

2. **It's slow.** Every `LoadGlobal` (the most common variable-read instruction) walks the entire scope_stack top-to-bottom doing `HashMap::get()` at each level before falling back to `vm.globals`. This is O(depth × hash-lookup) on the hot path.

3. **It has an addressing mismatch.** The compiler resolves variables by positional index into `[captures..., params...]`. The scope_stack resolves by symbol ID into `HashMap<u32, Value>`. The `StoreScoped` instruction bridges these by treating a positional index as a symbol ID, which is simply wrong.

4. **`LoadScoped` is already a no-op.** The read-side counterpart to `StoreScoped` was never implemented — it's a dead placeholder. This means the scope_stack has never been a complete read/write system; it's vestigial scaffolding.

Meanwhile, the closure_env (`Rc<Vec<Value>>`) is architecturally sound — it's per-call-frame, O(1) indexed, and already handles params + captures correctly. Its only limitation is immutability.

### The Target Architecture: Two Systems, Not Three

| Storage | Addressed by | Used for | Scope |
|---|---|---|---|
| **Closure env** (`Rc<Vec<RefCell<Value>>>`) | Positional index | All local vars: params, captures, `define`d locals | Per-call-frame |
| **Globals** (`HashMap<u32, Value>`) | Symbol ID | Top-level `define`, primitives | VM-wide |

The scope_stack, `LoadScoped`, `StoreScoped`, `DefineLocal`, `PushScope`, `PopScope` — all eliminated.

### Implementation Plan

#### Phase 1: Make Closure Env Mutable

Change the closure environment from `Rc<Vec<Value>>` to `Rc<Vec<RefCell<Value>>>` (or equivalently, `Rc<RefCell<Vec<Value>>>`).

**`Rc<RefCell<Vec<Value>>>` is simpler** and sufficient — you only need interior mutability on the Vec, not per-slot. The borrow is short-lived (one instruction at a time), so there's no risk of borrow conflicts within a single-threaded VM.

**Files to change:**
- `src/value.rs`: `Closure.env` type: `Rc<Vec<Value>>` → `Rc<RefCell<Vec<Value>>>`
- `src/vm/variables.rs`: `handle_load_upvalue` — borrow the `RefCell`, index into the Vec. `handle_store_upvalue` — borrow mutably, write the slot. Remove the "Cannot mutate closure environment variables yet" error.
- `src/vm/closure.rs`: `handle_make_closure` — wrap captured values in `RefCell`
- `src/vm/mod.rs`: Call/TailCall — build `new_env` as `Rc<RefCell<Vec<Value>>>`, pass it to `execute_bytecode`
- Signature of `execute_bytecode`: change `closure_env: Option<&Rc<Vec<Value>>>` to `Option<&Rc<RefCell<Vec<Value>>>>`

#### Phase 2: Support `define` Inside Lambda Bodies via Env Extension

When `define` appears inside a lambda body, the variable needs a slot in the closure env. Two approaches:

**Approach A (compile-time allocation — recommended):** During AST conversion, count all `define`s in the lambda body (the compiler already does `collect_defines()` for forward references). Pre-allocate slots in the closure env layout: `[captures..., params..., locals...]`. Each `define` gets a known positional index at compile time. `define` compiles to `StoreUpvalue(0, index)`. Reads compile to `LoadUpvalue(0, index)`. `set!` compiles to `StoreUpvalue(0, index)`. The env Vec is pre-sized at call time with `Nil` in the local slots.

**Approach B (runtime extension):** `define` appends a new slot to the env Vec at runtime. This requires the `RefCell<Vec<Value>>` to support `push()`. Reads/writes use the index assigned at push time. This is simpler but means the compiler can't know the index of a `define`d variable until runtime, which breaks the current compile-time index resolution. Not recommended.

**Go with Approach A.** The compiler already has `collect_defines()` which walks `Begin` blocks and finds all `define` names. Extend this to assign positional indices to each local `define`, appended after the params in the env layout.

**Changes for Approach A:**
- `src/compiler/converters.rs`: In the `lambda` handler, after parsing params, scan the body for `define`s. Assign each a slot index = `num_captures + num_params + local_define_index`. Push these names into the scope_stack (the compile-time one, not the runtime one) so subsequent variable resolution finds them. When resolving a variable reference or `set!` target that hits a local-define name, emit `Expr::Var` / `Expr::Set` with `depth=0` and the assigned index.
- `src/compiler/compile.rs`: `Expr::Define` inside a lambda → emit `StoreUpvalue(0, index)` instead of `DefineLocal`. `Expr::Set` inside a lambda with a known local index → emit `StoreUpvalue(0, index)` instead of `StoreScoped`. Remove the `StoreScoped` path entirely.
- `src/vm/mod.rs`: When calling a closure, pre-size the env Vec to `num_captures + num_params + num_locals`, filling local slots with `Nil`.
- `src/value.rs`: Add `num_defines: usize` to `Closure` (or reuse `num_locals` to mean `num_params + num_defines`).

#### Phase 3: Remove the Scope Stack

Once all variable access goes through `LoadUpvalue`/`StoreUpvalue` (for locals) or `LoadGlobal`/`StoreGlobal` (for globals), the runtime scope_stack is dead code.

**Remove:**
- `src/vm/scope/` — the entire module (`scope_stack.rs`, `runtime_scope.rs`, `handlers.rs`)
- `Instruction::PushScope`, `PopScope`, `LoadScoped`, `StoreScoped`, `DefineLocal` — from `bytecode.rs` and the dispatch loop in `mod.rs`
- `vm.scope_stack` field from `VM` struct in `core.rs`
- The scope-stack-first check in `handle_load_global` and `handle_store_global` (variables.rs lines 13-16, 40-51) — these become simple `vm.globals` lookups

**Keep:**
- The compile-time `scope_stack: Vec<Vec<SymbolId>>` in `converters.rs` — this is a separate concept (compile-time name resolution), not a runtime data structure. It stays.

#### Phase 4: Shared Mutable Captures (Correct Closure Semantics)

This is the one genuinely hard part. When an inner lambda captures a variable from an outer lambda and both can mutate it, they need to share the same mutable cell. With the current architecture, `MakeClosure` copies values out of the outer env into the inner env — mutations to one are invisible to the other.

The classic solution (used by Scheme, Lua, Python, etc.) is **boxing**: mutable captured variables are stored as `Rc<RefCell<Value>>` (a "cell" or "box"). Both the outer and inner closures hold a reference to the same `Rc<RefCell<Value>>`. Reads and writes go through the `RefCell`.

**This can be deferred.** For now, captures can remain copy-on-capture (current behavior). The fix for `set!` inside lambda bodies (Phase 2) only requires mutability of the *current* call frame's env, not shared mutability across frames. Add a compile-time error for `set!` on a captured variable (one from an outer lambda) — "cannot mutate captured variable; use a mutable binding in the current scope instead." This is honest and avoids silent incorrectness.

When you're ready to implement shared captures later:
- Introduce a `Value::Cell(Rc<RefCell<Value>>)` variant
- During compilation, identify variables that are both captured by an inner lambda AND mutated (by `set!` in either the defining scope or the capturing scope)
- For those variables, the `define` or parameter binding stores a `Value::Cell` instead of a raw value
- `LoadUpvalue` on a cell transparently unwraps it; `StoreUpvalue` on a cell writes through it
- `MakeClosure` copies the `Rc<RefCell<Value>>` (not the inner value), so both closures share the cell

### Migration Path and Risk Mitigation

The phases are ordered to be independently testable:

1. **Phase 1** (mutable env) is a pure refactor — no behavioral change, all existing tests should pass. The `StoreUpvalue` error goes away but nothing emits `StoreUpvalue` yet.
2. **Phase 2** (local defines in env) fixes issue #106 and the test in this PR. Run the full test suite plus the new `set!`-in-lambda test.
3. **Phase 3** (remove scope_stack) is a cleanup — if any test breaks, it reveals code that was silently depending on the scope_stack's cross-call leakage (which is a bug, not a feature).
4. **Phase 4** (shared captures) is optional and can be deferred indefinitely.

### What About Block/Loop Scoping?

The scope_stack currently provides block/loop scoping via `PushScope`/`PopScope` — variables defined inside a `begin` block or `while` loop are scoped to that block. Without the scope_stack, how do we handle this?

**Answer: the same way lambdas handle it — by compile-time slot allocation.** Variables defined in a block or loop get slots in the enclosing lambda's env, just like other local defines. Their "scoping" is enforced at compile time by the converter's `scope_stack` (the compile-time one): the name is only visible within the block's extent during name resolution. At runtime, the slot exists for the full lambda lifetime but is only written/read within the block's bytecode range. This is exactly how C handles block-scoped locals — they get stack slots that exist for the full function frame, but the compiler ensures they're only accessed within their lexical scope.

If you want to reclaim the slot or reset it to `Nil` at block exit (to avoid holding references longer than necessary), emit a `StoreUpvalue(slot, Nil)` at the end of the block. But this is an optimization, not a correctness requirement.
