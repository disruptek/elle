## Summary

This PR implements Phase 1 and 2 of a comprehensive refactoring to fix issue #106: `set!` not working inside lambda bodies. The changes unify variable storage architecture, moving from a broken three-system approach to a cleaner two-system design (closure env + globals).

**What's Fixed:**
- Issue #106: `set!` now correctly works on locally-defined variables inside lambda bodies
- Enables imperative patterns using `while` loops with mutation
- Closures can use `define` + `set!` for internal mutable state

## Changes

### Phase 1: Make Closure Env Mutable
- Change `Closure.env` type from `Rc<Vec<Value>>` to `Rc<RefCell<Vec<Value>>>`
- Update `handle_load_upvalue` to borrow the RefCell before accessing
- Update `handle_store_upvalue` to borrow_mut and write values (previously returned error)
- All tests pass; no behavioral changes

### Phase 2: Fix set! in Lambda Bodies
- Stop registering locally-defined variables in compile-time `scope_stack`
- Instead, route all `set!` through `StoreGlobal` which checks runtime `scope_stack` first
- Compile local defines as `DefineLocal` (stores in runtime scope_stack by symbol ID)
- This automatically makes reads/writes work via `LoadGlobal`/`StoreGlobal`

### Documentation & Examples
- Add comprehensive `examples/mutable-closures.lisp` showing 6 closure mutation patterns
- Update `SCOPING_GUIDE.md` with Pattern 5: Mutable Local Variables  
- Update `WHAT_S_NEW.md` announcing issue #106 fix with before/after examples

## Testing

- All 704 library unit tests pass
- New integration tests added for local `set!` patterns:
  - Basic local mutation
  - Multiple sequential mutations
  - `set!` inside while loops
  - Multiple locally-defined variables
  - Nested lambdas with local mutations
  - Multiple return values with mutation

## Architecture Notes

**Why this approach:**
- `LoadGlobal`/`StoreGlobal` already check runtime `scope_stack` (by symbol ID) before falling back to `vm.globals`
- By treating locally-defined variables as `GlobalVar`, they automatically work through the correct path
- Avoids the complexity of pre-allocating slots in the closure environment layout

**What we didn't do (future phases):**
- Phase 3: Remove the runtime scope_stack (larger cleanup)
- Phase 4: Shared mutable captures via `Value::Cell` (requires boxing)

## Migration Impact

- **Fully backward compatible** - existing code continues to work
- New functionality: local `set!` now works (previously errored)
- Performance: no change for top-level code; local mutation uses runtime scope lookup (symbol ID based, same as globals)

## Example

**Before (Issue #106):**
```lisp
(define counter (lambda ()
  (begin
    (define count 0)
    (set! count 42)  ; ❌ ERROR: Undefined global variable
    count)))
```

**After (Fixed):**
```lisp
(define counter (lambda ()
  (begin
    (define count 0)
    (set! count 42)  ; ✅ Works!
    count)))         ; Returns 42

(counter)  ; Returns 42
```

## Files Changed

- `src/value.rs`: Closure.env type change, add `env_from_vec()` helper
- `src/compiler/converters.rs`: Don't register local defines in compile-time scope_stack
- `src/compiler/compile.rs`: Route all `set!` through `StoreGlobal`
- `src/vm/variables.rs`: Update load/store upvalue to use RefCell borrows
- `src/vm/closure.rs`: Wrap closure env in RefCell
- `src/vm/mod.rs`: Update Call/TailCall to create RefCell-wrapped envs
- `docs/SCOPING_GUIDE.md`: Add Pattern 5 with examples
- `docs/WHAT_S_NEW.md`: Announce issue #106 fix
- `examples/mutable-closures.lisp`: New comprehensive example
- `tests/integration/closures_and_lambdas.rs`: 6 new integration tests
