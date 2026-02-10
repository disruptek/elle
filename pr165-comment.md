## Architecture Analysis: How to Fix `set!` Inside Lambda Bodies

The PR correctly identifies the root cause as architectural. Here's a concrete fix plan.

### The Problem: Three Separate Storage Mechanisms That Don't Talk to Each Other

The compiler and VM use three independent variable storage systems that have incompatible addressing schemes:

| Storage | Addressed by | Used for | Mutable? |
|---|---|---|---|
| **Closure env** (`Rc<Vec<Value>>`) | Positional index | Lambda params + captures | ❌ No (`Rc` is immutable) |
| **Runtime scope_stack** (`ScopeStack`) | Symbol ID (`u32`) | `DefineLocal` variables | ✅ Yes |
| **Globals** (`HashMap<u32, Value>`) | Symbol ID | Top-level `define` | ✅ Yes |

The read path (`Expr::Var` → `LoadUpvalue`) reads from the closure env by positional index. The write path (`Expr::Set` → `StoreScoped`) writes to the scope_stack by what it *thinks* is a symbol ID but is actually a positional index. The define path (`Expr::Define` → `DefineLocal`) writes to the scope_stack by symbol ID. These three paths are completely disjoint — writes are invisible to reads.

### The Fix: Unify on the Scope Stack for Lambda-Local Variables

The simplest correct fix is to stop using `LoadUpvalue`/`StoreUpvalue` for **locally-defined** variables inside lambda bodies, and instead route all local variable access through the scope_stack using symbol IDs. This avoids needing to make the closure env mutable (which would be a much larger change affecting captures across closure boundaries).

#### Step 1: Distinguish Parameters from Local Defines in the AST

In `converters.rs`, when `define` is encountered inside a lambda body (i.e., `scope_stack` is non-empty), the variable is already pushed onto `scope_stack` (line 552). But the resulting `Expr::Var` nodes for those variables are indistinguishable from parameter references — both get `depth`/`index` resolved against the same scope.

**Fix:** Add a new AST variant `Expr::ScopedVar(SymbolId)` (or reuse the existing `Expr::ScopeVar` if it serves this purpose) for variables that were introduced by `define` inside a lambda body, as opposed to lambda parameters or captures. These should carry the **symbol ID**, not a positional index.

Alternatively (and more minimally): in `value_to_expr_with_scope`, track which symbols in each scope came from `define` vs. from lambda parameters. When resolving a `set!` or a variable read that targets a `define`-introduced name, emit `Expr::GlobalVar` (which already uses symbol-ID-based `LoadGlobal`/`StoreGlobal` and correctly checks the scope_stack first).

#### Step 2: Compile Local Defines Consistently

Currently, `Expr::Define` inside a lambda emits:
```
Dup
DefineLocal  <sym_id>   ; stores to scope_stack by symbol ID
```

This is correct. The corresponding **read** must also go through the scope_stack. The easiest approach:

- For variables introduced by `define` inside a lambda body, compile reads as `LoadGlobal` (which already checks `scope_stack.get(sym_id)` first at line 13 of `variables.rs`).
- For `set!` on those variables, compile as `StoreGlobal` (which already checks `scope_stack.get(sym_id)` first and updates the scope_stack at lines 40-45 of `variables.rs`).

This works because `LoadGlobal` and `StoreGlobal` already have scope-stack-first lookup semantics. The names are misleading, but the behavior is exactly right: check scope_stack by symbol ID, fall back to globals.

#### Step 3: Leave Parameters and Captures Alone

Lambda parameters and captured variables should continue to use `LoadUpvalue` for reads. `set!` on a parameter should either:
- (a) Be compiled as `StoreGlobal` (which will find nothing in scope_stack or globals and create a new scope_stack entry — effectively shadowing the parameter), or
- (b) Emit a compile-time error: "cannot mutate lambda parameter; use `define` to create a mutable local binding."

Option (b) is cleaner and more Scheme-like. Real Scheme implementations use boxing (cells) for mutable parameters, which is a heavier lift.

### Concrete Implementation Plan

1. **In `converters.rs`**: Maintain a parallel `HashSet<SymbolId>` per scope (or a flag on each entry) tracking which names came from `define` vs. lambda params. When resolving a symbol reference inside a lambda body:
   - If it's a `define`-introduced name → emit `Expr::GlobalVar(sym)` (reuses existing infra)
   - If it's a lambda parameter or capture → emit `Expr::Var(sym, depth, index)` as today

2. **In `converters.rs` `set!` handling**: When the target variable was introduced by `define` → emit `Expr::Set` with `index = usize::MAX` (the global path), which compiles to `StoreGlobal` and correctly uses scope_stack-first semantics. When the target is a parameter → emit a compile error.

3. **No VM changes needed.** `LoadGlobal`, `StoreGlobal`, and `DefineLocal` already have the correct scope-stack-aware semantics. The scope_stack uses symbol IDs throughout, which is consistent.

4. **Tests**: The existing test in `tests/integration/closures_and_lambdas.rs` from this PR should pass. Also add:
   - `set!` on a `define`-introduced variable inside a lambda with a `while` loop
   - Nested lambdas where inner lambda `set!`s a variable `define`d in the outer lambda (this case still needs captures → defer or error)
   - `set!` on a lambda parameter → compile error

### What This Does NOT Fix (Future Work)

- **Mutable captures across closure boundaries**: If an inner lambda captures a variable from an outer lambda and mutates it via `set!`, the outer lambda won't see the change. This requires boxing (`Rc<RefCell<Value>>`) in the closure env. That's a separate, larger change.
- **`set!` on lambda parameters**: Requires either boxing or copy-to-scope-stack semantics. Recommend erroring for now.
