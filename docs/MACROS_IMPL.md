# Hygienic Macros: Implementation Plan

Reference: docs/MACROS.md

Two remaining PRs (PR 1 completed), each independently shippable.

---

## PR 1: Dead Code Cleanup ✓

### Changes (completed)

**src/symbol.rs**
- Remove `MacroDef` struct (lines 16-21)
- Remove `gensym_id()` function and `GENSYM_COUNTER` static (lines 6-13)
- Remove `macros` field from `SymbolTable` (line 40)
- Remove `define_macro()`, `get_macro()`, `is_macro()` methods (lines 82-96)

**src/primitives/meta.rs**
- Remove `prim_is_macro` (lines 49-65) — never registered, duplicate of macros.rs
- Remove `prim_expand_macro` (lines 26-46) — never registered, duplicate of macros.rs
- Keep `prim_gensym` (registered at registration.rs:743, may have runtime consumers)

**src/primitives/macros.rs**
- Remove `prim_is_macro` and `prim_expand_macro` — registered but useless stubs
  (always return false / passthrough)

**src/primitives/registration.rs**
- Remove `use super::macros::{prim_expand_macro, prim_is_macro}` (line 46)
- Remove registration of `expand-macro` (line 789) and `macro?` (line 797)
- Keep `use super::meta::prim_gensym` (line 51) and its registration (line 743)

**Verification**: `cargo test --workspace && cargo clippy --workspace --all-targets -- -D warnings`

---

## PR 2: VM-Based Macro Expansion

### Design

All macros are procedural. Macro bodies are normal Elle code that runs
in the VM during expansion. The body receives its arguments as quoted
data and returns a value that becomes code. No separate template
substitution path — quasiquote in macro bodies produces `(list ...)`
calls that the VM evaluates.

**Validation**: This is the same approach Janet uses (see
`docs/JANET-COMPILER.md`). Janet creates a fiber for the macro function,
resumes it with the form's arguments as unevaluated values, and uses the
output as the replacement form. Janet has shipped this successfully for
years. Our approach differs in two ways: (1) we have a typed `Syntax`
AST instead of raw values, giving us source locations and scope sets for
free; (2) we'll add automatic hygiene in PR 3, which Janet lacks
entirely.

```lisp
;; Simple template macro — works unchanged
(defmacro when (test body)
  `(if ,test ,body nil))

;; Procedural macro with gensym — now possible
(defmacro try (body handler)
  (let ((f (gensym "f"))
        (e (gensym "e")))
    `(let ((,f (fiber/new (fn () ,body) 1)))
       (fiber/resume ,f nil)
       (if (= (fiber/status ,f) :error)
         (let ((,e (fiber/value ,f)))
           (,handler ,e))
         (fiber/value ,f)))))

;; Conditional expansion — now possible
(defmacro assert (test . args)
  (if (empty? args)
    `(if (not ,test) (error "assertion failed"))
    `(if (not ,test) (error ,(first args)))))
```

The Expander gets access to `&mut SymbolTable` and `&mut VM` via
parameters. At macro call time:

1. Quote each argument: `Quote(Box::new(arg.clone()))`
2. Build a `Syntax` let-expression binding params to quoted arg values
3. Compile and execute via a new `eval_syntax()` pipeline entry point
4. Convert result `Value` → `Syntax` via `from_value()`
5. Add intro scope, continue expanding

The `eval_syntax()` function enters the pipeline at the Syntax stage
(skipping the Reader), reusing the same Expander for nested macro calls.

**Argument quoting**: `Quote(Box::new(arg.clone()))` works because the
Analyzer already handles `quote` by converting to a Value via
`to_value()` at `forms.rs:62-64` and `forms.rs:97-102`. Symbols inside
quoted arguments are interned, not resolved — no "unbound variable"
errors. This was verified by reading the Analyzer code.

**Semantic shift**: This change means macro bodies are **executed**, not
textually substituted. A macro like `(defmacro double (x) (* x 2))`
currently substitutes `x` textually, producing `(* 21 2)`. Under VM
evaluation, the body executes with `x` bound to the quoted value `21`,
producing the Value `42`, which becomes `Syntax::Int(42)`. The final
result is the same for literal arguments, but **breaks for expression
arguments**: `(double (+ 1 2))` would try to multiply a list by 2.

Macros that compute with their arguments must use quasiquote:
`(defmacro double (x) \`(* ,x 2))`. This produces code-as-data.
All macros in `examples/meta-programming.lisp` (~13 macros) need
rewriting. Integration tests using quasiquote (`my-when`, `add-one`)
work unchanged.

**VM state**: Macro bodies share the compilation VM's global state.
A macro body that calls `(define x 42)` creates a persistent global.
This is the same trade-off Janet makes. Isolation via fibers is a
possible future improvement but not needed for correctness.

### Step 1: Signature change + eval_syntax (atomic)

These must be done together — `eval_syntax` calls the new `expand()`
signature, and `expand()` calls `eval_syntax` for macro bodies.

**src/pipeline.rs** — new function:

```rust
pub fn eval_syntax(
     syntax: Syntax,
     expander: &mut Expander,
     symbols: &mut SymbolTable,
     vm: &mut VM,
 ) -> Result<Value, String> {
     let expanded = expander.expand(syntax, symbols, vm)?;
     // ... analyze, lower, emit, execute (same as compile)
     vm.execute(&bytecode).map_err(|e| e.to_string())
 }
```

**src/pipeline.rs** — change existing functions:

- `compile`: creates its own internal VM for macro expansion.
   Public signature stays `compile(source, symbols)`. The internal
   VM is throwaway — macro side effects don't persist.
- `compile_all`: same — internal VM.
- `eval`: already takes `&mut VM`. Pass it through to `expand()`.
- `analyze`: add `&mut VM` parameter. The LSP (`CompilerState`)
   and linter (`lint_str`) already have VMs — just thread them through.
- `analyze_all`: add `&mut VM` parameter. Same.

**src/syntax/expand/mod.rs** — change `expand()` signature:

```rust
pub fn expand(
    &mut self,
    syntax: Syntax,
    symbols: &mut SymbolTable,
    vm: &mut VM,
) -> Result<Syntax, String>
```

Thread `symbols` and `vm` through ALL internal methods:
- `expand_list`, `expand_vector`
- `handle_defmacro`
- `expand_macro_call`
- `handle_thread_first`, `handle_thread_last`
- `handle_macro_predicate`, `handle_expand_macro`
- `quasiquote_to_code`, `quasiquote_list_to_code`
- `resolve_qualified_symbol` (doesn't need them, but called from
  `expand` which has them in scope)

**Callers that need updating** (complete list):

| File | Call sites |
|------|-----------|
| `src/pipeline.rs` | 4 (`compile`, `compile_all`, `eval`, `analyze`, `analyze_all`) |
| `src/syntax/expand/tests.rs` | ~23 |
| `src/syntax/mod.rs` (tests) | ~7 |
| `src/hir/tailcall.rs` (test helper) | 1 |
| `tests/unittests/lir_debug.rs` | 1 |
| `tests/unittests/hir_debug.rs` | 1 |
| `src/lib.rs` | re-exports (signature change cascades) |
| `elle-lsp/src/compiler_state.rs` | calls `analyze_all` |
| `elle-lint/src/lib.rs` | calls `analyze_all` |

Tests calling `analyze`/`compile` indirectly:
| `src/hir/lint.rs` tests | ~4 |
| `src/hir/symbols.rs` tests | ~6 |
| `src/pipeline.rs` tests | ~30 |

**Borrow checker**: Passing `self`, `symbols`, and `vm` from
`expand_macro_call(&mut self, ..., symbols: &mut SymbolTable, vm: &mut VM)`
to `eval_syntax(syntax, self, symbols, vm)` works because they are three
distinct mutable references to three distinct objects. The existing
`.cloned()` on `self.macros.get(name)` at `mod.rs:131` releases the
borrow on `self.macros` before calling `expand_macro_call`.

### Step 3b: Add macro expansion recursion guard

**src/syntax/expand/mod.rs**

Add a recursion depth counter to the Expander:

```rust
const MAX_MACRO_EXPANSION_DEPTH: usize = 200;

pub struct Expander {
    // ... existing fields ...
    expansion_depth: usize,
}
```

At the top of `expand_macro_call`, increment and check:

```rust
self.expansion_depth += 1;
if self.expansion_depth > MAX_MACRO_EXPANSION_DEPTH {
    return Err(format!(
        "macro expansion depth exceeded {} (possible infinite expansion)",
        MAX_MACRO_EXPANSION_DEPTH
    ));
}
// ... expand ...
// decrement on all exit paths (use a guard or explicit decrement)
self.expansion_depth -= 1;
```

Janet uses the same limit (200) for the same reason — macros can expand
to other macros, and without a guard, recursive macros cause a stack
overflow. See `docs/JANET-COMPILER.md`, "Macros", step 3.

### Step 2: Rewrite `expand_macro_call` to use the VM

**src/syntax/expand/macro_expand.rs**

Replace template substitution with VM evaluation:

```rust
pub(super) fn expand_macro_call(
    &mut self,
    macro_def: &MacroDef,
    args: &[Syntax],
    call_site: &Syntax,
    symbols: &mut SymbolTable,
    vm: &mut VM,
) -> Result<Syntax, String> {
    // 1. Check arity
    // 2. Increment recursion guard

    // 3. Build let-expression: (let ((p1 'a1) (p2 'a2)) body)
    let span = call_site.span.clone();
    let bindings: Vec<Syntax> = macro_def.params.iter().zip(args)
        .map(|(param, arg)| {
            let quoted_arg = Syntax::new(
                SyntaxKind::Quote(Box::new(arg.clone())),
                span.clone(),
            );
            Syntax::new(
                SyntaxKind::List(vec![
                    Syntax::new(SyntaxKind::Symbol(param.clone()), span.clone()),
                    quoted_arg,
                ]),
                span.clone(),
            )
        })
        .collect();

    let let_expr = Syntax::new(
        SyntaxKind::List(vec![
            Syntax::new(SyntaxKind::Symbol("let".to_string()), span.clone()),
            Syntax::new(SyntaxKind::List(bindings), span.clone()),
            macro_def.template.clone(),
        ]),
        span.clone(),
    );

    // 4. Compile and execute in the VM
    let result_value = pipeline::eval_syntax(let_expr, self, symbols, vm)?;

    // 5. Convert result back to Syntax
    let result_syntax = Syntax::from_value(&result_value, symbols, span)?;

    // 6. Add intro scope for hygiene
    let intro_scope = self.fresh_scope();
    let hygienized = self.add_scope_recursive(result_syntax, intro_scope);

    // 7. Decrement recursion guard, continue expanding
    self.expand(hygienized, symbols, vm)
}
```

### Step 3: Delete template substitution machinery

**src/syntax/expand/macro_expand.rs**

Remove:
- `substitute()` — replaced by VM evaluation
- `substitute_quasiquote()` — replaced by VM evaluation
- `eval_quasiquote_to_syntax()` — replaced by VM evaluation

The quasiquote-to-code path (`quasiquote.rs`) stays — it's used for
non-macro quasiquotes (runtime list construction).

### Step 4: Rewrite existing macros for VM semantics

**Semantic shift**: Non-quasiquote macro bodies now compute values
instead of doing textual substitution. All macros that use arguments
in computation positions must be rewritten to use quasiquote.

**examples/meta-programming.lisp** — ~13 macros need rewriting:

| Before | After |
|--------|-------|
| `(defmacro double (x) (* x 2))` | `` (defmacro double (x) `(* ,x 2)) `` |
| `(defmacro triple (x) (* x 3))` | `` (defmacro triple (x) `(* ,x 3)) `` |
| `(defmacro square (x) (* x x))` | `` (defmacro square (x) `(* ,x ,x)) `` |
| `(defmacro negate (x) (not x))` | `` (defmacro negate (x) `(not ,x)) `` |
| etc. | etc. |

**src/syntax/expand/tests.rs** — `test_defmacro_registration` defines
`(defmacro double (x) (* x 2))` without quasiquote. Must be rewritten.

**tests/integration/pipeline_point.rs** — `my-when` and `add-one`
already use quasiquote. No changes needed.

### Step 5: Update ALL test call sites

~73 test call sites need the new `expand()` signature. Strategy:
make the signature change, let compilation errors guide updates.

Most tests that call `expand()` directly don't use macros — they test
basic expansion of lists, vectors, threading, etc. These just need
`&mut SymbolTable` and `&mut VM` parameters added. Create a test
helper:

```rust
fn setup_expand() -> (SymbolTable, VM) {
    let mut symbols = SymbolTable::new();
    let mut vm = VM::new();
    register_primitives(&mut vm, &mut symbols);
    (symbols, vm)
}
```

### Step 6: Update docs

- `src/syntax/AGENTS.md` — document VM-based expansion model
- `src/syntax/expand/AGENTS.md` — if exists, update

### Deferred to follow-up PRs

These were originally in PR 2 but are independently shippable:

- **Variadic macro parameters** (`rest_param` on `MacroDef`)
- **REPL Expander persistence** (optional `&mut Expander` on pipeline fns)
- **Key macros** (`try`/`catch`, `defer`, `with`, `bench`)

---

## PR 3: Sets-of-Scopes Hygiene

### Design

With VM-based expansion, gensym provides manual hygiene. This PR adds
automatic hygiene via scope-aware binding resolution. Macro-introduced
bindings can't capture call-site names and vice versa.

### Step 1: Fix scope stamping

The intro scope must be added to the macro result *after* VM evaluation
converts the Value back to Syntax. Step 5 of `expand_macro_call` above
already does this via `add_scope_recursive`. Call-site arguments (which
were quoted data during evaluation) get reconstructed from Values without
the intro scope — they keep their original (empty) scopes.

### Step 2: Thread scope sets into the Analyzer

**src/hir/analyze/mod.rs**

New types:
```rust
struct ScopedBinding {
    scopes: Vec<ScopeId>,
    id: BindingId,
}

struct Scope {
    bindings: HashMap<String, Vec<ScopedBinding>>,
    is_function: bool,
    next_local: u16,
}
```

Change `bind()` signature:
```rust
fn bind(&mut self, name: &str, scopes: &[ScopeId], kind: BindingKind) -> BindingId
```

Change `lookup()` signature and algorithm:
```rust
fn lookup(&mut self, name: &str, ref_scopes: &[ScopeId]) -> Option<BindingId>
```

The algorithm:
1. Walk scopes innermost to outermost, tracking `crossed_function_boundary`
2. For each scope, find all candidates with matching name where
   `is_subset(candidate.scopes, ref_scopes)` is true
3. Track the best match (largest scope set) along with its depth and
   whether it crossed a function boundary
4. After the scope walk, apply capture logic to the **winning** binding
   based on **its** position relative to function boundaries

Helper:
```rust
fn is_subset(subset: &[ScopeId], superset: &[ScopeId]) -> bool {
    subset.iter().all(|s| superset.contains(s))
}
```

**Pre-expansion code**: empty scopes `[]` is a subset of everything,
so code that hasn't been through macro expansion works identically.

### Step 3: Pass scope sets through ALL bind/lookup call sites

**`bind()` call sites** — all need `scopes` parameter added:

| File | Function | Source of scopes |
|------|----------|-----------------|
| `forms.rs` | `analyze_begin` (two-pass) | define's name syntax node |
| `forms.rs` | `analyze_for` | `items[1].scopes` (loop variable) |
| `binding.rs` | `analyze_let` | `pair[0].scopes` |
| `binding.rs` | `analyze_let_star` | `pair[0].scopes` |
| `binding.rs` | `analyze_letrec` | `pair[0].scopes` |
| `binding.rs` | `analyze_define` (local) | `items[1].scopes` |
| `binding.rs` | `analyze_define` (global) | `&[]` (globals are runtime) |
| `binding.rs` | `analyze_lambda` | `param.scopes` |
| `special.rs` | `analyze_pattern` | `syntax.scopes` |

**`lookup()` call sites** — all need `ref_scopes` parameter added:

| File | Function | Source of scopes |
|------|----------|-----------------|
| `forms.rs` | `analyze_expr` (Symbol) | `syntax.scopes` |
| `binding.rs` | `analyze_define` | `items[1].scopes` |
| `binding.rs` | `analyze_set` | `items[1].scopes` |

### Step 4: Tests

**tests/integration/hygiene.rs** (new file):

Core hygiene tests:
- `test_macro_no_capture`: macro `tmp` doesn't shadow caller's `tmp`
- `test_macro_no_leak`: caller can't see macro's internal bindings
- `test_nested_macro_hygiene`: nested expansions with overlapping names
- `test_non_macro_code`: non-macro code works identically

Capture interaction tests:
- `test_macro_closure_captures_callsite`: macro-generated closure captures
  a call-site variable correctly
- `test_set_through_macro`: `set!` on call-site variable works in macro
- `test_nested_closure_macro`: nested macro expansions generating closures

Counterfactual: remove scope stamping, verify capture occurs

### Step 5: Update docs

- `src/hir/AGENTS.md` — document scope-aware lookup
- `src/syntax/AGENTS.md` — update hygiene section
- `docs/MACROS.md` — update hygiene section as implemented

---

## Resolved Questions

1. ~~**Argument quoting mechanism.**~~ **Resolved**: `Quote(Box::new(arg.clone()))`
   works. The Analyzer handles `quote` at `forms.rs:62-64` by converting
   to a Value via `to_value()`. Symbols inside quotes are interned, not
   resolved. Verified by code reading and reviewer confirmation.

2. ~~**Circular dependency / borrow checker.**~~ **Resolved**: Three distinct
   `&mut` references (`Expander`, `SymbolTable`, `VM`) to three distinct
   objects. Rust tracks them independently. Recursive macro expansion
   works via reborrowing at each call level.

3. ~~**Analysis-only paths.**~~ **Resolved**: Both `elle-lsp` (`CompilerState`)
    and `elle-lint` (`lint_str`) already create VMs. Just thread them
    through to `analyze_all`. Mechanical change.

## Open Questions

1. **Performance.** Every macro call compiles and executes bytecode.
   For hot macros (e.g., `when` used hundreds of times), this could be
   slow. Mitigation: cache compiled bytecode per MacroDef. Deferred —
   correctness first per AGENTS.md.

2. **Error provenance.** When a macro body errors, the error should
   point to the macro definition, not the call site. The wrapping
   let-expression has synthetic spans — errors in argument binding
   might be confusing. Needs testing.

3. **Side effects during expansion.** Macro bodies run in the real VM,
   so they can perform I/O, spawn fibers, signal errors, etc. Janet
   accepts this as a feature. We do too. Document it.

4. **VM state pollution.** Macro bodies share the compilation VM's
   global state. A macro body that calls `(define x 42)` creates a
   persistent global. Isolation via fibers is a possible future
   improvement. For now, document as a known limitation.

## Risk Assessment

**Highest risk: semantic shift in existing macros.** All non-quasiquote
macro bodies change behavior. `examples/meta-programming.lisp` has ~13
macros that need rewriting. Integration tests using quasiquote are fine.

**Second risk: PR 3 `lookup()` rewrite.** This is the most complex
function in the Analyzer (~110 lines of capture tracking, function
boundary detection, transitive capture resolution). The scope-aware
matching adds a new dimension.

**Lower risk: PR 2 pipeline threading.** Mechanical change — adding
`symbols` and `vm` parameters to ~73 call sites. Tedious but
straightforward. The compiler catches missing parameters.
