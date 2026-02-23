# Hygienic Macros: Implementation Plan

Reference: docs/MACROS.md

Three PRs, each independently shippable.

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

The Expander gets access to `&mut SymbolTable` and `&mut VM` via a
context parameter. At macro call time:

1. Convert each argument `Syntax` → `Value` via `to_value()`
2. Build a `Syntax` let-expression binding params to quoted arg values
3. Compile and execute via a new `eval_syntax()` pipeline entry point
4. Convert result `Value` → `Syntax` via `from_value()`
5. Add intro scope, continue expanding

The `eval_syntax()` function enters the pipeline at the Syntax stage
(skipping the Reader), reusing the same Expander for nested macro calls.

### Step 1: Add `eval_syntax` to the pipeline

**src/pipeline.rs** — new function:

```rust
/// Compile and execute a Syntax tree directly.
/// Used by the macro expander to run macro bodies in the VM.
pub fn eval_syntax(
    syntax: Syntax,
    expander: &mut Expander,
    symbols: &mut SymbolTable,
    vm: &mut VM,
) -> Result<Value, String> {
    let expanded = expander.expand(syntax, symbols, vm)?;

    let primitive_effects = get_primitive_effects(symbols);
    let mut analyzer = Analyzer::new_with_primitive_effects(symbols, primitive_effects);
    let mut analysis = analyzer.analyze(&expanded)?;
    mark_tail_calls(&mut analysis.hir);

    let intrinsics = lir::intrinsics::build_intrinsics(symbols);
    let mut lowerer = Lowerer::new()
        .with_bindings(analysis.bindings)
        .with_intrinsics(intrinsics);
    let lir_func = lowerer.lower(&analysis.hir)?;

    let symbol_snapshot = symbols.all_names();
    let mut emitter = Emitter::new_with_symbols(symbol_snapshot);
    let bytecode = emitter.emit(&lir_func);

    vm.execute(&bytecode).map_err(|e| e.to_string())
}
```

Update `compile_new`, `compile_all_new`, `eval_new`, `analyze_new`,
`analyze_all_new` to pass `&mut SymbolTable` and `&mut VM` through to
`expander.expand()`. For analysis-only functions (`analyze_new`,
`analyze_all_new`), the VM is needed only if macros are procedural —
these functions will need a VM parameter too.

### Step 2: Thread context through the Expander

**src/syntax/expand/mod.rs**

Change `expand()` signature:

```rust
pub fn expand(
    &mut self,
    syntax: Syntax,
    symbols: &mut SymbolTable,
    vm: &mut VM,
) -> Result<Syntax, String>
```

All internal methods (`expand_list`, `expand_vector`, `handle_defmacro`,
`expand_macro_call`, etc.) get the same parameters threaded through.

### Step 3: Rewrite `expand_macro_call` to use the VM

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
    if args.len() != macro_def.params.len() {
        return Err(format!(...));
    }

    // 2. Build let-expression: (let ((p1 '<a1>) (p2 '<a2>)) body)
    let span = call_site.span.clone();
    let bindings: Vec<Syntax> = macro_def.params.iter().zip(args)
        .map(|(param, arg)| {
            let quoted_arg = Syntax::new(
                SyntaxKind::Quote(Box::new(arg.to_value_syntax(symbols))),
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

    // 3. Compile and execute in the VM
    let result_value = pipeline::eval_syntax(
        let_expr, self, symbols, vm
    )?;

    // 4. Convert result back to Syntax
    let result_syntax = Syntax::from_value(
        &result_value, symbols, span.clone()
    )?;

    // 5. Add intro scope for hygiene
    let intro_scope = self.fresh_scope();
    let hygienized = self.add_scope_recursive(result_syntax, intro_scope);

    // 6. Continue expanding
    self.expand(hygienized, symbols, vm)
}
```

Note on quoting: arguments are passed as quoted values so the macro
body receives data, not code. `to_value_syntax` converts a `Syntax`
tree to a `Syntax` that, when evaluated, produces the corresponding
`Value`. For simple cases this is just `Quote(arg)`. For cases where
the arg contains unresolvable symbols, we may need `Syntax::to_value()`
followed by a literal value embedding. The exact mechanism needs
prototyping — see open questions.

### Step 4: Delete template substitution machinery

**src/syntax/expand/macro_expand.rs**

Remove:
- `substitute()` — replaced by VM evaluation
- `substitute_quasiquote()` — replaced by VM evaluation
- `eval_quasiquote_to_syntax()` — replaced by VM evaluation

The quasiquote-to-code path (`quasiquote.rs`) stays — it's used for
non-macro quasiquotes (runtime list construction).

### Step 5: Add variadic macro parameters

**src/syntax/expand/mod.rs**

Extend `MacroDef`:
```rust
pub struct MacroDef {
    pub name: String,
    pub params: Vec<String>,
    pub rest_param: Option<String>,
    pub template: Syntax,
    pub definition_scope: ScopeId,
}
```

In `handle_defmacro`: detect dotted-pair syntax `(a b . rest)` in the
parameter list. At call time, bind excess arguments as a quoted list.

### Step 6: REPL Expander persistence

**src/pipeline.rs** — change `compile_new` and `eval_new` to accept an
optional `&mut Expander`. When provided, use it instead of creating a
fresh one. The REPL holds a persistent Expander so macros defined in one
input are available in the next.

`compile_all_new` already shares an Expander across forms — just thread
the context through.

### Step 7: Tests

**src/syntax/expand/tests.rs**:
- `test_vm_macro_simple_template`: `(defmacro when ...)` works via VM
- `test_vm_macro_with_gensym`: macro body calls `(gensym)` successfully
- `test_vm_macro_with_if`: conditional expansion works
- `test_vm_macro_with_let`: let-bindings in macro body work

**tests/integration/macros.rs** (new file):
- `test_try_catch_macro`: try/catch expands and executes correctly
- `test_swap_with_gensym`: swap doesn't capture caller's variables
- `test_macro_error_reporting`: bad macro body produces clear error
- `test_variadic_macro`: rest params work
- `test_existing_macros`: all existing macro tests still pass

Counterfactual: break gensym, verify capture occurs in swap

### Step 8: Implement key macros

**lib/macros.lisp** (loaded by stdlib):
- `try`/`catch` — fiber-based exception handling (see docs/EXCEPT.md)
- `defer` — cleanup on scope exit (see docs/JANET.md)
- `with` — resource management (see docs/JANET.md)
- `bench` — timing macro (see docs/DEBUGGING.md)

### Step 9: Update docs

- `docs/MACROS.md` — rewrite to reflect VM-based expansion
- `docs/EXCEPT.md` — update try/catch as implemented
- `docs/DEBUGGING.md` — update bench as implemented
- `src/syntax/AGENTS.md` — document new expansion model
- `src/syntax/expand/AGENTS.md` — if exists, update

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

## Open Questions

1. **Argument quoting mechanism.** How exactly do we pass Syntax
   arguments to the VM as data? The plan says `Quote(arg)`, but if `arg`
   contains symbols that aren't bound in the macro body's scope, the
   Analyzer will reject them. We may need to convert `Syntax` → `Value`
   via `to_value()` and embed the Value as a constant, bypassing
   analysis of the argument content. This needs prototyping.

2. **Circular dependency.** `expand_macro_call` calls `eval_syntax`
   which calls `expander.expand()`. This is mutual recursion between the
   Expander and the pipeline. The borrow checker should allow it since
   `eval_syntax` is a free function taking `&mut Expander`, and
   `expand_macro_call` passes `self`. But this needs verification.

3. **Performance.** Every macro call compiles and executes bytecode.
   For hot macros (e.g., `when` used hundreds of times), this could be
   slow. Mitigation: cache compiled bytecode per MacroDef. The body
   doesn't change between calls — only the argument bindings do. We
   could pre-compile the body at `defmacro` time and replay it with
   different globals for each call.

4. **Analysis-only paths.** `analyze_new` and `analyze_all_new` are used
   by the LSP and linter. They currently don't need a VM. With
   procedural macros, they do — macro bodies must be evaluated to
   produce the expanded code that gets analyzed. This means the LSP
   needs to create a VM at startup.

5. **Error provenance.** When a macro body errors (type mismatch, unbound
   variable), the error should point to the macro definition, not the
   call site. The `eval_syntax` path produces errors with spans from the
   macro body's Syntax nodes, which is correct. But the wrapping
   let-expression has synthetic spans — errors in argument binding might
   be confusing.

## Risk Assessment

**Highest risk: PR 2 argument quoting (open question 1).** The mechanism
for passing Syntax arguments as data to the VM is the least understood
part. If `Quote(arg)` doesn't work because the Analyzer rejects unbound
symbols inside quotes, we need an alternative (constant embedding,
special syntax form, or a pre-analysis pass that marks quoted regions).

**Second risk: PR 3 `lookup()` rewrite.** This is the most complex
function in the Analyzer (~110 lines of capture tracking, function
boundary detection, transitive capture resolution). The scope-aware
matching adds a new dimension.

**Lower risk: PR 2 pipeline threading.** Mechanical change — adding
`symbols` and `vm` parameters to many functions. Tedious but
straightforward. The compiler catches missing parameters.
