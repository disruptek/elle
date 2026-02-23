# Macros: Design Document

This document describes Elle's macro system — what exists today, what's
broken, and the plan to fix it.


## Current State

Macros in Elle are template-based. A macro is a name, a parameter list,
and a syntax template. Expansion substitutes arguments into the template
at the AST level.

```lisp
(defmacro my-when (test body)
  `(if ,test ,body nil))

(my-when (> x 0) (print "positive"))
;; Expands to: (if (> x 0) (print "positive") nil)
```

### What works

- **Template macros.** `defmacro` with quasiquote templates. Parameters
  are substituted, the result is recursively expanded.

- **Threading macros.** `->` and `->>` are built into the expander as
  structural rewrites.

- **Macro introspection.** `macro?` and `expand-macro` work at expansion
  time (handled by the Expander in `expand/introspection.rs`).

- **`define-macro` alias.** Both `defmacro` and `define-macro` are
  accepted. They are identical.

- **Macro definitions expand to nil.** `(defmacro ...)` returns
  `SyntaxKind::Nil` — the definition itself produces no code. This
  matters in `begin` forms where `defmacro` is mixed with expressions.

- **Arity checking.** Wrong argument count produces a clear error.

- **Define shorthand.** `(define (f x) body)` desugars to
  `(define f (fn (x) body))` during expansion.

### What's broken

**No compile-time evaluation.** Macro bodies are templates, not code.
You can't call `gensym`, `if`, `let`, or any function during expansion.
This blocks every macro that needs fresh bindings or conditional logic —
which is most useful macros. Documented as broken in `docs/DEBUGGING.md`.

**No hygiene.** The Expander stamps scope marks onto expansion results,
but the Analyzer ignores them. Binding resolution is pure string matching.
A macro that introduces a binding named `tmp` will shadow the caller's
`tmp`:

```lisp
(defmacro broken-swap (a b)
  `(let ((tmp ,a)) (set! ,a ,b) (set! ,b tmp)))

(let ((tmp 10) (x 1) (y 2))
  (broken-swap x y)
  tmp)  ; BUG: tmp is now 1, not 10
```

**Dead infrastructure.** `Syntax.scopes` is written but never read.
`MacroDef.definition_scope` is always `ScopeId(0)`. (The parallel
`MacroDef` in `symbol.rs`, `SymbolTable.macros`, `gensym_id()`, and
the runtime `prim_is_macro`/`prim_expand_macro` stubs were removed in
PR 1.)


## Architecture

### Pipeline position

```
Source → Reader → Syntax → Expander.expand() → Syntax → Analyzer → HIR
```

Expansion happens between parsing and analysis. The Expander is a
standalone struct with a `HashMap<String, MacroDef>` of registered macros
and a monotonic `ScopeId` counter.

### MacroDef

```rust
pub struct MacroDef {
    pub name: String,
    pub params: Vec<String>,
    pub template: Syntax,
    pub definition_scope: ScopeId,
}
```

A macro is a name, positional parameter names, a Syntax template, and a
scope ID. No pattern matching, no ellipsis, no multiple clauses.

### Expansion algorithm (current — template substitution)

1. Check arity: `args.len() == params.len()`
2. Substitute: walk the template via `substitute()`, replacing symbols
   matching parameter names with the corresponding argument syntax trees.
   Quasiquote internals use a separate `substitute_quasiquote()` path
   that only substitutes inside unquote/unquote-splicing nodes.
3. If the *substituted* result is a quasiquote, resolve it via
   `eval_quasiquote_to_syntax()` — collapses unquote nodes in-place
4. Stamp a fresh `ScopeId` onto every node in the result via
   `add_scope_recursive()`. **Bug:** this stamps ALL nodes, including
   substituted call-site arguments. See hygiene plan for the fix.
5. Recursively expand the result (handles macro-generated macro calls)

### Expander precedence

The Expander checks forms in this order: `defmacro`/`define-macro` →
threading macros → `macro?`/`expand-macro` → `define` shorthand →
user-defined macros → recursive child expansion. A user-defined macro
named `define` would never fire because the `define` shorthand is
checked first.

### The scope set mechanism

Every `Syntax` node carries `scopes: Vec<ScopeId>`. The Expander creates
a fresh scope per expansion and stamps it onto the result. The intent is
Racket's "sets of scopes" model: two identifiers match only if their
scope sets are compatible.

**The problem:** The Analyzer's `lookup()` method walks a `Vec<Scope>`
stack and matches by string name. It never examines `Syntax.scopes`. The
scope marks are dead data.

### Cross-form macro visibility

`compile_all` shares a single `Expander` across all top-level forms,
so macros defined in one form are visible in subsequent forms within the
same compilation unit. `eval` creates a fresh `Expander` per call,
so macros defined in one REPL input are lost before the next. The REPL
needs to persist the Expander across inputs.


## The Hygiene Problem

Macro hygiene means two things:

1. **No accidental capture.** A binding introduced by a macro doesn't
   shadow bindings at the call site, and vice versa.

2. **Referential transparency.** Free variables in a macro template
   resolve in the macro's definition environment, not the call site.

Without hygiene, macro authors must manually avoid name collisions. The
standard workaround is `gensym` — generating unique names that can't
collide.

### Prior art

**Common Lisp** has `defmacro` with manual `gensym`. No automatic
hygiene. Macro authors are responsible for avoiding capture. This works
in practice because experienced Lispers know the patterns, but it's a
source of subtle bugs.

**Scheme R5RS** has `syntax-rules`, a pattern-based macro system with
automatic hygiene. Patterns use ellipsis (`...`) for variadic matching.
Hygiene is enforced by the expander — no escape hatch. Limited: you
can't write procedural macros.

**Scheme R6RS / Racket** has `syntax-case`, which combines pattern
matching with procedural escape. Macros receive and return *syntax
objects* — s-expressions annotated with lexical context. Hygiene is
automatic but breakable via `datum->syntax`. This is the most powerful
and most complex model.

**Racket's "sets of scopes"** (Matthew Flatt, 2016) replaced the older
"marks and renames" model. Each identifier carries a set of scope IDs.
A binding is visible to a reference if the binding's scope set is a
subset of the reference's scope set. This is what Elle's `Syntax.scopes`
was designed for — the infrastructure exists, the wiring doesn't.


## Plan: VM-Based Macro Expansion + Hygiene

Two PRs after the completed cleanup. The key insight: macros are just
a phase change. The macro body is normal Elle code that operates on
data and returns code. We run it in the real VM during expansion.

### PR 2: VM-based macro expansion

**Goal:** Macro bodies are normal Elle code executed in the VM during
expansion. No separate mini-interpreter, no template substitution
machinery. The full language is available: `gensym`, `if`, `let`,
closures, list operations, recursion — everything.

**What changes:**

- The Expander gets access to `&mut SymbolTable` and `&mut VM`
- At macro call time, arguments are converted to quoted `Value` data
- The macro body is compiled and executed via a new `eval_syntax()`
  pipeline entry point that starts from `Syntax` (skipping the Reader)
- The result `Value` is converted back to `Syntax` via `from_value()`
- The template substitution path (`substitute`, `substitute_quasiquote`,
  `eval_quasiquote_to_syntax`) is deleted — all macros go through the VM

**How it works:**

```lisp
(defmacro when (test body) `(if ,test ,body nil))
(when (> x 0) (print x))
```

1. Expander sees `(when (> x 0) (print x))` — it's a macro call
2. Convert arguments to Values: `test` = `(> x 0)`, `body` = `(print x)`
3. Build a let-expression binding params to quoted args:
   `(let ((test '(> x 0)) (body '(print x))) \`(if ,test ,body nil))`
4. The quasiquote expander converts this to `(list ...)` calls
5. Compile and execute in the VM → produces list value `(if (> x 0) (print x) nil)`
6. Convert back to `Syntax` via `from_value()`
7. Add intro scope, continue expanding

**What this unblocks:**

```lisp
;; Procedural macro with gensym
(defmacro swap (a b)
  (let ((tmp (gensym "tmp")))
    `(let ((,tmp ,a)) (set! ,a ,b) (set! ,b ,tmp))))

;; try/catch over fiber primitives
(defmacro try (body handler)
  (let ((f (gensym "f"))
        (e (gensym "e")))
    `(let ((,f (fiber/new (fn () ,body) 1)))
       (fiber/resume ,f nil)
       (if (= (fiber/status ,f) :error)
         (let ((,e (fiber/value ,f)))
           (,handler ,e))
         (fiber/value ,f)))))

;; Conditional expansion
(defmacro assert (test . args)
  (if (empty? args)
    `(if (not ,test) (error "assertion failed"))
    `(if (not ,test) (error ,(first args)))))

;; defer — cleanup on scope exit
(defmacro defer (cleanup . body)
  (let ((f (gensym "f"))
        (r (gensym "r")))
    `(let ((,f (fiber/new (fn () ,@body) 1))
           (,r (fiber/resume ,f nil)))
       ,cleanup
       (if (= (fiber/status ,f) :error)
         (fiber/propagate ,r ,f)
         ,r))))
```

**What gets deleted:**

- `substitute()` in `macro_expand.rs`
- `substitute_quasiquote()` in `macro_expand.rs`
- `eval_quasiquote_to_syntax()` in `macro_expand.rs`

The `quasiquote_to_code()` path in `quasiquote.rs` stays — it handles
non-macro quasiquotes (runtime list construction).

### PR 3: Sets-of-scopes hygiene

**Goal:** Binding resolution respects scope marks. Macro-introduced
bindings can't capture call-site names and vice versa. Automatic —
no `gensym` needed for the common case.

**What changes:**

- The Analyzer's `Scope` struct stores scope sets alongside binding names
- `lookup()` uses scope-set subset matching instead of string matching
- `bind()` records the binding's scope set from the `Syntax` node
- The intro scope stamped by `expand_macro_call` (already done in PR 2)
  now has teeth — the Analyzer uses it

**How it works:**

The rule: a binding is visible to a reference if the **binding's** scope
set is a subset of the **reference's** scope set. When multiple bindings
match, the one with the **largest** scope set wins (most specific).

```
Before expansion:
  call-site `tmp` has scopes {0}       (user's let-binding)
  call-site `x` has scopes {0}

After expanding (swap x y) with intro scope 3:
  macro's `tmp` has scopes {0, 3}      ← from result, gets intro scope
  macro's `x` has scopes {0}           ← from call site, no intro scope
```

**Inside the macro body** — reference to `tmp` has scopes `{0, 3}`:
- Call-site binding `tmp` scopes `{0}`: is `{0} ⊆ {0, 3}`? Yes.
- Macro binding `tmp` scopes `{0, 3}`: is `{0, 3} ⊆ {0, 3}`? Yes.
- Both match, but `{0, 3}` is larger → macro's `tmp` wins. Correct.

**At the call site** — reference to `tmp` has scopes `{0}`:
- Call-site binding `tmp` scopes `{0}`: is `{0} ⊆ {0}`? Yes. Matches.
- Macro binding `tmp` scopes `{0, 3}`: is `{0, 3} ⊆ {0}`? No. Invisible.
- Only the call-site `tmp` is visible. No capture.

**Pre-expansion code**: empty scopes `[]` is a subset of everything,
so code that hasn't been through macro expansion works identically.


## What This Unblocks

These features are designed but blocked on the macro system:

| Feature | Defined in | Needs |
|---------|-----------|-------|
| `try`/`catch` | `docs/EXCEPT.md` | PR 2 (VM macros) |
| `defer` | `docs/JANET.md` | PR 2 (VM macros) |
| `with` | `docs/JANET.md` | PR 2 (VM macros) |
| `protect` | `docs/JANET.md` | PR 2 (VM macros) |
| `generate` | `docs/JANET.md` | PR 2 (VM macros) |
| `bench` | `docs/DEBUGGING.md` | PR 2 (VM macros) |
| `swap` | — | PR 2 (gensym) or PR 3 (automatic) |
| Anaphoric macros | — | PR 3 (hygiene escape) |
| `assert` (variadic) | — | PR 2 (VM macros + rest params) |
| `match` (as macro) | — | PR 2 (VM macros) |


## Files

| File | Role |
|------|------|
| `src/syntax/expand/mod.rs` | Expander struct, `defmacro` handling, scope stamping |
| `src/syntax/expand/macro_expand.rs` | Substitution (to be replaced with VM eval) |
| `src/syntax/expand/quasiquote.rs` | Quasiquote → `(list ...)` runtime calls |
| `src/syntax/expand/threading.rs` | `->` and `->>` |
| `src/syntax/expand/introspection.rs` | `macro?` and `expand-macro` |
| `src/syntax/expand/qualified.rs` | `module:name` resolution |
| `src/syntax/expand/tests.rs` | Expansion tests |
| `src/syntax/mod.rs` | `Syntax`, `SyntaxKind`, `ScopeId` |
| `src/syntax/convert.rs` | `Syntax` ↔ `Value` conversion |
| `src/hir/analyze/mod.rs` | `Analyzer`, `Scope`, `lookup()`, `bind()` |
| `src/pipeline.rs` | Compilation entry points (add `eval_syntax`) |


## Open Questions

1. **Argument quoting mechanism.** How do we pass Syntax arguments to
   the VM as data? The plan says `Quote(arg)`, but if `arg` contains
   symbols that aren't bound in the macro body's scope, the Analyzer
   may reject them. We may need to convert `Syntax` → `Value` via
   `to_value()` and embed the Value as a constant, bypassing analysis
   of the argument content. Needs prototyping.

2. **Performance.** Every macro call compiles and executes bytecode.
   For hot macros (e.g., `when` used hundreds of times), this could be
   slow. Mitigation: cache compiled bytecode per MacroDef. The body
   doesn't change between calls — only the argument bindings do.

3. **Analysis-only paths.** `analyze` and `analyze_all` are used
    by the LSP and linter. They currently don't need a VM. With VM-based
    macros, they do — macro bodies must be evaluated to produce the
    expanded code that gets analyzed. The LSP will need a VM at startup.

4. **How do macros interact with the effect system?** A macro that
   expands to `(fiber/signal ...)` should produce code with `Yields`
   effect. Currently this works because effect inference happens after
   expansion. This should continue to work with VM-based expansion.

5. **Interaction between `set!` and scope-aware lookup.** `set!` goes
   through the Analyzer's `lookup()`. With scope-aware resolution, a
   macro that uses `set!` on a call-site variable must have the right
   scope set for the reference to resolve. This should work naturally
   (call-site arguments keep their original scopes) but needs careful
   testing, especially for mutable captures across closure boundaries.

6. **Scope representation in the Analyzer.** The current
   `HashMap<String, BindingId>` is fast for string lookup. Scope-aware
   lookup needs to find all bindings with a given name and then pick the
   best match. A `HashMap<String, Vec<(Vec<ScopeId>, BindingId)>>` would
   work. Profile before optimizing.
