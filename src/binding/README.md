# Lexical Binding Resolution (Legacy)

**This module is superseded.** The HIR pipeline now uses `hir::Binding` — a
NaN-boxed Value wrapping `HeapObject::Binding(RefCell<BindingInner>)` — for
all binding resolution. The types here (`VarRef`, `ResolvedVar`, `Scope`,
`ScopeStack`) are no longer imported anywhere.

See `src/hir/binding.rs` for the current binding system.
See `src/hir/README.md` for how bindings work in the current pipeline.
