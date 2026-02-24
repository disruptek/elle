# binding

**SUPERSEDED**: This module is legacy code from before the binding rethink.
The HIR pipeline now uses `hir::Binding` (a NaN-boxed Value wrapping
`HeapObject::Binding`) instead of `BindingId` + side-channel HashMap.

The types in this module (`VarRef`, `ResolvedVar`, `Scope`, `ScopeStack`)
are no longer imported anywhere in the codebase. They remain for reference
but should be removed in a future cleanup.

## Original Responsibility

Compile-time variable resolution. Transforms symbol references into concrete
locations (local slot, upvalue index, global lookup).

## Files

| File | Lines | Content |
|------|-------|---------|
| `mod.rs` | 30 | Re-exports |
| `scope.rs` | 280 | `Scope`, `ScopeStack`, `Binding` (old) |
| `varref.rs` | 150 | `VarRef`, `ResolvedVar` |
