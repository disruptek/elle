# Changelog

## v0.1.0 - 2026-02-04

### Features
- Fast bytecode compilation and execution
- Register-based virtual machine
- Interactive REPL
- Rich primitive operations:
  - Arithmetic: `+`, `-`, `*`, `/`
  - Comparisons: `=`, `<`, `>`, `<=`, `>=`
  - Lists: `cons`, `first`, `rest`, `list`
  - Type predicates: `nil?`, `pair?`, `number?`, `symbol?`, `string?`
  - I/O: `display`, `newline`
- Special forms: `if`, `quote`, `define`, `begin`

### Technical Details
- ~2,200 lines of Rust
- 473KB stripped binary
- Zero-cost abstractions via Rust enums and pattern matching
- Symbol interning for fast comparisons
- SmallVec for stack optimization
- Minimal dependencies (rustc-hash, smallvec)

### Known Limitations
- Lambda/closures need upvalue support
- Tail call optimization not fully implemented
- Macro system not yet implemented
