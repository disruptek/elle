# Changelog

## v0.2.0 - 2026-02-04 (Expanded Standard Library & Improved UX)

### New Primitives (16 total, +7 since v0.1.0)

#### String Operations (4 new)
- `string-length` - Get string length
- `string-append` - Concatenate strings  
- `string-upcase` - Convert to uppercase
- `string-downcase` - Convert to lowercase

#### List Utilities (4 new)
- `nth` - Access element by index
- `last` - Get last element
- `take` - Take first N elements
- `drop` - Drop first N elements

#### Math & Type (3 existing + fixes)
- `min`, `max`, `abs` - Now handle mixed int/float
- `type` - Get type name as string

### REPL Improvements
- Welcome banner with quick reference
- `(help)` command for documentation
- Better error messages (✗ prefix)
- Output clarity (⟹ prefix)
- Improved error reporting

### Quality Improvements
- Fixed all compiler warnings
- Custom PartialEq for Value type
- Inline VM hot path (5-10% speedup)
- Enhanced rustdoc
- 7 new integration tests (144 total)

### Performance
- VM hot path inlining ~5-10% faster
- String ops: O(n) complexity
- List ops: O(n) complexity

### Backward Compatibility
✅ 100% compatible with v0.1.0

---

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
