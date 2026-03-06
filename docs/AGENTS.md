# docs

Design documents and guides for Elle.

## Navigation index

| Document | Purpose |
|----------|---------|
| `pipeline.md` | Compilation pipeline architecture: Reader → Expander → Analyzer → Lowerer → Emitter → VM |
| `language.md` | Language reference: syntax, special forms, operators, control flow |
| `types.md` | Type system: immediate values, heap types, collections, type predicates |
| `effects.md` | Effect system: Pure, Yields, Polymorphic; effect inference and enforcement |
| `fibers.md` | Fiber concurrency: independent execution contexts, suspension, resumption, parent/child chains |
| `macros.md` | Macro system: syntax objects, scope sets, expansion, hygiene |
| `ffi.md` | Foreign function interface: loading libraries, calling C functions, callbacks, type marshalling |
| `except.md` | Exception handling: error tuples, try/catch, error propagation |
| `semantics.md` | Semantic details: nil vs empty list, destructuring, pattern matching |
| `testing.md` | Testing strategy: where to place tests, property tests, integration tests, examples |
| `debugging.md` | Debugging tools: bytecode disassembly, tracing, memory profiling, REPL commands |
| `cookbook.md` | Step-by-step recipes for common changes: new primitives, heap types, bytecode instructions, special forms |
| `pipeline.dot` | Graphviz diagram of the compilation pipeline |
| `pipeline.svg` | Rendered SVG of the pipeline diagram |
| `elle-saem.jpg` | Elle logo/artwork |
| `logo.svg` | Elle logo in SVG format |

## reference/

External reference documentation (Janet language design) for inspiration. Not Elle's implementation.

## How to use this index

- **Starting out?** Read `pipeline.md` first, then `language.md`
- **Adding a feature?** Check `cookbook.md` for the recipe
- **Debugging?** See `debugging.md`
- **Understanding effects?** Read `effects.md`
- **Working with concurrency?** Read `fibers.md`
- **Implementing FFI?** Read `ffi.md`
- **Writing tests?** Read `testing.md`

## Maintaining documentation

When you change a module's interface or discover undocumented behavior:
1. Update the relevant doc file
2. Update the module's AGENTS.md
3. If adding a new module, create both AGENTS.md and README.md

Documentation debt compounds. A few minutes now saves hours of confusion later.
