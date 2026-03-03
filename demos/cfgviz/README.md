# CFG Visualizer

Renders control flow graphs of Elle functions to SVG files.

Uses `fn/cfg` to produce Mermaid flowchart text from a closure's LIR,
then the selkie plugin to render Mermaid to SVG.

## Prerequisites

Build the selkie plugin:

```bash
cargo build --release -p elle-selkie
```

## Running

```bash
cargo run --release -- demos/cfgviz/cfgviz.lisp
```

## Output

Produces SVG files in the current directory:

| File | Function | Shows |
|------|----------|-------|
| `identity.svg` | `(defn identity [x] x)` | Single block, no branching |
| `factorial.svg` | Recursive factorial | Branch + self-call |
| `fizzbuzz.svg` | Nested cond | Multiple branch paths |
| `make-adder.svg` | Returns closure | Captured variable handling |
| `eval-expr.svg` | 6-way match expression evaluator | Complex: match dispatch, recursion, let-binding, conditional error, many blocks with cross-edges |
