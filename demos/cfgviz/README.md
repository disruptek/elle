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

### identity

`(defn identity [x] x)` — single block, no branching.

![identity](identity.svg)

### factorial

Recursive factorial — branch + self-call.

![factorial](factorial.svg)

### fizzbuzz

Nested `cond` — multiple branch paths.

![fizzbuzz](fizzbuzz.svg)

### make-adder

Returns a closure — shows captured variable handling.

![make-adder](make-adder.svg)

### eval-expr

6-way `match` expression evaluator — match dispatch, recursion, `let*` binding, conditional error. Many blocks with cross-edges.

![eval-expr](eval-expr.svg)
