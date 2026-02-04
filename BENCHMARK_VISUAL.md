# Elle Benchmark Suite - Visual Guide

## The Interpreter Pipeline & Benchmarks

```
INPUT SOURCE CODE
        ↓
        │
    PARSING ←────────────────────────────────────────┐
    ├─ simple_number:      "42"                      │
    ├─ list_literal:       "(1 2 3 4 5)"             │ PARSING GROUP
    ├─ nested_expr:        "(+ (* 2 3) (- 10 5))"    │ 5 benchmarks
    ├─ deep_nesting:       "(((((1)))))"             │ ~5-100 μs
    └─ large_list_100:     "(0 1...99)"              │
        ↓                                             │
        │                                             │
    SYMBOL INTERNING ←──────────────────────────────┐
    ├─ first_intern:       New symbol insertion      │
    ├─ repeat_intern:      Symbol cache hit          │ SYMBOL GROUP
    └─ many_unique:        Bulk interning (100)      │ 3 benchmarks
        ↓                                             │ ~50-100 ns
        │                                             │
    COMPILATION ←─────────────────────────────────────┐
    ├─ simple_arithmetic:  "(+ 1 2)"                 │
    ├─ conditional:        "(if (> 5 3) 100 200)"    │ COMPILATION GROUP
    └─ nested_arithmetic:  "(+ (* 2 3) ...)"         │ 3 benchmarks
        ↓                                             │ ~500 ns - 5 μs
        │                                             │
    EXECUTION (VM) ←──────────────────────────────────┐
    ├─ int_add:            Integer addition (fast)   │
    ├─ mixed_arithmetic:   Int + Float (generic)     │ EXECUTION GROUP
    ├─ comparison:         Less-than check           │ 5 benchmarks
    ├─ cons:               List construction         │ ~300 ns - 3 μs
    └─ first:              List access               │
        ↓                                             │
        │                                             │
    SPECIAL CASES ←────────────────────────────────────┐
    ├─ if_true:            Branch taken              │
    ├─ nested_if:          Conditional nesting       │ SPECIAL GROUP
    ├─ simple (e2e):       Total pipeline            │ 2 benchmarks
    └─ complex (e2e):      Complex expression        │ ~1.5-10 μs
        ↓                                             │
        │                                             │
    SCALABILITY ←──────────────────────────────────────┐
    ├─ list_construction:  Size 10, 50, 100, 500     │ SCALABILITY GROUP
    └─ addition_chain:     Size 10, 50, 100, 500     │ 8 benchmarks
        ↓                                             │ Verify O(n)
        │                                             │
    MEMORY OPS ←────────────────────────────────────────┐
    ├─ value_clone:        Rc overhead               │ MEMORY GROUP
    └─ list_to_vec:        Traversal + allocation    │ 2 benchmarks
        ↓                                             │ ~300 ns - 2 μs
        │                                             │
    OUTPUT
```

---

## Benchmark Groups at a Glance

```
┌─────────────────────────────────────────────────────┐
│            BENCHMARK SUMMARY TABLE                  │
├──────────────────┬──────┬──────────┬────────────────┤
│ Group            │ Count│ Speed    │ Purpose        │
├──────────────────┼──────┼──────────┼────────────────┤
│ Parsing          │  5   │ 1-100 μs │ Lex/parse      │
│ Symbol Intern    │  3   │ 50-20k ns│ Hash table     │
│ Compilation      │  3   │ 500-5 μs │ AST→Bytecode   │
│ VM Execution     │  5   │ 300ns-3μs│ Bytecode run   │
│ Conditionals     │  2   │ 1-4 μs   │ Branch speed   │
│ End-to-End       │  2   │ 3-10 μs  │ Total latency  │
│ Scalability      │  8   │ Variable │ O(n) check     │
│ Memory Ops       │  2   │ 300-2 μs │ GC overhead    │
├──────────────────┼──────┼──────────┼────────────────┤
│ TOTAL            │ 30+  │ 50ns-100μs│ Full coverage │
└──────────────────┴──────┴──────────┴────────────────┘
```

---

## Performance Characteristics

### Expected Performance Profile

```
PARSING SPEED (input → AST)
────────────────────────────
    1 μs │     ●
         │     ├─ simple_number
    5 μs │     ├─ list_literal, nested_expr, deep_nesting
         │     │
   50 μs │     ├─ large_list_100
         │
  100 μs │●
         └─────────────────────────

COMPILATION SPEED (AST → Bytecode)
────────────────────────────────────
  100 ns │
         │●─ simple_arithmetic
  500 ns │
         │
    1 μs │●─ conditional
         │
    5 μs │●─ nested_arithmetic
         │
   10 μs │
         └─────────────────────────

EXECUTION SPEED (Bytecode → Result)
────────────────────────────────────
  100 ns │●─ comparison
         │
  300 ns │●─ int_add, first
         │
    1 μs │
         │
    2 μs │●─ cons, mixed_arithmetic
         │
    5 μs │
         └─────────────────────────
```

---

## Where Time Is Spent

### Typical "Simple Expression" Timeline

```
EXPRESSION: (+ 1 2 3)
─────────────────────────────

Parsing:     "(+ 1 2 3)" → Value::Cons(...) :  ~2 μs  (40%)
Compilation: AST → Bytecode                  :  ~1 μs  (20%)
Symbol Look: "+" → function pointer          :  ~100 ns (2%)
Execution:   Bytecode → Int(6)               :  ~1.5 μs(30%)
─────────────────────────────────────────────────────────
TOTAL:       ~5 μs

Bottleneck: PARSING (40% of time)
```

### Typical "Complex Expression" Timeline

```
EXPRESSION: (+ (* 2 3) (- 10 (/ 8 2)))
──────────────────────────────────────

Parsing:     Parse full expression           :  ~8 μs  (33%)
Compilation: Compile all operators          :  ~4 μs  (17%)
Symbol Look: Multiple function lookups      :  ~500 ns (2%)
Execution:   Run all bytecode               :  ~12 μs  (48%)
──────────────────────────────────────────────────────────
TOTAL:       ~24 μs

Bottleneck: EXECUTION (48% of time)
```

---

## Scalability Characteristics

### List Construction Performance

```
SIZE:        10      50     100    500
TIME:        1 μs    5 μs   10 μs  50 μs
MULTIPLIER:  1x      5x     10x    50x

Expected: Linear (5x → 10x → 50x) ✓
Bad:      Quadratic (5x → 30x → 150x) ✗
```

### Visualization

```
GOOD (O(n) - Linear):
Time
  ▲
  │                                    ●
  │                            ●
  │                    ●
  │            ●
  │────────────────────────────────────► Size
  10   50   100  500

BAD (O(n²) - Quadratic):
Time
  ▲
  │                                    ●
  │                    ●
  │            ●
  │        ●
  │────────────────────────────────────► Size
  10   50   100  500
```

---

## Common Benchmark Patterns

### Pattern 1: Fast Path vs Slow Path

```
MEASUREMENT: Integer Addition
──────────────────────────────

int_add:            300 ns  ✓ (specialized fast path)
mixed_arithmetic:   2 μs    ✓ (generic slower path)

Ratio: 6.6x slower for generic path - NORMAL
(type checking + conversion has cost)
```

### Pattern 2: Allocation Cost

```
MEASUREMENT: List Operations
──────────────────────────────

first (read):       500 ns  ✓ (no allocation)
cons (write):       1.5 μs  ✓ (allocates Rc)
list_to_vec:        2 μs    ✓ (allocates Vec)

Cost breakdown:
- No allocation:   500 ns baseline
- Rc allocation:  +1 μs (2x cost)
- Vec allocation: +1.5 μs (3x cost)
```

### Pattern 3: Structure Cost

```
MEASUREMENT: Parsing Cost
──────────────────────────

simple_number:      1 μs
list_literal:       5 μs    (5x for structure)
nested_expr:        10 μs   (10x for nesting)

Cost breakdown:
- Number parsing:  1 μs baseline
- List overhead:   +4 μs (for parentheses, vec alloc)
- Nesting depth:   +5 μs (recursive parsing)
```

---

## Benchmark Reliability

### Criterion's Statistical Analysis

```
BENCHMARK RESULT
═════════════════════════════════════════════════════

parsing/simple_number
  time:   [1.20 us 1.25 us 1.30 us]
           └──────┬─────┘
                  └─ 95% confidence interval

change: [-2.5% +0.0% +2.6%] with 100 IID samples
        └─────┬──────┘
              └─ % change from baseline

slope:  1.2525 us (1000 times faster than 1 sec)
R²:     0.99872 (excellent fit - very consistent)

variance: low (measurement is reliable)
```

### What The Statistics Mean

- **Time Range:** If you run it again, it'll be in this range 95% of time
- **Change:** How much it improved/regressed vs previous run
- **R²:** How consistent the measurements are (>0.99 is excellent)

---

## Reading Benchmark Output

### Good Output

```
running 30 benchmarks
parsing/simple_number              time:   [1.23 us 1.25 us 1.27 us]
parsing/list_literal               time:   [5.42 us 5.48 us 5.55 us]
parsing/nested_expr                time:   [9.87 us 10.1 us 10.3 us]
parsing/deep_nesting               time:   [5.21 us 5.25 us 5.30 us]
parsing/large_list_100             time:   [98.5 us 101 us 104 us]

criterion notice: Performance has improved.
criterion notice: 28 measurements remain statistically significant.
```

✓ Times are consistent (tight ranges)
✓ Scaling looks right (1x → 5x → 10x)
✓ No surprise slowdowns

### Warning Signs

```
parsing/simple_number              time:   [1.2 us  1.8 us  3.5 us]
                                           └──────┬──────┘
                                                  └─ HUGE variance!
```

⚠ Inconsistent measurements (noisy system)
⚠ May need to rerun, close other programs, or use `--sample-size`

---

## Using Benchmarks In Development

### Pre-Commit Workflow

```bash
# 1. Establish baseline
$ cargo bench -- --save-baseline before-change
   Running benches/benchmarks.rs
   criterion notice: Benchmarking...
   ✓ 30 benchmarks complete

# 2. Make your change
$ vim src/vm/mod.rs
$ # ... optimize something ...

# 3. Check if it's better/worse
$ cargo bench -- --baseline before-change
   parsing/simple_number: time: [-3.5% +2.1% +7.2%]
   ✓ small change - acceptable
   
   vm_execution/int_add:  time: [+15.2% +18.7% +21.5%]
   ✗ 18% slower - investigate!

# 4. If regression > 10%, revert or try different approach
$ git revert HEAD
$ cargo bench  # Verify we're back to baseline
```

---

## Optimization Priorities

### Based On Time Spent

```
1. EXECUTION (48% of complex expression time)
   │
   ├─ Specialization: int vs float paths ✓
   ├─ Inlining: primitive operations ✓
   └─ Next: Inline caching for lookups

2. PARSING (40% of simple expression time)
   │
   ├─ Lexer: hand-written, already optimized ✓
   ├─ Reader: minimal allocations ✓
   └─ Next: Streaming for very large inputs

3. COMPILATION (20% of typical time)
   │
   ├─ Bytecode generation: minimal ✓
   └─ Next: Rarely the bottleneck

4. SYMBOL INTERNING (2% of total time)
   │
   └─ FxHashMap: already optimal ✓
```

**Focus on EXECUTION first, then PARSING.**

---

## Summary

**The benchmark suite provides:**

1. **Performance baseline** - Know current speed
2. **Regression detection** - Catch slowdowns before commit
3. **Optimization targets** - Know where to focus effort
4. **Scalability verification** - Ensure O(n), not O(n²)
5. **Statistical confidence** - Measurements are reliable
6. **Historical tracking** - See improvements over time

**Run benchmarks:**
```bash
cargo bench
```

**Compare to baseline:**
```bash
cargo bench -- --baseline main
```

**Optimize and verify:**
```bash
# Make change
$ cargo bench
# See improvement
✓ Better! 15% faster
```
