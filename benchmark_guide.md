# Running JIT Comparison Benchmarks

## Overview

The Elle Lisp project has several benchmark suites. The JIT comparison benchmarks are in `benches/jit_vs_bytecode.rs` and compare native bytecode execution against the JIT coordinator overhead.

## Quick Start

### Run All Benchmarks (Default)
```bash
cargo bench --bench jit_vs_bytecode
```

This runs with default criterion settings:
- Sample size: 100
- Warmup time: 3 seconds per benchmark
- Measurement time: 5 seconds per benchmark

### Run with Fewer Samples (Faster)
```bash
cargo bench --bench jit_vs_bytecode -- --sample-size 10
```

**Note**: Criterion requires minimum 10 samples, so this is the minimum practical sample size.

### Run Specific Benchmark Groups

#### Coordinator Overhead Only
```bash
cargo bench --bench jit_vs_bytecode -- jit_coordinator
```

#### Arithmetic Patterns Only
```bash
cargo bench --bench jit_vs_bytecode -- arithmetic_patterns
```

#### Conditional Expressions Only
```bash
cargo bench --bench jit_vs_bytecode -- conditionals
```

#### Compilation Time Only
```bash
cargo bench --bench jit_vs_bytecode -- compilation_time
```

### Run Specific Test
```bash
cargo bench --bench jit_vs_bytecode -- bytecode_add_1000x
```

### Quiet Output (Less Verbose)
```bash
cargo bench --bench jit_vs_bytecode -- --quiet
```

### Verbose Output (More Details)
```bash
cargo bench --bench jit_vs_bytecode -- --verbose
```

## Benchmark Groups Explained

### 1. Coordinator Overhead (`jit_coordinator`)
Compares pure bytecode vs bytecode with JIT coordinator enabled:

- **bytecode_add_1000x**: 1000 iterations of `(+ i 1)` without JIT coordinator
- **coordinator_add_1000x**: Same but with JIT coordinator enabled

This measures the profiling overhead.

### 2. Arithmetic Patterns (`arithmetic_patterns`)
Tests various arithmetic operations:

- **bytecode/add**: `(+ 5 3)` - 1,000,000x iterations
- **bytecode/multiply**: `(* 5 3)` - 1,000,000x iterations
- **bytecode/subtract**: `(- 10 3)` - 1,000,000x iterations
- **bytecode/divide**: `(/ 20 4)` - 1,000,000x iterations
- **bytecode/mixed**: `(+ (* 2 3) (- 10 5))` - Complex expression

### 3. Conditionals (`conditionals`)
Tests if expressions:

- **bytecode/simple_if**: Simple `(if (> 5 3) 100 200)` - 1000x
- **bytecode/nested_if**: Nested conditions - 1000x

### 4. Compilation Time (`compilation_time`)
Measures pure compilation overhead:

- **compile_literal**: Time to compile a literal
- **compile_add**: Time to compile addition
- **compile_nested**: Time to compile nested expressions
- **compile_conditional**: Time to compile if expression
- **compile_complex**: Time to compile complex expression

## Output Interpretation

Criterion produces output like:

```
jit_coordinator/bytecode_add_1000x
                        time:   [484.02 µs 489.10 µs 494.25 µs]
                        change: [+1.1478% +2.3105% +3.4710%] (p = 0.00 < 0.05)
                        Performance has regressed.
```

- **time**: Lower bound, mean, upper bound of measured time
- **change**: Percentage change vs baseline (if available)
- **p-value**: Statistical significance (< 0.05 is significant)

## Performance Expectations

### Bytecode Performance
- Simple arithmetic: ~4.8 µs per operation
- Simple conditionals: ~5-10 µs
- Overhead per operation: ~5 µs

### Coordinator Overhead
- Profiling adds ~0-1% overhead to bytecode execution
- Negligible impact on baseline performance

### Compilation Time
- Literal compilation: < 1 µs
- Simple expression compilation: ~5-10 µs
- Complex expression compilation: ~20-50 µs

## Example Complete Run

```bash
# Run full benchmark suite with reduced sample size for speed
cargo bench --bench jit_vs_bytecode -- --sample-size 25
```

Expected time: ~5-10 minutes (depending on system)

## Tips

1. **For quick validation**: Use `--sample-size 10` (minimum)
2. **For accurate results**: Use default (100 samples) - takes ~30+ minutes
3. **For specific test**: Filter by name: `cargo bench --bench jit_vs_bytecode -- bytecode_add`
4. **Save baseline**: Results saved in `target/criterion/` for comparison
5. **Compare runs**: Criterion automatically compares against previous runs

## Troubleshooting

### Benchmark Takes Too Long
- Use `--sample-size 10` to reduce iterations
- Filter to specific benchmark groups
- Run in release mode (default for benchmarks)

### Inconsistent Results
- Close other applications to reduce system noise
- Run multiple times and compare
- Look at confidence intervals (lower/upper bounds)

### Out of Memory
- Run individual benchmarks instead of all at once
- Reduce sample size further
- Check available system RAM

## All Available Benchmarks

List all benchmarks:
```bash
cargo bench --bench jit_vs_bytecode -- --list
```

## Output Files

Criterion saves detailed results in:
```
target/criterion/
  ├── jit_coordinator/
  ├── arithmetic_patterns/
  ├── conditionals/
  └── compilation_time/
```

Each contains HTML reports with graphs and statistics.

## Next Steps

After running benchmarks:

1. **Analyze**: Review the criterion HTML reports in `target/criterion/`
2. **Compare**: Run again after code changes to see impact
3. **Baseline**: Save good baseline results for comparison
4. **Report**: Use graphs for performance documentation

## What These Benchmarks Tell Us

When native JIT code generation is fully implemented, these benchmarks will show:

- **Bytecode baseline**: Current performance without JIT
- **JIT overhead**: Cost of profiling and compilation
- **JIT speedup**: Performance improvement for hot functions (future)

Currently they establish baseline metrics for the profiling-enabled path.
