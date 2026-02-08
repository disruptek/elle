# Elle Lisp Demos: Dogfooding & Cross-Language Comparison

This directory contains demonstration programs that serve two purposes:

1. **Dogfooding**: Testing Elle itself with non-trivial algorithms
2. **Cross-Language Comparison**: Implementing the same algorithms in multiple Lisps to compare ergonomics, idioms, and performance

## The Philosophy

Elle is designed with Janet ergonomics in mind. These demos allow us to:

- Validate that Elle can express the same algorithms as other Lisps
- Identify missing features or pain points in Elle
- Compare code clarity and readability across languages
- Understand performance characteristics relative to mature implementations
- Discover and document bugs in Elle that real code would expose

## Demo Categories

### N-Queens Problem (`./nqueens/`)

A classic backtracking algorithm that solves the N-Queens chess problem. Tests:
- **Recursion depth** and tail-call patterns
- **List operations** (cons, append, reverse, first, rest)
- **Functional predicates** (safe? function to check placement validity)
- **Backtracking logic** (exploring multiple branches, accumulating solutions)

**Results Summary:**
| Language | N=4 | N=8 | Status |
|----------|-----|-----|--------|
| Chez Scheme | 2 ✓ | 92 ✓ | Working |
| SBCL | 2 ✓ | 92 ✓ | Working |
| Janet | 0 ✗ | 0 ✗ | [Issue #155](https://github.com/disruptek/elle/issues/155) |
| Elle | 1 ✗ | 1 ✗ | [Issue #154](https://github.com/disruptek/elle/issues/154) |

### Matrix Operations (`./matrix-ops/`) - TODO

Pure Lisp and FFI-based matrix operations. Tests:
- **Dense matrix representation** (vector of vectors)
- **Numeric computation** (matrix multiply, transpose, decomposition)
- **FFI integration** (calling BLAS/LAPACK for performance comparison)
- **Performance across implementation boundaries**

## Running the Demos

### N-Queens

```bash
# Chez Scheme
chezscheme --script nqueens/nqueens.scm

# SBCL
sbcl --script nqueens/nqueens.lisp.cl

# Janet
janet nqueens/nqueens.janet

# Elle
cargo run --release -- nqueens/nqueens.lisp
```

### Expected Output

For correct implementations (Chez, SBCL):
```
Solving N-Queens for N=4... Found 2 solution(s)
Solving N-Queens for N=8... Found 92 solution(s)
Solving N-Queens for N=10... Found 724 solution(s)
Solving N-Queens for N=12... Found 14200 solution(s)
```

## Known Issues

### Elle Bug #154: Incomplete Solution Search
Elle's append operation in recursive contexts only preserves the first solution found.
- File: `nqueens/nqueens.lisp`
- Issue: [#154](https://github.com/disruptek/elle/issues/154)
- Impact: All test sizes return 1 solution instead of the correct count

### Janet Bug #155: Array Accumulation in Recursion
Janet's `array/concat` in recursive backtracking contexts fails to accumulate solutions.
- File: `nqueens/nqueens.janet`
- Issue: [#155](https://github.com/disruptek/elle/issues/155)
- Impact: All test sizes return 0 solutions
- Note: Safe function verified correct in isolation; bug is in recursive accumulation

## Code Organization

Each demo typically has implementations for:
- `demo.scm` - Chez Scheme (reference implementation)
- `demo.lisp.cl` - SBCL Common Lisp
- `demo.janet` - Janet
- `demo.lisp` - Elle

This structure makes it easy to compare implementations side-by-side.

## Contributing Fixes

If you fix one of the known issues:

1. Update the status table above
2. Run the demo to verify it produces correct results
3. Create a pull request with the fix and a note about which issue it closes

## Future Demos

Planned demonstrations:

- **Matrix Operations** (pure + FFI)
- **Symbolic Differentiation** (macro/metaprogramming showcase)
- **Game Tree Search** (Minimax with alpha-beta pruning)
- **JSON Processing Pipeline** (data transformation idioms)

## Notes for Implementers

When implementing a demo for a new language:

1. Try to match the algorithm structure of the reference implementation (Chez)
2. Use idiomatic patterns for that language (don't force Scheme-style code)
3. Comment anywhere the language's approach differs from the reference
4. Include debugging output to help diagnose issues if results are incorrect
5. Document any discovered limitations or surprising behaviors

## References

- [Chez Scheme Documentation](https://www.scheme.com/csug10.0/)
- [SBCL Documentation](http://www.sbcl.org/manual/)
- [Janet Documentation](https://janet-lang.org/docs/index.html)
- [Elle Documentation](../docs/)
