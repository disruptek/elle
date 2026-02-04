# Elle Quick Test Reference

## Run Everything
```bash
cargo test           # All tests (~1 second)
cargo bench          # All benchmarks (~2 minutes)
```

## Run Specific Tests
```bash
cargo test --lib                    # Unit tests only
cargo test --test value_tests       # Value tests
cargo test --test reader_tests      # Parser tests
cargo test --test symbol_tests      # Symbol table tests
cargo test --test primitives_tests  # Built-in functions
cargo test --test integration_tests # End-to-end tests
cargo test --test property_tests    # Property-based tests
```

## Run Specific Benchmarks
```bash
cargo bench parsing              # Parser speed
cargo bench symbol_interning     # Symbol table speed
cargo bench compilation          # Compiler speed
cargo bench vm_execution         # VM speed
cargo bench conditionals         # If/then/else speed
cargo bench end_to_end           # Full pipeline speed
cargo bench scalability          # Large input performance
cargo bench memory_operations    # GC overhead
```

## Useful Test Flags
```bash
cargo test -- --nocapture        # Show println! output
cargo test test_name             # Run single test
cargo test -- --ignored          # Run ignored tests
cargo test -- --test-threads=1   # Serial execution
```

## CI/CD Commands
```bash
cargo test --all-features        # Full test suite
cargo bench --no-run             # Verify benchmarks compile
cargo clippy -- -D warnings      # Lints
cargo fmt -- --check             # Format check
```

## Test Count by Category
- Unit: 8
- Value: 14
- Reader: 27
- Symbol: 10
- Primitives: 24
- Integration: 32
- Property: 22
- **Total: 105 tests**

## Benchmark Count
- 8 groups
- 30+ individual benchmarks

## Performance Targets
- Test execution: < 2 seconds
- Benchmark compilation: < 1 minute
- Full benchmark run: < 5 minutes

## Test Success Criteria
✓ All tests pass
✓ No warnings
✓ No panics
✓ Deterministic results
