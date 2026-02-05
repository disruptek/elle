# Contributing to Elle

Thank you for your interest in contributing to Elle! This document provides guidelines and instructions for contributing.

## Code of Conduct

Be respectful, inclusive, and professional. We're all here to build great software together.

## Getting Started

### Prerequisites

- Rust 1.56+ (stable, beta, or nightly)
- Cargo
- Git

### Setting Up Development Environment

```bash
# Clone the repository
git clone https://github.com/disruptek/elle.git
cd elle

# Build the project
cargo build

# Run tests
cargo test

# Run benchmarks
cargo bench

# Generate and view documentation
cargo doc --open
```

## Making Changes

### Code Quality Requirements

All code must pass the following checks before being merged:

1. **Tests** - All tests must pass
   ```bash
   cargo test
   ```

2. **Formatting** - Code must be formatted with rustfmt
   ```bash
   cargo fmt
   ```

3. **Linting** - Code must pass clippy checks
   ```bash
   cargo clippy -- -D warnings
   ```

4. **Benchmarks** - Benchmarks must build and run
   ```bash
   cargo bench --no-run
   cargo bench --bench benchmarks
   ```

5. **Examples** - Examples must compile
   ```bash
   cargo build --examples
   ```

6. **Documentation** - Documentation must build without warnings
   ```bash
   RUSTDOCFLAGS="-D warnings" cargo doc --no-deps
   ```

### Workflow

1. **Create a branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes**
   - Keep commits atomic and well-documented
   - Write meaningful commit messages

3. **Test locally**
   ```bash
   cargo test
   cargo fmt
   cargo clippy -- -D warnings
   ```

4. **Push and create a pull request**
   ```bash
   git push origin feature/your-feature-name
   ```

5. **Wait for CI** - GitHub Actions will automatically:
   - Run tests on stable, beta, and nightly Rust
   - Check formatting with rustfmt
   - Run linting with clippy
   - Execute benchmarks
   - Build examples
   - Generate coverage reports
   - Build documentation

**No pull request can be merged unless all CI checks pass.**

## Pull Request Guidelines

- Write descriptive PR titles and descriptions
- Reference any related issues
- Keep PRs focused on a single feature or fix
- Ensure all commits are clean and well-documented
- Update documentation if your changes affect user-facing APIs

## Commit Message Guidelines

- Use clear, descriptive commit messages
- Start with a verb in imperative mood (Add, Fix, Improve, etc.)
- Keep the first line under 50 characters
- Add more detail in the body if needed

Example:
```
Add FFI support for calling C functions

This implements Phase 2 of the FFI roadmap, allowing Elle code to call
C functions with proper type marshaling and error handling. Includes
comprehensive tests and documentation.

Fixes #42
```

## Project Structure

```
elle/
├── src/
│   ├── compiler/      # Lisp to bytecode compiler
│   ├── ffi/           # Foreign Function Interface (C interop)
│   ├── primitives.rs  # Built-in Lisp functions
│   ├── reader.rs      # S-expression reader
│   ├── symbol.rs      # Symbol interning
│   ├── value.rs       # Elle value types
│   ├── vm/            # Virtual machine (bytecode executor)
│   └── lib.rs         # Library root
├── tests/             # Integration tests
├── examples/          # Example programs
├── benches/           # Performance benchmarks
└── .github/
    └── workflows/     # GitHub Actions CI/CD
```

## Areas for Contribution

### High Priority

- **FFI Phase 2b** - Floating-point returns, 7+ arguments, variadic functions
- **FFI Phase 3** - Header parsing and auto-binding
- **Performance** - Optimize hot paths identified in benchmarks
- **Documentation** - Improve API documentation and examples

### Medium Priority

- **Error Messages** - Improve clarity and helpfulness
- **Testing** - Increase test coverage
- **Examples** - Add more practical examples
- **Refactoring** - Improve code quality and maintainability

### Low Priority

- **WASM Support** - Extend platform support
- **Additional Languages** - FFI for other languages beyond C

## Testing

### Running Tests

```bash
# All tests
cargo test

# Specific test
cargo test test_name

# With output
cargo test -- --nocapture

# Only integration tests
cargo test --test '*'

# Only unit tests
cargo test --lib
```

### Writing Tests

- Add unit tests next to the code being tested
- Add integration tests in `tests/` directory
- Aim for high code coverage
- Test both happy paths and error cases

Example:
```rust
#[test]
fn test_example() {
    let result = my_function(42);
    assert_eq!(result, expected_value);
}
```

## Documentation

- Document public APIs with doc comments
- Use `///` for public items
- Include examples in doc comments when helpful
- Keep the README up to date

Example:
```rust
/// Loads a shared library and returns a handle.
///
/// # Arguments
/// * `path` - Path to the library file
///
/// # Returns
/// * `Ok(handle)` - Library handle for future reference
/// * `Err(msg)` - Error message if loading fails
///
/// # Example
/// ```no_run
/// # use elle::ffi::FFISubsystem;
/// let mut ffi = FFISubsystem::new();
/// let lib = ffi.load_library("/lib/libc.so.6")?;
/// # Ok::<(), String>(())
/// ```
pub fn load_library(&mut self, path: &str) -> Result<u32, String> {
    // ...
}
```

## Performance

- Run benchmarks to check for regressions
- Profile hot paths before optimizing
- Keep code readable unless optimization is critical
- Document trade-offs

## Questions?

- Open a GitHub issue for questions
- Check existing issues before creating new ones
- Use clear, descriptive titles and descriptions

Thank you for contributing to Elle! 🎉
