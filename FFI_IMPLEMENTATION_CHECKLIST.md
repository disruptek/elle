# Elle FFI Implementation Checklist

This document provides concrete first steps for implementing the FFI roadmap.

## Pre-Implementation Tasks

### Documentation Review
- [ ] Read FFI_ROADMAP.md fully
- [ ] Review Phase 1 detailed requirements
- [ ] Study libffi documentation
- [ ] Review System V AMD64 ABI

### Repository Setup
- [ ] Create GitHub issue for Phase 1
- [ ] Create feature branch: `feature/ffi-phase-1`
- [ ] Add FFI crates to Cargo.toml (see below)
- [ ] Update CI/CD for FFI testing

### Dependency Additions

Add to `Cargo.toml`:

```toml
[dependencies]
# Phase 1 dependencies
libloading = "0.8"
libffi-sys = "0.4"

# Phase 3+ (add later)
# bindgen = "0.69"
# wasm-bindgen = { version = "0.2", optional = true }
# js-sys = { version = "0.3", optional = true }

[features]
default = []
wasm-support = ["wasm-bindgen", "js-sys"]
```

## Phase 1 Implementation Checklist

### 1.1 Module Structure

- [ ] Create `src/ffi/mod.rs`
  - [ ] Define FFISubsystem struct
  - [ ] Implement FFISubsystem::new()
  - [ ] Plan VM integration points

- [ ] Create `src/ffi/loader.rs`
  - [ ] Define LibraryHandle struct
  - [ ] Implement load_library() method
  - [ ] Handle platform differences (Linux only for now)
  - [ ] Add error handling for missing files

- [ ] Create `src/ffi/symbol.rs`
  - [ ] Define symbol cache structure
  - [ ] Implement symbol resolution
  - [ ] Add caching for repeated lookups

- [ ] Create `src/ffi/types.rs`
  - [ ] Define CType enum (Void, Bool, Char, Int, Float, Double, Pointer, Struct, etc.)
  - [ ] Implement type size calculations
  - [ ] Add platform-specific size/alignment logic
  - [ ] Define FunctionSignature struct

- [ ] Create `src/ffi/call.rs`
  - [ ] Define FunctionCall wrapper struct
  - [ ] Implement libffi binding
  - [ ] Implement call() method
  - [ ] Add error handling for invalid calls

### 1.2 VM Integration

- [ ] Modify `src/vm/mod.rs`
  - [ ] Add `ffi_subsystem: FFISubsystem` field to VM
  - [ ] Implement `load_library()` method
  - [ ] Implement `resolve_symbol()` method

- [ ] Modify `src/value.rs`
  - [ ] Add `CHandle(CValue)` variant to Value enum
  - [ ] Add `LibraryHandle(u32)` variant to Value enum
  - [ ] Implement Display for new variants
  - [ ] Implement Debug for new variants

### 1.3 Primitives

- [ ] Create FFI primitives module
  - [ ] `(load-library path)` → library-handle
  - [ ] `(get-library-function lib-handle name)` → function-handle
  - [ ] `(call-c-function func ...args)` → result

- [ ] Register primitives in `register_primitives()`

### 1.4 Tests

- [ ] Create `tests/ffi_loader_tests.rs`
  - [ ] Test loading system libc
  - [ ] Test missing library error
  - [ ] Test symbol resolution success
  - [ ] Test symbol not found error

- [ ] Create `tests/ffi_types_tests.rs`
  - [ ] Test type size calculations (i32, f64, etc.)
  - [ ] Test type alignment
  - [ ] Test pointer sizes

- [ ] Create `tests/ffi_call_tests.rs`
  - [ ] Test calling strlen from libc
  - [ ] Test calling abs from libc
  - [ ] Test calling sin from libm
  - [ ] Test return value correctness
  - [ ] Test type conversions

### 1.5 Integration

- [ ] Update `src/lib.rs` to export FFI modules
- [ ] Update `src/main.rs` REPL help text
- [ ] Add FFI section to README.md
- [ ] Update CHANGELOG.md

### 1.6 Documentation

- [ ] Add inline rustdoc for all public items
- [ ] Create `docs/FFI_GETTING_STARTED.md`
- [ ] Create first example: `examples/ffi-strlen.lisp`
- [ ] Create first example: `examples/ffi-math.lisp`

### 1.7 Testing Checklist

```bash
# Unit tests
cargo test --lib ffi::loader
cargo test --lib ffi::types
cargo test --lib ffi::call

# Integration tests
cargo test --test ffi_loader_tests
cargo test --test ffi_types_tests
cargo test --test ffi_call_tests

# Example programs
cargo run --example ffi-strlen
cargo run --example ffi-math

# Benchmark
cargo bench ffi_call_overhead
```

### 1.8 Deliverables

- [ ] All Phase 1 tests passing (15+ tests)
- [ ] Example: Simple strlen call works
- [ ] Example: Math function call works
- [ ] Documentation: Getting started guide
- [ ] No compiler warnings
- [ ] Performance: Call overhead < 1µs

---

## Phase 2 Preparation (Concurrent)

### Type System Planning
- [ ] Design struct representation
- [ ] Plan struct layout calculations
- [ ] Design Value::CStruct variant
- [ ] Plan marshaling traits

### Documentation
- [ ] Create marshaling design doc
- [ ] List all C types to support
- [ ] Plan struct field access API

---

## Code Skeleton

### src/ffi/mod.rs

```rust
pub mod loader;
pub mod symbol;
pub mod types;
pub mod call;

use std::collections::HashMap;

pub struct FFISubsystem {
    libraries: HashMap<u32, LibraryHandle>,
    next_lib_id: u32,
}

impl FFISubsystem {
    pub fn new() -> Self {
        FFISubsystem {
            libraries: HashMap::new(),
            next_lib_id: 1,
        }
    }

    pub fn load_library(&mut self, path: &str) -> Result<u32, String> {
        // Delegate to loader module
        let lib = loader::load_library(path)?;
        let id = self.next_lib_id;
        self.next_lib_id += 1;
        self.libraries.insert(id, lib);
        Ok(id)
    }

    pub fn get_library(&self, id: u32) -> Option<&LibraryHandle> {
        self.libraries.get(&id)
    }
}

#[derive(Clone)]
pub struct LibraryHandle {
    pub id: u32,
    pub path: String,
    pub native: Option<libloading::Library>,
}
```

### src/ffi/loader.rs

```rust
use libloading::{Library, Symbol};
use std::ffi::OsStr;

pub struct LibraryHandle {
    pub id: u32,
    pub path: String,
    pub native: Option<libloading::Library>,
}

pub fn load_library(path: &str) -> Result<LibraryHandle, String> {
    #[cfg(target_os = "linux")]
    unsafe {
        match Library::new(path) {
            Ok(lib) => Ok(LibraryHandle {
                id: 0, // Will be set by FFISubsystem
                path: path.to_string(),
                native: Some(lib),
            }),
            Err(e) => Err(format!("Failed to load library {}: {}", path, e)),
        }
    }

    #[cfg(not(target_os = "linux"))]
    Err("Library loading only supported on Linux".to_string())
}

pub fn get_symbol<'a, T>(lib: &'a libloading::Library, name: &str) -> Result<Symbol<'a, T>, String> {
    unsafe {
        lib.get::<T>(name.as_bytes())
            .map_err(|e| format!("Symbol {} not found: {}", name, e))
    }
}
```

### src/ffi/types.rs

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CType {
    Void,
    Bool,
    Char,
    Short,
    Int,
    Long,
    LongLong,
    Float,
    Double,
    // More types in Phase 2+
}

impl CType {
    pub fn size(&self) -> usize {
        match self {
            CType::Void => 0,
            CType::Bool => 1,
            CType::Char => 1,
            CType::Short => 2,
            CType::Int => 4,
            CType::Long => 8, // x86-64
            CType::LongLong => 8,
            CType::Float => 4,
            CType::Double => 8,
        }
    }

    pub fn alignment(&self) -> usize {
        // x86-64 ABI alignment
        self.size() // Simplified; real version more complex
    }
}

pub struct FunctionSignature {
    pub name: String,
    pub args: Vec<CType>,
    pub return_type: CType,
}
```

### src/ffi/call.rs

```rust
use crate::value::Value;
use super::types::CType;

pub struct FunctionCall {
    pub signature: FunctionSignature,
    pub func_ptr: *const std::ffi::c_void,
}

impl FunctionCall {
    pub fn call(&self, args: &[Value]) -> Result<Value, String> {
        // Type check arguments
        if args.len() != self.signature.args.len() {
            return Err(format!(
                "Function {} expects {} arguments, got {}",
                self.signature.name,
                self.signature.args.len(),
                args.len()
            ));
        }

        // TODO: Implement libffi call
        // This is where the actual FFI magic happens

        Ok(Value::Nil)
    }
}
```

---

## Git Workflow

### Branch Strategy

```bash
# Create feature branch
git checkout -b feature/ffi-phase-1

# Create separate commits for each logical section
git commit -m "ffi: Add loader module with library loading"
git commit -m "ffi: Add types module with C type definitions"
git commit -m "ffi: Add call module with libffi wrapper"
git commit -m "ffi: Add FFI primitives to VM"
git commit -m "ffi: Add Phase 1 tests"
git commit -m "ffi: Add FFI documentation"

# Submit PR for review
git push origin feature/ffi-phase-1
```

### Commit Message Template

```
ffi: Add [module name] for [feature description]

Implements Phase 1 [requirement].

- [What was done]
- [How it works]
- [Testing approach]

Closes #[issue-number]
```

---

## Common Pitfalls to Avoid

1. **Platform Assumptions** - Always check target_os/target_arch
2. **Calling Convention** - Don't assume x86-64 SysV; use libffi
3. **Memory Safety** - Track unsafe blocks carefully
4. **Type Mismatches** - Validate types before calling C
5. **Resource Leaks** - Ensure libraries are properly closed
6. **Error Handling** - Provide context in error messages
7. **Testing** - Test both success and failure paths

---

## Resources

### Rust FFI Books/Guides
- The Rustonomicon (FFI chapter)
- "The Art of Writing Rust FFI" (Medium articles)
- Rust FFI guidelines in Rust API Guidelines

### C Standards
- System V AMD64 ABI: https://refspecs.linuxbase.org/elf/x86-64-abi-0.99.pdf
- C11 Standard (type sizes and alignment)

### Library Documentation
- libffi: https://github.com/libffi/libffi
- libloading: https://docs.rs/libloading/
- libc: https://docs.rs/libc/

---

## Success Metrics for Phase 1

- [ ] 15+ tests passing
- [ ] Can load libc from REPL
- [ ] Can call strlen() successfully
- [ ] Can call sin() from libm
- [ ] Call overhead < 1µs
- [ ] Zero unsafe code issues detected
- [ ] Documentation complete
- [ ] Code review approved
- [ ] Merged to main branch

Once these are complete, proceed to Phase 2 planning.
