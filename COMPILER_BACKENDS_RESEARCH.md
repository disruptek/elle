# Embeddable Compiler Backends and Runtime Libraries for Rust

## Overview

This document provides a comprehensive analysis of embeddable compiler backends and runtime libraries that can be used to compile and execute custom languages (like Lisp) within a Rust application.

---

## 1. JIT (Just-In-Time) Compilation Backends

### 1.1 Cranelift (Recommended for most cases)

**Overview:**
Cranelift is a code generator from the Bytecode Alliance that generates machine code for WebAssembly and other bytecode formats. It's designed to be fast both for compilation and execution.

**Crate:** `cranelift` (v0.128.3 as of latest)
**Repository:** https://github.com/bytecodealliance/wasmtime
**License:** Apache-2.0 WITH LLVM-exception

**Key Components:**
- `cranelift-codegen` - Core code generation
- `cranelift-jit` - JIT compilation support
- `cranelift-frontend` - IR construction helpers
- `cranelift-module` - Module building interface
- `cranelift-native` - Native architecture support

**How to Use:**

```rust
use cranelift::prelude::*;

fn main() {
    // Create an ISA (Instruction Set Architecture) builder
    let mut settings = settings::builder();
    settings.set("use_colocated_libcalls", "true").unwrap();
    
    let isa = cranelift_native::builder()
        .unwrap()
        .finish(settings::Flags::new(settings))
        .unwrap();
    
    // Create a context for building functions
    let mut context = Context::new();
    
    // Build your intermediate representation (IR)
    // ...
    
    // Compile to machine code
    let codegen = context.codegen(isa.as_ref());
}
```

**Complexity:** Medium
- Simple code generation: easy
- Advanced optimizations: medium
- IR construction: requires understanding Cranelift IR

**Performance Characteristics:**
- Compilation speed: ~10-50 MB/s depending on optimization level
- Runtime speed: ~80-90% of LLVM-optimized code (good for development)
- Memory usage: Low to medium
- Best for: Development, rapid iteration, platforms without LLVM

**Production Readiness:** Fully production-ready
- Used by Wasmtime, Wasmer
- Active development by Bytecode Alliance
- Comprehensive error handling
- Good debugging support

**Example Use Cases:**
- Scripting languages with JIT
- DSL compilers
- Quick prototyping of language backends
- Embedded JavaScript/Python alternatives

---

### 1.2 Inkwell (LLVM Bindings)

**Overview:**
Inkwell is a safe Rust wrapper around LLVM's C API, providing a strongly-typed interface for LLVM IR construction and code generation.

**Crate:** `inkwell` (v0.8.0 as of latest)
**Repository:** https://github.com/TheDan64/inkwell
**License:** Apache-2.0

**Supported LLVM Versions:** 8-21
**Key Dependencies:**
- `llvm-sys` - Low-level LLVM C bindings
- `libc` - C library interface

**How to Use:**

```rust
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};

fn main() {
    // Create context
    let context = Context::create();
    
    // Create module
    let module = context.create_module("my_module");
    
    // Create builder
    let builder = context.create_builder();
    
    // Create execution engine for JIT
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::Aggressive)
        .unwrap();
    
    // Build IR and compile
    let i32_type = context.i32_type();
    let fn_type = i32_type.fn_type(&[i32_type.into()], false);
    let function = module.add_function("my_func", fn_type, None);
    
    // ... add basic blocks and instructions ...
    
    // Get compiled function
    type MyFunc = unsafe extern "C" fn(i32) -> i32;
    let func: JitFunction<MyFunc> = unsafe {
        execution_engine.get_function("my_func").unwrap()
    };
}
```

**Complexity:** Heavy
- LLVM API is comprehensive but complex
- IR construction: detailed, requires IR knowledge
- Optimization configuration: granular but requires tuning

**Performance Characteristics:**
- Compilation speed: 1-10 MB/s (slower than Cranelift)
- Runtime speed: ~95-100% of hand-written native code (excellent)
- Memory usage: High (LLVM overhead)
- Best for: Maximum runtime performance

**Production Readiness:** Production-ready
- Stable LLVM interface
- Multiple LLVM version support
- Good documentation (Inkwell-specific)
- Active maintenance

**Example Use Cases:**
- High-performance language implementations (PyPy-like)
- Compiled languages (Go, Rust-like)
- Numerical/scientific computing DSLs
- When runtime performance is critical

**Important Notes:**
- Requires LLVM development libraries installed
- Compilation can take significant time
- Longer startup time than Cranelift due to LLVM initialization
- Larger compiled binary size

---

### 1.3 DynASM and DynASMRT

**Overview:**
DynASM is a macro-based JIT assembler that allows writing assembly code inline with Rust. It's very low-level but extremely fast.

**Crates:**
- `dynasm` (v4.0.2) - Procedural macro
- `dynasmrt` - Runtime support library

**Repository:** https://github.com/CensoredUsername/dynasm-rs
**License:** MPL-2.0

**Key Features:**
- Inline assembly syntax mixing Rust and x86/ARM assembly
- Backward assembly support
- Label support with relative references

**How to Use:**

```rust
use dynasmrt::{dynasm, DynasmLabelApi, DynasmApi, x64};
use dynasmrt::x64::*;

fn main() {
    let mut assembly = x64::Assembler::new().unwrap();
    
    dynasm!(assembly
        ; mov rax, 42
        ; ret
    );
    
    let code = assembly.finalize().unwrap();
    let func: extern "C" fn() -> i64 = unsafe {
        std::mem::transmute(code.ptr(dynasmrt::AssemblyOffset(0)))
    };
    
    println!("{}", func()); // Output: 42
}
```

**Complexity:** Light to Medium
- Assembly knowledge required
- Macro-based (compile-time checking)
- Limited to assembly semantics

**Performance Characteristics:**
- Compilation speed: Extremely fast (inline, minimal overhead)
- Runtime speed: Native performance
- Memory usage: Minimal
- Best for: Small, hot code paths; kernels; tight loops

**Production Readiness:** Production-ready
- Stable API
- Used in production JIT compilers
- Good for embedded scenarios

**Example Use Cases:**
- Hotspot JIT compilation
- VM instruction dispatch
- Numerical kernels
- Real-time systems where latency matters

**Limitations:**
- Requires manual assembly writing
- Architecture-specific (x86-64, ARM64)
- No high-level optimizations
- Better for specific hot paths than whole language compilation

---

## 2. VM (Virtual Machine) Backends

### 2.1 Wasmtime (WASM JIT Runtime) - RECOMMENDED

**Overview:**
Wasmtime is a standalone WebAssembly JIT engine from the Bytecode Alliance, capable of executing WebAssembly modules with near-native performance.

**Crate:** `wasmtime` (v41.0.3 as of latest)
**Repository:** https://github.com/bytecodealliance/wasmtime
**License:** Apache-2.0 WITH LLVM-exception

**Key Components:**
- `Engine` - Global compilation environment
- `Module` - Compiled WebAssembly module
- `Instance` - Instantiated module with state
- `Linker` - Connects imports/exports
- `Store<T>` - Heap for instances and data

**How to Use:**

```rust
use wasmtime::*;

fn main() -> Result<()> {
    let engine = Engine::default();
    
    let module = Module::new(&engine, r#"
        (module
            (func (export "add") (param i32 i32) (result i32)
                (i32.add (local.get 0) (local.get 1)))
        )
    "#)?;
    
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[])?;
    
    let add = instance.get_typed_func::<(i32, i32), i32>(&mut store, "add")?;
    let result = add.call(&mut store, (5, 3))?;
    println!("Result: {}", result); // Output: 8
    
    Ok(())
}
```

**Complexity:** Light
- High-level API
- Abstracts complexity of memory management
- Good documentation
- Similar to JavaScript WebAssembly API

**Performance Characteristics:**
- Compilation speed: Fast (~10-50 MB/s)
- Runtime speed: 90-95% of native (Cranelift backend), up to 99% (LLVM backend)
- Memory usage: Moderate to High (depends on module size)
- Startup time: Fast
- Best for: General-purpose language runtime

**Production Readiness:** Excellent
- Used in major projects (Fastly, Shopify, etc.)
- Comprehensive feature set
- Good error messages
- Active development
- Security-focused design

**Key Features:**
- Multiple compiler backends (Cranelift, LLVM, Singlepass)
- AOT compilation support
- Async support
- Component Model support
- Tail calls, SIMD, GC
- Cross-compilation support
- Profiling/debugging

**Cargo Features:**
```toml
[dependencies]
wasmtime = { version = "41", features = [
    "cranelift",      # JIT compiler (default)
    "llvm",          # Alternative LLVM backend
    "async",         # Async function support
    "cache",         # Module caching
    "profiling",     # Performance profiling
] }
```

**Example Use Cases:**
- Scripting languages compiled to WASM
- Plugin systems
- Sandboxed code execution
- Cloud computing (Fastly Compute@Edge, Cloudflare Workers)
- Game scripting

**Advantages:**
- Large ecosystem (can compile many languages to WASM)
- Security through sandboxing
- Resource limits/metering built-in
- Excellent tooling
- Portable binaries

**Disadvantages:**
- Must compile to WASM first (two-step compilation)
- WASM overhead (instructions are more restricted)
- Not suitable for very low-level code

---

### 2.2 Wasmer (Alternative WASM Runtime)

**Overview:**
Wasmer is a feature-rich WebAssembly runtime emphasizing portability and performance.

**Crate:** `wasmer` (v7.0.1 as of latest)
**Repository:** https://github.com/wasmerio/wasmer
**License:** MIT

**Key Differences from Wasmtime:**
- More compiler backends available
- Support for non-WASM runtimes (V8, WAMR)
- Slightly different API design

**How to Use:**

```rust
use wasmer::{Store, Module, Instance, Value, imports};

fn main() -> anyhow::Result<()> {
    let module_wat = r#"
        (module
            (func (export "add") (param i32 i32) (result i32)
                (i32.add (local.get 0) (local.get 1))
            )
        )
    "#;
    
    let mut store = Store::default();
    let module = Module::new(&store, &module_wat)?;
    let instance = Instance::new(&mut store, &module, &imports! {})?;
    
    let add = instance.exports.get_function("add")?;
    let result = add.call(&mut store, &[Value::I32(5), Value::I32(3)])?;
    println!("Result: {:?}", result[0]);
    
    Ok(())
}
```

**Complexity:** Light (similar to Wasmtime)

**Performance Characteristics:**
- Similar to Wasmtime
- Cranelift: ~10-50 MB/s compilation
- LLVM: ~95% native performance
- Singlepass: Fast compilation, slower execution

**Compiler Backend Options:**
```toml
[dependencies]
wasmer = { version = "7", features = [
    "cranelift",      # Balanced (default)
    "llvm",          # Maximum performance
    "singlepass",    # Fastest compilation
] }
```

**Production Readiness:** Production-ready

**Key Features:**
- Pluggable compilers
- Headless mode (pre-compiled modules)
- Cross-compilation support
- WASIX support (extended WASI)

**Example Use Cases:**
- Similar to Wasmtime
- Plugin systems
- Edge computing
- Portable bytecode distribution

---

### 2.3 Wasmi (Interpreter)

**Overview:**
Wasmi is a pure-Rust WebAssembly interpreter, useful when JIT compilation is not allowed or feasible.

**Crate:** `wasmi` (v1.0.8 as of latest)
**Repository:** https://github.com/wasmi-labs/wasmi
**License:** MIT/Apache-2.0

**How to Use:**

```rust
use wasmi::*;

fn main() -> Result<(), wasmi::Error> {
    let wasm = r#"
        (module
            (func (export "add") (param i32 i32) (result i32)
                (i32.add (local.get 0) (local.get 1))
            )
        )
    "#;
    
    let engine = Engine::default();
    let module = Module::new(&engine, wasm)?;
    
    type HostState = ();
    let mut store = Store::new(&engine, ());
    let mut linker = <Linker<HostState>>::new(&engine);
    
    let instance = linker.instantiate_and_start(&mut store, &module)?;
    let add = instance.get_typed_func::<(i32, i32), i32>(&store, "add")?;
    let result = add.call(&mut store, (5, 3))?;
    println!("Result: {}", result);
    
    Ok(())
}
```

**Complexity:** Light

**Performance Characteristics:**
- Compilation speed: Very fast (no JIT)
- Runtime speed: 5-20% of native (interpreter overhead)
- Memory usage: Low
- Startup time: Extremely fast
- Best for: Embedded systems, platforms without JIT

**Production Readiness:** Production-ready

**Key Features:**
- No external dependencies (pure Rust)
- `#![no_std]` compatible
- SIMD support (optional)
- Resource limiting
- Portable

**Example Use Cases:**
- iOS/WebOS where JIT is restricted
- Embedded systems
- Scripting in games
- Policy evaluation

---

## 3. Custom VM Implementations

### 3.1 Stack-Based VM

**Characteristics:**
- Simple to implement
- Easy to understand bytecode
- Generally slower than register-based

**Crate Architecture:**

```rust
pub struct VM {
    stack: Vec<Value>,
    instructions: Vec<Opcode>,
    pc: usize,  // program counter
}

pub enum Opcode {
    Push(Value),
    Add,
    Sub,
    Call(usize),
    Return,
    // ... more opcodes
}

impl VM {
    pub fn execute(&mut self) -> Result<Value> {
        while self.pc < self.instructions.len() {
            match self.instructions[self.pc] {
                Opcode::Push(val) => {
                    self.stack.push(val);
                    self.pc += 1;
                }
                Opcode::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a + b);
                    self.pc += 1;
                }
                // ...
            }
        }
        Ok(self.stack.pop().unwrap())
    }
}
```

**Complexity:** Light to Medium

**Performance:** 2-10x slower than native code (acceptable for scripting)

**Best for:**
- Educational purposes
- Embedded scripting
- Prototyping language features
- Straightforward interpretation

---

### 3.2 Register-Based VM

**Characteristics:**
- More complex to implement
- Closer to real CPU architecture
- Better performance than stack-based
- Smaller bytecode

**Performance:** 5-25% slower than native (better than stack-based)

**Best for:**
- Production scripting languages
- When performance matters more than simplicity

---

## 4. Hybrid Approaches

### 4.1 Interpreter with Tiering JIT

**Architecture:**
1. Interpret bytecode initially (fast startup)
2. Profile hot paths
3. JIT compile hot functions with Cranelift/LLVM
4. Call compiled code from interpreter

**Implementation Strategy:**

```rust
pub struct HybridVM {
    interpreter: Interpreter,
    jit: CraneliftJIT,
    hot_paths: HashMap<FuncId, (usize, CompiledFunc)>,
    call_counts: HashMap<FuncId, usize>,
}

impl HybridVM {
    pub fn execute(&mut self, func: FuncId) -> Result<Value> {
        self.call_counts[&func] += 1;
        
        // JIT if hot
        if self.call_counts[&func] > 1000 && !self.hot_paths.contains_key(&func) {
            let compiled = self.jit.compile(func)?;
            self.hot_paths.insert(func, compiled);
        }
        
        // Execute via JIT or interpreter
        if let Some((_, func)) = self.hot_paths.get(&func) {
            func.call()
        } else {
            self.interpreter.execute(func)
        }
    }
}
```

**Complexity:** Heavy

**Performance Characteristics:**
- Startup: Fast (interpreter)
- Steady state: Near-native (JIT'd hot paths)
- Best for: Long-running applications

**Production Examples:**
- V8 JavaScript engine
- Java HotSpot VM
- PyPy

---

### 4.2 Partial JIT with Specialization

**Concept:**
- JIT compile frequently-accessed code
- Keep less-used code interpreted
- Specialize based on runtime types

**Benefits:**
- Reduced memory footprint
- Faster startup
- Good peak performance

---

## 5. Comparison Matrix

| Feature | Cranelift | Inkwell | DynASM | Wasmtime | Wasmer | Wasmi | Custom |
|---------|-----------|---------|--------|----------|--------|-------|--------|
| **Compilation Speed** | Fast | Slow | Very Fast | Fast | Fast | N/A | Variable |
| **Runtime Speed** | 80-90% | 95-100% | 100% | 90-95% | 90-95% | 5-20% | Variable |
| **Setup Complexity** | Medium | Heavy | Medium | Light | Light | Light | Heavy |
| **IR Construction** | Cranelift IR | LLVM IR | Assembly | WASM | WASM | WASM | Custom |
| **Memory Overhead** | Low-Med | High | Very Low | Med-High | Med-High | Low | Variable |
| **Production Ready** | Yes | Yes | Yes | Yes | Yes | Yes | Depends |
| **Documentation** | Good | Good | Excellent | Excellent | Good | Excellent | N/A |
| **Architecture Support** | All | All | x64/ARM | All | All | All | Variable |
| **Sandboxing** | No | No | No | Yes | Yes | Yes | Depends |
| **Resource Limits** | No | No | No | Yes | Yes | Yes | Depends |
| **Hot Reload** | No | No | No | Yes (AOT) | Yes | No | Possible |

---

## 6. Recommended Approaches by Use Case

### 6.1 Lisp Implementation

**Recommended Stack:**
1. **Language Frontend:** Parse Lisp to custom AST
2. **Intermediate Compilation:** Compile AST to WASM
3. **Runtime:** Wasmtime or Wasmer

**Alternative Approach:**
1. Compile to custom bytecode
2. Interpret with Wasmi (if JIT not needed)
3. JIT compile hot paths with Cranelift

**Rationale:**
- WASM has mature tooling
- Can leverage LLVM backend for production
- Good error handling and debugging
- Sandboxing is free feature

### 6.2 Dynamic Scripting Language

**Recommended Stack:**
- **Fast startup:** Wasmi (pure interpreter)
- **Better performance:** Wasmtime with Cranelift
- **Maximum perf:** Wasmtime with LLVM backend

### 6.3 High-Performance JIT Compiler

**Recommended Stack:**
1. Bytecode interpreter (custom or WASM)
2. Cranelift for initial JIT
3. Optional: Migrate to LLVM for production

### 6.4 Embedded/Constrained Environments

**Recommended Stack:**
- Wasmi (smallest runtime)
- Custom lightweight interpreter
- DynASM for critical paths (if x64)

### 6.5 Production System with Plugins

**Recommended Stack:**
1. Wasmtime for plugin execution
2. Resource limits via Store::new()
3. Custom FFI for host functions
4. Module caching for performance

---

## 7. License Compatibility Summary

| Crate | License | Restrictions |
|-------|---------|--------------|
| Cranelift | Apache-2.0 + LLVM-exception | Must include notice, can use in proprietary |
| Inkwell | Apache-2.0 | Must include notice, can use in proprietary |
| DynASM | MPL-2.0 | Source modifications must be shared |
| Wasmtime | Apache-2.0 + LLVM-exception | Must include notice |
| Wasmer | MIT | Very permissive |
| Wasmi | MIT/Apache-2.0 | Very permissive |

All are compatible with proprietary software when properly attributed.

---

## 8. Resources for Further Learning

### Official Documentation
- Cranelift: https://docs.wasmtime.dev/
- Wasmtime: https://docs.wasmtime.dev/
- Wasmer: https://docs.wasmer.io/
- Wasmi: https://github.com/wasmi-labs/wasmi
- Inkwell: https://thedan64.github.io/inkwell/

### Learning Resources
- Crafting Interpreters (free online book)
- WebAssembly specification: https://webassembly.org/
- LLVM documentation: https://llvm.org/docs/

### Example Projects
- `cargo search lisp` - Find Lisp implementations in Rust
- `rhai` - Embedded Rust scripting language
- `mlua` - Lua bindings for Rust
- `mun` - Statically typed Rust-like language with hot-reloading

---

## Conclusion

For most Lisp and custom language implementations in Rust:

1. **Start with:** Wasmtime + custom AST-to-WASM compiler
   - Good balance of simplicity and performance
   - Mature ecosystem
   - Built-in safety features

2. **Optimize with:** Cranelift JIT for hot paths
   - When WASM overhead becomes limiting
   - Need finer control over code generation

3. **Maximum performance:** LLVM via Inkwell
   - When runtime performance is critical
   - For compiled languages (not scripting)
   - Worth the setup complexity

4. **Embedded systems:** Wasmi interpreter
   - Minimal dependencies
   - No JIT overhead on startup
   - Pure Rust implementation

Choose based on your performance requirements, development time, and target platform constraints.
