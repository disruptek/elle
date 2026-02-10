# WASM Migration Risk Analysis: Elle Lisp Compiler

## Executive Summary

Converting Elle's bytecode VM to target WebAssembly (WASM) instead of custom bytecode is a **MODERATE-TO-HIGH complexity migration** with significant architectural challenges. Success depends on how you handle value representation, FFI bindings, and closure environments.

---

## Current Architecture Overview

### Bytecode Format
- **Custom stack-based bytecode** with ~45 instruction types
- **Constants pool** for storing literals
- **Inline caches** for function lookups
- **Instructions:** LoadConst, LoadLocal, Call, TailCall, Cons, MakeClosure, exception handling, etc.

### Value Type
```rust
pub enum Value {
    Nil, Bool(bool), Int(i64), Float(f64), Symbol(SymbolId),
    Keyword(SymbolId), String(Rc<str>), Cons(Rc<Cons>),
    Vector(Rc<Vec<Value>>), Table(Rc<RefCell<BTreeMap<TableKey, Value>>>),
    Struct(Rc<BTreeMap<TableKey, Value>>), Closure(Rc<Closure>),
    NativeFn(NativeFn), LibHandle(LibHandle), CHandle(CHandle),
    Exception(Rc<Exception>), Condition(Rc<Condition>)
}
```

### VM Execution
- Stack-based with manual IP (instruction pointer) management
- Exception handlers stack
- Call frame tracking for stack traces
- Closure environments as vectors of Values
- FFI subsystem for C library bindings

---

## Major Risks & Challenges

### 🔴 CRITICAL RISKS

#### 1. **Value Representation Mismatch**
**Risk Level:** CRITICAL

**The Problem:**
- Elle uses `enum Value` with complex variants (Rc-wrapped types, mutable Tables)
- WASM has limited native types: i32, i64, f32, f64
- You CANNOT directly represent Elle's Value enum in WASM native types

**Mitigation Options:**
1. **Tagged representation (encoding):** Use i64 with tag bits
   - Bits 0-3: tag (0=nil, 1=bool, 2=int, 3=float, 4=symbol, etc.)
   - Bits 4-63: payload or index into external table
   - **Cost:** Encoding/decoding overhead on every value operation
   - **Complexity:** Moderate

2. **External value table:** Keep Values in Rust, pass indices (i32/i64) through WASM
   - WASM operates on indices only
   - Rust side dereferences for actual operations
   - **Cost:** Table lookup on every operation
   - **Complexity:** High (essentially a VM-within-VM)

3. **Limit to primitive types only:** Only support Int, Float, Bool in WASM
   - Lisp functions return early from WASM to Rust for complex types
   - **Cost:** Massive loss of expressivity
   - **Complexity:** Very high (architectural split)

**Recommendation:** Option 1 (tagged i64) is most practical but has 20-50% performance overhead.

---

#### 2. **Closure Environment Handling**
**Risk Level:** CRITICAL

**The Problem:**
```rust
pub struct Closure {
    pub bytecode: Rc<Vec<u8>>,
    pub env: Rc<Vec<Value>>,  // ← Captured variables
    pub constants: Rc<Vec<Value>>,
}
```

- WASM cannot directly reference Rust Rc<Vec<Value>>
- Closure must capture environment at compile time
- Current system allows dynamic closure creation

**Current Flow:**
```rust
Instruction::MakeClosure => {
    // Create Closure { bytecode, env: captured_vars, constants }
    // env contains captured upvalues from outer scope
}
```

**WASM Challenge:**
- WASM modules are static after compilation
- Cannot embed runtime-created closures
- Environment variables must be passed differently

**Mitigation Options:**
1. **Environment tables:** Store all closures' environments in a global Rust table
   - Each closure gets an index to its environment
   - WASM passes environment index around
   - **Cost:** Indirection, GC complexity
   - **Complexity:** High

2. **Inline environments:** For simple cases, capture values at compile time
   - Only works for known constants
   - Breaks runtime-defined functions
   - **Complexity:** High

**Recommendation:** Use environment tables with reference counting for GC.

---

#### 3. **FFI Interoperability**
**Risk Level:** CRITICAL

**The Problem:**
```rust
pub enum Value {
    LibHandle(LibHandle),  // Dynamically loaded C library
    CHandle(CHandle),      // Opaque pointer to C data
    NativeFn(NativeFn),    // Rust function pointer
}
```

- WASM cannot call C functions directly
- Rust FFI layer must bridge between WASM and C
- Current `ffi` subsystem is tightly coupled to VM

**Current System:**
- `ffi::FFISubsystem` manages library loading, symbol lookup
- `NativeFn` is Rust function pointer
- `Call` instruction can dispatch to either closures or native functions

**WASM Challenge:**
- WASM → Rust callback overhead on every FFI call
- Cannot use function pointers in WASM directly
- Memory layout compatibility issues (calling conventions)

**Mitigation:**
1. **WASM callback functions:** Import host functions, call via WASM imports
   - Define WASM imports for all native functions
   - Rust implements imports
   - **Cost:** Pre-define all FFI functions (loses dynamic loading)
   - **Complexity:** High

2. **Keep FFI in Rust:** WASM-only for Lisp, native fns in Rust
   - WASM compiles user-defined Lisp functions
   - Built-in natives remain in Rust
   - **Cost:** Two execution environments
   - **Complexity:** Very high (hard to maintain)

3. **Thunk layer:** Generate WASM thunks for native calls
   - WASM thunk → Rust callback → C function
   - **Cost:** Extra indirection on every native call
   - **Complexity:** Moderate

**Recommendation:** Option 1 (callback imports) but requires pre-registering all FFI functions.

---

### 🟡 HIGH RISKS

#### 4. **Tail Call Optimization (TCO)**
**Risk Level:** HIGH

**The Problem:**
```rust
Instruction::TailCall => {
    // Don't increment call_depth, return directly
    return self.execute_bytecode(...);
}
```

- Current bytecode supports explicit tail calls
- WASM lacks native tail call support (WebAssembly spec limitation)
- WASM indirect calls can be optimized in some engines but not guaranteed

**Impact:**
- Recursion patterns that rely on TCO may stack overflow
- Common in Lisp (loop-via-recursion)
- Performance penalty from lost TCO

**Mitigation:**
1. **Trampoline pattern:** Convert to explicit loop
   - Return function/args, caller re-invokes
   - **Cost:** Extra object allocation per call
   - **Complexity:** Moderate

2. **Use Wasmtime's tail.call proposal:** Enable experimental feature
   - Only works on LLVM codegen, not all engines
   - **Cost:** Portability loss
   - **Complexity:** Low but risky

3. **Rewrite recursive functions:** Convert to iteration
   - Manual stack management
   - **Cost:** Major change to user code
   - **Complexity:** Very high

**Recommendation:** Trampoline pattern is most portable.

---

#### 5. **Exception Handling System**
**Risk Level:** HIGH

**Current System:**
```rust
pub struct ExceptionHandler {
    pub handler_offset: i16,
    pub finally_offset: Option<i16>,
    pub stack_depth: usize,
}

Instruction::PushHandler
Instruction::PopHandler
Instruction::CheckException
Instruction::MatchException
```

- Common Lisp condition system with handlers/restarts
- Manual stack management with bytecode instructions
- Exception unwinding with finally blocks

**WASM Challenge:**
- WASM exception handling (exception-handling proposal) is still experimental
- No native support for condition system (handlers, restarts)
- Stack unwinding must be explicit

**Mitigation:**
1. **Use wasm-bindgen exceptions:** Throw JS Error objects
   - Limited to simple error data
   - Loses condition system features
   - **Complexity:** Moderate

2. **Manual unwinding:** Explicit control flow instead of exceptions
   - Check error codes at each step
   - Convert exception handlers to if/else
   - **Cost:** Significant performance hit
   - **Complexity:** High

3. **Keep condition system in Rust:** Only catch/throw at boundary
   - WASM can't use full condition system
   - Built-in conditions in Rust only
   - **Cost:** Splits execution between WASM/Rust
   - **Complexity:** High

**Recommendation:** Use experimental WASM exceptions or manual unwinding.

---

#### 6. **Mutable State Management**
**Risk Level:** HIGH

**The Problem:**
```rust
Value::Table(Rc<RefCell<BTreeMap<TableKey, Value>>>)  // Mutable
```

- Tables are mutable, shared state
- WASM memory is shared but managing RefCell/Rc across boundary is complex
- Multiple WASM functions might reference same table

**Impact:**
- Synchronization issues between WASM and Rust
- GC/reference counting complexity
- Memory safety at boundary

**Mitigation:**
1. **Use WASM linear memory:** Tables stored in WASM memory
   - Need custom allocator
   - Manage RefCell semantics manually
   - **Complexity:** Very high

2. **Keep mutable state in Rust:** WASM only manipulates indices
   - WASM calls Rust for mutations
   - **Cost:** Every table operation crosses boundary
   - **Complexity:** High

**Recommendation:** Keep mutable tables in Rust, pass indices to WASM.

---

### 🟠 MEDIUM RISKS

#### 7. **Inline Caches**
**Risk Level:** MEDIUM

**Current System:**
```rust
pub struct CacheEntry {
    pub symbol_id: u32,
    pub cached_value: Option<Value>,
}
pub struct Bytecode {
    pub inline_caches: HashMap<usize, CacheEntry>,
}
```

- Optimization for symbol lookups
- Must be mutable (cache misses trigger updates)
- Keyed by instruction pointer position

**WASM Challenge:**
- Inline caches are implementation detail of custom bytecode
- WASM doesn't have this optimization mechanism
- Could re-implement but adds complexity

**Mitigation:**
1. **Skip inline caches:** Rely on WASM engine's optimization
   - Assume WASM JIT will do better job
   - **Cost:** Potential performance regression
   - **Complexity:** Low

2. **Implement custom caching in WASM:** Explicit cache management
   - **Cost:** Code size increase
   - **Complexity:** Moderate

**Recommendation:** Skip for now, WASM JIT should handle it.

---

#### 8. **Debugging & Stack Traces**
**Risk Level:** MEDIUM

**Current System:**
```rust
pub struct CallFrame {
    pub name: String,
    pub ip: usize,
}

pub fn format_stack_trace(&self) -> String {
    // Formats call stack with function names and IPs
}
```

- Manual call frame tracking
- IP-based stack traces
- Function name mapping

**WASM Challenge:**
- WASM call stack is implicit, harder to customize
- Source mapping requires WASM debugging support
- Name indices in constants, not native to WASM

**Mitigation:**
1. **Use WASM source maps:** Generate .map files
   - Requires build system integration
   - Some platforms don't support
   - **Complexity:** Moderate

2. **Store call frames in WASM memory:** Explicit tracking
   - Similar to current approach but in WASM memory
   - **Complexity:** Moderate

**Recommendation:** Implement explicit call frame tracking in WASM.

---

#### 9. **Performance Uncertainty**
**Risk Level:** MEDIUM

**The Unknowns:**
- Tagged value representation: 20-50% overhead
- WASM JIT vs custom bytecode JIT: ???
- Cranelift backend vs LLVM codegen: 80-90% native speed
- Boundary crossing overhead: Unmeasured

**Impact:**
- Can't predict final performance
- Might be slower than current bytecode VM
- Regression risk

**Mitigation:**
1. **Prototype early:** Build MVP, benchmark before full migration
2. **Profile ruthlessly:** Measure overhead of each component
3. **Have fallback plan:** Keep old bytecode VM working

**Recommendation:** Build benchmark suite first, test incrementally.

---

## Summary Table

| Risk | Severity | Impact | Mitigation Complexity |
|------|----------|--------|----------------------|
| Value representation | CRITICAL | Can't represent Value enum natively | HIGH |
| Closure environments | CRITICAL | Runtime closures won't work | HIGH |
| FFI interop | CRITICAL | Can't call C functions dynamically | HIGH |
| Tail calls | HIGH | Recursion stack overflow risk | MODERATE |
| Exception handling | HIGH | Condition system won't work | HIGH |
| Mutable state | HIGH | RefCell semantics lost | HIGH |
| Inline caches | MEDIUM | Performance regression | LOW |
| Debugging | MEDIUM | Stack traces broken | MODERATE |
| Performance | MEDIUM | Unpredictable overhead | HIGH |

---

## Recommended Approach: Hybrid Execution

Given these risks, **don't convert the entire compiler to WASM**. Instead:

1. **Keep current bytecode VM as baseline**
2. **Implement hybrid execution:**
   - Simple arithmetic/control flow → WASM (with Cranelift JIT)
   - Complex operations (closures, FFI, mutable state) → Stay in Rust bytecode VM
3. **Phase 1:** Export Lisp functions that only use primitives to WASM
4. **Phase 2:** Add closure support via environment tables
5. **Phase 3:** Add FFI via callback imports

This avoids the architectural cliff of full WASM migration while still getting benefits of:
- Fast compilation via Cranelift for hot paths
- Portability via WASM for simple code
- Gradual migration path

---

## Alternative: Custom VM Optimization

Instead of WASM, consider:
1. **Optimize current bytecode VM** (it's already pretty good)
2. **Add JIT with Cranelift** for hot functions
3. **Keep custom bytecode format** (proven architecture)
4. **Add profiling/tiering** (interpreter → JIT)

This avoids WASM complexity entirely while getting similar performance gains.

---

## Conclusion

WASM migration is **technically feasible but high-risk**. The gap between Elle's semantics (runtime closures, complex Value types, FFI) and WASM's limitations (static modules, 4 primitive types, no native exceptions) is substantial.

**Recommendation:** Start with the hybrid approach above. Full WASM migration should only happen if:
- Portability (browser/sandboxing) is critical
- Performance proves acceptable
- Team is willing to rewrite major subsystems

