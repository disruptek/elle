# Lambda/Closure Compilation in Elle4 - Comprehensive Summary

## Overview

Lambda compilation in Elle4 involves a sophisticated multi-stage process that handles parameter binding, variable capture, and local variable definition within closure bodies. The system has evolved through multiple phases, with Phase 4 (shared mutable captures via cell boxing) being the most recent major feature.

## Key Files and Their Roles

### 1. AST Definition: `src/compiler/ast.rs`
**Core data structure for compiled expressions**

```rust
Lambda {
    params: Vec<SymbolId>,              // Lambda parameters
    body: Box<Expr>,                    // Lambda body
    captures: Vec<(SymbolId, usize, usize)>, // (sym_id, depth, index)
    locals: Vec<SymbolId>,              // NEW in Phase 4: locally-defined variables
}
```

Key points:
- `captures`: Variables from outer scopes that lambda references
- `locals`: Variables defined inside the lambda body via `define`
- `depth` in captures: How many scopes up the variable is defined
- `index` in captures: Position within that scope level

---

## Two-Stage Compilation Pipeline

### Stage 1: Converter (Value → Expr AST)

**File: `src/compiler/converters.rs` (1776 lines)**

This converts Scheme s-expressions to an intermediate AST, handling the critical scope tracking.

#### Key Functions:

1. **`convert_lambda()`** (lines 405-514)
   - Parses lambda parameters
   - Pre-registers define names to enable recursion
   - Analyzes free variables
   - Computes captures with scope resolution
   - Adjusts variable indices for closure environment layout

   Process:
   ```
   1. Push lambda params to scope_stack
   2. Pre-scan body for all define names (mutually-recursive)
   3. Process body expressions
   4. Analyze free vars (vars referenced but not bound locally)
   5. Filter out dead captures (unused free vars)
   6. Adjust indices to account for closure env layout: [captures..., params..., locals...]
   7. Pop scope_stack
   ```

2. **`pre_register_defines()`** (lines 41-74)
   - Recursively walks body to find all `define` expressions
   - Registers them in scope_stack BEFORE parsing values
   - Enables self-recursion: `(lambda () (define f (lambda () (f)))`
   - Also handles recursion in nested `begin` blocks

3. **`adjust_var_indices()`** (lines 76-202)
   - CRITICAL: Maps variable indices to closure environment layout
   - Closure environment at runtime: `[capture_0, capture_1, ..., param_0, param_1, ..., local_0, ...]`
   - Remaps all `Var` and `Set` nodes to use correct environment indices
   - Does NOT recurse into nested lambdas (they adjust themselves)

#### Closure Environment Layout:
```
Index 0..captures.len()-1:      Captured variables
Index captures.len()..captures.len()+params.len()-1:  Parameters
Index captures.len()+params.len()...:                  Locally-defined variables
```

---

### Stage 2: Compiler (Expr AST → Bytecode)

**File: `src/compiler/compile.rs` (1112 lines)**

Converts AST to bytecode instructions for the VM.

#### Lambda Compilation (`compile_expr` for `Expr::Lambda`, lines 229-322):

1. **Setup**: Creates new compiler context with:
   - `lambda_locals`: Variables defined in lambda body
   - `lambda_captures_len`: Number of captures
   - `lambda_params_len`: Number of parameters

2. **Body compilation**: Compiles body expressions directly (no scope stack for lambdas in Phase 4)

3. **Closure creation**:
   - **No captures, no locals**: Load closure template directly
   - **Has locals but no captures**: Emit MakeClosure (0 captures) for closure env allocation
   - **Has captures**: 
     - Emit captured values onto stack (in order)
     - For global captures: emit symbol constant
     - For local captures: use `LoadUpvalueRaw` to preserve cell wrappers
     - Check if captured variable is mutated; if so, wrap with `MakeCell`
     - Emit `MakeClosure` instruction with capture count

#### Define Handling in Lambda Bodies:

When `Expr::Define` appears in a lambda:
```rust
// Pre-declare the variable in the closure environment
emit(Instruction::Nil);
emit(Instruction::StoreUpvalue);
emit_byte(1);  // depth = 1 (current closure)
emit_byte(env_idx);  // Index in closure env
```

The variable is stored directly in the closure environment (immutable Rc<Vec<Value>>) at a position calculated as:
```
env_idx = captures.len() + params.len() + local_position
```

---

## Symbol Resolution and Analysis

### File: `src/compiler/analysis.rs` (507 lines)

Three critical analysis functions:

#### 1. `analyze_free_vars()` (lines 175-332)
- Identifies variables referenced but not bound locally
- Respects scope boundaries (params, lets, lambdas)
- In `Begin`/`Block`: tracks "growing bindings" so later expressions see earlier defines
- For nested lambdas: only propagates free vars that are also free in outer scope

Key logic:
```rust
Expr::Begin(exprs) => {
    let mut growing_bindings = local_bindings.clone();
    for e in exprs {
        if let Expr::Define { name, value } = e {
            free_vars.extend(analyze_free_vars(value, &growing_bindings));
            growing_bindings.insert(*name);  // Now visible to subsequent exprs
        } else {
            free_vars.extend(analyze_free_vars(e, &growing_bindings));
        }
    }
}
```

#### 2. `analyze_capture_usage()` (lines 8-171)
- Filters captures to only those actually used in the body
- Eliminates "dead captures" that were identified as free but never referenced
- Also handles nested lambda recursion

#### 3. `analyze_mutated_vars()` (lines 402-506)
- Finds which variables have `set!` applied to them
- Used to determine which captures need cell boxing for Phase 4
- Does NOT recurse into nested lambdas (each analyzes its own mutations)

---

## Capture Resolution Post-Processing

**File: `src/compiler/capture_resolution.rs` (293 lines)**

Two-phase post-processing pass after AST construction:

### Phase A: Fix Capture Indices
When a nested lambda captures from an outer lambda:
- The capture index was initially relative to the outer lambda's scope_stack
- Need to adjust by `outer_lambda.num_captures` to account for runtime layout

```rust
for (_sym, depth, index) in captures.iter_mut() {
    if *index != usize::MAX && env_stack.is_empty() {
        continue;  // Global variable
    }
    if *depth < env_stack.len() {
        *index += env_stack[*depth].num_captures;  // Adjust for captures offset
    }
}
```

### Phase B: Build Environment Info
For each lambda, build a map of where each symbol lives:
```rust
symbol_to_env_index: {
    captures[0..n] → 0..n
    params[0..m] → n..(n+m)
    locals[0..k] → (n+m)..(n+m+k)
}
```

### Phase C: Remap Body Variables
Variables referencing outer scopes (depth > 0) are remapped to use capture indices:
```rust
Expr::Var(sym, depth=1, index=2) →
    Look up in parent lambda's symbol_to_env_index
    Remap index to capture slot position
```

---

## Scope Handling

### File: `src/compiler/scope.rs` (15 lines)

Simple enum defining scope types:
```rust
pub enum ScopeType {
    Global,    // Top-level defines
    Function,  // Lambda parameters and captures
    Block,     // let, begin blocks
    Loop,      // while, for loops
    Let,       // Let-binding specific
}
```

### Scope Stack During Conversion

While converting expressions, `scope_stack` tracks all active scopes:
```
scope_stack = [
    [top-level defines],
    [outer_lambda_params, outer_lambda_locals],
    [inner_lambda_params, inner_lambda_locals],
]
```

When a variable is referenced:
1. Search from the end (innermost scope) backwards
2. Record (symbol, depth, index) where depth = levels from current scope

---

## Variable Compilation: Locals vs Globals vs Captures

### In Lambda Bodies

**Local Variables** (defined via `define`):
- Compiled to `StoreUpvalue` during `define`
- Accessed via `LoadUpvalue` in body
- Stored in closure environment at index: `captures.len() + params.len() + local_index`

**Parameters**:
- Stored in closure environment at index: `captures.len() + param_index`
- Accessed via `LoadUpvalue`

**Captured Variables** (from outer scopes):
- Stored in closure environment at index: `capture_index`
- Loaded via `LoadUpvalue` with transparent cell unwrapping
- If mutated in body, wrapped in `Value::Cell` for shared mutable state

**Global Variables**:
- Captured as symbol constants
- Resolved at runtime via `LoadGlobal` → symbol table lookup

---

## Phase 4: Shared Mutable Captures via Cell Boxing

**Recent changes**: Commits from Feb 9, 2026

### Problem Addressed
Without cell boxing:
```scheme
(define x 10)
(define f (lambda ()
  (define y x)
  (lambda () (set! y 20))))  ; Error: y not accessible in nested lambda
```

Nested closures needed to share mutable state from parent closures.

### Solution: Value::Cell

Added new value type:
```rust
pub enum Value {
    // ... existing variants
    Cell(Rc<RefCell<Box<Value>>>),  // Shared mutable container
}
```

### Implementation

1. **Compiler**: When emitting captures that are mutated, wrap with `MakeCell` instruction
2. **VM Call/TailCall**: Pre-allocate cells for locally-defined variables
3. **LoadUpvalue/StoreUpvalue**: Transparently handle cell wrapping
4. **Phase 4 Bug Fix**: Made `MakeCell` idempotent to prevent double-wrapping

### Environment Layout Changes

Phase 4 extends the environment to include locals:
```
[captures..., params..., locals...]
     ↑
     All locals are pre-allocated as cells by Call instruction
```

---

## Error Handling: Undefined Global Variables

**File: `src/vm/variables.rs` (lines 4-46)**

When a lambda references a global variable:

1. **Compilation**: Captured as symbol constant
2. **Runtime**: `LoadUpvalue` receives symbol, looks it up in globals
3. **Error**: If symbol not in globals, emit "Undefined global variable" error

```rust
pub fn handle_load_global(vm: &mut VM, ...) -> Result<(), String> {
    if let Value::Symbol(sym_id) = constants[idx] {
        if let Some(val) = vm.scope_stack.get(sym_id.0) { return Ok(()); }
        if let Some(val) = vm.globals.get(&sym_id.0) { return Ok(()); }
        return Err(format!("Undefined global variable: {:?}", sym_id));
    }
}
```

### Common Sources of This Error

1. **Missing global definition**: `(lambda () x)` where x is not defined
2. **Incorrect scoping**: Variable defined after lambda creation
3. **Module-qualified names**: `other-module:var` where var doesn't exist

---

## Recent Changes and Issues

### Recent Commits (Last 2 weeks)

1. **e9388cf** (Feb 9): Refactor remove dead scope instructions
   - Cleaned up obsolete `LoadScoped`, `StoreScoped`, etc. instructions

2. **3fe9033** (Feb 9): Phase 4 — shared mutable captures via cell boxing
   - Added `Value::Cell` variant
   - Fixed double-wrapping with idempotent `MakeCell`
   - Updated capture resolution for locals

3. **47427c0** (Feb 9): Fix self-recursive define in lambda bodies
   - Pre-register defines before parsing nested lambdas
   - Enables `(lambda () (define f (lambda () (f))))`

4. **7d5bba6** (Feb 8): WIP: Investigate set! in lambda bodies
   - Documents architectural decisions about mutable captures

### Known Issues

From ARCHITECTURAL_ISSUE.md:

**Problem**: Phases 2/3 used `StoreGlobal` for local defines, breaking semantics
**Solution**: Phase 4+ uses proper `StoreUpvalue` and cell-based storage
**Status**: Fixed by Phase 4 implementation

---

## Testing Coverage

### Main Test Files

1. **`tests/integration/closures_and_lambdas.rs`**
   - Tests basic lambda creation and calls
   - Tests parameter passing
   - Tests closure capture
   - Tests recursive lambdas
   - Tests set! in lambda bodies

2. **`tests/unittests/scope_compilation.rs`**
   - Unit tests for scope handling

### Example Files

1. **`examples/recursive-define.lisp`** (22 lines)
   - Demonstrates self-recursive and mutual-recursive defines in lambdas
   - Tests Phase 4 locally-defined variable support

2. **`examples/closures-phase4.lisp`** (262 lines)
   - Comprehensive Phase 4 examples
   - Tests mutable captures via cells

---

## Summary of Key Functions by Category

### Converter (converters.rs)

| Function | Lines | Purpose |
|----------|-------|---------|
| `convert_lambda()` | 405-514 | Main lambda conversion |
| `pre_register_defines()` | 41-74 | Enable recursion |
| `adjust_var_indices()` | 76-202 | Fix closure env layout |
| `convert_let()` | 519-610 | Transform let to lambda |

### Analyzer (analysis.rs)

| Function | Lines | Purpose |
|----------|-------|---------|
| `analyze_free_vars()` | 175-332 | Find vars to capture |
| `analyze_capture_usage()` | 8-171 | Dead capture elimination |
| `analyze_mutated_vars()` | 402-506 | Find vars needing cells |

### Compiler (compile.rs)

| Section | Lines | Purpose |
|---------|-------|---------|
| Lambda compilation | 229-322 | Generate bytecode |
| Define handling | 130-165 | Pre-declare variables |
| Var compilation | 88-97 | Load captured vars |

### Capture Resolution (capture_resolution.rs)

| Function | Lines | Purpose |
|----------|-------|---------|
| `resolve_captures()` | 16-19 | Entry point |
| `resolve_in_expr()` | 21-170 | Walk AST phases |
| `remap_body_vars()` | 178-292 | Fix indices |

### VM (vm/variables.rs)

| Function | Lines | Purpose |
|----------|-------|---------|
| `handle_load_global()` | 4-46 | LoadGlobal instruction |
| `handle_load_upvalue()` | 98-141 | LoadUpvalue with cells |
| `handle_store_global()` | 48-86 | StoreGlobal instruction |

---

## Key Design Decisions

1. **Two-stage compilation**: Conversion (Value→AST) separate from code generation (AST→bytecode)
2. **Scope stack tracking**: During conversion, not at runtime (eliminated in Phase 4)
3. **Closure environment as immutable array**: Parameters and captures fixed at lambda creation
4. **Locals as closure environment slots**: Enables cell boxing for mutations
5. **Cell boxing for mutability**: Transparent wrapping/unwrapping at load/store points
6. **Symbol capture for globals**: Globals stored as symbols, resolved at runtime
7. **Deterministic capture ordering**: Captured vars sorted by SymbolId for consistent env layout

---

## Integration Points

### Where Lambda Compilation Fits

1. **Parser** → **Converter** (converters.rs)
   - Converts Scheme syntax to Expr AST

2. **Converter** → **Analyzer** (analysis.rs)
   - Analyzes captures and free variables

3. **Analyzer** → **Compiler** (compile.rs)
   - Generates bytecode from AST

4. **Compiler** → **VM** (vm/variables.rs, vm/closure.rs)
   - Executes bytecode with proper variable binding

5. **Capture Resolution** (capture_resolution.rs)
   - Post-processing pass after conversion, before compilation

