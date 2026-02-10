# Lambda Compilation Quick Reference

## File Locations and Responsibilities

| File | Lines | Responsibility |
|------|-------|-----------------|
| `src/compiler/converters.rs` | 1776 | Convert Value → Expr AST, track scopes, compute captures |
| `src/compiler/analysis.rs` | 507 | Analyze free vars, capture usage, mutated vars |
| `src/compiler/capture_resolution.rs` | 293 | Post-process: fix nested capture indices |
| `src/compiler/compile.rs` | 1112 | Expr AST → Bytecode instructions |
| `src/vm/variables.rs` | 180+ | Runtime: LoadGlobal, LoadUpvalue, StoreUpvalue |
| `src/compiler/ast.rs` | 180+ | AST definition with Lambda node |
| `src/vm/closure.rs` | 100+ | Closure value type and execution |
| `src/value.rs` | 250+ | Value enum (includes Closure and Cell) |

---

## Key Data Structures

### Lambda AST Node
```rust
Lambda {
    params: Vec<SymbolId>,                    // Function parameters
    body: Box<Expr>,                          // Function body expression
    captures: Vec<(SymbolId, usize, usize)>,  // (symbol, depth, index)
    locals: Vec<SymbolId>,                    // Locally-defined variables
}
```

### Closure Runtime Value
```rust
pub struct Closure {
    bytecode: Rc<Vec<u8>>,           // Compiled instructions
    arity: Arity,                     // Expected arg count
    env: Rc<Vec<Value>>,              // Captured values at call time
    num_locals: usize,                // params + captures + locals
    num_captures: usize,              // For env layout calculation
    constants: Rc<Vec<Value>>,        // Literal values used in bytecode
}
```

### Variable (Var) AST Node
```rust
Var(SymbolId, usize, usize)  // (symbol, depth, index)
```
- `depth`: How many scopes up the variable is defined
  - 0 = parameter
  - 1 = from outer lambda/scope
  - usize::MAX = global variable
- `index`: Position within that scope level
  - Gets adjusted by `adjust_var_indices()` to account for closure env layout

---

## Compilation Flow

### Step 1: Variable Parsing (converters.rs)
```
Input:  (lambda (x) (+ x y))
        └─ Params: [x]
        └─ Body: (+ x y)

Actions:
1. Push [x] to scope_stack
2. Parse body, track references
   - x → Var(x, 0, 0)    // depth=0: parameter
   - y → Var(y, 1, 1)    // depth=1: from outer scope
3. Call analyze_free_vars(body)
   - Result: {y}
4. Call analyze_capture_usage(body, {y})
   - Result: {y}  // y is actually used
5. Pop scope_stack

Output: Lambda {
    params: [x],
    captures: [(y, 1, 1)],
    locals: [],
    body: <compiled>
}
```

### Step 2: Index Adjustment (converters.rs)
```
Closure env layout: [captures..., params..., locals...]

For lambda (x) (+ x y):
  y (capture[0])  → index = 0
  x (param[0])    → index = 1  (= captures.len() + 0)

So in body:
  y → Var(y, 1, 0)  ◄── index adjusted
  x → Var(x, 0, 1)  ◄── index adjusted
```

### Step 3: Capture Resolution (capture_resolution.rs)
```
For nested lambdas, fix captures that reference other lambda locals.

Before:  (lambda (x) (lambda (y) (+ x y)))
Inner lambda sees x as:
  captures: [(x, 0, 0)]
  
After resolution (if outer has captures):
  captures: [(x, 0, 0 + outer.num_captures)]  // Adjust index
```

### Step 4: Bytecode Compilation (compile.rs)
```
For each Lambda node:
1. Compile body recursively
2. Create Closure template
3. Emit capture loading instructions (if needed)
   - For globals: LoadConst(symbol)
   - For locals: LoadUpvalueRaw(depth, index)
4. Check if captures are mutated via analyze_mutated_vars()
   - If mutated: emit MakeCell to wrap in Rc<RefCell<>>
5. Emit MakeClosure instruction

Bytecode:
  LoadUpvalueRaw(1, 1)  // Load y from outer scope
  MakeClosure(tmpl, 1)   // Create closure with 1 capture
  Return
```

### Step 5: Runtime Execution (vm/)
```
When closure is called:
1. Call instruction handler processes Call stack
2. Create new environment Rc<Vec<Value>>
3. Populate with:
   - Captured values (in order)
   - Argument values (parameters)
   - Pre-allocated Cell values (for locals)
4. Execute bytecode with closure_env available
5. LoadUpvalue(depth, index) accesses closure_env[index]
```

---

## Critical Functions

### converters.rs

**`convert_lambda()` - Main lambda converter**
```rust
fn convert_lambda(
    list: &[Value],
    symbols: &mut SymbolTable,
    scope_stack: &mut Vec<Vec<SymbolId>>,
) -> Result<Expr, String>
```
Lines: 405-514
- Entry point for lambda conversion
- Manages scope_stack pushing/popping
- Calls analyze_free_vars() and adjust_var_indices()

**`pre_register_defines()` - Enable recursion**
```rust
fn pre_register_defines(
    body_vals: &[Value],
    symbols: &SymbolTable,
    scope_stack: &mut Vec<Vec<SymbolId>>,
)
```
Lines: 41-74
- Pre-scans body for all `define` statements
- Registers them in scope_stack before parsing values
- Enables `(define f (lambda () (f)))`

**`adjust_var_indices()` - Fix closure env layout**
```rust
fn adjust_var_indices(
    expr: &mut Expr,
    captures: &[(SymbolId, usize, usize)],
    params: &[SymbolId],
    locals: &[SymbolId],
)
```
Lines: 76-202
- CRITICAL: Maps var indices to closure env positions
- Does NOT recurse into nested lambdas
- Handles Expr::Var and Expr::Set nodes

---

### analysis.rs

**`analyze_free_vars()` - Find variables to capture**
```rust
pub fn analyze_free_vars(
    expr: &Expr,
    local_bindings: &HashSet<SymbolId>,
) -> HashSet<SymbolId>
```
Lines: 175-332
- Identifies unbound variables
- Tracks "growing bindings" for sequential defines
- Filters through lambda parameter boundaries

**`analyze_capture_usage()` - Dead capture elimination**
```rust
pub fn analyze_capture_usage(
    expr: &Expr,
    local_bindings: &HashSet<SymbolId>,
    candidates: &HashSet<SymbolId>,
) -> HashSet<SymbolId>
```
Lines: 8-171
- Filters captures to only those actually referenced
- Eliminates unused free variables

**`analyze_mutated_vars()` - Find set! targets**
```rust
pub fn analyze_mutated_vars(expr: &Expr) -> HashSet<SymbolId>
```
Lines: 402-506
- Finds all variables with `set!` applied
- Used to determine which captures need cell boxing

---

### capture_resolution.rs

**`resolve_captures()` - Entry point**
```rust
pub fn resolve_captures(expr: &mut Expr)
```
Lines: 16-19
- Entry point for post-processing pass
- Walks AST maintaining env_stack

**`resolve_in_expr()` - Walk AST**
```rust
fn resolve_in_expr(
    expr: &mut Expr,
    env_stack: &mut Vec<LambdaEnvInfo>,
)
```
Lines: 21-170
- Recursively processes all expression types
- Builds LambdaEnvInfo for each lambda
- Calls remap_body_vars() before recursing into body

**`remap_body_vars()` - Fix variable references**
```rust
fn remap_body_vars(expr: &mut Expr, env_info: &LambdaEnvInfo)
```
Lines: 178-292
- Remaps Var/Set nodes referencing outer scopes
- Uses symbol_to_env_index to find correct env position
- Does NOT recurse into nested lambdas

---

### compile.rs

**`compile_expr()` - Lambda pattern**
```rust
match expr {
    Expr::Lambda { params, body, captures, locals } => {
        // Lines 229-322
    }
}
```
Key steps:
1. Create new Compiler with lambda context
2. Compile body recursively
3. Create Closure template
4. Emit capture loading instructions
5. Check mutated vars and wrap with MakeCell if needed
6. Emit MakeClosure instruction

**Pre-declaration of defines**
```rust
// Lines 130-165
let defines = Self::collect_defines(expr);
for sym_id in defines {
    // Emit Nil + StoreUpvalue to pre-declare
}
```
Enables mutual recursion in lambda bodies.

---

### vm/variables.rs

**`handle_load_upvalue()` - Load from closure env**
```rust
pub fn handle_load_upvalue(
    vm: &mut VM,
    bytecode: &[u8],
    ip: &mut usize,
    closure_env: Option<&Rc<Vec<Value>>>,
) -> Result<(), String>
```
Lines: 98-141
- Loads from closure_env[index]
- Transparently unwraps Cell values
- Resolves Symbol values via globals

**`handle_load_global()` - Load from globals**
```rust
pub fn handle_load_global(
    vm: &mut VM,
    bytecode: &[u8],
    ip: &mut usize,
    constants: &[Value],
) -> Result<(), String>
```
Lines: 4-46
- Looks up symbol in scope_stack first
- Falls back to globals map
- Returns "Undefined global variable" error if not found

---

## Common Patterns

### Pattern 1: Simple Lambda with Capture
```scheme
(define x 10)
(define f (lambda (y) (+ x y)))
(f 5)  ; Returns 15
```

Compilation:
- x → global variable (captured as symbol)
- f → lambda with captures: [(x, 1, usize::MAX)]
- Body contains: (+ x y)
  - x → Var(x, 1, usize::MAX)
  - y → Var(y, 0, 0)

---

### Pattern 2: Nested Lambda
```scheme
(define add (lambda (x) (lambda (y) (+ x y))))
((add 3) 5)  ; Returns 8
```

Outer lambda compilation:
- params: [x]
- captures: []
- body: inner lambda

Inner lambda compilation:
- params: [y]
- captures: [(x, 0, 0)]  // x from parent lambda
- body: (+ x y)

At runtime:
- Outer lambda creates closure with x=3
- Inner lambda captures outer closure_env
- Inner lambda creates closure with x=3 (captured), y=5 (param)

---

### Pattern 3: Mutable Capture (Phase 4)
```scheme
(define counter 0)
(define inc (lambda () (set! counter (+ counter 1))))
(inc) (inc) counter  ; Returns 2
```

Compilation:
- counter → global variable (symbol capture)
- analyze_mutated_vars() finds set! on counter
- counter wrapped with MakeCell
- StoreUpvalue updates cell contents

---

### Pattern 4: Local Define in Lambda
```scheme
(define f (lambda () (define x 10) (+ x 5)))
(f)  ; Returns 15
```

Compilation:
- Lambda locals: [x]
- Pre-declare x with StoreUpvalue to closure_env[1]
- Body references x → Var(x, 0, 1)
- x stored/loaded via LoadUpvalue

Environment layout:
```
[0]: (empty - no captures)
[1]: x (local variable)
```

---

## Error Sources

### 1. "Undefined global variable"
**Source**: `vm/variables.rs` line 40
**Cause**: LoadGlobal tried to look up symbol that doesn't exist
**Solutions**:
- Define variable before lambda
- Check spelling
- Ensure module-qualified names are correct

### 2. Index out of bounds in LoadUpvalue
**Source**: `vm/variables.rs` line 131
**Cause**: Captured var index exceeds closure_env size
**Solutions**:
- Check adjust_var_indices() calculations
- Verify closure env layout: [captures..., params..., locals...]

### 3. Stack underflow
**Source**: Various VM handlers
**Cause**: Bytecode tried to pop from empty stack
**Solutions**:
- Check that all expressions produce values
- Verify tail call optimization isn't over-aggressive

---

## Testing

### Key Test Files
- `tests/integration/closures_and_lambdas.rs` - Main closure tests
- `tests/unittests/scope_compilation.rs` - Scope handling tests
- `examples/recursive-define.lisp` - Phase 4 example
- `examples/closures-phase4.lisp` - Comprehensive Phase 4 tests

### Running Tests
```bash
# Run all tests
cargo test

# Run closure-specific tests
cargo test closure

# Run with output
cargo test closure -- --nocapture

# Run specific example
cargo run --example recursive-define.lisp
```

---

## Recent Changes (Feb 9, 2026)

1. **e9388cf**: Removed dead scope instructions
   - Cleaned up LoadScoped, StoreScoped, etc.
   - Simplified Phase 4 implementation

2. **3fe9033**: Phase 4 — shared mutable captures
   - Added Value::Cell for mutable captures
   - Made MakeCell idempotent
   - Updated capture resolution for locals

3. **47427c0**: Fixed self-recursive define
   - Pre-register defines before parsing
   - Enables self-references in lambda bodies

---

## Performance Considerations

1. **Scope stack operations**: O(1) per variable lookup (via depth)
2. **Free variable analysis**: O(n) where n = AST nodes
3. **Capture resolution**: O(n * m) where n = lambdas, m = avg captures
4. **Runtime closure creation**: O(k) where k = captures (copy values)
5. **Variable access**: O(1) (direct array indexing)

---

## Future Improvements

1. **Better capture filtering**: Reduce captures for partial application
2. **Escape analysis**: Determine if closures can be stack-allocated
3. **Specialization**: Generate specialized code for capture-free lambdas
4. **Tail call optimization**: Extend to more patterns
5. **JIT compilation**: Compile hot closures to native code

