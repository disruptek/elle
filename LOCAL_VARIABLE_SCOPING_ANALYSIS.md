# Local Variable Scoping Architecture Analysis

## Executive Summary

The Elle compiler currently supports **closures with captured variables** (upvalues) and **local variables within lambda expressions**, but **recursive functions are not fully supported** for lambdas without workarounds. This document outlines the current architecture and identifies where local variable scoping needs enhancement for recursive functions.

---

## 1. Current Compiler Architecture

### 1.1 Compiler Structure

**File:** `src/compiler/compile.rs`

The compiler is organized around:

```rust
struct Compiler {
    bytecode: Bytecode,
    symbols: HashMap<SymbolId, usize>,  // Currently unused (#[allow(dead_code)])
}

impl Compiler {
    fn compile_expr(&mut self, expr: &Expr, tail: bool)
}
```

**Key observations:**
- Single-pass compilation
- No symbol table management in the struct (currently empty HashMap)
- No scope tracking mechanism
- No depth/frame information for nested functions

### 1.2 Symbol Resolution at Line 689

**File:** `src/compiler/compile.rs`, lines 681-689

```rust
pub fn value_to_expr(value: &Value, symbols: &mut SymbolTable) -> Result<Expr, String> {
    match value {
        Value::Symbol(id) => {
            // Treat all symbols as global vars for now
            Ok(Expr::GlobalVar(*id))  // LINE 689
        }
        // ...
    }
}
```

**Current behavior:**
- **All symbols are treated as global variables** at the point of parsing/conversion
- No distinction between local, parameter, and global scope during this phase
- Symbols are later analyzed for free variables but not resolved to local scope initially

### 1.3 Variable Resolution in AST

**File:** `src/compiler/ast.rs`, lines 30-34

The AST has three variable representation types:

```rust
pub enum Expr {
    /// Variable reference (symbol, depth, index)
    Var(SymbolId, usize, usize),          // Local/upvalue variable
    
    /// Global variable reference
    GlobalVar(SymbolId),                   // Global variable
    // ...
}
```

**Semantics:**
- `Var(sym, depth, index)`:
  - `depth = 0`: local variable in current scope
  - `depth > 0`: upvalue in enclosing scope (captured variable)
  - `index`: position in the environment
- `GlobalVar(sym)`: Variables in global store, accessed via symbol lookup

---

## 2. VM Instruction Set

### 2.1 Variable Access Instructions

**File:** `src/compiler/bytecode.rs`, lines 6-100

Available instructions for variable access:

```rust
pub enum Instruction {
    /// Load local variable (depth, index)
    LoadLocal,                 // Takes 1 byte operand (index)
    
    /// Load global variable
    LoadGlobal,               // Takes 2 byte operand (constant index)
    
    /// Store local variable (depth, index)
    StoreLocal,               // Takes 1 byte operand (index)
    
    /// Store global variable
    StoreGlobal,              // Takes 2 byte operand (constant index)
    
    /// Load from closure environment
    LoadUpvalue,              // Takes 2 byte operands (depth, index)
}
```

### 2.2 Current Instruction Limitations

**For recursive functions:**
- `LoadLocal` uses only the **index** operand (1 byte)
- No `LocalVar` instruction that combines depth + index
- Upvalue loading (`LoadUpvalue`) expects values to be in closure's environment
- **Global variables cannot refer to function-scoped variables**

**The problem with recursion:**
When a function calls itself recursively:
1. The function name is looked up as a **GlobalVar**
2. It's stored in the global environment via `StoreGlobal`
3. During execution, recursive calls look it up from globals
4. **BUT**: Inside a lambda, the function name isn't available for recursive reference

### 2.3 Closure Handling

**File:** `src/compiler/bytecode.rs`, lines 46-47

```rust
/// Create closure (const_idx, num_upvalues)
MakeClosure,
```

The `MakeClosure` instruction:
- Takes a closure template (constant index) and number of upvalues
- Pops `num_upvalues` values from stack
- Creates a closure with those values in its environment (`env: Rc<Vec<Value>>`)

---

## 3. Lambda/Function Handling

### 3.1 Lambda Compilation

**File:** `src/compiler/compile.rs`, lines 106-143

```rust
Expr::Lambda {
    params,
    body,
    captures,
} => {
    // 1. Create a new compiler for the lambda body
    let mut lambda_compiler = Compiler::new();
    
    // 2. Compile the body
    lambda_compiler.compile_expr(body, true);
    lambda_compiler.bytecode.emit(Instruction::Return);
    
    // 3. Create closure with num_locals = params.len() + captures.len()
    let closure = Closure {
        bytecode: Rc::new(lambda_compiler.bytecode.instructions),
        arity: crate::value::Arity::Exact(params.len()),
        env: Rc::new(Vec::new()),  // Empty at compile time
        num_locals: params.len() + captures.len(),
        constants: Rc::new(lambda_compiler.bytecode.constants),
    };
    
    // 4. Emit captured values and MakeClosure
    for (sym, _depth, _index) in captures {
        let sym_idx = self.bytecode.add_constant(Value::Symbol(*sym));
        self.bytecode.emit(Instruction::LoadGlobal);
        self.bytecode.emit_u16(sym_idx);
    }
    
    self.bytecode.emit(Instruction::MakeClosure);
    self.bytecode.emit_u16(idx);
    self.bytecode.emit_byte(captures.len() as u8);
}
```

**Key characteristics:**

1. **Child Compiler Model**
   - Each lambda gets its own `Compiler` instance
   - No shared state with parent scope
   - No way to reference the parent function itself

2. **Capture Analysis**
   - Free variables are analyzed (see section 3.2)
   - Captures are stored as `(SymbolId, depth, index)`
   - But depth/index are **hardcoded to 0** (placeholder!)

3. **Environment Population**
   - At compile time: captured values are loaded as **globals only**
   - At runtime: `MakeClosure` collects these stack values into `closure.env`
   - Parameters are stored in stack positions 0 to `params.len()-1`

### 3.2 Free Variable Analysis

**File:** `src/compiler/analysis.rs`, lines 5-130

```rust
pub fn analyze_free_vars(expr: &Expr, local_bindings: &HashSet<SymbolId>) -> HashSet<SymbolId> {
    let mut free_vars = HashSet::new();
    
    match expr {
        Expr::Lambda { params, body, .. } => {
            // Create new local bindings that include lambda parameters
            let mut new_bindings = local_bindings.clone();
            for param in params {
                new_bindings.insert(*param);
            }
            free_vars.extend(analyze_free_vars(body, &new_bindings));
        }
        // ...
    }
}
```

**Behavior:**
- Recursively analyzes expression tree
- Maintains `local_bindings`: set of symbols bound in current scope
- Returns set of free variables (not locally bound)
- **Lambda parameters are considered local bindings**
- **The function name itself is NOT in local_bindings**

---

## 4. Runtime Execution

### 4.1 VM Structure

**File:** `src/vm/core.rs`

```rust
pub struct VM {
    pub stack: Vec<Value>,
    pub globals: HashMap<u32, Value>,
    pub call_depth: usize,
    // ... other fields
}
```

### 4.2 Bytecode Execution

**File:** `src/vm/mod.rs`, lines 23-240+

The main execution loop:

```rust
fn execute_bytecode(
    &mut self,
    bytecode: &[u8],
    constants: &[Value],
    closure_env: Option<&Rc<Vec<Value>>>,
) -> Result<Value, String> {
    let mut ip = 0;
    
    loop {
        match instr {
            Instruction::LoadLocal => {
                let idx = vm.read_u8(bytecode, ip) as usize;
                if idx >= vm.stack.len() {
                    return Err("Local variable index out of bounds".to_string());
                }
                let val = vm.stack[idx].clone();
                vm.stack.push(val);
            }
            
            Instruction::LoadGlobal => {
                let idx = vm.read_u16(bytecode, ip) as usize;
                if let Value::Symbol(sym_id) = constants[idx] {
                    if let Some(val) = vm.globals.get(&sym_id.0) {
                        vm.stack.push(val.clone());
                    } else {
                        return Err(format!("Undefined global variable: {:?}", sym_id));
                    }
                }
            }
            
            Instruction::LoadUpvalue => {
                let _depth = vm.read_u8(bytecode, ip);
                let idx = vm.read_u8(bytecode, ip) as usize;
                if let Some(env) = closure_env {
                    if idx < env.len() {
                        vm.stack.push(env[idx].clone());
                    }
                }
            }
        }
    }
}
```

**Critical observation:**
- `LoadLocal` checks `vm.stack` directly
- Stack contains: **parameters + local variables + temporary values**
- No frame pointer or offset tracking
- This works for **single-level functions** but breaks with **nested scopes**

### 4.3 Function Calls

**File:** `src/vm/mod.rs`, lines 90-121

```rust
Instruction::Call => {
    let arg_count = self.read_u8(bytecode, &mut ip) as usize;
    let func = self.stack.pop().ok_or("Stack underflow")?;
    
    let mut args = Vec::with_capacity(arg_count);
    for _ in 0..arg_count {
        args.push(self.stack.pop().ok_or("Stack underflow")?);
    }
    args.reverse();
    
    match func {
        Value::NativeFn(f) => {
            let result = f(&args)?;
        }
        Value::Closure(closure) => {
            self.call_depth += 1;
            if self.call_depth > 1000 {
                return Err("Stack overflow".to_string());
            }
            
            // ISSUE: Arguments are passed on stack, not in closure.env
            let result = self.execute_bytecode(
                &closure.bytecode,
                &closure.constants,
                Some(&closure.env),
            )?;
            
            self.call_depth -= 1;
            result
        }
    }
    
    self.stack.push(result);
}
```

**Problem with recursion:**
1. When calling `Instruction::Call`, arguments are **popped from the main stack**
2. Then execution switches to the closure's bytecode
3. The closure environment (`closure.env`) only contains **captured variables**, not parameters
4. Parameters should be on the local stack, but there's **no frame offset management**
5. **Recursive calls lose access to parameters and parent scope**

---

## 5. Closure Structure

**File:** `src/value.rs`, lines 58-66

```rust
pub struct Closure {
    pub bytecode: Rc<Vec<u8>>,
    pub arity: Arity,
    pub env: Rc<Vec<Value>>,           // Captured variables only
    pub num_locals: usize,              // params + captures count
    pub constants: Rc<Vec<Value>>,
}
```

**Analysis:**
- `env`: Contains only **captured free variables** (from outer scopes)
- **No mechanism to store parameters** in the closure environment
- **No parent frame reference** for upvalue resolution
- Parameters are expected to be on the runtime stack, but stack management is not scope-aware

---

## 6. Current Workarounds

### 6.1 Top-Level Recursion (Documented at src/main.rs:60)

```rust
// First pass: collect all top-level definitions to pre-register them
// This allows recursive functions to reference themselves
```

**How it works:**
1. Pre-scan all top-level `define` statements
2. Register functions in the global environment before execution
3. Functions can then call themselves via `LoadGlobal`

**Limitation:** Only works at top-level, not for nested/lambda recursion

### 6.2 Lambda Non-Support (Noted in tests)

**File:** `tests/integration/core.rs`, line 209

```rust
#[test]
fn test_factorial_logic() {
    // Simulate factorial without recursion: (if (<= n 1) 1 (* n ...))
    assert_eq!(eval("(if (<= 1 1) 1 (* 1 1))").unwrap(), Value::Int(1));
}
```

Tests explicitly avoid recursive lambda patterns.

---

## 7. Analysis Summary: Gaps and Issues

### 7.1 Missing Scope Tracking

| Aspect | Status | Issue |
|--------|--------|-------|
| Global variable resolution | ✅ Works | Hardcoded globals |
| Local parameter access | ⚠️ Partial | No frame offset |
| Upvalue (captured) access | ✅ Works | Limited to captured vars |
| Recursive lambda calls | ❌ Broken | Function name not in scope |
| Nested scope depth | ❌ Missing | No depth/offset tracking |
| Parameter binding | ⚠️ Partial | Stack-based, not scoped |

### 7.2 Key Missing Components

1. **Symbol Table per Scope**
   - Current: Empty HashMap in Compiler
   - Needed: Track variable mappings (name → depth, index, type)

2. **Scope Stack During Compilation**
   - Current: None
   - Needed: Maintain nested scope information (depth levels)

3. **Frame Offset Management in Runtime**
   - Current: Stack-based, no frame info
   - Needed: Frame pointers or offset tracking for nested calls

4. **Function Self-Reference in Lambda**
   - Current: Not available
   - Needed: Include function name in its own captured variables

5. **Proper Upvalue Chain**
   - Current: Single-level upvalues in closure.env
   - Needed: Multi-level upvalue traversal through parent scopes

---

## 8. Architecture Recommendations

### 8.1 Proposed Scope System

**Phase 1: Compile-Time Scope Tracking**

Enhance `Compiler` struct:

```rust
struct Compiler {
    bytecode: Bytecode,
    scopes: Vec<Scope>,              // Stack of scopes
    current_depth: usize,             // Current nesting depth
}

struct Scope {
    variables: HashMap<SymbolId, VarInfo>,
    scope_type: ScopeType,
}

enum ScopeType {
    Global,
    Function,
    Lambda,
    Let,
}

struct VarInfo {
    depth: usize,                    // Lexical depth
    index: usize,                    // Position in scope
    kind: VarKind,
}

enum VarKind {
    Parameter,                       // Function parameter
    Local,                           // Local variable
    Captured,                        // Free variable from outer scope
}
```

**Phase 2: Runtime Frame Management**

Enhance execution with proper frame handling:

```rust
struct CallFrame {
    bytecode: Rc<Vec<u8>>,
    constants: Rc<Vec<Value>>,
    local_offset: usize,             // Offset in stack for locals
    parent_frame: Option<*mut CallFrame>,  // Parent scope
}
```

---

## 9. Instruction Set Enhancement

### 9.1 Proposed New Instructions

To support recursive lambdas without breaking existing code:

```rust
pub enum Instruction {
    // Existing (keep for compatibility)
    LoadLocal,          // (index) - from current stack
    LoadGlobal,         // (sym_idx)
    LoadUpvalue,        // (depth, index)
    
    // NEW: Explicit local variable with depth
    LoadLocalVar,       // (depth, index) - resolve through scope chain
    StoreLocalVar,      // (depth, index)
    
    // NEW: Self-reference for recursion
    LoadSelf,           // Load current function (for recursion)
    
    // NEW: Frame management
    PushFrame,          // (num_locals) - create new frame
    PopFrame,           // Clean up frame
}
```

---

## 10. Implementation Path for Recursive Lambdas

### Step 1: Enhance Compiler Structure
- Add scope stack to track variables
- Implement scope entry/exit
- Build symbol table per scope level

### Step 2: Modify Lambda Compilation
- Register function name in its own captures
- Track parameter positions with proper depth
- Emit correct `Var` expressions instead of `GlobalVar`

### Step 3: Enhance Variable Resolution
- During `analyze_free_vars`, track depth information
- Properly fill in `(depth, index)` in `Var` expressions
- Handle self-reference in lambdas

### Step 4: Bytecode Emission Improvements
- Emit proper `LoadLocal` with correct indices
- Use `LoadUpvalue` for captured variables at correct depths
- Add self-reference to closure captures

### Step 5: Runtime Execution
- Implement frame offset tracking
- Ensure parameters are accessible via `LoadLocal` with proper offsets
- Test recursive lambda execution

---

## 11. Test Case: Factorial Lambda

**Current Status:** ❌ Does NOT work

```lisp
(define fact
  (lambda (n)
    (if (<= n 1)
      1
      (* n (fact (- n 1))))))
```

**Why it fails:**
1. `fact` is defined at top-level (works via global pre-registration)
2. Inside the lambda body, `fact` is treated as a global variable
3. But the parameter `n` is **not properly scoped** for recursive calls
4. **Actual problem**: The closure doesn't include the function name itself

**Fix needed:**
1. Add `fact` to lambda's captured variables
2. Ensure parameters are properly indexed for `LoadLocal`
3. Use `LoadUpvalue` to access captured `fact`

---

## 12. Files That Need Modification

| File | Purpose | Changes Needed |
|------|---------|-----------------|
| `src/compiler/compile.rs` | Main compilation | Add scope tracking |
| `src/compiler/ast.rs` | AST definition | May add new nodes |
| `src/compiler/analysis.rs` | Variable analysis | Depth tracking |
| `src/compiler/bytecode.rs` | Instruction set | Add new instructions |
| `src/vm/mod.rs` | Execution loop | Frame management |
| `src/vm/variables.rs` | Variable access | Depth-aware loading |
| `src/value.rs` | Closure structure | Possibly enhance CallFrame |

---

## Summary Table

| Component | Current State | Issue | Priority |
|-----------|--------------|-------|----------|
| Symbol resolution | Global-only | No scope awareness | High |
| Compiler state | No scope stack | Can't track depth | High |
| Lambda compilation | Captures only globals | Can't capture self | High |
| Free var analysis | Works for basics | Doesn't track depth | Medium |
| VM execution | Stack-based | No frame offsets | Medium |
| Closure environment | Upvalue only | No parameter binding | Medium |
| Recursive functions | Top-level only | Lambda unsupported | High |

---

## Conclusion

The Elle compiler has a solid foundation for closures and capture mechanics, but **lacks proper scope tracking** for implementing recursive lambdas. The main gaps are:

1. **No compile-time scope management** - variables aren't tracked with depth information
2. **No runtime frame management** - stack access is index-only, not depth-aware
3. **No self-reference mechanism** - lambdas can't capture their own name
4. **Limited upvalue chain** - only single-level captured variables in closure.env

Implementing local variable scoping for recursive functions requires:
- Adding a scope stack to the compiler
- Implementing proper depth/index tracking during compilation
- Enhancing the closure structure to handle parameters and frame offsets
- Adding frame management to the VM execution
- Enabling functions to be included in their own captured variables list

This is a significant but feasible enhancement that would unlock recursive lambda definitions.
