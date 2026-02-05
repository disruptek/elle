# Variable Scoping Architecture Diagrams

## Current Architecture

### 1. Symbol Resolution Flow

```
┌─────────────────────────────────────────────────────────────┐
│                    Lisp Source Code                          │
│  (define fact (lambda (n) (if (<= n 1) 1 (* n (fact ...)))))│
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                  Reader/Parser                               │
│             Returns Value::Cons structures                  │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│         value_to_expr() [Line 681 of compile.rs]            │
│                                                              │
│  CRITICAL: All symbols → GlobalVar(SymbolId)               │
│  No distinction between local/param/global                 │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│         AST After value_to_expr()                           │
│                                                              │
│  Define {                                                   │
│    name: fact,                                              │
│    value: Lambda {                                          │
│      params: [n],                                           │
│      body: If { ... GlobalVar(fact) ... },                 │
│      captures: [] (empty at this point)                    │
│    }                                                        │
│  }                                                          │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│    analyze_free_vars() [analysis.rs]                       │
│                                                              │
│  local_bindings = {n}  (parameter)                          │
│  Finds free_vars = {fact}  (GlobalVar - not in bindings)   │
│                                                              │
│  Result: captures = [(fact, 0, 0)]                         │
│          ^^^ depth and index are PLACEHOLDER zeros!       │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│    compile_expr() Lambda branch [compile.rs:106-143]       │
│                                                              │
│  Creates child Compiler (NEW INSTANCE - no state!)         │
│  Compiles lambda body with Expr::GlobalVar(fact)          │
│                                                              │
│  Bytecode emitted:                                         │
│    LoadGlobal(fact)      # Look up 'fact' in globals      │
│    Call(1)               # But 'fact' might not exist!    │
│                                                              │
│  Then:                                                     │
│    For each capture:                                       │
│      LoadGlobal(capture) # Load from global store         │
│    MakeClosure           # Create closure                 │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│             Bytecode Generated                              │
│                                                              │
│  Main level:                                               │
│    LoadConst(<lambda_template>)                            │
│    LoadGlobal(fact)      # TRY to load 'fact'             │
│    MakeClosure(1)        # Make closure with 1 capture    │
│    StoreGlobal(fact)     # Store back to global           │
│                                                              │
│  Lambda bytecode:                                          │
│    LoadLocal(0)          # Load parameter 'n' ✓           │
│    LoadConst(1)          # Load 1                         │
│    Le                    # <= comparison ✓                │
│    JumpIfFalse           # Branch ✓                       │
│    ... (if branch) ...                                    │
│    LoadGlobal(fact)      # Load function for recursion ✗ │
│                          # Problem: fact might be nil!    │
│    LoadLocal(0)          # Load 'n'                       │
│    ... (recursive call)                                   │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│        VM Execution                                         │
│                                                              │
│  Stack: [...]                                              │
│  Globals: {fact: <closure>}  # Pre-registered at top-level │
│                                                              │
│  When executing lambda:                                    │
│    Stack: [n_value, ...]                                  │
│    LoadLocal(0) → Reads vm.stack[0]  ✓                    │
│    LoadGlobal(fact) → Reads globals[fact]  ✓              │
│    Call → Recursion works (at top-level due to pre-reg)  │
└─────────────────────────────────────────────────────────────┘
```

**THE PROBLEM:**
```
┌────────────────────────────────────────────────────────┐
│ When lambda is defined inside another lambda:         │
│                                                        │
│  (lambda (x)                                          │
│    (lambda (y)                                        │
│      (the-inner-lambda y (the-inner-lambda ...))))   │
│                                                        │
│ ❌ the-inner-lambda is NOT in captures!              │
│ ❌ It's treated as GlobalVar but won't exist!        │
│ ❌ Recursion fails                                    │
└────────────────────────────────────────────────────────┘
```

---

### 2. Compiler State During Lambda Compilation

```
┌─────────────────────────────────────────────────────────────┐
│             Outer Compiler (Main Level)                     │
├─────────────────────────────────────────────────────────────┤
│ bytecode: [LoadConst, LoadGlobal, MakeClosure, ...]       │
│ symbols: {} (UNUSED - allow(dead_code))                    │
│                                                              │
│ Scope information:                                          │
│   - No scope tracking                                       │
│   - No depth information                                    │
│   - No parameter/local tracking                            │
└─────────────────────────────────────────────────────────────┘

           │ Encounters Lambda { params: [n], body, captures }
           │
           ▼

┌─────────────────────────────────────────────────────────────┐
│             NEW Child Compiler (Lambda)                     │
├─────────────────────────────────────────────────────────────┤
│ bytecode: [LoadLocal(0), LoadConst(1), ...]               │
│ symbols: {} (STILL UNUSED)                                  │
│                                                              │
│ ⚠️ LOST CONNECTION to outer compiler:                      │
│   - Can't access outer variables                            │
│   - Can't reference parent scope                            │
│   - Can't track that 'n' is a parameter                     │
│   - Can't track captured variables with depth info         │
│   - Can't know lambda's own name for recursion             │
└─────────────────────────────────────────────────────────────┘
```

---

### 3. Stack Layout During Execution

```
Current (Stack-Based, No Frame Management):

Top of Stack                
    ▲
    │ [temporary values]
    │ [local variables?]
    │ [parameters?]  
    │ [outer scope values?]
    │
    │ When LoadLocal(2) executes:
    │ Reads vm.stack[2]
    │ But which 2? Parameter? Local? Captured? ❓
    │
    └─────────────────────────

Problem: LoadLocal uses absolute index into shared stack
         No frame offset, depth, or scope awareness


Proposed (Frame-Based):

┌──────────────────────────────────┐
│ Frame 1 (Main)                   │
├──────────────────────────────────┤
│ offset: 0                        │
│ Stack[0-5]: main variables       │
└──────────────────────────────────┘
           │
           ├─ Function Call
           │
           ▼
┌──────────────────────────────────┐
│ Frame 2 (Lambda 1)               │
├──────────────────────────────────┤
│ offset: 6                        │
│ Stack[6-8]: parameters           │
│ Stack[9-15]: locals              │
│ parent_frame: Frame 1            │
└──────────────────────────────────┘
           │
           ├─ Function Call (Recursion)
           │
           ▼
┌──────────────────────────────────┐
│ Frame 3 (Lambda 1 recursive)     │
├──────────────────────────────────┤
│ offset: 16                       │
│ Stack[16-18]: parameters         │
│ Stack[19-25]: locals             │
│ parent_frame: Frame 2            │
└──────────────────────────────────┘

With frame offsets:
LoadLocal(0, offset=16) means Stack[16+0] = first parameter
LoadLocal(1, offset=16) means Stack[16+1] = second parameter

Recursion works because each frame has its own offset!
```

---

### 4. Variable Resolution Process

```
┌─────────────────────────────────────┐
│ Looking up variable 'n' in Lambda   │
└─────────────────────────────────────┘

Step 1: Is 'n' a parameter?
   ├─ Check lambda params list
   └─ YES: It's parameter #0
      Result: Expr::Var(n, depth=0, index=0)
      → Emit: LoadLocal(0)

Step 2: Is 'n' a free variable?
   ├─ Check analyze_free_vars()
   └─ NO: It's bound locally

Result: LoadLocal(0) reads vm.stack[0]


┌─────────────────────────────────────┐
│ Looking up variable 'x' in nested   │
│ (lambda (y) ... x ...) within       │
│ (lambda (x) ...)                    │
└─────────────────────────────────────┘

Step 1: Is 'x' a parameter?
   ├─ Check inner lambda params: [y]
   └─ NO: Not in current params

Step 2: Is 'x' a captured variable?
   ├─ Check outer scope
   └─ YES: It's parameter of outer lambda
      Result: Expr::Var(x, depth=1, index=0)
      → Emit: LoadUpvalue(depth=1, index=0)

Step 3: VM execution
   ├─ closure_env contains captured values
   ├─ Read index 0 from closure_env
   └─ Returns the captured 'x' value


┌─────────────────────────────────────┐
│ Current Problem: Looking up 'fact'  │
│ inside (lambda (n) (fact n))        │
└─────────────────────────────────────┘

Step 1: Is 'fact' a parameter?
   ├─ Check lambda params: [n]
   └─ NO

Step 2: Is 'fact' a captured variable?
   ├─ analyze_free_vars() finds it
   ├─ Creates captures = [(fact, 0, 0)]
   └─ YES! It's free

Step 3: Compilation
   ├─ Emit LoadGlobal(fact)
   ├─ Emit MakeClosure with 1 capture
   └─ Problem: LoadGlobal expects global store
              But 'fact' might not be defined yet!

Step 4: VM execution
   ├─ execute_bytecode() is called for lambda
   ├─ LoadGlobal tries: globals.get(&fact)
   └─ FAILS: 'fact' not in globals (not pre-registered)
             OR succeeds at top-level (pre-registered workaround)
```

---

## Comparison: Current vs. Proposed

### Current System

```
┌─────────────────┐
│   Parse         │  All symbols → GlobalVar
└────────┬────────┘
         │
┌────────▼─────────────────────────┐
│ value_to_expr()                  │
│ (No scope info)                  │
└────────┬────────────────────────┘
         │
┌────────▼─────────────────────────┐
│ analyze_free_vars()              │
│ (Returns set of symbols)         │
│ (No depth tracking)              │
└────────┬────────────────────────┘
         │
┌────────▼─────────────────────────┐
│ compile_expr() Lambda            │
│ (Child compiler, no state share) │
│ (Captures lost depth/index)      │
└────────┬────────────────────────┘
         │
┌────────▼─────────────────────────┐
│ Bytecode Emission                │
│ (LoadGlobal for all captures)   │
└────────┬────────────────────────┘
         │
┌────────▼─────────────────────────┐
│ VM Execution                     │
│ (Stack-based, no frames)        │
│ (LoadLocal uses direct index)    │
└────────┬────────────────────────┘
         │
┌────────▼─────────────────────────┐
│ Result: Works for top-level only │
└─────────────────────────────────┘
```

### Proposed System

```
┌─────────────────┐
│   Parse         │
└────────┬────────┘
         │
┌────────▼─────────────────────────────────┐
│ value_to_expr() with Scope Tracking      │
│ (Input: global scope context)            │
│ (Output: Var/GlobalVar with depth)       │
└────────┬─────────────────────────────────┘
         │
┌────────▼──────────────────────────────────────┐
│ analyze_free_vars() with Depth              │
│ (Tracks: symbol, depth, index, kind)       │
│ (Populates captures with real depth/index)  │
└────────┬──────────────────────────────────────┘
         │
┌────────▼────────────────────────────────────────┐
│ compile_expr() Lambda with Scope Stack       │
│ (Maintains scope stack: depth 0, 1, 2, ...) │
│ (Tracks parameters with indices)            │
│ (Includes function in own captures)         │
└────────┬────────────────────────────────────────┘
         │
┌────────▼──────────────────────────────────────────┐
│ Bytecode Emission                               │
│ (LoadLocal for parameters)                    │
│ (LoadUpvalue for captured with depth)        │
│ (LoadSelf for recursion)                     │
└────────┬──────────────────────────────────────────┘
         │
┌────────▼──────────────────────────────────────────┐
│ VM Execution with Frame Management              │
│ (Stack frames with offsets)                    │
│ (LoadLocal(idx, offset) resolves correctly)   │
│ (Parent frame chain for upvalue lookup)        │
└────────┬──────────────────────────────────────────┘
         │
┌────────▼──────────────────────────────────────────┐
│ Result: Recursive lambdas work!                  │
└──────────────────────────────────────────────────┘
```

---

## Data Structures

### Current Compiler

```rust
struct Compiler {
    bytecode: Bytecode,
    symbols: HashMap<SymbolId, usize>,  // ← UNUSED
}

// Result: No scope awareness during compilation
```

### Proposed Compiler Enhancement

```rust
struct Compiler {
    bytecode: Bytecode,
    scopes: Vec<Scope>,                 // ← Scope stack
    current_depth: usize,                // ← Track nesting
}

struct Scope {
    variables: HashMap<SymbolId, VarInfo>,
    scope_type: ScopeType,
}

struct VarInfo {
    depth: usize,        // Lexical depth (0=current, 1=parent, etc.)
    index: usize,        // Position in scope
    kind: VarKind,       // Parameter, Local, or Captured
}

enum VarKind {
    Parameter,
    Local,
    Captured,
}

enum ScopeType {
    Global,
    Function,
    Lambda,
    Let,
}
```

### Current Closure

```rust
pub struct Closure {
    pub bytecode: Rc<Vec<u8>>,
    pub arity: Arity,
    pub env: Rc<Vec<Value>>,        // Captured values only
    pub num_locals: usize,           // params.len() + captures.len()
    pub constants: Rc<Vec<Value>>,
}

// Problem: No parameter binding mechanism
// No frame offset information
```

### Proposed Closure Enhancement

```rust
pub struct Closure {
    pub bytecode: Rc<Vec<u8>>,
    pub arity: Arity,
    pub env: Rc<Vec<Value>>,        // Captured variables + parameters
    pub num_locals: usize,
    pub num_params: usize,           // ← NEW: explicit param count
    pub num_captures: usize,         // ← NEW: explicit capture count
    pub constants: Rc<Vec<Value>>,
    pub parent_frame: Option<Rc<Closure>>,  // ← NEW: parent scope chain
}
```

---

## Execution Flow Comparison

### Current: Non-Recursive Lambda Call

```
MainBytecode:
  LoadConst(<lambda_template>)
  LoadGlobal(external_var)
  MakeClosure(1)                    <- Stack: [<lambda>]
  Call(0)

Execution:
  1. Stack: [<lambda>]
  2. Pop function: <lambda>
  3. Call execute_bytecode():
     - bytecode: <lambda's bytecode>
     - closure_env: <lambda.env> = [external_var]
     - No parameters
  4. Inside lambda, LoadUpvalue(0,0) reads closure_env[0] ✓


Current: Recursive Lambda Call (FAILS)

MainBytecode:
  LoadConst(<lambda_template>)
  LoadGlobal(factorial)           <- WAIT: not defined yet!
  MakeClosure(1)
  StoreGlobal(factorial)          <- Stored AFTER closure created

LambdaBytecode:
  LoadLocal(0)                    <- parameter 'n'
  LoadConst(1)                    <- literal 1
  Le                              <- comparison
  JumpIfFalse(+10)
  ... (if body)
  LoadGlobal(factorial)           <- TRY to call 'factorial'
  LoadLocal(0)                    <- parameter 'n'
  LoadConst(1)
  Sub
  Call(1)                         <- Recursive call

Execution:
  1. LoadGlobal(factorial)
     ├─ Look in globals
     ├─ During first call: factorial IS defined (pre-registered)
     └─ But captures don't contain it!

Problem: 
  - Recursion only works if 'factorial' is pre-registered globally
  - For nested lambdas, won't work at all
  - The closure.env doesn't include the function itself


Proposed: Recursive Lambda Call (WORKS)

CompilerBytecode (with scope tracking):
  LoadConst(<lambda_template>)
  [FOR EACH CAPTURE: load captured values]
  LoadSelf           <- ← NEW: Load current function for recursion
  MakeClosure(n_captures + 1)
  StoreGlobal(factorial)

LambdaBytecode (with proper indices):
  LoadLocal(0)       <- parameter 'n' (depth=0, idx=0)
  LoadConst(1)
  Le
  JumpIfFalse(+10)
  LoadUpvalue(1,0)   <- captured 'factorial' (depth=1, idx=n_captures)
  LoadLocal(0)       <- parameter 'n'
  LoadConst(1)
  Sub
  Call(1)

Execution (with frame management):
  Frame 1 (main):
    offset=0
    Stack[0-n]: global/main variables

  Frame 2 (factorial call #1):
    offset=n+1
    Stack[n+1]: parameter 'n' = 5
    Stack[n+2-...]: locals
    closure_env[n_captures]: factorial closure itself ✓

  Frame 3 (factorial call #2, recursive):
    offset=m+1
    Stack[m+1]: parameter 'n' = 4
    closure_env[n_captures]: factorial closure ✓
    parent_frame: Frame 2

Result: Recursion works because function is in captured env!
```

---

## Key Insight: The Self-Reference Problem

```
Current Problem:

┌────────────────────────────────────────┐
│ (define factorial                      │
│   (lambda (n)                          │
│     (if (<= n 1)                       │
│       1                                │
│       (* n (factorial (- n 1))))))    │
└────────────────────────────────────────┘

When compiling the lambda body:
  1. Find free variables: {factorial, n, -, *, <=, 1}
  2. Parameter: {n}
  3. Free vars (non-params): {factorial, -, *, <=}
  4. Create captures: [(factorial, 0, 0), (-, 0, 0), ...]
  5. Emit: LoadGlobal(factorial), LoadGlobal(-), ...
  6. Create closure, then MakeClosure

Problem:
  - MakeClosure instruction pops captured values from stack
  - LoadGlobal(factorial) tries to read from globals
  - But 'factorial' isn't in globals yet!
  - StoreGlobal happens AFTER closure is created
  - Circular dependency!

Solution:
  - Include function in its own captures
  - Use LoadSelf instruction instead of LoadGlobal
  - Self-reference resolves at runtime via closure object itself


Execution Timeline:

Current (Fails for lambdas):
  1. Eval LoadConst(<lambda_template>)
     Stack: [<lambda_template>]
  
  2. Eval LoadGlobal(factorial)
     Lookup: globals.get(factorial)
     Problem: factorial not in globals yet!
     Error: "Undefined global variable: factorial"

Proposed (Works):
  1. Eval LoadConst(<lambda_template>)
     Stack: [<lambda_template>]
  
  2. Eval LoadGlobal(external_vars...)
     Stack: [<lambda>, val1, val2, ...]
  
  3. Eval LoadSelf or skip self-load initially
  
  4. Eval MakeClosure(1)
     Pop captures, create closure object
     Stack: [<closure>]
     At this point, closure contains itself in captures!
  
  5. Eval StoreGlobal(factorial)
     globals[factorial] = <closure>
     Stack: [<closure>]

  6. When lambda is called later:
     closure.env[0] = the closure itself ✓
     Recursive calls work!
```

---

## Summary Diagram

```
╔═══════════════════════════════════════════════════════════════╗
║                  CURRENT ARCHITECTURE                         ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║  Parse → value_to_expr() → analyze_free_vars()             ║
║           (no scope)       (returns set)                     ║
║                                                              ║
║           ↓                                                  ║
║                                                              ║
║  compile_expr() → Lambda compilation (new compiler)        ║
║  (no scope stack)  (LoadGlobal for captures)               ║
║                                                              ║
║           ↓                                                  ║
║                                                              ║
║  Bytecode → VM execute_bytecode()                          ║
║            (stack-based, no frames)                         ║
║                                                              ║
║  Result: ✓ Works for top-level                            ║
║          ✗ Fails for nested/recursive lambdas             ║
║                                                              ║
╚═══════════════════════════════════════════════════════════════╝


╔═══════════════════════════════════════════════════════════════╗
║                  PROPOSED ARCHITECTURE                        ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║  Parse → value_to_expr()                                     ║
║  (with scope param) ↓                                       ║
║                                                              ║
║           analyze_free_vars()                               ║
║  (returns set with depth info) ↓                           ║
║                                                              ║
║           compile_expr()                                    ║
║  (with scope stack,             ↓                          ║
║   tracks depth/index)                                       ║
║                                                              ║
║           ↓                                                  ║
║                                                              ║
║  Bytecode → VM execute_bytecode()                          ║
║            (frame-based,                                    ║
║             proper offsets,                                 ║
║             parent chain)                                   ║
║                                                              ║
║  Result: ✓ Top-level functions                            ║
║          ✓ Nested lambdas                                 ║
║          ✓ Recursive lambdas                              ║
║          ✓ Proper variable scoping                         ║
║                                                              ║
╚═══════════════════════════════════════════════════════════════╝
```
