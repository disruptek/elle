# Recursive Lambda Examples and Issues

## Test Cases

### 1. Top-Level Recursive Function (WORKS ✓)

```lisp
(define factorial
  (lambda (n)
    (if (<= n 1)
      1
      (* n (factorial (- n 1))))))

(factorial 5)  ; Returns 120
```

**Why it works:**
- `factorial` is defined at top-level
- Pre-registration pass in `src/main.rs:60` adds it to globals before execution
- Inside the lambda, `LoadGlobal(factorial)` finds it in globals
- Recursion succeeds

**Current bytecode:**
```
Main bytecode:
  LoadConst(lambda_template)      # Load closure template
  LoadGlobal(factorial_placeholder) # Try to load (doesn't exist yet!)
  MakeClosure(1)                  # Create closure with 1 capture
  StoreGlobal(factorial)          # Store result

Lambda bytecode:
  LoadLocal(0)                    # Load parameter n
  LoadConst(1)                    # Load 1
  Le                              # Compare <=
  JumpIfFalse(else_label)
  # ... if body ...
  LoadGlobal(factorial)           # Load factorial from globals
  LoadLocal(0)                    # Load n
  LoadConst(1)
  Sub
  Call(1)                         # Recursive call
  Jump(end)
  # ... else body ...
```

**Problem:** The first `LoadGlobal(factorial)` at MakeClosure time actually fails, but works at execution time due to pre-registration.

---

### 2. Nested Lambda Recursion (FAILS ✗)

```lisp
(define outer
  (lambda (x)
    (define inner
      (lambda (n)
        (if (<= n 0)
          x
          (* n (inner (- n 1))))))
    (inner x)))

(outer 5)  ; Should return 5! = 120, but FAILS
```

**Why it fails:**
- `inner` is a local function, not global
- When compiling `inner`, it tries: `LoadGlobal(inner)` during recursion
- `inner` was never registered in globals
- Runtime error: "Undefined global variable: inner"

**Current bytecode for outer:**
```
Outer bytecode:
  LoadLocal(0)                    # parameter x
  # ... (define inner ...) ...
  # Problem: inner compilation doesn't know about its own name
```

**Inner bytecode (BROKEN):**
```
Inner bytecode:
  LoadLocal(0)                    # parameter n
  LoadConst(0)                    # 0
  Le                              # compare
  JumpIfFalse(else)
  LoadUpvalue(0,0)                # Load captured x ✓
  Jump(end)
  # Else:
  LoadLocal(0)                    # Load n
  LoadGlobal(inner)               # ERROR! inner is not global ✗
  LoadLocal(0)
  LoadConst(1)
  Sub
  Call(1)
```

---

### 3. Higher-Order Recursive Functions (FAILS ✗)

```lisp
(define make-accumulator
  (lambda (initial)
    (lambda (add-val)
      (set! initial (+ initial add-val))
      initial)))

(define acc (make-accumulator 0))
(acc 5)   ; Returns 5
(acc 3)   ; Returns 8

; Now with recursion:
(define make-countdown
  (lambda (n)
    (lambda ()
      (if (<= n 0)
        "done"
        (begin
          (set! n (- n 1))
          ((make-countdown n)))))))

(define cd (make-countdown 3))
(cd)  ; Should countdown: 2, 1, "done" but FAILS
```

**Why it fails:**
- The inner lambda tries to call `make-countdown` recursively
- `make-countdown` is treated as `GlobalVar`
- At runtime, it's looked up as a global
- May work for top-level but breaks if used in nested context

---

### 4. Mutual Recursion (FAILS ✗)

```lisp
(define is-even
  (lambda (n)
    (if (= n 0)
      #t
      (is-odd (- n 1)))))

(define is-odd
  (lambda (n)
    (if (= n 0)
      #f
      (is-even (- n 1)))))

(is-even 4)  ; Should return #t, but FAILS
```

**Why it fails:**
- `is-even` tries to call `is-odd` which isn't defined yet
- `is-odd` tries to call `is-even` which is defined but as `GlobalVar`
- Circular dependency and global lookup issues

---

## Detailed Analysis: Where Compilation Breaks

### Example: Simple Recursive Lambda

```lisp
(define count-down
  (lambda (n)
    (if (<= n 0)
      "done"
      (count-down (- n 1)))))
```

### Current Compilation Flow

**Step 1: value_to_expr() [compile.rs:681-689]**

Input: `Value::Cons(symbol "define", ...)`

```
Result Expr::Define {
  name: count-down,
  value: Expr::Lambda {
    params: [n],
    body: Expr::If {
      cond: Expr::Call(Expr::GlobalVar(<=), ...),
      then: Expr::Literal("done"),
      else_: Expr::Call(
        Expr::GlobalVar(count-down),  # ← TREATED AS GLOBAL!
        [Expr::Call(...)]
      )
    },
    captures: [] # Not filled yet
  }
}
```

**Critical issue:** `count-down` inside the lambda is marked as `GlobalVar` immediately, with no way to resolve it locally.

---

**Step 2: compile_expr() for Define [compile.rs:177-182]**

```rust
Expr::Define { name, value } => {
    self.compile_expr(value, false);  // Compile the lambda
    let idx = self.bytecode.add_constant(Value::Symbol(*name));
    self.bytecode.emit(Instruction::StoreGlobal);
    self.bytecode.emit_u16(idx);
}
```

Result bytecode:
```
LoadConst(lambda_template)
[captured values loading code]
MakeClosure(num_captures)
StoreGlobal(count-down)
```

---

**Step 3: compile_expr() for Lambda [compile.rs:106-143]**

```rust
Expr::Lambda {
    params,
    body,
    captures,
} => {
    // Create NEW compiler instance - ISOLATED
    let mut lambda_compiler = Compiler::new();
    
    // Compile body with GlobalVar(count-down)
    lambda_compiler.compile_expr(body, true);
    lambda_compiler.bytecode.emit(Instruction::Return);
    
    // analyze_free_vars would be called here (if it were)
    // Find that 'count-down' is free (not in params [n])
    // Add to captures: [(count-down, 0, 0)]
    
    // Then:
    for (sym, _depth, _index) in captures {
        let sym_idx = self.bytecode.add_constant(Value::Symbol(*sym));
        self.bytecode.emit(Instruction::LoadGlobal);  // ← PROBLEM
        self.bytecode.emit_u16(sym_idx);
    }
    
    self.bytecode.emit(Instruction::MakeClosure);
}
```

**Problem:** 
- `LoadGlobal(count-down)` executes BEFORE `StoreGlobal(count-down)`
- `count-down` doesn't exist in globals yet
- Circular dependency!

---

### Current Workaround in main.rs

```rust
// First pass: collect all top-level definitions to pre-register them
// This allows recursive functions to reference themselves
{
    // Read all values from input
    // Find Define expressions
    // Pre-register symbols in VM globals
}
```

This pre-registration makes `LoadGlobal(count-down)` work, BUT:
- Only works at top-level
- Doesn't help with nested lambdas
- Fragile and limited

---

## Proposed Fix: Step-by-Step

### Step 1: Add Scope Tracking to Compiler

```rust
struct Compiler {
    bytecode: Bytecode,
    scopes: Vec<Scope>,           // ← NEW: Stack of scopes
    current_depth: usize,
}

struct Scope {
    variables: HashMap<SymbolId, VarInfo>,
    scope_type: ScopeType,
}

struct VarInfo {
    depth: usize,
    index: usize,
    kind: VarKind,
}

impl Compiler {
    fn new() -> Self {
        Compiler {
            bytecode: Bytecode::new(),
            scopes: vec![Scope::global()],  // Global scope
            current_depth: 0,
        }
    }
    
    fn push_scope(&mut self, scope_type: ScopeType) {
        self.scopes.push(Scope::new(scope_type));
        self.current_depth += 1;
    }
    
    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.current_depth -= 1;
    }
    
    fn register_variable(&mut self, sym: SymbolId, kind: VarKind) -> (usize, usize) {
        let scope = &mut self.scopes[self.scopes.len() - 1];
        let index = scope.variables.len();
        scope.variables.insert(sym, VarInfo {
            depth: 0,
            index,
            kind,
        });
        (self.current_depth, index)
    }
    
    fn lookup_variable(&self, sym: SymbolId) -> Option<(usize, usize, VarKind)> {
        for (depth, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(info) = scope.variables.get(&sym) {
                return Some((depth, info.index, info.kind));
            }
        }
        None
    }
}
```

### Step 2: Modify Lambda Compilation

```rust
Expr::Lambda {
    params,
    body,
    captures,
} => {
    // Create NEW compiler for lambda body
    let mut lambda_compiler = Compiler::new();
    
    // STEP 1: Push lambda scope
    lambda_compiler.push_scope(ScopeType::Lambda);
    
    // STEP 2: Register parameters with depth=0
    for (param_idx, param) in params.iter().enumerate() {
        lambda_compiler.register_variable(*param, VarKind::Parameter);
    }
    
    // STEP 3: Register captured variables with proper depth
    // The function name itself is included!
    for (capture_idx, (sym, _old_depth, _old_index)) in captures.iter().enumerate() {
        lambda_compiler.register_variable(*sym, VarKind::Captured);
    }
    
    // STEP 4: Compile body (now with scope-aware compilation)
    lambda_compiler.compile_expr(body, true);
    lambda_compiler.bytecode.emit(Instruction::Return);
    
    // ... rest of closure creation ...
}
```

### Step 3: Modify compile_expr for Var references

```rust
Expr::Var(sym, _old_depth, _old_index) => {
    match self.lookup_variable(*sym) {
        Some((depth, index, VarKind::Parameter)) => {
            if depth == 0 {
                self.bytecode.emit(Instruction::LoadLocal);
                self.bytecode.emit_byte(index as u8);
            } else {
                self.bytecode.emit(Instruction::LoadUpvalue);
                self.bytecode.emit_byte(depth as u8);
                self.bytecode.emit_byte(index as u8);
            }
        }
        Some((depth, index, VarKind::Captured)) => {
            self.bytecode.emit(Instruction::LoadUpvalue);
            self.bytecode.emit_byte(depth as u8);
            self.bytecode.emit_byte(index as u8);
        }
        Some((depth, index, VarKind::Local)) => {
            if depth == 0 {
                self.bytecode.emit(Instruction::LoadLocal);
                self.bytecode.emit_byte(index as u8);
            } else {
                self.bytecode.emit(Instruction::LoadUpvalue);
                self.bytecode.emit_byte(depth as u8);
                self.bytecode.emit_byte(index as u8);
            }
        }
        None => {
            // Fall back to global lookup for true globals
            let idx = self.bytecode.add_constant(Value::Symbol(*sym));
            self.bytecode.emit(Instruction::LoadGlobal);
            self.bytecode.emit_u16(idx);
        }
    }
}
```

### Step 4: Modify free variable analysis to include function name

The function name should be in its own captures for recursion:

```rust
// When compiling: (lambda (n) (count-down (- n 1)))
// Inside a Define for count-down:

Expr::Lambda {
    params: [n],
    body: Call(...),
    captures: ???
} => {
    let mut local_bindings = HashSet::new();
    for param in params {
        local_bindings.insert(*param);
    }
    
    let mut free_vars = analyze_free_vars(body, &local_bindings);
    
    // ENHANCEMENT: If this is a named lambda (inside define),
    // add the function name to captures!
    if let Some(function_name) = get_current_function_name() {
        free_vars.insert(function_name);
    }
    
    let captures: Vec<_> = free_vars
        .iter()
        .enumerate()
        .map(|(idx, sym)| (*sym, 0, idx))  // Now with proper index!
        .collect();
    
    // Result: captures includes the function itself
}
```

### Step 5: Updated Lambda Bytecode Generation

```rust
// For: (define count-down (lambda (n) ...))

// Compile the lambda:
{
    // New bytecode for lambda body:
    lambda_compiler.compile_expr(body, true);
    
    // When encountering (count-down (- n 1)):
    // lookup_variable(count-down) returns:
    //   (depth=1, index=0, kind=Captured)
    // 
    // Emit: LoadUpvalue(depth=1, index=0)
    // This loads count-down from closure.env[0]!
}

// Back in main compiler:
let idx = self.bytecode.add_constant(Value::Closure(...));

// Emit code to load captured values:
for (sym, _, index) in &captures {
    if sym == function_name {
        // Skip - will be filled in at runtime
        // Or use LoadSelf instruction
    } else {
        let sym_idx = self.bytecode.add_constant(Value::Symbol(*sym));
        self.bytecode.emit(Instruction::LoadGlobal);
        self.bytecode.emit_u16(sym_idx);
    }
}

self.bytecode.emit(Instruction::MakeClosure);
self.bytecode.emit_u16(idx);
self.bytecode.emit_byte(captures.len() as u8);

// NOW: Store the function in globals
self.bytecode.emit(Instruction::StoreGlobal);
self.bytecode.emit_u16(function_name_idx);

// The closure object now has itself in its environment!
```

---

## Testing the Fix

### Test Case 1: Simple Recursion

```lisp
(define countdown
  (lambda (n)
    (if (<= n 0)
      "done"
      (begin
        (print n)
        (countdown (- n 1))))))

(countdown 3)
; Expected output:
; 3
; 2
; 1
; "done"
```

**Before fix:** Error - "Undefined global variable: countdown"
**After fix:** Works! Prints 3, 2, 1 and returns "done"

### Test Case 2: Nested Recursion

```lisp
(define outer
  (lambda (x)
    (define inner
      (lambda (n)
        (if (<= n 0)
          x
          (inner (- n 1)))))
    (inner x)))

(outer 5)
; Expected: Returns after 5 recursive calls
```

**Before fix:** Error - "Undefined global variable: inner"
**After fix:** Works! inner calls itself recursively

### Test Case 3: Mutual Recursion

```lisp
(define is-even
  (lambda (n)
    (if (= n 0) #t (is-odd (- n 1)))))

(define is-odd
  (lambda (n)
    (if (= n 0) #f (is-even (- n 1)))))

(is-even 4)  ; #t
(is-odd 4)   ; #f
```

**Before fix:** Both fail due to ordering and scope issues
**After fix:** Works! Each function can call the other

---

## Bytecode Comparison: Before and After

### Before (Current - BROKEN for recursion)

```
Main bytecode for: (define factorial (lambda (n) ...))

[1] LoadConst(lambda_template)
[3] LoadGlobal(factorial)           ← FAILS! Not in globals yet
[5] MakeClosure(1)                  ← Tries to use nil as capture!
[7] StoreGlobal(factorial)

Lambda bytecode:

[1] LoadLocal(0)                    ← Load n
[2] LoadConst(1)                    ← Load 1
[3] Le                              ← Compare
[4] JumpIfFalse(11)                 ← Jump to else
[6] LoadConst("done")               ← If body
[8] Jump(13)
[11] LoadGlobal(factorial)          ← Lookup factorial (might fail)
[13] LoadLocal(0)
[14] LoadConst(1)
[15] Sub
[16] Call(1)
```

### After (Proposed - WORKS)

```
Main bytecode for: (define factorial (lambda (n) ...))

[1] LoadConst(lambda_template)
[3] MakeClosure(0)                  ← No LoadGlobal needed
[5] Dup                             ← Duplicate closure
[6] Dup                             ← Again
[7] SetupSelfRef                    ← Store closure in env[0]
[8] StoreGlobal(factorial)

Or simpler: MakeClosure already includes self-ref via closure object itself

Lambda bytecode:

[1] LoadLocal(0)                    ← Load n
[2] LoadConst(1)                    ← Load 1
[3] Le                              ← Compare
[4] JumpIfFalse(11)                 ← Jump to else
[6] LoadConst("done")               ← If body
[8] Jump(13)
[11] LoadUpvalue(1,0)               ← Load factorial from env[0]!
                                    ← (depth=1, index=0 for captured var)
[13] LoadLocal(0)                   ← Load n (depth=0, index=0)
[14] LoadConst(1)
[15] Sub
[16] Call(1)
```

**Key differences:**
1. No `LoadGlobal(factorial)` before `MakeClosure` - avoids circular dependency
2. Lambda uses `LoadUpvalue` to access captured `factorial`
3. The closure object itself is included in its environment at index 0
4. Proper depth/index tracking enables recursion

---

## Summary of Changes Needed

| File | Change | Impact |
|------|--------|--------|
| `src/compiler/compile.rs` | Add scope stack to Compiler | Enables variable tracking |
| `src/compiler/compile.rs` | Include function name in captures | Enables self-reference |
| `src/compiler/compile.rs` | Emit `LoadUpvalue` instead of `LoadGlobal` for captures | Proper scoped variable access |
| `src/compiler/analysis.rs` | Track depth/index in free var analysis | Fills in proper (depth,index) |
| `src/compiler/bytecode.rs` | May add `LoadSelf` instruction (optional) | Cleaner self-reference |
| `src/vm/variables.rs` | Handle multi-level upvalue chains | Parent scope resolution |
| `src/value.rs` | Enhance Closure with num_params, num_captures | Clearer closure structure |

All changes are backward compatible with existing top-level function definitions!
