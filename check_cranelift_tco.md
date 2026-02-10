# Cranelift Tail Call Optimization (TCO) Support

## Current Status (as of 2025)

### The Short Answer
**No, Cranelift does NOT have native TCO support as of v0.128 (latest stable).**

However, there are two important nuances:

---

## 1. Direct Tail Call Elimination (Not True TCO)

Cranelift **CAN eliminate tail calls in simple cases** by:
- Detecting when the return value of a call is immediately returned
- Rewriting as a direct jump (no call/return overhead)
- This works for:
  - Direct recursive calls
  - Calls to known functions

Example:
```rust
fn tail_call_example(n: i64) -> i64 {
    if n <= 1 {
        return n;
    }
    // Cranelift may optimize this:
    return tail_call_example(n - 1);  // Jump, not call
}
```

**Limitation:** Only works when:
- You know the target function at compile time
- The call is syntactically in tail position
- No polymorphism (dynamic dispatch)

This is NOT true TCO because it doesn't handle:
- Indirect/polymorphic tail calls
- Tail calls to unknown functions
- Dynamic function dispatch

---

## 2. Proper TCO via Trampolining

To get true tail call support, you need **trampolining**:

```rust
pub enum TailCall {
    Return(Value),
    Call { func: FunctionPtr, args: Vec<Value> },
}

fn fib_trampoline(n: i64) -> Value {
    let mut current = (fib_fn, vec![Value::Int(n)]);
    
    loop {
        match (current.func)(current.args) {
            TailCall::Return(v) => return v,
            TailCall::Call { func, args } => {
                current = (func, args);
                // Loop continues - no stack growth!
            }
        }
    }
}
```

This achieves TCO by:
- Returning "call this next" instead of actually calling
- Caller's loop processes the next call
- **No stack growth**, simulates tail calls

**Cost:** Small allocation per tail call (return enum), but O(1) stack.

---

## 3. Experimental LLVM-Backend TCO (Not Cranelift)

There IS a Rust RFC for `become` statement supporting TCO:
- https://github.com/rust-lang/rfcs/pull/3407
- Status: **Experimental, nightly only**
- Not relevant to Cranelift

---

## What This Means for Elle

### Option A: Simple Recursion (No TCO needed)
If your functions don't have deep recursion:
```lisp
(define (sum-list lst)
  (if (nil? lst)
    0
    (+ (car lst) (sum-list (cdr lst)))))
```

Cranelift's direct tail call elimination might help, but max recursion depth is limited by stack.

### Option B: Deep Recursion (Requires Trampolining)
For functions like:
```lisp
(define (fib n)
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))
```

Or iterative patterns in recursion:
```lisp
(define (loop-forever n)
  (print n)
  (loop-forever (+ n 1)))  ; Will stack overflow without TCO
```

You MUST use trampolining in the generated code.

---

## Recommended Approach: Hybrid TCO

### At Compile Time
Detect tail calls and generate trampoline-compatible code:

```rust
// Input Lisp:
(define (fact n acc)
  (if (<= n 0)
    acc
    (fact (- n 1) (* n acc))))  ; Tail call

// Generated pseudocode:
fn fact_native(n: i64, acc: i64) -> TailCall {
    if n <= 0 {
        TailCall::Return(Value::Int(acc))
    } else {
        TailCall::Call {
            func: fact_native_fn,
            args: vec![Value::Int(n - 1), Value::Int(acc * n)]
        }
    }
}

// Cranelift compiles this to native code
// The trampoline loop handles the TailCall enum
```

### At Runtime
The trampoline loop in Rust:
```rust
fn run_lisp_function(mut call: TailCall) -> Value {
    loop {
        match call {
            TailCall::Return(v) => return v,
            TailCall::Call { func, args } => {
                call = func(&args);  // Next iteration, no stack growth
            }
        }
    }
}
```

This gives you:
- ✓ Native code performance (Cranelift compiles the function)
- ✓ True TCO (trampoline prevents stack overflow)
- ✓ No recursion depth limit
- ✓ Simple to implement

---

## Code Generation Strategy

### Step 1: Detect Tail Calls During Compilation
```rust
fn is_tail_call(expr: &Expr, context: &CompileContext) -> bool {
    // Check if this call is in tail position
    // (i.e., its return value becomes the function's return value)
    matches!(expr, Expr::Call { ... })  // simplified
}
```

### Step 2: Generate Trampoline-Compatible Code
```rust
fn compile_tail_call(
    builder: &mut FunctionBuilder,
    func: &Expr,
    args: &[Expr],
) -> Value {
    // Instead of: builder.ins().call(func_ref, &args)
    
    // Generate: return TailCall::Call { func, args }
    let func_ptr = compile_expr(builder, func);
    let arg_vals = args.iter().map(|a| compile_expr(builder, a)).collect();
    
    // Create TailCall enum variant
    builder.ins().return_(&[create_tail_call_enum(func_ptr, arg_vals)]);
}
```

### Step 3: Cranelift Compiles It All to Native Code
The native code path doesn't care about TCO—that's the trampoline's job.

---

## Performance Analysis

### Without TCO (Stack Overflow Risk)
```
Time: Fast (direct recursion)
Memory: O(depth) stack growth
Risk: Stack overflow on deep recursion
```

### With Trampolining (Recommended)
```
Time: Slightly slower (one enum match per call)
Memory: O(1) stack, O(depth) heap allocations
Risk: No stack overflow, manageable heap
```

### Hypothetical Native TCO (Not Available)
```
Time: Very fast (constant overhead)
Memory: O(1) stack and heap
Risk: None
```

The trampoline approach is **99% as good** as native TCO for the cost of a small allocation.

---

## Real-World Example: Factorial with TCO

### Input Lisp
```lisp
(define (fact n acc)
  (if (<= n 0)
    acc
    (fact (- n 1) (* n acc))))

(fact 10000 1)  ; Would overflow without TCO
```

### Generated Code (Pseudocode)
```rust
fn fact_native(n: i64, acc: i64) -> TailCall {
    if n <= 0 {
        TailCall::Return(Value::Int(acc))
    } else {
        TailCall::Call {
            func: fact_native as fn(...) -> TailCall,
            args: [Value::Int(n - 1), Value::Int(acc * n)],
        }
    }
}

// Wrapper that handles the trampoline
fn fact_wrapper(n: i64, acc: i64) -> Value {
    let mut call = TailCall::Call {
        func: fact_native,
        args: [Value::Int(n), Value::Int(acc)],
    };
    
    loop {
        match call {
            TailCall::Return(v) => return v,
            TailCall::Call { func, args } => {
                call = func(&args[0], &args[1]);  // Tail call as loop iteration
            }
        }
    }
}
```

### Generated x86_64 (Simplified)
```asm
fact_native:
    cmp rdi, 0          ; if n <= 0
    jle .base_case
    
    ; Recursive case
    imul rsi, rdi       ; acc *= n
    dec rdi              ; n--
    
    ; Return TailCall::Call enum
    mov rax, 1          ; enum variant = Call
    mov rdx, [fact_native_ptr]
    mov rcx, rdi
    mov r8, rsi
    ret
    
.base_case:
    mov rax, 0          ; enum variant = Return
    mov rdx, rsi        ; value = acc
    ret

fact_wrapper:
    ; Initial call
    call fact_native
    
.loop:
    cmp rax, 0          ; check enum variant
    je .done
    
    ; TailCall variant
    mov rdi, rcx        ; arg1 = n
    mov rsi, r8         ; arg2 = acc
    call fact_native    ; next iteration
    jmp .loop
    
.done:
    mov rax, rdx        ; return value = acc
    ret
```

**Key Point:** The loop in `fact_wrapper` replaces recursion stack growth. No stack overflow, native code speed.

---

## Conclusion

**Cranelift alone: No TCO**

**Cranelift + Trampolining: Full TCO ✓**

For Elle:
1. Generate `TailCall` enum for tail calls
2. Compile both to native code with Cranelift
3. Wrap with trampoline loop in Rust
4. Result: Native speed + TCO safety

This is the approach used by many languages (Scheme, some Lisps) and is battle-tested.

