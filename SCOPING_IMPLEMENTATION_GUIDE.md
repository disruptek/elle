# Local Variable Scoping Implementation Guide

## Quick Navigation

This directory contains a complete analysis of the Elle compiler's variable scoping architecture and a blueprint for implementing recursive lambdas.

### Documentation Files

**Start here based on your needs:**

1. **[LOCAL_VARIABLE_SCOPING_ANALYSIS.md](LOCAL_VARIABLE_SCOPING_ANALYSIS.md)** - Comprehensive Technical Analysis
   - Best for: Understanding the current architecture in depth
   - Contains: 12 detailed sections on compiler structure, VM instructions, closure handling
   - Length: 622 lines of detailed analysis
   - Key sections:
     - Section 1: Compiler Structure
     - Section 2: VM Instruction Set
     - Section 3: Lambda/Function Handling
     - Section 6: Current Workarounds
     - Section 7: Gaps and Issues Analysis
     - Section 8-10: Recommendations and Implementation Path

2. **[ARCHITECTURE_DIAGRAMS.md](ARCHITECTURE_DIAGRAMS.md)** - Visual Architecture Guide
   - Best for: Visual learners and understanding data flow
   - Contains: ASCII diagrams and flow charts
   - Length: 680 lines of visual representations
   - Key diagrams:
     - Symbol Resolution Flow (current broken path)
     - Compiler State During Compilation
     - Stack Layout Comparison
     - Variable Resolution Process
     - Current vs Proposed System Architecture
     - Execution Flow Comparisons

3. **[RECURSIVE_LAMBDA_EXAMPLES.md](RECURSIVE_LAMBDA_EXAMPLES.md)** - Practical Implementation Guide
   - Best for: Understanding failures and implementing fixes
   - Contains: Code examples, test cases, and fix proposals
   - Length: 634 lines with concrete examples
   - Key sections:
     - Section 1-4: Test Cases (what works, what fails)
     - Section 5: Detailed Failure Analysis
     - Section 6: Proposed Fix with Code
     - Section 7: Testing Plan
     - Section 8: Before/After Bytecode Comparison

---

## Quick Summary

### The Problem

The Elle compiler cannot execute recursive lambdas (nested functions that call themselves). This is a significant limitation for functional programming patterns.

**Current Status:**
- ✓ Top-level recursive functions work
- ✗ Recursive lambdas fail
- ✗ Mutual recursion fails
- ✗ Nested function scoping is broken

### Root Cause

Line 689 of `src/compiler/compile.rs`:

```rust
pub fn value_to_expr(value: &Value, symbols: &mut SymbolTable) -> Result<Expr, String> {
    match value {
        Value::Symbol(id) => {
            Ok(Expr::GlobalVar(*id))  // ← ALL symbols become global!
        }
    }
}
```

**The issue:** All symbols are treated as global variables at parse time, with no tracking of local scope, function parameters, or captured variables.

### Why It Matters

1. **Functional Programming Support**: Can't define recursive helper functions
2. **Scoping Correctness**: Variable resolution is not lexically scoped
3. **Closure Completeness**: Lambdas can't reference themselves
4. **Advanced Patterns**: Mutual recursion, Y-combinator patterns impossible

### Solution Overview

Add proper scope tracking to the compiler:

1. **Compile-Time**: Track variables with depth and index information
2. **Variable Binding**: Include function name in its own captures
3. **Code Generation**: Use LoadLocal/LoadUpvalue instead of LoadGlobal
4. **Runtime**: Maintain proper frame offsets for variable access

**Impact:** Fully backward compatible - no breaking changes!

---

## Implementation Roadmap

### Phase 1: Analysis (COMPLETED ✓)
- [x] Understand current architecture
- [x] Identify breaking points
- [x] Document issues
- [x] Create visualizations

**Deliverables:** The three documents you're reading now

### Phase 2: Design (READY)
- [ ] Design scope tracking system
- [ ] Plan compiler modifications
- [ ] Design VM frame management
- [ ] Plan testing strategy

**Next Steps:**
1. Review the three documents thoroughly
2. Use ARCHITECTURE_DIAGRAMS.md to understand data flow
3. Use RECURSIVE_LAMBDA_EXAMPLES.md for implementation details

### Phase 3: Implementation (PLANNED)
- [ ] Add scope stack to Compiler
- [ ] Implement variable tracking
- [ ] Modify lambda compilation
- [ ] Update free variable analysis
- [ ] Enhance runtime execution
- [ ] Add comprehensive tests

**Estimated Effort:** 2-3 weeks (medium priority changes)

### Phase 4: Testing (PLANNED)
- [ ] Unit tests for scope tracking
- [ ] Integration tests for recursive lambdas
- [ ] Regression tests for existing functionality
- [ ] Performance benchmarks

---

## Key Files to Modify

| File | Change | Complexity | Priority |
|------|--------|-----------|----------|
| `src/compiler/compile.rs` | Add scope stack, modify lambda compilation | Medium | High |
| `src/compiler/analysis.rs` | Track depth/index in free vars | Low | High |
| `src/compiler/ast.rs` | May need minor updates | Low | Medium |
| `src/compiler/bytecode.rs` | Possibly add new instructions | Low | Low |
| `src/vm/variables.rs` | Handle depth-aware variable access | Low | Medium |
| `src/vm/mod.rs` | Frame offset management | Medium | Medium |
| `src/value.rs` | Enhance Closure structure | Low | Low |

---

## Document Reading Guide

### For Compiler Engineers

**Read in this order:**
1. ARCHITECTURE_DIAGRAMS.md - Sections 1-3 (understand current flow)
2. LOCAL_VARIABLE_SCOPING_ANALYSIS.md - Sections 1-5 (understand components)
3. LOCAL_VARIABLE_SCOPING_ANALYSIS.md - Sections 7-10 (understand gaps and fixes)
4. RECURSIVE_LAMBDA_EXAMPLES.md - All sections (understand test cases)

**Focus areas:**
- How symbols are resolved (line 689)
- Lambda compilation process (lines 106-143)
- Free variable analysis (analysis.rs)
- VM execution for closures

### For Runtime/VM Engineers

**Read in this order:**
1. ARCHITECTURE_DIAGRAMS.md - Section 3 (stack layout)
2. LOCAL_VARIABLE_SCOPING_ANALYSIS.md - Sections 2, 4, 5 (instructions, runtime)
3. RECURSIVE_LAMBDA_EXAMPLES.md - Section 8 (bytecode comparison)
4. ARCHITECTURE_DIAGRAMS.md - Sections 6-7 (execution flow)

**Focus areas:**
- LoadLocal, LoadGlobal, LoadUpvalue instructions
- Closure structure and environment
- Function call mechanism
- Stack management for nested calls

### For Language Designers

**Read in this order:**
1. LOCAL_VARIABLE_SCOPING_ANALYSIS.md - Executive Summary + Section 7
2. ARCHITECTURE_DIAGRAMS.md - Current vs Proposed comparison
3. LOCAL_VARIABLE_SCOPING_ANALYSIS.md - Section 8 (recommendations)
4. RECURSIVE_LAMBDA_EXAMPLES.md - Section 1 (test cases)

**Focus areas:**
- Scope system design
- Variable binding semantics
- Function definition semantics
- Language feature limitations

### For Students/Learning

**Read in this order:**
1. ARCHITECTURE_DIAGRAMS.md - All sections (visual understanding)
2. RECURSIVE_LAMBDA_EXAMPLES.md - Section 1 (see failing examples)
3. LOCAL_VARIABLE_SCOPING_ANALYSIS.md - Section 1-3 (understand components)
4. RECURSIVE_LAMBDA_EXAMPLES.md - Section 6 (see the fix)

**Best for:**
- Understanding compiler architecture
- Learning about scope and closures
- Studying functional language implementation

---

## Critical Insights

### The Circular Dependency Problem

When compiling:
```lisp
(define factorial
  (lambda (n) (factorial (- n 1))))
```

Current code tries to:
1. `LoadGlobal(factorial)` - but it's not defined yet!
2. Create closure with that value
3. Store closure to global as `factorial`

This creates a circular dependency that only works at top-level due to pre-registration.

**Solution:** Include the function in its own captured environment via closure object self-reference.

### The Scope Chain Problem

When compiling nested lambdas:
```lisp
(lambda (x)
  (lambda (y)
    (inner-lambda-here y)))
```

Current code can't:
- Track that `x` is from outer scope
- Distinguish parameter vs captured vs global
- Emit correct LoadLocal vs LoadUpvalue instructions

**Solution:** Maintain a scope stack during compilation that tracks depth and binding information.

### The Frame Offset Problem

When executing nested calls:
```
Stack: [main_var1, main_var2, lambda1_param, lambda1_local, lambda2_param, ...]
```

Current code can't:
- Know where each frame's variables start
- Correctly resolve LoadLocal indices
- Support recursive calls with proper parameter isolation

**Solution:** Add frame offset tracking to VM execution and maintain parent frame chain.

---

## Quick Reference: Key Numbers

| Aspect | Value | Notes |
|--------|-------|-------|
| All symbols default scope | Global | Line 689 of compile.rs |
| Compiler state for lambdas | None | No shared scope tracking |
| Parameter depth tracking | Missing | Should be 0 for current scope |
| Upvalue max supported | 1 level | Single closure.env, no chain |
| Stack management | Direct indexing | No frame offset awareness |
| Pre-registration workaround | Top-level only | Doesn't help nested functions |

---

## Implementation Checklist

When you're ready to implement, use this checklist:

### Pre-Implementation
- [ ] Read all three documents thoroughly
- [ ] Understand symbol resolution flow
- [ ] Understand closure/capture mechanism
- [ ] Understand VM execution model
- [ ] Write test cases for recursive lambdas

### Compiler Changes
- [ ] Add Scope struct to track variables
- [ ] Add scope stack to Compiler
- [ ] Implement push/pop scope methods
- [ ] Implement register_variable method
- [ ] Implement lookup_variable method
- [ ] Modify compile_expr for Lambda
- [ ] Modify compile_expr for Var
- [ ] Update free variable analysis

### VM Changes
- [ ] Understand current execute_bytecode
- [ ] Plan frame management strategy
- [ ] Implement frame offset tracking
- [ ] Update LoadLocal handling
- [ ] Update LoadUpvalue handling
- [ ] Test with nested calls

### Testing
- [ ] Test simple recursion
- [ ] Test nested recursion
- [ ] Test mutual recursion
- [ ] Test with multiple captures
- [ ] Regression test all existing code
- [ ] Performance benchmarking

### Documentation
- [ ] Update code comments
- [ ] Add examples to docstrings
- [ ] Update ROADMAP.md
- [ ] Create test documentation

---

## Additional Resources

### Related Files in Codebase
- `src/main.rs:60` - Current workaround (pre-registration)
- `src/compiler/analysis.rs` - Free variable analysis
- `src/vm/closure.rs` - Closure handling
- `src/vm/variables.rs` - Variable access handlers
- `tests/integration/core.rs` - Test cases

### Relevant Lisp Concepts
- Lexical scoping
- Closures and capture
- Closure environment chains
- Recursive functions
- Mutual recursion
- Y-combinator (advanced)

### Compiler Concepts
- Symbol tables
- Scope chains
- Upvalues/free variables
- Closure conversion
- Lambda lifting (not needed here)

---

## Next Steps

1. **Today**: Read ARCHITECTURE_DIAGRAMS.md for visual understanding
2. **Today**: Review RECURSIVE_LAMBDA_EXAMPLES.md test cases
3. **Tomorrow**: Deep dive into LOCAL_VARIABLE_SCOPING_ANALYSIS.md sections 1-5
4. **Next Day**: Study sections 7-10 on gaps and recommendations
5. **Implementation**: Start with scope tracking in Compiler struct

---

## Questions to Answer While Reading

Use these questions to guide your reading:

1. Why do recursive lambdas fail?
2. What is in a Closure's `env` field and why?
3. How does the current pre-registration workaround work?
4. What is the difference between parameters and captured variables?
5. How would you track variable scope during compilation?
6. What information needs to be in the AST `Var` expression?
7. How would you modify `MakeClosure` to include self-reference?
8. What frame information would the VM need for proper execution?
9. How would you handle mutual recursion?
10. Why is this implementation backward compatible?

---

## Success Criteria

Your implementation is complete when:

- [ ] All test cases in RECURSIVE_LAMBDA_EXAMPLES.md pass
- [ ] Simple recursive lambda works: `((lambda (n) (if (<= n 0) 0 (recfn (- n 1)))) 5)`
- [ ] Nested recursion works: lambda inside lambda can call itself
- [ ] Mutual recursion works: two lambdas can call each other
- [ ] All existing tests still pass (backward compatible)
- [ ] No performance regression
- [ ] Code has comprehensive comments
- [ ] Documentation is updated

---

## Summary

You now have everything needed to implement local variable scoping for recursive functions in Elle. The three documents provide:

1. **Complete architectural analysis** (LOCAL_VARIABLE_SCOPING_ANALYSIS.md)
2. **Visual understanding** (ARCHITECTURE_DIAGRAMS.md)
3. **Practical implementation guide** (RECURSIVE_LAMBDA_EXAMPLES.md)

Start with the diagrams to understand the flow, then use the detailed analysis and examples to implement the fix. The implementation is well-scoped, backward compatible, and has clear success criteria.

Good luck with the implementation!
