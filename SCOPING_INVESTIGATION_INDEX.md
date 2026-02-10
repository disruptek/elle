# Elle Lisp Scoping Investigation - Complete Index

## Overview

This is a comprehensive investigation of a variable scoping bug in the Elle Lisp compiler where `set!` incorrectly treats locally-defined variables (via `define` inside `begin` blocks) as global variables.

**Start here:** Read `EXECUTIVE_SUMMARY.md` for a quick overview.

---

## Documentation Files

### 1. **EXECUTIVE_SUMMARY.md** (START HERE)
**Purpose:** Quick overview of the entire bug  
**Length:** ~300 lines  
**Contains:**
- Problem statement with example code
- Root cause in one sentence
- Three-part bug explanation
- Visual flow diagram showing where it breaks
- Why lambda parameters work
- The fix options at high level
- Specific code locations
- Conclusion

**Best for:** Understanding the bug quickly and at a high level

---

### 2. **elle_scoping_investigation.md** (DETAILED ANALYSIS)
**Purpose:** Full technical investigation with detailed analysis  
**Length:** ~395 lines  
**Contains:**
- Comprehensive root cause analysis
- How `scope_stack` works during parsing
- Critical problem: `begin` doesn't push a scope
- Why `define` doesn't register variables
- Why `set!` fails at lookup
- Detailed scope stack flow diagram
- AST structure explanation
- Why `lambda` works differently
- Current test cases
- Root cause chain
- Summary table with line numbers
- Implications and next steps

**Best for:** Deep understanding of the exact mechanics

---

### 3. **scope_stack_visualization.md** (VISUAL BREAKDOWN)
**Purpose:** Step-by-step visual trace of the bug  
**Length:** ~258 lines  
**Contains:**
- The problematic code
- Parse timeline with scope_stack state visualization
- Step-by-step execution trace
- Code snippets showing each step
- Comparison with lambda (why it works)
- Visual representation of the fix
- Core issue summary box diagram

**Best for:** Visual learners who want to see the state at each step

---

### 4. **code_references.md** (EXACT CODE LOCATIONS)
**Purpose:** All relevant code sections with line numbers  
**Length:** ~418 lines  
**Contains:**
- File structure
- Key code sections quoted
- Main entry point function
- The `begin` bug (exact code)
- Contrast with `lambda` (exact code)
- The `define` problem (exact code)
- The `set!` manifestation (exact code)
- The `let*` correct example (exact code)
- AST type definitions
- Scope stack type definition
- Tests that would fail
- Function call graph
- Summary table
- The fix locations

**Best for:** Developers who need exact line numbers and code snippets

---

## Quick Navigation

### By Reading Level

**Beginner/Manager:**
1. Read: EXECUTIVE_SUMMARY.md
2. Done!

**Developer (Overview):**
1. Read: EXECUTIVE_SUMMARY.md (sections 1-3)
2. Skim: code_references.md (Summary Table section)
3. Read: scope_stack_visualization.md (Visual Flow)

**Developer (Deep Dive):**
1. Read: EXECUTIVE_SUMMARY.md (all)
2. Read: elle_scoping_investigation.md (all)
3. Read: scope_stack_visualization.md (all)
4. Reference: code_references.md (as needed)

**Developer (Implementation):**
1. Read: code_references.md (all)
2. Read: EXECUTIVE_SUMMARY.md (Fix section)
3. Read: elle_scoping_investigation.md (Implications)
4. Look at: scope.rs module for potential infrastructure

---

### By Question

**Q: What exactly is the bug?**  
A: EXECUTIVE_SUMMARY.md - "The Problem" section

**Q: Why does it happen?**  
A: EXECUTIVE_SUMMARY.md - "Root Cause" section

**Q: Where is the bug in the code?**  
A: code_references.md - Search for line numbers

**Q: What would the fix look like?**  
A: EXECUTIVE_SUMMARY.md - "The Fix" section  
OR: scope_stack_visualization.md - "What SHOULD Happen"

**Q: Can you show me the bug in action step-by-step?**  
A: scope_stack_visualization.md - "Parse Timeline"

**Q: How does the scope_stack work normally?**  
A: elle_scoping_investigation.md - "How scope_stack Works"

**Q: Why do lambda parameters work but defines don't?**  
A: EXECUTIVE_SUMMARY.md - "Why Lambda Parameters Work"  
AND: elle_scoping_investigation.md - "Why lambda Works Differently"

**Q: What code needs to be changed?**  
A: code_references.md - "The Fix Locations" section

---

## Key Findings Summary

| Finding | Details |
|---------|---------|
| **Bug Type** | Variable scoping / scope tracking |
| **Severity** | High - breaks common patterns |
| **Scope** | Affects `begin` blocks with `define` and `set!` |
| **Root Cause** | `begin` doesn't create scope, `define` doesn't register vars, `set!` treats unfound as global |
| **Affected Code** | converters.rs lines 442-448, 535-542, 831-859 |
| **Impact** | Any code using local `define` inside `begin` + `set!` breaks |
| **Potential Fix** | Make `begin` push scope, have `define` register variables, or use two-pass analysis |

---

## Code Location Reference

**Quick lookup table for urgent cases:**

| Issue | File | Lines | Issue |
|-------|------|-------|-------|
| `begin` doesn't push scope | converters.rs | 442-448 | ⚠️ BUG |
| `lambda` does it right | converters.rs | 468-477 | ✓ Reference |
| `define` doesn't update scope | converters.rs | 535-542 | ⚠️ BUG |
| `set!` treats unfound as global | converters.rs | 840-851 | ⚠️ CONSEQUENCE |
| `let*` does it correctly | converters.rs | 682, 701-703 | ✓ Reference |
| Set AST node definition | ast.rs | 84-90 | Structure |
| Scope infrastructure | scope.rs | 1-285 | Unused |

---

## Test Case Example

The following test case would **FAIL** with the current implementation:

```rust
#[test]
fn test_set_bang_on_locally_defined_variable() {
    let mut eval = ScopeEval::new();
    let result = eval.eval(
        "((lambda () (begin (define x 0) (set! x 42) x)))"
    ).unwrap();
    assert_eq!(result, Value::Int(42));
    // Currently fails because set! x treats x as global
}
```

This test appears in none of the existing test files!

---

## The Three-Part Bug Explained Simply

1. **Problem #1: `begin` doesn't create a scope**
   - Location: converters.rs lines 442-448
   - Should: Call `scope_stack.push(...)` before processing
   - Actually: Doesn't call push, no scope created

2. **Problem #2: `define` doesn't register variables**
   - Location: converters.rs lines 535-542
   - Should: Add variable to `scope_stack`
   - Actually: Just creates an AST node, doesn't register

3. **Problem #3: `set!` assumes unfound = global**
   - Location: converters.rs lines 840-851
   - Should: Know about locally-defined variables
   - Actually: Only knows about lambda parameters, treats others as global

---

## Proposed Solutions

### Option A: Extend `begin` with Scoping
- Modify `begin` to push/pop scope (like lambda)
- Modify `define` to update scope when inside block scope
- Pros: Matches how `let*` already works
- Cons: Requires tracking where defines belong

### Option B: Two-Pass Analysis  
- First pass: Collect all defines in a block
- Second pass: Process normally with variables in scope
- Pros: Cleaner separation
- Cons: Requires two iterations

### Option C: Use Existing Infrastructure
- Replace `Vec<Vec<SymbolId>>` with `CompileScope` from scope.rs
- Use the existing `define_local()` method
- Pros: Leverages existing infrastructure
- Cons: Requires refactoring converters.rs

---

## Implementation Checklist

If implementing the fix:

- [ ] Choose which fix approach (A, B, or C)
- [ ] Modify `begin` handling (lines 442-448)
- [ ] Modify `define` handling (lines 535-542)
- [ ] Test with failing case from test_set_bang_on_locally_defined_variable
- [ ] Run all existing scoping tests (tests/integration/scoping.rs)
- [ ] Add regression test
- [ ] Verify `let*` still works (uses similar pattern)
- [ ] Check for other constructs that need similar fixes

---

## Related Code Worth Examining

1. **`let*` implementation** (converters.rs 649-776)
   - Shows how to properly update scope during iteration
   - Uses `scope_stack.last_mut().push(var)` pattern

2. **`lambda` implementation** (converters.rs 458-532)
   - Shows proper scope push/pop pattern
   - Reference for correct behavior

3. **Symbol resolution** (converters.rs 346-360)
   - Shows how scope_stack is searched
   - Current logic depends on proper scope management

4. **Scope module** (scope.rs 1-285)
   - Provides `CompileScope` and `VariableBinding` structures
   - Not currently used in converters.rs but could be

---

## Document Statistics

| Document | Lines | Words (approx) | Focus |
|----------|-------|----------------|-------|
| EXECUTIVE_SUMMARY.md | 294 | 2,800 | Overview & quick ref |
| elle_scoping_investigation.md | 395 | 3,200 | Deep technical |
| scope_stack_visualization.md | 258 | 2,000 | Visual/step-by-step |
| code_references.md | 418 | 2,500 | Code & references |
| **Total** | **1,365** | **10,500** | Complete analysis |

---

## How to Use This Investigation

### For Bug Confirmation
1. Read EXECUTIVE_SUMMARY.md - "The Problem"
2. Run the test case example
3. Confirm it fails as described

### For Understanding
1. Read EXECUTIVE_SUMMARY.md (all)
2. Read scope_stack_visualization.md (Parse Timeline)
3. Reference code_references.md as needed

### For Implementation
1. Read elle_scoping_investigation.md (Root Cause & Implications)
2. Read EXECUTIVE_SUMMARY.md (The Fix section)
3. Reference code_references.md (Code Locations)
4. Review scope.rs for potential infrastructure
5. Follow Implementation Checklist

### For Presentation
1. Use EXECUTIVE_SUMMARY.md for overview
2. Use scope_stack_visualization.md for step-by-step explanation
3. Reference code_references.md for code examples

---

## Key Takeaways

1. **The bug is real and significant** - affects any code using local `define` + `set!`
2. **It's a parse-time vs runtime mismatch** - compiler doesn't know about runtime scoping
3. **The fix is localized** - changes needed in 3 places in converters.rs
4. **There are proven patterns** - `let*` and `lambda` show how to do it correctly
5. **Infrastructure exists** - scope.rs has structures that could help
6. **The solution is not complex** - requires scope stack management, like let*

---

## Quick Links

- **Start here:** EXECUTIVE_SUMMARY.md
- **Visual explanation:** scope_stack_visualization.md
- **Full details:** elle_scoping_investigation.md
- **Code locations:** code_references.md

---

## Feedback and Questions

For questions about this investigation:
- Check EXECUTIVE_SUMMARY.md "By Question" section
- Refer to code_references.md for exact line numbers
- Look at elle_scoping_investigation.md for detailed explanation
