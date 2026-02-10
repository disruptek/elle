# Phase 9: Full Try/Catch Integration

## Current State (Phases 1-8)

### What Works
- Try/catch syntax is parsed into AST: `Expr::Try { body, catch, finally }`
- Handler-case is fully compiled with exception handling bytecode
- Exception hierarchy and matching works (Phase 7)
- Introspection primitives work (Phase 8)
- Division by zero creates Condition objects internally

### What Doesn't Work Yet
- Try/catch currently just executes the body and skips the catch clause
- No exception catching happens - exceptions propagate without being caught
- The catch variable is not bound to the caught exception

## Architecture

### Bytecode Instructions Available (from handler-case)
- `PushHandler`: Set up exception handler frame
- `PopHandler`: Clean up handler on success
- `CheckException`: Check if exception was caught
- `MatchException`: Match exception ID with inheritance
- `BindException`: Bind exception to variable
- `ClearException`: Clear exception state
- `JumpIfFalse`: Conditional jump

### Try/Catch Compilation Strategy

The key insight is that try/catch should compile to essentially the same bytecode as handler-case:

```
try body
(catch e handler)
```

Should compile to (conceptually):

```
(handler-case body (0 (e) handler))
```

Where `0` is a wildcard or "any exception" ID. But better would be to:
1. Execute body with handler frame
2. On exception, bind to catch variable
3. Execute handler
4. On no exception, skip handler

## Implementation Plan

### Step 1: Modify Try Compilation (compile.rs)
- Use PushHandler to set up frame (like handler-case)
- Compile body as protected code
- Pop handler on success
- Jump past handler on success
- Emit CheckException after catching
- For catch clause: bind exception to variable
- Execute catch handler
- Clear exception

### Step 2: Handle Finally (compile.rs)
- Finally should execute in both success and failure paths
- Current code structure has finally after body - needs adjustment

### Step 3: Test Integration
- Test try without catch
- Test try with catch  
- Test try with finally
- Test try with catch and finally
- Test nested try expressions
- Test that exceptions are actually caught

### Step 4: Error Cases
- Test unhandled exceptions still propagate
- Test catch type matching (if we add types to try)
- Test multiple try blocks

## Modified Try Compilation Pseudocode

```rust
Expr::Try { body, catch, finally } => {
    // If no catch clause, just compile body + finally
    if catch.is_none() {
        self.compile_expr(body, false);
        if let Some(finally_expr) = finally {
            self.bytecode.emit(Instruction::Dup);
            self.compile_expr(finally_expr, false);
            self.bytecode.emit(Instruction::Pop);
        }
        return;
    }
    
    // With catch clause: use handler-case-like structure
    let (catch_var, catch_handler) = catch.as_ref().unwrap();
    
    // PushHandler with exception frame
    self.bytecode.emit(Instruction::PushHandler);
    let handler_offset_pos = self.bytecode.current_pos();
    self.bytecode.emit_i16(0); // Placeholder
    self.bytecode.emit_i16(-1); // No finally support in PushHandler yet
    
    // Compile body
    self.compile_expr(body, tail);
    
    // On success: pop handler
    self.bytecode.emit(Instruction::PopHandler);
    
    // Jump past catch
    self.bytecode.emit(Instruction::Jump);
    let end_jump = self.bytecode.current_pos();
    self.bytecode.emit_i16(0);
    
    // Handler code
    let handler_code_offset = self.bytecode.current_pos() as i16;
    self.bytecode.patch_jump(handler_offset_pos, handler_code_offset);
    
    // Bind exception to catch variable
    self.bytecode.emit(Instruction::BindException);
    let var_idx = self.bytecode.add_constant(Value::Symbol(*catch_var));
    self.bytecode.emit_u16(var_idx);
    
    // Execute handler
    self.compile_expr(catch_handler, tail);
    
    // Patch end jump
    let final_end = self.bytecode.current_pos() as i16;
    self.bytecode.patch_jump(end_jump, final_end);
    
    // Clear exception
    self.bytecode.emit(Instruction::ClearException);
    
    // Finally (on both paths)
    if let Some(finally_expr) = finally {
        self.bytecode.emit(Instruction::Dup);
        self.compile_expr(finally_expr, false);
        self.bytecode.emit(Instruction::Pop);
    }
}
```

## Testing Strategy

1. **Basic catch**: (try (/ 1 0) (catch e "error"))
2. **No exception**: (try (+ 1 1) (catch e 0)) -> 2
3. **Multiple catches**: (try body (catch e1 h1) (catch e2 h2))
4. **Nested try**: (try (try body) (catch e 0))
5. **Exception propagation**: No catch -> exception propagates
6. **Finally execution**: Always runs regardless of exception
7. **Introspection in handler**: (try body (catch e (exception-id e)))

## Files to Modify
- `src/compiler/compile.rs`: Try compilation
- `tests/integration/exception_handling.rs`: New tests
- `examples/exception-handling.lisp`: Phase 9 examples
