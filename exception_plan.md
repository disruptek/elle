# Elle Condition System Implementation Plan

## Design Summary

Based on user decisions, the implementation will feature:
- ✅ Full CL-style condition system with structured exception types
- ✅ Restarts for recovery options (low-level defines, high-level invokes)
- ✅ Explicit `define-exception` declarations with fields
- ✅ Three signaling levels: `error`, `warn`, `signal`
- ✅ Programmatic-only (no interactive restart selection)
- ✅ Complete implementation in one phase

## Architecture Overview

### 1. Compile-Time: Exception ID Table

**Location**: `src/compiler/exception_registry.rs` (new)

The compiler builds a registry of all exception types:

```rust
pub struct ExceptionRegistry {
    exceptions: HashMap<String, ExceptionInfo>,
    next_id: u32,
}

pub struct ExceptionInfo {
    id: u32,
    name: String,
    fields: Vec<FieldInfo>,
    parent: Option<String>, // for inheritance
}

pub struct FieldInfo {
    name: String,
    reader_id: u32, // symbol ID for field accessor
}
```

**Process**:
1. Parser encounters `(define-exception my-error (field1 field2))`
2. Compiler registers it with auto-incrementing ID
3. Compiler generates field accessor symbols
4. Final bytecode includes exception ID table as constant

### 2. Runtime: Condition Objects

**Location**: `src/value.rs` (modify) + new `src/value/condition.rs`

```rust
pub struct Condition {
    pub exception_id: u32,
    pub fields: HashMap<u32, Value>, // field_id -> value
    pub backtrace: Option<Backtrace>,
}

pub enum Value {
    // ... existing variants
    Condition(Rc<Condition>),
}
```

**Key differences from current Exception**:
- Exception ID instead of string message
- Structured fields (not just message + data)
- Field access via symbol-based getters
- Backtrace support for debugging

### 3. Signaling & Handling

**Location**: `src/vm/condition.rs` (new, replaces exception.rs)

Three signaling primitives:

```lisp
(signal condition-id field1 field2 ...)  ; Silent - just propagate
(warn condition-id field1 field2 ...)    ; Warning - print if unhandled
(error condition-id field1 field2 ...)   ; Error - debugger if unhandled
```

Handler macros:

```lisp
(handler-case (protected-code)
  (exception-type (var) handler-code))

(handler-bind ((exception-type handler-fn))
  protected-code)

(restart-case (protected-code)
  (restart-name (arg1 arg2) restart-code))
```

### 4. Exception Propagation Model

**VM Changes**: `src/vm/mod.rs`

Current problem: Errors immediately return `Err(String)` up the call stack.

Solution: Three-level exception context:

```rust
pub struct ExceptionContext {
    pub current_exception: Option<Rc<Condition>>,
    pub handlers: Vec<Handler>,      // handler-bind stack
    pub restarts: Vec<Restart>,      // restart-case stack
}

pub struct Handler {
    condition_id: u32,
    handler_fn: Value, // callable that handles condition
    preserves_stack: bool, // true for handler-bind, false for handler-case
}

pub struct Restart {
    name: u32, // symbol ID for restart name
    restart_fn: Value, // callable that performs restart
    arguments: Vec<(u32, Value)>, // arg names and values
}
```

**Execution Flow**:

1. Instruction encounters error (e.g., division by zero, type mismatch)
2. Create Condition object from error
3. Check `handlers` stack from most recent to oldest
4. If handler found:
   - Call handler function with condition
   - Handler can `invoke-restart` or return normally
5. If `invoke-restart` called:
   - Find named restart on `restarts` stack
   - Execute restart code with provided arguments
   - Continue from restart point
6. If no handler matches:
   - Check if `error` or `warn` was used
   - Propagate up call stack or print + continue

### 5. Bytecode Instructions

**New Instructions** (in addition to existing PushHandler/PopHandler):

```rust
pub enum Instruction {
    // ... existing
    
    // Signaling
    Signal,           // arg: condition_id (top of stack has values)
    Warn,
    Error,
    
    // Handler management
    PushHandlerBind,  // arg: handler_fn_idx, condition_id
    PushHandlerCase,  // arg: handler_fn_idx, condition_id, unwind_depth
    PopHandler,
    
    // Restart management
    PushRestart,      // arg: restart_name_id, restart_fn_idx, arg_count
    PopRestart,
    InvokeRestart,    // arg: restart_name_id
    
    // Handler/restart stack inspection
    FindRestart,      // arg: restart_name_id (pushes restart onto stack)
}
```

### 6. Compilation Strategy

**File**: `src/compiler/compile.rs`

For try/catch/finally (maps to handler-case):

```lisp
(try body (catch e handler) (finally cleanup))
```

Compiles to:

```
PushHandlerCase condition_type handler_fn unwind_depth
[body bytecode]
[if no exception: Jump to after-handler]
[handler code]
PopHandler
[finally code]
ClearException
```

For restart-case:

```lisp
(restart-case body
  (restart1 (arg) code1)
  (restart2 (arg) code2))
```

Compiles to:

```
PushRestart restart1_id restart1_fn 1
PushRestart restart2_id restart2_fn 1
[body bytecode]
PopRestart
PopRestart
```

### 7. Standard Exceptions

Define built-in exception hierarchy in compiler:

```
(define-exception condition)
  (define-exception error :parent condition)
    (define-exception type-error :parent error (expected actual))
    (define-exception division-by-zero :parent error (dividend divisor))
    (define-exception undefined-variable :parent error (variable))
  (define-exception warning :parent condition)
```

Compiler auto-generates IDs and registrations.

### 8. Integration Points

**Primitives** (`src/primitives/`):
- `prim_divide`: When divisor is 0, signal division-by-zero
- `prim_car`: When not pair, signal type-error
- All primitives that can fail: create appropriate conditions

**VM Execution** (`src/vm/mod.rs`):
- Wrap handler calls in match that catches errors
- On error: create condition, search handler stack
- If no handler: propagate or signal depending on signaling function

**Compiler** (`src/compiler/compile.rs`):
- `define-exception` creates registry entries
- `throw/error/warn/signal` emit bytecode
- `handler-case/handler-bind/restart-case` emit handler setup

## Implementation Phases (All in One Release)

### Phase 1: Foundation (2-3 days)
- [ ] ExceptionRegistry infrastructure
- [ ] Condition value type and field access
- [ ] Basic signaling (signal, warn, error functions)
- [ ] Handler-case basic execution
- [ ] Standard exceptions (type-error, division-by-zero, etc.)

### Phase 2: Restarts (1-2 days)
- [ ] Restart-case compilation and bytecode
- [ ] Restart stack management
- [ ] invoke-restart primitive
- [ ] find-restart primitive
- [ ] Restart argument passing

### Phase 3: Handler-bind & Advanced (1-2 days)
- [ ] Handler-bind (non-unwinding handlers)
- [ ] Multiple handlers on stack
- [ ] Exception propagation through call stack
- [ ] Backtrace capture in conditions
- [ ] Field access via getters

### Phase 4: Integration (1 day)
- [ ] Update all primitives to signal conditions
- [ ] Fix existing tests (remove string exceptions)
- [ ] Enable ignored exception tests
- [ ] Performance optimization

### Phase 5: Polish (1 day)
- [ ] Documentation
- [ ] Error messages with condition details
- [ ] Final testing and edge cases

**Total Estimated Effort**: 6-8 days

## Testing Strategy

1. **Unit Tests**:
   - Condition object creation and field access
   - Handler matching and invocation
   - Restart definition and invocation
   - Signal/warn/error behavior

2. **Integration Tests** (enable ignored tests):
   - test_try_catch_catches_thrown_exception
   - test_try_catch_exception_with_data
   - test_nested_try_catch_*
   - test_catch_handler_can_throw
   - test_finally_always_executes_with_exception

3. **Edge Cases**:
   - Exception in handler
   - Exception in restart
   - Nested handlers
   - Restart not found
   - Multiple matching handlers
   - Finally with exception

## Migration from Current System

**Current**: 
- `Value::Exception(Rc<Exception>)` with message string
- Simple `throw/catch` parsing

**New**:
- `Value::Condition(Rc<Condition>)` with structured fields
- `define-exception` declarations
- signal/warn/error/throw with exception IDs
- handler-case/handler-bind/restart-case macros

**Breaking Changes**:
- Remove string-based exceptions entirely
- All existing `(throw "message")` becomes `(throw my-error)`
- Update all exception tests

## Key Design Decisions Locked In

✅ Structured exception types with fields (CL-style)
✅ Full restart system (low-level recovery paths)
✅ Explicit exception declarations required
✅ Three signaling levels (error, warn, signal)
✅ Programmatic handlers (no interactive REPL selection)
✅ Complete implementation in single coordinated phase

---

## Next Steps

Ready to start implementation? Should I:
1. Create detailed specification for each phase?
2. Sketch pseudocode for key algorithms?
3. Begin Phase 1 implementation?
