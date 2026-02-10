# Key Files Reference - Resident Compiler Error Handling

## Quick File Location Reference

### Core Resident Compiler (resident_compiler/)
```
/home/adavidoff/git/elle3/src/resident_compiler/
├── compiler.rs          (234 lines)  - Main ResidentCompiler implementation
├── compiled_doc.rs      (67 lines)   - CompiledDocument result type
├── cache.rs             (Not analyzed - caching layer)
└── mod.rs               (15 lines)   - Module exports
```

### Error Infrastructure (error/)
```
/home/adavidoff/git/elle3/src/error/
├── mod.rs               (338 lines)  - Error module, LocationMap type alias
├── runtime.rs           (53 lines)   - RuntimeError with location
├── sourceloc.rs         (7 lines)    - SourceLoc re-export
├── types.rs             (Not fully analyzed)
└── builders.rs          (Not fully analyzed)
```

### Source Location Tracking (reader/)
```
/home/adavidoff/git/elle3/src/reader/
├── token.rs             (119 lines)  - SourceLoc + TokenWithLoc definitions
├── lexer.rs             (Not fully analyzed - generates SourceLoc)
└── mod.rs               (Exports)
```

### Compiler Infrastructure (compiler/)
```
/home/adavidoff/git/elle3/src/compiler/
├── ast.rs               (223 lines)  - ExprWithLoc definition
├── compile/mod.rs       (1150+ lines) - Bytecode compilation, LocationMap generation
├── linter/
│   ├── mod.rs           (262 lines)  - Linter engine, ast traversal
│   ├── diagnostics.rs   (120 lines)  - Diagnostic + Severity types
│   └── rules.rs         (194 lines)  - Linting rules (naming, arity checks)
├── symbol_index.rs      (Not fully analyzed - IDE features)
└── ... other compiler modules
```

### Runtime (vm/)
```
/home/adavidoff/git/elle3/src/vm/
└── core.rs              (Not fully analyzed - VM with LocationMap storage)
```

## File Details by Purpose

### 1. Error Type Definitions

#### compiler.rs (resident_compiler/)
- **CompileError**: Simple string-based error wrapper
- **ResidentCompiler::compile_text()**: Main compilation entry point
- Error wrapping at 4 stages: Lexing, Parsing, Conversion, File I/O

```
Lines 11-22:  CompileError definition
Lines 88-105: Lexer error handling
Lines 111-113: Parser error handling
Lines 117-119: Conversion error handling
Lines 62-64:  File I/O error handling
```

#### runtime.rs (error/)
- **RuntimeError**: Runtime errors with optional location and context
- Display formatting: "Error at file:line:col: message"

```
Lines 7-50: RuntimeError definition and Display trait
Lines 14-34: Builder methods (with_location, with_context)
```

#### diagnostics.rs (compiler/linter/)
- **Diagnostic**: Structured linting diagnostics
- **Severity**: Info, Warning, Error levels
- Human-readable formatting with suggestions

```
Lines 6-22:   Severity enum
Lines 24-50:  Diagnostic structure and creation
Lines 58-84:  format_human() display method
```

### 2. Location Tracking

#### token.rs (reader/)
- **SourceLoc**: Core location structure (file, line, col)
- Display format: `file:line:col`
- Factory methods: `new()`, `from_line_col()`, `start()`
- **TokenWithLoc**: Token + location pair

```
Lines 1-42:   SourceLoc definition and methods
Lines 44-48:  TokenWithLoc definition
Lines 10-14:  SourceLoc Display trait
Lines 16-42:  SourceLoc factory methods
```

#### ast.rs (compiler/)
- **ExprWithLoc**: AST node + optional location
- `format_loc()`: Display location as "line:col"

```
Lines 6-22:   ExprWithLoc definition
Lines 16-21:  format_loc() method
```

#### mod.rs (error/)
- **LocationMap**: Type alias `HashMap<usize, SourceLoc>`
- Maps bytecode instruction indices to source locations

```
Lines 21-26:  LocationMap definition and documentation
```

### 3. Compilation Integration

#### compiler.rs (resident_compiler/)
- **ResidentCompiler::compile_text()**: Main pipeline
- Stages: Lexing → Parsing → Conversion → AST → Compilation → Linting
- Returns CompiledDocument with all metadata

```
Lines 75-150: compile_text() implementation
Lines 88-105: Token collection from lexer
Lines 107-113: Value reading and parsing
Lines 116-119: Expression conversion
Lines 121-125: ExprWithLoc wrapping
Lines 128-144: Bytecode compilation, linting, document creation
```

#### compile/mod.rs (compiler/)
- **compile_with_metadata()**: Bytecode generation
- Currently returns empty LocationMap (Phase 2 TODO)

```
Lines 1149-1152: compile_with_metadata() signature and LocationMap creation
```

### 4. Linting Integration

#### mod.rs (compiler/linter/)
- **Linter**: Main linting engine
- **lint_expr()**: Entry point for AST linting
- **check_expr()**: Recursive AST traversal with location passing

```
Lines 16-32:   Linter definition
Lines 29-31:   lint_expr() public method
Lines 40-217:  check_expr() recursive traversal
Lines 92-101:  Call arity checking integration
Lines 129-131: Naming convention checking integration
```

#### rules.rs (compiler/linter/)
- **check_naming_convention()**: Kebab-case validation
- **check_call_arity()**: Function arity validation
- Helper functions: is_valid_kebab_case(), to_kebab_case(), builtin_arity()

```
Lines 8-48:    check_naming_convention() with suggestions
Lines 51-79:   check_call_arity() validation
Lines 37-46:   Diagnostic creation pattern (naming)
Lines 64-75:   Diagnostic creation pattern (arity)
```

### 5. Result Collection

#### compiled_doc.rs (resident_compiler/)
- **CompiledDocument**: Complete compilation result
- Contains: source_text, AST, bytecode, location_map, symbols, diagnostics

```
Lines 10-67: CompiledDocument definition and creation
Lines 37-54: CompiledDocument::new() constructor
Lines 56-65: is_valid_for_file() cache validation
```

#### mod.rs (resident_compiler/)
- Module documentation and exports
- Exports: ResidentCompiler, CompiledDocument

```
Lines 1-15: Module documentation and re-exports
```

## Usage Patterns by Scenario

### Scenario 1: Handling Lexer Error
```
File: compiler.rs (Lines 99-104)
Context: In compile_text() after Lexer::next_token()
Pattern: match/return with CompileError wrapping
```

### Scenario 2: Checking Function Call Arity
```
File: linter/rules.rs (Lines 51-79) + linter/mod.rs (Lines 92-101)
Context: During AST traversal when visiting Expr::Call
Pattern: Symbol lookup → arity validation → Diagnostic creation
```

### Scenario 3: Validating Naming Convention
```
File: linter/rules.rs (Lines 8-48) + linter/mod.rs (Lines 126-131)
Context: During Expr::Define processing
Pattern: Symbol name extraction → kebab-case check → Diagnostic with suggestion
```

### Scenario 4: Creating Runtime Error with Location
```
File: error/runtime.rs (Lines 14-35) - Builder pattern
Usage example: RuntimeError::new(msg).with_location(loc).with_context(ctx)
```

### Scenario 5: Formatting Diagnostic for Display
```
File: linter/diagnostics.rs (Lines 58-84)
Method: Diagnostic::format_human()
Output: Multi-line formatted string with severity, message, suggestions
```

## Cross-File Dependencies

```
ResidentCompiler (compiler.rs)
    ↓ uses
    ├─→ Lexer (reader/lexer.rs) → TokenWithLoc → SourceLoc
    ├─→ Reader (reader/mod.rs) → Values
    ├─→ value_to_expr() (compiler/converters.rs) → Expr
    ├─→ ExprWithLoc (compiler/ast.rs)
    ├─→ compile_with_metadata (compiler/compile/mod.rs) → (Bytecode, LocationMap)
    ├─→ Linter (compiler/linter/mod.rs)
    │   ├─→ check_expr() with location
    │   └─→ check_naming_convention() + check_call_arity() (compiler/linter/rules.rs)
    ├─→ Diagnostic (compiler/linter/diagnostics.rs) with SourceLoc
    └─→ CompiledDocument (compiled_doc.rs)
```

## Key Takeaways

1. **Three-level Error Architecture**:
   - CompileError: Early compilation stages (lexing, parsing, I/O)
   - Diagnostic: Linting analysis with severity levels
   - RuntimeError: Runtime execution with location context

2. **Location Tracking Throughout Pipeline**:
   - Lexer: Creates SourceLoc at token level
   - AST: Optional SourceLoc in ExprWithLoc
   - Bytecode: LocationMap planned (Phase 2)
   - Diagnostics: Attached to each finding
   - Runtime: Passed via RuntimeError

3. **Structured Diagnostic System**:
   - Severity levels (Info, Warning, Error)
   - Rule codes (W001, W002, etc.)
   - Descriptive messages
   - Actionable suggestions
   - Human-readable formatting

4. **Known Limitations**:
   - Location lost during Value→Expr conversion
   - LocationMap not yet populated during compilation
   - CompileError lacks location information (simple string-based)

5. **Design Patterns Used**:
   - Builder pattern for errors (RuntimeError)
   - Visitor pattern for AST traversal (Linter)
   - Option type for optional locations
   - Diagnostic code + rule naming convention
