# Resident Compiler Error Message Handling Analysis

## Overview
The resident_compiler in Elle uses a sophisticated error tracking infrastructure that integrates with the lexer, parser, compiler, and linter to provide rich error messages with source location information.

## 1. Error Message Structure

### A. ResidentCompiler Error Type
**File**: `/home/adavidoff/git/elle3/src/resident_compiler/compiler.rs` (Lines 11-22)

The compiler defines a simple error wrapper:
```rust
#[derive(Debug, Clone)]
pub struct CompileError {
    pub message: String,
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
```

**Key Points**:
- Simple string-based error messages at the resident_compiler level
- Implements Display and Error traits for standard Rust error handling
- Used for lexing, parsing, and compilation errors during `compile_text()`

### B. Error Wrapping at Different Stages
The compiler wraps errors from different compilation stages:

1. **Lexer errors** (Line 99-104):
   ```rust
   Err(e) => {
       return Err(CompileError {
           message: format!("Lexer error: {}", e),
       });
   }
   ```

2. **Parser errors** (Line 111-113):
   ```rust
   .map_err(|e| CompileError {
       message: format!("Parse error: {}", e),
   })?
   ```

3. **Conversion errors** (Line 117-119):
   ```rust
   .map_err(|e| CompileError {
       message: format!("Conversion error: {}", e),
   })?
   ```

4. **File I/O errors** (Line 62-64):
   ```rust
   let source = std::fs::read_to_string(path).map_err(|e| CompileError {
       message: format!("Failed to read {}: {}", path, e),
   })?
   ```

## 2. SourceLoc Infrastructure

### A. SourceLoc Definition
**File**: `/home/adavidoff/git/elle3/src/reader/token.rs` (Lines 1-42)

Core location tracking structure:
```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLoc {
    pub file: String,
    pub line: usize,
    pub col: usize,
}
```

**Tracking Components**:
- `file`: Source file name or identifier
- `line`: Line number (1-indexed)
- `col`: Column number (1-indexed)

**Display Format**: `file:line:col` (Line 10-14)

**Factory Methods** (Lines 16-42):
1. `new(file, line, col)`: Full location creation
2. `from_line_col(line, col)`: Location with `<unknown>` file
3. `start()`: Beginning of file (1:1)

### B. SourceLoc in Compilation Pipeline

1. **Lexer generates SourceLoc** (lexer.rs, line 45-46):
   - Maintains file name, line, and column during tokenization
   - Emits `TokenWithLoc` containing token + SourceLoc

2. **Tokens carry location info** (token.rs, lines 44-48):
   ```rust
   pub struct TokenWithLoc<'a> {
       pub token: Token<'a>,
       pub loc: SourceLoc,
   }
   ```

3. **ExprWithLoc wraps AST nodes** (ast.rs, lines 6-22):
   ```rust
   pub struct ExprWithLoc {
       pub expr: Expr,
       pub loc: Option<SourceLoc>,
   }
   ```
   - AST nodes can optionally carry their source location
   - Provides `format_loc()` method for display formatting

## 3. Tracking Source Locations During Compilation

### A. LocationMap Infrastructure
**File**: `/home/adavidoff/git/elle3/src/error/mod.rs` (Lines 21-26)

```rust
/// Mapping from bytecode instruction index to source code location
///
/// Used for generating runtime error messages with source location information.
/// Maps instruction pointers to the source location they originated from.
/// Uses SourceLoc from the reader module which includes file information.
pub type LocationMap = HashMap<usize, SourceLoc>;
```

**Purpose**: Maps bytecode instruction indices back to source code locations for runtime error reporting

### B. LocationMap Population
**File**: `/home/adavidoff/git/elle3/src/compiler/compile/mod.rs` (Lines 1149-1152)

During compilation:
```rust
pub fn compile_with_metadata(
    expr: &Expr,
    _location: Option<crate::reader::SourceLoc>,
) -> (Bytecode, LocationMap) {
    // ... compilation logic ...
    let location_map = LocationMap::new(); // Empty for now - phase 2 will populate this
```

**Current Status**: Location mapping is planned but not yet fully implemented (Phase 2)

### C. VM Integration
**File**: `/home/adavidoff/git/elle3/src/vm/core.rs` (Lines 40, 111-116)

The VM stores and uses location maps:
```rust
pub location_map: LocationMap,  // Bytecode instruction index → source location mapping

pub fn set_location_map(&mut self, map: LocationMap) {
    self.location_map = map;
}

pub fn get_location_map(&self) -> &LocationMap {
    &self.location_map
}
```

## 4. Error Formatting for Display

### A. RuntimeError Display
**File**: `/home/adavidoff/git/elle3/src/error/runtime.rs` (Lines 7-50)

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeError {
    pub message: String,
    pub location: Option<SourceLoc>,
    pub context: Option<String>,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.location {
            Some(loc) => write!(f, "Error at {}: {}", loc, self.message)?,
            None => write!(f, "Error: {}", self.message)?,
        }

        if let Some(ref ctx) = self.context {
            write!(f, "\n  Context: {}", ctx)?;
        }

        Ok(())
    }
}
```

**Display Patterns**:
- With location: `Error at file:line:col: message`
- Without location: `Error: message`
- Optional context on separate line: `Context: ...`

**Builder Pattern**:
- `.with_location(SourceLoc)`: Add source location
- `.with_context(String)`: Add context information

### B. Diagnostic Display
**File**: `/home/adavidoff/git/elle3/src/compiler/linter/diagnostics.rs` (Lines 58-84)

```rust
pub fn format_human(&self) -> String {
    let mut output = String::new();

    match &self.location {
        Some(loc) => {
            output.push_str(&format!(
                "{}:{} {}: {}\n",
                loc.line, loc.col, self.severity, self.rule
            ));
            output.push_str(&format!("  message: {}\n", self.message));
        }
        None => {
            output.push_str(&format!("{}: {}\n", self.severity, self.rule));
            output.push_str(&format!("  message: {}\n", self.message));
        }
    }

    if !self.suggestions.is_empty() {
        output.push_str("  suggestions:\n");
        for suggestion in &self.suggestions {
            output.push_str(&format!("    - {}\n", suggestion));
        }
    }

    output
}
```

**Display Format**:
```
line:col severity: rule
  message: <message>
  suggestions:
    - suggestion 1
    - suggestion 2
```

## 5. Shared Error Infrastructure

### A. Diagnostic Type
**File**: `/home/adavidoff/git/elle3/src/compiler/linter/diagnostics.rs` (Lines 24-50)

```rust
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,  // Info, Warning, Error
    pub code: String,        // W001, E001, etc.
    pub rule: String,        // Rule name (e.g., "naming-kebab-case")
    pub message: String,
    pub location: Option<SourceLoc>,
    pub suggestions: Vec<String>,
}
```

**Severity Levels** (Lines 7-22):
- `Info`: Informational messages
- `Warning`: Non-critical issues
- `Error`: Critical issues

**Diagnostic Examples**:
1. **Naming Convention** (rules.rs, lines 37-46):
   - Rule: "naming-kebab-case"
   - Code: "W001"
   - Severity: Warning
   - Includes suggestions for fixes

2. **Arity Mismatch** (rules.rs, lines 64-75):
   - Rule: "arity-mismatch"
   - Code: "W002"
   - Includes function name and expected vs actual arguments

### B. Linter Integration
**File**: `/home/adavidoff/git/elle3/src/compiler/linter/mod.rs` (Lines 16-32)

```rust
pub struct Linter {
    diagnostics: Vec<Diagnostic>,
}

impl Linter {
    pub fn new() -> Self {
        Self { diagnostics: Vec::new() }
    }

    pub fn lint_expr(&mut self, expr: &ExprWithLoc, symbol_table: &crate::SymbolTable) {
        self.check_expr(&expr.expr, &expr.loc, symbol_table);
    }
```

**Linting Features**:
- Traverses AST and collects diagnostics
- Passes location information through entire check
- Integrates with symbol table for semantic checks
- Query methods: `diagnostics()`, `has_errors()`, `has_warnings()`

### C. CompiledDocument Integration
**File**: `/home/adavidoff/git/elle3/src/resident_compiler/compiled_doc.rs` (Lines 10-67)

```rust
pub struct CompiledDocument {
    pub source_text: String,
    pub ast: ExprWithLoc,
    pub bytecode: Bytecode,
    pub location_map: LocationMap,
    pub symbols: SymbolIndex,
    pub diagnostics: Vec<Diagnostic>,
    pub compiled_at: SystemTime,
}
```

**Complete Compilation Result**:
- Source code snapshot
- Parsed AST with locations
- Compiled bytecode
- Location mapping for runtime errors
- IDE symbol index
- All diagnostics (errors, warnings, info)

## 6. Error Reporting Flow

### Compilation Pipeline:
```
Source Text
    ↓
Lexer (generates TokenWithLoc)
    ↓ [LexerError → CompileError]
Reader (generates Value)
    ↓ [ParseError → CompileError]
value_to_expr (generates Expr)
    ↓ [ConversionError → CompileError]
ExprWithLoc (wraps with SourceLoc)
    ↓
compile_with_metadata (generates Bytecode + LocationMap)
    ↓
Linter (generates Diagnostics with SourceLoc)
    ↓
CompiledDocument (collects all results)
```

### Diagnostic Collection:
```
ExprWithLoc → Linter.lint_expr()
    ↓
AST traversal with location passing
    ↓
Rules check (naming, arity, etc.)
    ↓
Diagnostic::new(severity, code, rule, message, location)
    ↓
Diagnostic.format_human() for display
```

## 7. Key Patterns and Best Practices

### Pattern 1: Builder Pattern for Errors
```rust
RuntimeError::new("message")
    .with_location(loc)
    .with_context("context")
```

### Pattern 2: Location Wrapping
```rust
ExprWithLoc {
    expr: expr_value,
    loc: Some(SourceLoc),  // or None if unknown
}
```

### Pattern 3: Diagnostic Creation
```rust
Diagnostic::new(
    Severity::Warning,
    "W001",
    "rule-name",
    "descriptive message",
    location,
)
.with_suggestions(vec!["suggestion 1".to_string()])
```

### Pattern 4: Error Propagation
```rust
some_operation()
    .map_err(|e| CompileError {
        message: format!("Context: {}", e),
    })?
```

## 8. Notable Implementation Details

### Location Preservation
- **Token level**: TokenWithLoc carries location info
- **AST level**: ExprWithLoc wraps expressions with optional locations
- **Diagnostic level**: Each diagnostic stores its source location
- **Runtime level**: LocationMap allows mapping bytecode back to source

### Current Limitations
1. **Location mapping in compiler** (Phase 2): Currently empty, pending implementation
   - Bytecode instruction → source location mapping not yet populated
   
2. **Location loss in conversion**: Converting from Value to Expr loses some location info
   - Current workaround: Set ExprWithLoc location to None
   - Future: Full tracing through conversion pipeline

### Design Decisions
1. **Optional locations**: Uses `Option<SourceLoc>` to handle cases where location is unknown
2. **Severity levels**: Diagnostic severity allows filtering and prioritization
3. **Suggestion system**: Diagnostics include actionable suggestions for fixes
4. **Separate error types**: CompileError (compilation stage) vs Diagnostic (linting)

## Files Summary

| File | Purpose | Key Types |
|------|---------|-----------|
| `compiler.rs` | Main resident compiler | `CompileError`, `ResidentCompiler` |
| `compiled_doc.rs` | Result container | `CompiledDocument` |
| `error/mod.rs` | Error module exports | `LocationMap` type alias |
| `error/runtime.rs` | Runtime errors | `RuntimeError` |
| `error/sourceloc.rs` | Re-exports | `SourceLoc` |
| `reader/token.rs` | Lexer output | `SourceLoc`, `TokenWithLoc` |
| `compiler/ast.rs` | AST definition | `ExprWithLoc` |
| `compiler/linter/diagnostics.rs` | Linting output | `Diagnostic`, `Severity` |
| `compiler/linter/mod.rs` | Linting engine | `Linter` |
| `compiler/linter/rules.rs` | Linting rules | Naming, arity checks |
| `vm/core.rs` | Runtime execution | LocationMap storage |

