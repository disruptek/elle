# Error Handling Patterns and Code Examples

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    SOURCE CODE INPUT                         │
└──────────────────────────┬──────────────────────────────────┘
                           │
                    ResidentCompiler
                      compile_text()
                           │
            ┌──────────────┼──────────────┐
            │              │              │
            ▼              ▼              ▼
    ┌─────────────┐ ┌──────────┐ ┌─────────────┐
    │   LEXER     │ │  READER  │ │  CONVERTER  │
    │ (tokens)    │ │ (values) │ │ (expr)      │
    └─────────────┘ └──────────┘ └─────────────┘
            │              │              │
            └──────────────┼──────────────┘
                           │
                    TokenWithLoc +
                   SourceLoc info
                           │
            ┌──────────────┴──────────────┐
            │                             │
            ▼                             ▼
    ┌───────────────────┐      ┌──────────────────┐
    │   ExprWithLoc     │      │     COMPILER     │
    │ + SourceLoc       │      │   compile_with   │
    │                   │      │   _metadata()    │
    └───────────────────┘      │ (LocationMap)    │
            │                  └──────────────────┘
            │                           │
            │            ┌──────────────┘
            │            │
            │            ▼
            │      ┌─────────────┐
            │      │  Bytecode   │
            │      │ + LocationMap
            │      └─────────────┘
            │            │
            └────────┬───┘
                     │
            ┌────────▼──────────┐
            │     LINTER        │
            │ lint_expr()       │
            │                   │
            └────────┬──────────┘
                     │
                     ▼
         ┌───────────────────────┐
         │  Diagnostic Vec       │
         │ - severity            │
         │ - location (SourceLoc)│
         │ - message             │
         │ - suggestions         │
         └───────────────────────┘
                     │
            ┌────────▼──────────┐
            │ CompiledDocument  │
            │ - source_text     │
            │ - ast: ExprWithLoc│
            │ - bytecode        │
            │ - location_map    │
            │ - diagnostics     │
            └───────────────────┘
```

## Error Flow Patterns

### Pattern 1: Compilation Error Wrapping

**Location in code**: `compiler.rs:88-105` (Lexer loop)

```rust
// Direct error: Lexer error
loop {
    match lex.next_token() {
        Ok(Some(token)) => {
            tokens.push(crate::reader::OwnedToken::from(token));
        }
        Ok(None) => break,
        Err(e) => {
            return Err(CompileError {
                message: format!("Lexer error: {}", e),
            });
        }
    }
}

// Map_err pattern: Parser error
let value = reader
    .read(&mut self.symbol_table)
    .map_err(|e| CompileError {
        message: format!("Parse error: {}", e),
    })?;

// IO error wrapping: File read error
let source = std::fs::read_to_string(path).map_err(|e| CompileError {
    message: format!("Failed to read {}: {}", path, e),
})?;
```

**Key Points**:
- Adds context prefix ("Lexer error: ", "Parse error: ", etc.)
- Wraps lower-level errors in CompileError
- Uses `?` for early return propagation

### Pattern 2: Source Location Generation

**Location in code**: `reader/token.rs:16-42`

```rust
impl SourceLoc {
    // Full constructor - used by lexer
    pub fn new(file: impl Into<String>, line: usize, col: usize) -> Self {
        SourceLoc {
            file: file.into(),
            line,
            col,
        }
    }

    // Fallback when file is unknown - used in tests
    pub fn from_line_col(line: usize, col: usize) -> Self {
        SourceLoc {
            file: "<unknown>".to_string(),
            line,
            col,
        }
    }

    // Start of file - used for default locations
    pub fn start() -> Self {
        SourceLoc {
            file: "<unknown>".to_string(),
            line: 1,
            col: 1,
        }
    }
}

// Display format: file:line:col
impl fmt::Display for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}
```

### Pattern 3: ExprWithLoc Creation

**Location in code**: `compiler.rs:121-125`

```rust
// Create wrapped expression (for now, without location since we lose it in conversion)
let expr_with_loc = ExprWithLoc {
    expr: expr.clone(),
    loc: None,  // ← Location lost during conversion (Phase 2 TODO)
};

// Later access:
// In linter (mod.rs:30)
pub fn lint_expr(&mut self, expr: &ExprWithLoc, symbol_table: &crate::SymbolTable) {
    self.check_expr(&expr.expr, &expr.loc, symbol_table);
    //                                    ↑
    //                    Pass location through checking
}

// Format location for display (ast.rs:16-21)
impl ExprWithLoc {
    pub fn format_loc(&self) -> String {
        match &self.loc {
            Some(loc) => format!("{}:{}", loc.line, loc.col),
            None => "unknown".to_string(),
        }
    }
}
```

### Pattern 4: Diagnostic Creation

**Location in code**: `linter/rules.rs:37-46` (Naming convention check)

```rust
// Diagnostic with suggestions
let diag = Diagnostic::new(
    Severity::Warning,
    "W001",
    "naming-kebab-case",
    format!("identifier '{}' should use kebab-case", name),
    location.clone(),  // Optional SourceLoc
)
.with_suggestions(vec![format!("rename to '{}'", suggested_name)]);

diagnostics.push(diag);
```

**Location in code**: `linter/rules.rs:64-75` (Arity check)

```rust
// Diagnostic without suggestions
let diag = Diagnostic::new(
    Severity::Warning,
    "W002",
    "arity-mismatch",
    format!(
        "function '{}' expects {} argument(s) but got {}",
        func_name, expected_arity, arg_count
    ),
    location.clone(),
);

diagnostics.push(diag);
```

### Pattern 5: RuntimeError Builder

**Location in code**: `error/runtime.rs:14-35`

```rust
// Create and build runtime error
let err = RuntimeError::new("division by zero".to_string())
    .with_location(SourceLoc::from_line_col(42, 15))
    .with_context("in function calculate".to_string());

// Display output:
// "Error at <unknown>:42:15: division by zero"
// "  Context: in function calculate"
```

### Pattern 6: Diagnostic Display

**Location in code**: `linter/diagnostics.rs:58-84`

```rust
// Given this diagnostic:
let diag = Diagnostic::new(
    Severity::Warning,
    "W001",
    "naming-kebab-case",
    "identifier 'myVariable' should use kebab-case".to_string(),
    Some(SourceLoc::from_line_col(5, 2)),
);

// format_human() produces:
// 5:2 warning: naming-kebab-case
//   message: identifier 'myVariable' should use kebab-case
//   suggestions:
//     - rename to 'my-variable'
```

## Type Hierarchy

```
CompileError (resident_compiler level)
├── message: String
└── Implements Display, Error

SourceLoc (shared infrastructure)
├── file: String
├── line: usize
├── col: usize
└── Implements Display

TokenWithLoc (lexer output)
├── token: Token
└── loc: SourceLoc

ExprWithLoc (AST)
├── expr: Expr
└── loc: Option<SourceLoc>

RuntimeError (runtime execution)
├── message: String
├── location: Option<SourceLoc>
└── context: Option<String>

Diagnostic (linter output)
├── severity: Severity {Info, Warning, Error}
├── code: String (W001, E001, etc.)
├── rule: String (naming-kebab-case, arity-mismatch, etc.)
├── message: String
├── location: Option<SourceLoc>
└── suggestions: Vec<String>

CompiledDocument (final result)
├── source_text: String
├── ast: ExprWithLoc
├── bytecode: Bytecode
├── location_map: LocationMap (HashMap<usize, SourceLoc>)
├── symbols: SymbolIndex
├── diagnostics: Vec<Diagnostic>
└── compiled_at: SystemTime
```

## Information Flow Example

### Scenario: Naming convention violation

```
Input: (define myVariable 42)

Step 1: Lexing
  Token: Symbol("define")  @ line:1, col:2
  Token: Symbol("myVariable")  @ line:1, col:9
  Token: Integer(42)  @ line:1, col:22
  └─→ TokenWithLoc carries locations

Step 2: Parsing
  Value: List([Symbol("define"), Symbol("myVariable"), Integer(42)])
  └─→ Location info from tokens (but becomes implicit in Value)

Step 3: Conversion to Expr
  Expr::Define { name: SymbolId(3), value: Box::new(Expr::Literal(42)) }
  └─→ Wrapped in ExprWithLoc { expr: ..., loc: None }
      (Location lost in conversion - Phase 2 TODO)

Step 4: Linting
  check_expr():
    Expr::Define { name: SymbolId(3), value: ... }
    │
    ├─→ Get symbol name: "myVariable"
    ├─→ Call check_naming_convention("myVariable", &None, diagnostics)
    │
    └─→ Create Diagnostic {
          severity: Warning,
          code: "W001",
          rule: "naming-kebab-case",
          message: "identifier 'myVariable' should use kebab-case",
          location: None,  // ← Because we lost it in conversion
          suggestions: vec!["rename to 'my-variable'"]
        }

Step 5: Output
  Diagnostic.format_human():
    "warning: naming-kebab-case
      message: identifier 'myVariable' should use kebab-case
      suggestions:
        - rename to 'my-variable'"

Final Result: CompiledDocument {
  source_text: "(define myVariable 42)",
  ast: ExprWithLoc { ... },
  bytecode: ...,
  location_map: {},  // Empty for Phase 2
  symbols: ...,
  diagnostics: [ Diagnostic { ... } ],
  compiled_at: SystemTime::now()
}
```

## Integration Points

### 1. Lexer Integration (reader/lexer.rs)
```rust
fn get_loc(&self) -> SourceLoc {
    SourceLoc::new(&self.file, self.line, self.col)
}
// Used to create TokenWithLoc during tokenization
```

### 2. Linter Integration (compiler/linter/mod.rs)
```rust
pub fn lint_expr(&mut self, expr: &ExprWithLoc, symbol_table: &crate::SymbolTable) {
    self.check_expr(&expr.expr, &expr.loc, symbol_table);
    //                                    ↑
    //                        Passes location to all checks
}
```

### 3. VM Integration (vm/core.rs)
```rust
pub struct VM {
    pub location_map: LocationMap,  // HashMap<usize, SourceLoc>
}

pub fn set_location_map(&mut self, map: LocationMap) { ... }
pub fn get_location_map(&self) -> &LocationMap { ... }
```

## Proposed Improvements (Phase 2 & Beyond)

### Issue 1: Location Loss in Conversion
**Current**: `ExprWithLoc.loc` is always None after conversion from Value to Expr

**Solution**: 
```rust
// Proposed: Pass location through conversion pipeline
fn value_to_expr(
    value: &Value,
    symbol_table: &mut SymbolTable,
    location: Option<SourceLoc>,  // ← Add location parameter
) -> Result<Expr, Box<dyn Error>> {
    let expr = match value {
        // ... conversion logic ...
    };
    Ok(ExprWithLoc {
        expr,
        loc: location,  // ← Preserve location
    })
}
```

### Issue 2: Empty LocationMap
**Current**: `compile_with_metadata()` returns empty LocationMap

**Solution**:
```rust
pub fn compile_with_metadata(
    expr: &Expr,
    location: Option<crate::reader::SourceLoc>,
) -> (Bytecode, LocationMap) {
    let mut location_map = LocationMap::new();
    let mut compiler = Compiler::new();
    
    // During compilation, track instruction → location mapping
    compiler.compile_with_tracking(expr, &mut location_map);
    
    (compiler.bytecode, location_map)
}
```

### Issue 3: Context in Compilation Errors
**Current**: No context about what was being compiled when error occurred

**Proposed**: Add context information to CompileError
```rust
pub struct CompileError {
    pub message: String,
    pub location: Option<SourceLoc>,  // ← Add location
    pub stage: CompileStage,          // ← Add stage (Lex, Parse, Compile)
}

enum CompileStage {
    Lexing,
    Parsing,
    Conversion,
    Compilation,
    Linting,
}
```

