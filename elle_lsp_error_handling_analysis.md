# Elle-LSP Error Message and Source Location Analysis

## Overview
Elle-LSP implements a comprehensive error handling and diagnostic system that integrates the Elle compiler's linter with LSP protocol messages. The system uses a layered approach with source location tracking throughout the compilation pipeline.

---

## 1. Diagnostic Message Structure

### 1.1 Core Diagnostic Type
**Location:** `/home/adavidoff/git/elle3/src/compiler/linter/diagnostics.rs`

```rust
pub struct Diagnostic {
    pub severity: Severity,
    pub code: String,
    pub rule: String,
    pub message: String,
    pub location: Option<SourceLoc>,
    pub suggestions: Vec<String>,
}
```

**Key Components:**
- **severity**: Enum with three levels (Info, Warning, Error)
- **code**: Diagnostic code (e.g., "E0001", "W001")
- **rule**: Rule name that triggered the diagnostic (e.g., "syntax-error", "naming-kebab-case")
- **message**: Human-readable error message
- **location**: Optional SourceLoc struct with file, line, and column information
- **suggestions**: Vec of actionable fix suggestions

**Severity Enum:**
```rust
pub enum Severity {
    Info,
    Warning,
    Error,
}
```
Implements Display and ordering (Info < Warning < Error)

---

## 2. SourceLoc Infrastructure

### 2.1 SourceLoc Structure
**Location:** `/home/adavidoff/git/elle3/src/reader/token.rs`

```rust
pub struct SourceLoc {
    pub file: String,
    pub line: usize,      // 1-based line numbers
    pub col: usize,       // 1-based column numbers
}
```

**Key Characteristics:**
- **1-based line and column numbering** (matches compiler convention)
- **file field**: Filename or `<unknown>` for dynamically generated code
- **Display trait**: Formats as `file:line:col`

**Constructor Methods:**
- `new(file, line, col)` - Full construction
- `from_line_col(line, col)` - Creates with `<unknown>` file
- `start()` - Creates location at file start (1, 1)

### 2.2 TokenWithLoc Structure
```rust
pub struct TokenWithLoc<'a> {
    pub token: Token<'a>,
    pub loc: SourceLoc,
}
```

Associates every token with its source location during lexing.

---

## 3. Error Creation and Flow

### 3.1 Diagnostic Creation Pattern
**Location:** `/home/adavidoff/git/elle3/src/compiler/linter/rules.rs`

#### Naming Convention Check Example:
```rust
let diag = Diagnostic::new(
    Severity::Warning,
    "W001",
    "naming-kebab-case",
    format!("identifier '{}' should use kebab-case", name),
    location.clone(),
)
.with_suggestions(vec![format!("rename to '{}'", suggested_name)]);

diagnostics.push(diag);
```

#### Arity Mismatch Example:
```rust
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
```

**Key Patterns:**
1. `location.clone()` - Location is Option<SourceLoc> (None if not available)
2. `with_suggestions()` - Chains suggestions for actionable diagnostics
3. Diagnostic codes follow pattern: X#### (E=Error, W=Warning, I=Info)

### 3.2 Linter Integration
**Location:** `/home/adavidoff/git/elle3/src/compiler/linter/mod.rs`

```rust
pub fn lint_expr(&mut self, expr: &ExprWithLoc, symbol_table: &crate::SymbolTable) {
    self.check_expr(&expr.expr, &expr.loc, symbol_table);
}
```

The Linter processes `ExprWithLoc` which contains both the AST expression and optional source location.

---

## 4. LSP Diagnostic Conversion

### 4.1 Diagnostic to LSP Format Conversion
**Location:** `/home/adavidoff/git/elle3/elle-lsp/src/main.rs` (lines 159-182, 220-243)

**Conversion Code:**
```rust
let diags: Vec<_> = doc
    .diagnostics
    .iter()
    .map(|d| {
        let (line, col) = match &d.location {
            Some(loc) => (loc.line as u32, loc.col as u32),
            None => (0, 0),
        };
        json!({
            "range": {
                "start": { "line": line - 1, "character": col - 1 },
                "end": { "line": line - 1, "character": col }
            },
            "severity": match d.severity {
                elle::compiler::linter::diagnostics::Severity::Error => 1,
                elle::compiler::linter::diagnostics::Severity::Warning => 2,
                elle::compiler::linter::diagnostics::Severity::Info => 3,
            },
            "code": d.code,
            "source": "elle-lint",
            "message": d.message
        })
    })
    .collect();
```

**Conversion Details:**

| Elle | LSP | Notes |
|------|-----|-------|
| Severity::Error | 1 | DiagnosticSeverity.Error |
| Severity::Warning | 2 | DiagnosticSeverity.Warning |
| Severity::Info | 3 | DiagnosticSeverity.Information |
| line - 1 | line (0-based) | **Key conversion:** Elle uses 1-based, LSP uses 0-based |
| col - 1 | character (0-based) | **Key conversion:** Elle uses 1-based, LSP uses 0-based |
| code | code | Preserved directly |
| rule | - | Not sent to client (used internally) |
| message | message | Preserved directly |
| location.file | - | Not sent in diagnostic (handled via URI in notification) |
| suggestions | - | Not included in LSP diagnostic (could be in future enhancement) |

**LSP Diagnostic JSON Structure:**
```json
{
    "range": {
        "start": { "line": 0, "character": 0 },
        "end": { "line": 0, "character": 1 }
    },
    "severity": 1,
    "code": "E0001",
    "source": "elle-lint",
    "message": "Lexer error: ..."
}
```

---

## 5. Protocol Integration

### 5.1 Diagnostic Notification Flow
**Location:** `/home/adavidoff/git/elle3/elle-lsp/src/main.rs`

**Notification Structure:**
```rust
notifications.push(json!({
    "jsonrpc": "2.0",
    "method": "textDocument/publishDiagnostics",
    "params": {
        "uri": uri,
        "diagnostics": diags
    }
}));
```

**Triggers:**
1. `textDocument/didOpen` - On document open (line 157-191)
2. `textDocument/didChange` - On document change (line 218-252)

**File Location Handling:**
- Document URI comes from LSP client
- Diagnostic file location in SourceLoc is not used in current implementation
- All diagnostics in a notification are associated with the document URI

---

## 6. Error Handling in Compilation Pipeline

### 6.1 Compilation Stages
**Location:** `/home/adavidoff/git/elle3/elle-lsp/src/compiler_state.rs` (lines 84-179)

**Stages and Error Handling:**

#### Stage 1: Lexing (lines 96-116)
```rust
match lexer.next_token() {
    Ok(Some(token)) => tokens.push(OwnedToken::from(token)),
    Ok(None) => break,
    Err(e) => {
        let msg = format!("Lexer error: {}", e);
        doc.diagnostics.push(Diagnostic::new(
            Severity::Error,
            "E0001",
            "syntax-error",
            msg,
            None,  // No source location from lexer error
        ));
        return false;
    }
}
```

**Code: E0001** | **Severity: Error** | **Location: None**

#### Stage 2: Reading (lines 118-137)
```rust
match result {
    Ok(value) => values.push(value),
    Err(e) => {
        let msg = format!("Reader error: {}", e);
        doc.diagnostics.push(Diagnostic::new(
            Severity::Error,
            "E0002",
            "syntax-error",
            msg,
            None,
        ));
        return false;
    }
}
```

**Code: E0002** | **Severity: Error** | **Location: None**

#### Stage 3: Conversion (lines 140-158)
```rust
match value_to_expr(&value, &mut self.symbol_table) {
    Ok(expr) => exprs.push(ExprWithLoc::new(expr, None)),
    Err(e) => {
        let msg = format!("Conversion error: {}", e);
        doc.diagnostics.push(Diagnostic::new(
            Severity::Error,
            "E0003",
            "syntax-error",
            msg,
            None,
        ));
        return false;
    }
}
```

**Code: E0003** | **Severity: Error** | **Location: None**

#### Stage 4: Linting (lines 164-171)
```rust
let mut linter = Linter::new();
for expr in &exprs {
    linter.lint_expr(expr, &self.symbol_table);
}
doc.diagnostics.extend(linter.diagnostics().iter().cloned());
```

**Codes: W001, W002, etc.** | **Severity: Warning/Info** | **Location: From ExprWithLoc**

---

## 7. File Location Handling

### 7.1 Current Implementation
- **SourceLoc.file field**: Set to `<unknown>` during dynamic compilation (LSP)
- **Document URI**: Comes from LSP client in notification params
- **Conversion**: File path in SourceLoc not utilized in current diagnostic format

### 7.2 File Location in Navigation
**Location and Definition Example** (`definition.rs`, lines 57-71):
```rust
let uri = format!("file://{}", def_loc.file);

Some(json!({
    "uri": uri,
    "range": {
        "start": {
            "line": def_loc.line.saturating_sub(1),
            "character": def_loc.col.saturating_sub(1)
        },
        "end": {
            "line": def_loc.line.saturating_sub(1),
            "character": def_loc.col
        }
    }
}))
```

**References Example** (`references.rs`, lines 62-75):
```rust
let uri = format!("file://{}", usage_loc.file);
references.push(json!({
    "uri": uri,
    "range": {
        "start": {
            "line": usage_loc.line.saturating_sub(1),
            "character": usage_loc.col.saturating_sub(1)
        },
        "end": {
            "line": usage_loc.line.saturating_sub(1),
            "character": usage_loc.col
        }
    }
}));
```

**Hover Example** (`hover.rs`, lines 17-19):
```rust
// LSP uses 0-based line numbers but SourceLoc uses 1-based
let target_line = line as usize + 1;
let target_col = character as usize + 1;
```

### 7.3 Line/Column Conversion Pattern
**Universal Pattern** across all LSP handlers:
1. LSP sends 0-based line and character
2. Convert to 1-based: `line + 1`, `character + 1`
3. Compare with SourceLoc values
4. Convert back to 0-based for response: `line - 1`, `character - 1`

---

## 8. Utility Functions for Error Formatting

### 8.1 Diagnostic Formatting
**Location:** `/home/adavidoff/git/elle3/src/compiler/linter/diagnostics.rs` (lines 58-84)

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

**Human-Readable Output Example:**
```
5:2 warning: naming-kebab-case
  message: identifier 'squareNumber' should use kebab-case
  suggestions:
    - rename to 'square-number'
```

### 8.2 Linting Rules Utilities
**Location:** `/home/adavidoff/git/elle3/src/compiler/linter/rules.rs`

#### Naming Convention Check:
```rust
pub fn check_naming_convention(
    name: &str,
    location: &Option<SourceLoc>,
    diagnostics: &mut Vec<Diagnostic>,
)
```

**Process:**
1. Validates kebab-case for identifiers
2. Generates suggestion via `to_kebab_case()`
3. Creates diagnostic with suggestion

#### Arity Validation:
```rust
pub fn check_call_arity(
    func_sym: SymbolId,
    arg_count: usize,
    location: &Option<SourceLoc>,
    symbol_table: &crate::SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
)
```

**Process:**
1. Looks up function in symbol table
2. Checks against `builtin_arity()` table
3. Validates argument count

### 8.3 Document Processing Utilities
**Location:** `/home/adavidoff/git/elle3/elle-lsp/src/formatting.rs` (lines 54-65)

```rust
pub fn document_end_position(source: &str) -> (u32, u32) {
    let lines: Vec<&str> = source.lines().collect();

    if lines.is_empty() {
        return (0, 0);
    }

    let last_line = (lines.len() - 1) as u32;
    let last_char = lines[lines.len() - 1].len() as u32;

    (last_line, last_char)
}
```

Used to calculate the end range for document-level operations.

---

## 9. Key Design Patterns

### 9.1 Location-Aware Error Construction
```rust
// Pattern used throughout:
Diagnostic::new(
    severity,
    code,
    rule_name,
    message,
    location.clone(),  // Option<SourceLoc>
)
.with_suggestions(suggestions)
```

### 9.2 LSP Coordinate Conversion
```rust
// Pattern in all handlers:
let (line, col) = match &d.location {
    Some(loc) => (loc.line as u32, loc.col as u32),  // 1-based from Elle
    None => (0, 0),
};

// Convert to LSP 0-based:
json!({
    "start": { "line": line - 1, "character": col - 1 }
})
```

### 9.3 Diagnostic Notification Flow
```rust
// Triggered by didOpen/didChange:
compiler_state.compile_document(uri);
if let Some(doc) = compiler_state.get_document(uri) {
    // Convert diagnostics
    let diags = doc.diagnostics.iter().map(|d| { ... }).collect();
    
    // Send notification
    notifications.push(json!({
        "method": "textDocument/publishDiagnostics",
        "params": { "uri": uri, "diagnostics": diags }
    }));
}
```

---

## 10. Document State Management

### 10.1 DocumentState Structure
**Location:** `/home/adavidoff/git/elle3/elle-lsp/src/compiler_state.rs` (lines 14-31)

```rust
pub struct DocumentState {
    pub uri: String,
    pub source_text: String,
    pub compiled_expr: Option<ExprWithLoc>,
    pub symbol_index: SymbolIndex,
    pub diagnostics: Vec<elle::compiler::linter::diagnostics::Diagnostic>,
}
```

**Key Fields:**
- **uri**: Document identifier from client
- **source_text**: Current document content
- **compiled_expr**: Last successfully compiled expression (with location)
- **symbol_index**: Index for navigation features
- **diagnostics**: All diagnostics from compilation (errors + linter warnings)

### 10.2 Compilation State Flow
```
Document Open/Change
    ↓
Parse (Lexing, Reading, Conversion)
    ↓ [E0001-E0003 errors collected]
Lint (Semantic analysis)
    ↓ [W001, W002, etc. collected]
Publish Diagnostics Notification
    ↓ [Converted to LSP format]
Client receives diagnostic list
```

---

## 11. Summary of Key Files

| File | Purpose | Key Content |
|------|---------|-------------|
| `src/compiler/linter/diagnostics.rs` | Diagnostic type definition | Diagnostic struct, Severity enum, format_human() |
| `src/compiler/linter/mod.rs` | Linter integration | Linter::lint_expr(), error accumulation |
| `src/compiler/linter/rules.rs` | Linting rules | check_naming_convention(), check_call_arity(), builtin_arity() |
| `src/reader/token.rs` | Source location | SourceLoc struct with 1-based numbering |
| `elle-lsp/src/compiler_state.rs` | Document state | DocumentState, compile_document(), diagnostics collection |
| `elle-lsp/src/main.rs` | LSP protocol handler | Diagnostic conversion (lines 159-182, 220-243), publishDiagnostics |
| `elle-lsp/src/definition.rs` | Go-to-definition | Location conversion pattern (file, line, col → LSP range) |
| `elle-lsp/src/references.rs` | Find references | Location array building |
| `elle-lsp/src/hover.rs` | Hover information | 1-based ↔ 0-based conversion |
| `elle-lsp/src/formatting.rs` | Code formatting | document_end_position() utility |

---

## 12. Error Code Reference

### Compilation Errors
| Code | Stage | Message Pattern |
|------|-------|-----------------|
| E0001 | Lexing | "Lexer error: ..." |
| E0002 | Reading | "Reader error: ..." |
| E0003 | Conversion | "Conversion error: ..." |

### Linter Warnings
| Code | Rule | Message Pattern |
|------|------|-----------------|
| W001 | naming-kebab-case | "identifier '...' should use kebab-case" |
| W002 | arity-mismatch | "function '...' expects N argument(s) but got M" |

---

## 13. Notable Design Characteristics

1. **Layered Error Collection**: Errors from multiple stages (lexer, reader, converter, linter) accumulate in a single `diagnostics` vector

2. **Optional Source Locations**: Compilation errors (E0001-E0003) don't include source locations, while linter warnings (W001+) may have them from `ExprWithLoc`

3. **1-based Numbering in Elle, 0-based in LSP**: Consistent conversion pattern throughout all handlers

4. **No Suggestions in LSP**: The `suggestions` field on Diagnostic is populated by rules but not sent to LSP clients (potential enhancement)

5. **Dynamic File Handling**: File location in SourceLoc defaults to `<unknown>` for LSP-edited documents; actual file URI comes from client

6. **Severity Mapping**: Direct numeric mapping (Error→1, Warning→2, Info→3) follows LSP spec exactly

7. **Source Identifier**: All diagnostics include `"source": "elle-lint"` to identify the origin

---
