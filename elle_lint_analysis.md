# Elle-Lint Error Message Handling and Formatting Analysis

## Overview
Elle-lint uses a sophisticated two-tier error handling architecture:
1. **Compiler-integrated linter** with SourceLoc-aware diagnostics
2. **Elle-lint wrapper** that provides output formatting and configuration

## Architecture

### 1. SourceLoc Infrastructure (Source of Truth)

**Location:** `/home/adavidoff/git/elle3/src/reader/token.rs`

The `SourceLoc` struct serves as the foundational location tracking mechanism:

```rust
pub struct SourceLoc {
    pub file: String,
    pub line: usize,
    pub col: usize,
}
```

**Key Features:**
- Tracks file name, line number, and column number
- Implements `Display` trait for formatting: `file:line:col`
- Constructor methods:
  - `new(file, line, col)` - Full location with file
  - `from_line_col(line, col)` - Location without file (defaults to "<unknown>")
  - `start()` - Beginning of file location

---

## 2. Compiler Linter Diagnostic System

**Location:** `/home/adavidoff/git/elle3/src/compiler/linter/`

### Diagnostic Structure

**File:** `diagnostics.rs`

```rust
pub struct Diagnostic {
    pub severity: Severity,
    pub code: String,
    pub rule: String,
    pub message: String,
    pub location: Option<SourceLoc>,  // Contains file:line:col
    pub suggestions: Vec<String>,
}
```

**Severity Levels:**
```rust
pub enum Severity {
    Info,
    Warning,
    Error,
}
```
- Implements `Ord` trait for severity ordering
- Implements `Display` for string representation

### Error Formatting (Compiler Level)

The compiler's diagnostic provides a `format_human()` method:

```rust
pub fn format_human(&self) -> String {
    let mut output = String::new();
    
    match &self.location {
        Some(loc) => {
            // With location: "line:col severity: rule"
            output.push_str(&format!(
                "{}:{} {}: {}\n",
                loc.line, loc.col, self.severity, self.rule
            ));
            output.push_str(&format!("  message: {}\n", self.message));
        }
        None => {
            // Without location: "severity: rule"
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

### Linter Module Structure

**File:** `mod.rs`

The `Linter` struct operates on compiled AST expressions:

```rust
pub struct Linter {
    diagnostics: Vec<Diagnostic>,
}

impl Linter {
    pub fn lint_expr(&mut self, expr: &ExprWithLoc, symbol_table: &SymbolTable) {
        self.check_expr(&expr.expr, &expr.loc, symbol_table);
    }
}
```

**Key Operations:**
- `lint_expr(expr, symbol_table)` - Lint single expression with location
- `lint_exprs(exprs, symbol_table)` - Lint multiple expressions
- Recursively checks expression tree, passing location info through call stack
- Rules are applied via `rules::check_naming_convention()` and `rules::check_call_arity()`

### Rules Module

**File:** `rules.rs`

```rust
pub fn check_naming_convention(
    name: &str,
    location: &Option<SourceLoc>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Check kebab-case compliance
    if !is_valid_kebab_case(base_name) {
        let diag = Diagnostic::new(
            Severity::Warning,
            "W001",
            "naming-kebab-case",
            format!("identifier '{}' should use kebab-case", name),
            location.clone(),  // Pass location through
        )
        .with_suggestions(vec![format!("rename to '{}'", suggested_name)]);
        
        diagnostics.push(diag);
    }
}

pub fn check_call_arity(
    func_sym: SymbolId,
    arg_count: usize,
    location: &Option<SourceLoc>,
    symbol_table: &SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Validate function argument counts
    if arg_count != expected_arity {
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
    }
}
```

**Implemented Rules:**
1. **Naming Convention (W001):** Kebab-case validation
2. **Arity Mismatch (W002):** Function argument count validation
3. Built-in function arity definitions included

---

## 3. Elle-Lint Wrapper Layer

**Location:** `/home/adavidoff/git/elle3/elle-lint/`

### Main Components

**File:** `src/diagnostics.rs`

Elle-lint defines its own Diagnostic struct (different from compiler's):

```rust
pub struct Diagnostic {
    pub severity: Severity,
    pub code: String,
    pub rule: String,
    pub message: String,
    pub file: String,              // Explicit file field
    pub line: usize,               // Explicit line field
    pub column: usize,             // Explicit column field
    pub context: String,           // Source code line context
    pub suggestions: Vec<String>,
}
```

### Enhanced Formatting (Elle-Lint Level)

```rust
pub fn format_human(&self) -> String {
    let mut output = String::new();
    
    // Header line with all location details
    output.push_str(&format!(
        "{}:{}:{} {}: {}\n",
        self.file, self.line, self.column, self.severity, self.rule
    ));
    
    // Arrow pointing to location
    output.push_str(&format!("  --> {}:{}\n", self.file, self.line));
    output.push_str("    |\n");
    output.push_str(&format!("  {} | {}\n", self.line, self.context));
    
    // Visual caret pointing to column
    let spaces = " ".repeat(self.column.saturating_sub(1));
    output.push_str(&format!("    | {}{}\n", spaces, "^"));
    
    // Main message
    output.push_str(&format!("\n{}\n", self.message));
    
    // Suggestions
    if !self.suggestions.is_empty() {
        output.push_str("suggestions:\n");
        for suggestion in &self.suggestions {
            output.push_str(&format!("  - {}\n", suggestion));
        }
    }
    
    output.push('\n');
    output
}
```

**JSON Output Method:**

```rust
pub fn to_json(&self) -> Value {
    json!({
        "severity": self.severity.to_string(),
        "code": self.code,
        "rule": self.rule,
        "message": self.message,
        "file": self.file,
        "line": self.line,
        "column": self.column,
        "context": self.context,
        "suggestions": self.suggestions,
    })
}
```

### Main Linter Wrapper

**File:** `src/lib.rs`

```rust
pub struct Linter {
    config: LintConfig,
    compiler_linter: CompilerLinter,
}

impl Linter {
    pub fn lint_str(&mut self, code: &str, filename: &str) -> Result<(), String> {
        // Parse code using lexer and reader
        let mut lexer = elle::Lexer::new(code);
        let tokens = /* read all tokens */;
        
        let mut reader = elle::Reader::new(tokens);
        while let Some(result) = reader.try_read(&mut symbols) {
            match result {
                Ok(value) => {
                    // Convert value to expr for linting
                    let expr = value_to_expr(&value, &mut symbols)?;
                    // Wrap in ExprWithLoc with location info
                    let expr_with_loc = ExprWithLoc::new(expr, None);
                    self.compiler_linter.lint_expr(&expr_with_loc, &symbols);
                }
                Err(e) => return Err(format!("Read error: {}", e)),
            }
        }
        Ok(())
    }
    
    pub fn format_output(&self) -> String {
        match self.config.format {
            OutputFormat::Human => self.format_human(),
            OutputFormat::Json => self.format_json(),
        }
    }
}
```

### Output Filtering

```rust
fn format_human(&self) -> String {
    let mut output = String::new();
    for diag in self.diagnostics() {
        if diag.severity >= self.config.min_severity {
            output.push_str(&diag.format_human());
        }
    }
    output
}

fn format_json(&self) -> String {
    let diagnostics: Vec<_> = self
        .diagnostics()
        .iter()
        .filter(|d| d.severity >= self.config.min_severity)
        .map(|d| {
            let (line, col) = match &d.location {
                Some(loc) => (loc.line as u32, loc.col as u32),
                None => (0, 0),
            };
            json!({
                "severity": d.severity.to_string(),
                "code": d.code,
                "rule": d.rule,
                "message": d.message,
                "line": line,
                "column": col,
                "suggestions": d.suggestions,
            })
        })
        .collect();
    
    serde_json::to_string_pretty(&json!({
        "diagnostics": diagnostics
    })).unwrap_or_default()
}
```

---

## 4. Command-Line Interface

**File:** `src/main.rs`

The CLI provides user-facing configuration:

```rust
fn main() {
    let mut format = OutputFormat::Human;
    let mut min_severity = Severity::Info;
    
    // Parse arguments:
    // --format [json|text]
    // --level [error|warning|info]
    
    let config = LintConfig {
        min_severity,
        format,
    };
    let mut linter = Linter::new(config);
    
    for file_path in files {
        linter.lint_file(path);
    }
    
    println!("{}", linter.format_output());
    process::exit(linter.exit_code());
}
```

**Exit Codes:**
- `0` - No errors
- `1` - Has errors
- `2` - Has warnings (but no errors)

---

## 5. Error Message Flow Diagram

```
Source Code File
       ↓
[Lexer] → Tokens with SourceLoc
       ↓
[Reader] → Values
       ↓
[value_to_expr] → Expr
       ↓
[ExprWithLoc] → (Expr, Option<SourceLoc>)
       ↓
[Compiler Linter] → Applies rules
       ├─ check_naming_convention()
       ├─ check_call_arity()
       └─ Produces: Vec<Diagnostic> with Option<SourceLoc>
       ↓
[Elle-lint Wrapper] 
       ├─ Filters by severity
       └─ Formats for output
           ├─ Human-readable (with visual cues)
           └─ JSON (structured data)
       ↓
[Output]
```

---

## 6. Key Features & Patterns

### Builder Pattern for Suggestions
```rust
let diag = Diagnostic::new(/* fields */)
    .with_suggestions(vec![suggestion1, suggestion2]);
```

### Severity Ordering
```rust
if diag.severity >= self.config.min_severity {
    // Include in output
}
```
Severity is ordered: `Info < Warning < Error`

### Optional Location Handling
The system gracefully handles missing location info:
- With location: Shows `file:line:col severity: rule`
- Without location: Shows `severity: rule` only

### Visual Formatting (Human Output)
- File location with arrow: `  --> file:line`
- Source code line with line number
- Caret (^) pointing to column
- Message and suggestions below

### JSON Structure
Provides structured output for tooling integration with all fields separately

---

## 7. Actual File Paths

| Component | Path |
|-----------|------|
| Compiler SourceLoc | `/home/adavidoff/git/elle3/src/reader/token.rs` |
| Compiler Linter (main) | `/home/adavidoff/git/elle3/src/compiler/linter/mod.rs` |
| Compiler Diagnostics | `/home/adavidoff/git/elle3/src/compiler/linter/diagnostics.rs` |
| Compiler Rules | `/home/adavidoff/git/elle3/src/compiler/linter/rules.rs` |
| Elle-lint Diagnostics | `/home/adavidoff/git/elle3/elle-lint/src/diagnostics.rs` |
| Elle-lint Main | `/home/adavidoff/git/elle3/elle-lint/src/lib.rs` |
| Elle-lint CLI | `/home/adavidoff/git/elle3/elle-lint/src/main.rs` |
| Elle-lint Rules | `/home/adavidoff/git/elle3/elle-lint/src/rules/` |
| Integration Tests | `/home/adavidoff/git/elle3/elle-lint/tests/integration_tests.rs` |

---

## 8. Example Error Output

**Human format:**
```
test.lisp:5:2 warning: naming-kebab-case
  --> test.lisp:5
    |
  5 | (define myVariable ...)
    | ^
  
identifier 'myVariable' should use kebab-case
suggestions:
  - rename to 'my-variable'

```

**JSON format:**
```json
{
  "diagnostics": [
    {
      "severity": "warning",
      "code": "W001",
      "rule": "naming-kebab-case",
      "message": "identifier 'myVariable' should use kebab-case",
      "line": 5,
      "column": 2,
      "suggestions": ["rename to 'my-variable'"]
    }
  ]
}
```

---

## Summary

Elle-lint uses a **two-tier architecture**:

1. **Compiler Layer** (core functionality)
   - Uses `SourceLoc` for precise location tracking
   - Provides basic diagnostic formatting
   - Implements linting rules
   - Passes location through AST traversal

2. **Elle-lint Wrapper** (user-facing)
   - Rewraps compiler diagnostics
   - Adds visual formatting enhancements
   - Provides multiple output formats
   - Implements severity filtering
   - Provides CLI interface

The design ensures that location information is preserved throughout the pipeline, from lexing through rule application to final output formatting.
