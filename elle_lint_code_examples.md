# Elle-Lint Error Handling - Code Examples

## 1. Creating a Diagnostic with Location

### Compiler Level (Basic)
```rust
// From src/compiler/linter/rules.rs
pub fn check_naming_convention(
    name: &str,
    location: &Option<SourceLoc>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if !is_valid_kebab_case(name) {
        let diag = Diagnostic::new(
            Severity::Warning,
            "W001",
            "naming-kebab-case",
            format!("identifier '{}' should use kebab-case", name),
            location.clone(),  // Pass location from AST
        )
        .with_suggestions(vec![format!("rename to '{}'", to_kebab_case(name))]);
        
        diagnostics.push(diag);
    }
}
```

### Elle-Lint Level (Enhanced)
```rust
// From elle-lint/src/rules/naming.rs
fn check_identifier_naming(
    name: &str,
    filename: &str,
    line: usize,
    column: usize,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if !is_valid_kebab_case(name) {
        let diag = Diagnostic::new(
            Severity::Warning,
            "W001".to_string(),
            "naming-kebab-case".to_string(),
            format!("identifier '{}' should use kebab-case", name),
            filename.to_string(),
            line,
            column,
            format!("(define {} ...)", name),  // Add source context
        )
        .with_suggestions(vec![format!("rename to '{}'", to_kebab_case(name))]);
        
        diagnostics.push(diag);
    }
}
```


## 2. Propagating Location Through AST Traversal

```rust
// From src/compiler/linter/mod.rs
fn check_expr(
    &mut self,
    expr: &Expr,
    loc: &Option<SourceLoc>,  // Location passed through recursion
    symbol_table: &crate::SymbolTable,
) {
    match expr {
        Expr::Define { name, value } => {
            self.check_expr(value, loc, symbol_table);
            
            // Check naming convention with location info
            if let Some(sym_name) = symbol_table.name(*name) {
                rules::check_naming_convention(sym_name, loc, &mut self.diagnostics);
            }
        }
        
        Expr::Call { func, args, .. } => {
            self.check_expr(func, loc, symbol_table);
            for arg in args {
                self.check_expr(arg, loc, symbol_table);
            }
            
            // Check arity with location info
            if let Expr::GlobalVar(sym) = &**func {
                rules::check_call_arity(
                    *sym,
                    args.len(),
                    loc,  // Pass location to rule checker
                    symbol_table,
                    &mut self.diagnostics,
                );
            }
        }
        
        Expr::If { cond, then, else_ } => {
            self.check_expr(cond, loc, symbol_table);
            self.check_expr(then, loc, symbol_table);
            self.check_expr(else_, loc, symbol_table);
        }
        
        // ... other expression types
    }
}
```


## 3. Severity Ordering and Filtering

```rust
// From src/compiler/linter/diagnostics.rs
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Info,      // Lowest priority
    Warning,   // Medium priority
    Error,     // Highest priority
}

// In elle-lint/src/lib.rs - filter by minimum severity
fn format_human(&self) -> String {
    let mut output = String::new();
    for diag in self.diagnostics() {
        if diag.severity >= self.config.min_severity {
            output.push_str(&diag.format_human());
        }
    }
    output
}

// Example: if min_severity is Warning
// - Info diagnostics are EXCLUDED
// - Warning diagnostics are INCLUDED
// - Error diagnostics are INCLUDED
```


## 4. Formatting Diagnostic Output

### Human-Readable Format
```rust
// From elle-lint/src/diagnostics.rs
pub fn format_human(&self) -> String {
    let mut output = String::new();
    
    // Header: file:line:col severity: rule
    output.push_str(&format!(
        "{}:{}:{} {}: {}\n",
        self.file, self.line, self.column, self.severity, self.rule
    ));
    
    // Location arrow
    output.push_str(&format!("  --> {}:{}\n", self.file, self.line));
    output.push_str("    |\n");
    
    // Source code line with line number
    output.push_str(&format!("  {} | {}\n", self.line, self.context));
    
    // Visual caret pointing to exact column
    let spaces = " ".repeat(self.column.saturating_sub(1));
    output.push_str(&format!("    | {}{}\n", spaces, "^"));
    
    // Main message
    output.push_str(&format!("\n{}\n", self.message));
    
    // Suggestions (if any)
    if !self.suggestions.is_empty() {
        output.push_str("suggestions:\n");
        for suggestion in &self.suggestions {
            output.push_str(&format!("  - {}\n", suggestion));
        }
    }
    
    output.push('\n');
    output
}

// Output example:
// test.lisp:5:2 warning: naming-kebab-case
//   --> test.lisp:5
//     |
//   5 | (define myVariable ...)
//     | ^
//
// identifier 'myVariable' should use kebab-case
// suggestions:
//   - rename to 'my-variable'
//
```

### JSON Format
```rust
// From elle-lint/src/lib.rs
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
            serde_json::json!({
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
    
    serde_json::to_string_pretty(&serde_json::json!({
        "diagnostics": diagnostics
    })).unwrap_or_default()
}

// Output example:
// {
//   "diagnostics": [
//     {
//       "severity": "warning",
//       "code": "W001",
//       "rule": "naming-kebab-case",
//       "message": "identifier 'myVariable' should use kebab-case",
//       "line": 5,
//       "column": 2,
//       "suggestions": ["rename to 'my-variable'"]
//     }
//   ]
// }
```


## 5. Builder Pattern for Diagnostic Construction

```rust
// Pattern: New -> Configure -> Build

// Basic diagnostic
let diag = Diagnostic::new(
    Severity::Warning,
    "W001",
    "naming-kebab-case",
    "identifier should use kebab-case",
    Some(source_loc),
);

// With suggestions chained
let diag = Diagnostic::new(
    Severity::Warning,
    "W001",
    "naming-kebab-case",
    "identifier should use kebab-case",
    Some(source_loc),
)
.with_suggestions(vec![
    "rename to 'foo-bar'".to_string(),
    "rename to 'baz-qux'".to_string(),
]);

// Without location
let diag = Diagnostic::new(
    Severity::Info,
    "I001",
    "test-rule",
    "test message",
    None,  // No location available
)
.with_suggestions(vec!["fix this".to_string()]);
```


## 6. SourceLoc Creation and Usage

```rust
// From src/reader/token.rs
pub struct SourceLoc {
    pub file: String,
    pub line: usize,
    pub col: usize,
}

impl SourceLoc {
    // Full construction
    pub fn new(file: impl Into<String>, line: usize, col: usize) -> Self {
        SourceLoc {
            file: file.into(),
            line,
            col,
        }
    }
    
    // Without file (defaults to unknown)
    pub fn from_line_col(line: usize, col: usize) -> Self {
        SourceLoc {
            file: "<unknown>".to_string(),
            line,
            col,
        }
    }
    
    // Beginning of file
    pub fn start() -> Self {
        SourceLoc {
            file: "<unknown>".to_string(),
            line: 1,
            col: 1,
        }
    }
}

impl Display for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}

// Usage in tests
#[test]
fn test_diagnostic_with_location() {
    let loc = SourceLoc::from_line_col(5, 2);
    let diag = Diagnostic::new(
        Severity::Warning,
        "W001",
        "naming-kebab-case",
        "test message",
        Some(loc),
    );
    assert_eq!(diag.location.unwrap().line, 5);
}
```


## 7. Optional Location Handling

```rust
// From src/compiler/linter/diagnostics.rs
pub fn format_human(&self) -> String {
    let mut output = String::new();
    
    match &self.location {
        Some(loc) => {
            // With location: full format
            output.push_str(&format!(
                "{}:{} {}: {}\n",
                loc.line, loc.col, self.severity, self.rule
            ));
            output.push_str(&format!("  message: {}\n", self.message));
        }
        None => {
            // Without location: graceful fallback
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


## 8. Arity Checking Rule

```rust
// From src/compiler/linter/rules.rs
pub fn check_call_arity(
    func_sym: SymbolId,
    arg_count: usize,
    location: &Option<SourceLoc>,
    symbol_table: &crate::SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(func_name) = symbol_table.name(func_sym) {
        if let Some(expected_arity) = builtin_arity(func_name) {
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
                )
                // Note: No suggestions added for arity mismatches
                ;
                
                diagnostics.push(diag);
            }
        }
    }
}

// Built-in arity database
fn builtin_arity(name: &str) -> Option<usize> {
    match name {
        // Arithmetic
        "+" | "-" | "*" | "/" => Some(2),
        // List operations
        "cons" => Some(2),
        "first" | "rest" | "length" => Some(1),
        // Math functions
        "abs" | "sqrt" | "sin" | "cos" | "floor" | "ceil" => Some(1),
        "pow" => Some(2),
        // String operations
        "string-length" => Some(1),
        "string-append" => Some(2),
        "substring" => Some(3),
        // Variadic/special forms return None
        "list" | "define" | "quote" | "begin" | "if" => None,
        _ => None,
    }
}
```


## 9. Integration Example

```rust
// Complete flow from file to output
// From elle-lint/src/lib.rs

let mut linter = Linter::new(config);

// 1. Lint a file
linter.lint_file(Path::new("test.l"))?;

// 2. Get all diagnostics (from compiler linter)
let diags = linter.diagnostics();
// These are compiler Diagnostic structs with Option<SourceLoc>

// 3. Format for output
let output = linter.format_output();

// Output selected based on config.format:
// - OutputFormat::Human: calls format_human()
//   - Filters by min_severity
//   - Calls diag.format_human() for each
// - OutputFormat::Json: calls format_json()
//   - Filters by min_severity
//   - Converts to JSON with line/col extraction

// 4. Display and exit
println!("{}", output);
let exit_code = linter.exit_code();
// 0 = no errors
// 1 = has errors
// 2 = has warnings
process::exit(exit_code);
```


## 10. Testing Patterns

```rust
// From elle-lint/src/diagnostics.rs and compiler/linter/diagnostics.rs

#[test]
fn test_diagnostic_creation() {
    let diag = Diagnostic::new(
        Severity::Error,
        "E001",
        "undefined-function",
        "function 'foo' is not defined",
        Some(SourceLoc::from_line_col(5, 2)),
    );
    
    assert_eq!(diag.severity, Severity::Error);
    assert_eq!(diag.rule, "undefined-function");
    assert_eq!(diag.line, 5);
}

#[test]
fn test_diagnostic_with_suggestions() {
    let diag = Diagnostic::new(
        Severity::Warning,
        "W001",
        "naming-kebab-case",
        "should use kebab-case",
        Some(SourceLoc::from_line_col(1, 1)),
    )
    .with_suggestions(vec![
        "rename to 'foo-bar'".to_string(),
        "rename to 'baz-qux'".to_string(),
    ]);
    
    assert_eq!(diag.suggestions.len(), 2);
}

#[test]
fn test_severity_ordering() {
    assert!(Severity::Info < Severity::Warning);
    assert!(Severity::Warning < Severity::Error);
    assert!(!(Severity::Error < Severity::Warning));
}

#[test]
fn test_location_formatting() {
    let loc = SourceLoc::new("test.lisp", 5, 10);
    assert_eq!(loc.to_string(), "test.lisp:5:10");
}

#[test]
fn test_format_with_location() {
    let diag = Diagnostic::new(
        Severity::Warning,
        "W001",
        "test-rule",
        "test message",
        Some(SourceLoc::from_line_col(42, 7)),
    );
    
    let formatted = diag.format_human();
    assert!(formatted.contains("42:7"));
    assert!(formatted.contains("warning"));
    assert!(formatted.contains("test-rule"));
}
```

