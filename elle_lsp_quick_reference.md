# Elle-LSP Error Handling - Quick Reference Guide

## File Structure Map

```
elle3/
├── src/
│   ├── reader/
│   │   └── token.rs                    # SourceLoc (1-based line/col)
│   └── compiler/
│       └── linter/
│           ├── diagnostics.rs         # Diagnostic struct
│           ├── mod.rs                 # Linter class
│           └── rules.rs               # Linting rules & utilities
└── elle-lsp/src/
    ├── compiler_state.rs              # Document state & compilation
    ├── main.rs                        # LSP protocol & diagnostic conversion
    ├── definition.rs                  # Location format pattern
    ├── references.rs                  # Location array pattern
    ├── hover.rs                       # 1-based ↔ 0-based conversion
    └── formatting.rs                  # Document utilities
```

## Critical Conversion Points

### 1. SourceLoc Creation (Elle compiler)
```rust
// 1-based numbering
SourceLoc::new("file.l", 5, 10)    // Line 5, Column 10
SourceLoc::from_line_col(5, 10)    // Same, with <unknown> file
```

### 2. LSP Input (0-based from client)
```json
{
  "position": {
    "line": 4,         // LSP 0-based = Elle line 5
    "character": 9     // LSP 0-based = Elle col 10
  }
}
```

### 3. Conversion Pattern (in all handlers)
```rust
// Inbound: LSP 0-based → Elle 1-based
let target_line = lsp_line as usize + 1;
let target_col = lsp_char as usize + 1;

// Compare with SourceLoc (1-based)
if usage_loc.line == target_line { ... }

// Outbound: Elle 1-based → LSP 0-based
json!({
    "line": elle_line - 1,           // Back to 0-based
    "character": elle_col - 1        // Back to 0-based
})
```

## Diagnostic Conversion (Key Code)

### Input: Elle Diagnostic
```rust
Diagnostic {
    severity: Severity::Warning,
    code: "W001",
    rule: "naming-kebab-case",
    message: "identifier 'squareNumber' should use kebab-case",
    location: Some(SourceLoc { file: "<unknown>", line: 5, col: 10 }),
    suggestions: vec!["rename to 'square-number'"],
}
```

### Conversion in main.rs (lines 162-181)
```rust
let (line, col) = match &d.location {
    Some(loc) => (loc.line as u32, loc.col as u32),      // 1-based
    None => (0, 0),                                         // Default
};
json!({
    "range": {
        "start": { "line": line - 1, "character": col - 1 },  // 0-based
        "end": { "line": line - 1, "character": col }          // End at col
    },
    "severity": 2,          // Error→1, Warning→2, Info→3
    "code": "W001",         // Preserved
    "source": "elle-lint",  // Always "elle-lint"
    "message": "identifier 'squareNumber' should use kebab-case"  // Preserved
})
```

### Output: LSP Diagnostic
```json
{
    "range": {
        "start": { "line": 4, "character": 9 },
        "end": { "line": 4, "character": 10 }
    },
    "severity": 2,
    "code": "W001",
    "source": "elle-lint",
    "message": "identifier 'squareNumber' should use kebab-case"
}
```

## Error Codes by Stage

```
Compilation Pipeline:
├─ Lexing       → E0001 (Lexer error)
├─ Reading      → E0002 (Reader error)
├─ Conversion   → E0003 (Conversion error)
└─ Linting      → W001, W002, ... (Warnings & Info)
```

## Diagnostic Flow

```
1. Document Opens/Changes (LSP client)
   ↓
2. compiler_state.compile_document(uri)
   ├─ Lexing        [E0001 if error, location=None]
   ├─ Reading       [E0002 if error, location=None]
   ├─ Conversion    [E0003 if error, location=None]
   └─ Linting       [W001+, may have location from ExprWithLoc]
   ↓
3. Diagnostics collected in DocumentState.diagnostics
   ↓
4. LSP Protocol Handler (main.rs)
   ├─ Extract location (line, col) with None→(0,0)
   ├─ Convert line/col from 1-based to 0-based
   ├─ Map severity: Error→1, Warning→2, Info→3
   ├─ Include code, source="elle-lint", message
   └─ Skip: rule, suggestions, location.file
   ↓
5. textDocument/publishDiagnostics Notification
   └─ Sent to LSP client
```

## SourceLoc Properties

| Property | Type | Example | Note |
|----------|------|---------|------|
| file | String | `"<unknown>"` or `"file.l"` | LSP diagnostics don't use |
| line | usize | `5` | **1-based** |
| col | usize | `10` | **1-based** |
| Display | fmt | `"file.l:5:10"` | Manual format |

## Location Access Patterns

### In compiler_state.rs (Diagnostics Collection)
```rust
// No location available from early-stage errors
Diagnostic::new(Severity::Error, "E0001", "syntax-error", msg, None)

// Location available from linter
let location: Option<SourceLoc> = expr.loc.clone();
```

### In definition.rs (Navigation)
```rust
// Pattern for converting SourceLoc to LSP location:
if let Some(def_loc) = symbol_index.symbol_locations.get(&sym_id) {
    let uri = format!("file://{}", def_loc.file);
    json!({
        "uri": uri,
        "range": {
            "start": {
                "line": def_loc.line.saturating_sub(1),     // 1-based → 0-based
                "character": def_loc.col.saturating_sub(1)   // 1-based → 0-based
            },
            "end": {
                "line": def_loc.line.saturating_sub(1),
                "character": def_loc.col
            }
        }
    })
}
```

### In hover.rs (Symbol Lookup)
```rust
// Pattern for finding symbol at LSP position:
let target_line = line as usize + 1;      // 0-based → 1-based
let target_col = character as usize + 1;  // 0-based → 1-based

for (sym_id, usages) in &symbol_index.symbol_usages {
    for usage_loc in usages {
        if usage_loc.line == target_line {  // Compare 1-based values
            // Found symbol at position
        }
    }
}
```

## Severity Mapping

```rust
// Elle Internal
Severity::Error   → LSP Code: 1  → DiagnosticSeverity.Error
Severity::Warning → LSP Code: 2  → DiagnosticSeverity.Warning
Severity::Info    → LSP Code: 3  → DiagnosticSeverity.Information
```

## Key Utility Functions

### format_human() - Human Readable Output
```rust
// Input: Diagnostic
// Output: String like:
//   5:10 warning: naming-kebab-case
//     message: identifier 'squareNumber' should use kebab-case
//     suggestions:
//       - rename to 'square-number'

diagnostic.format_human()
```

### document_end_position() - Document Bounds
```rust
// Calculates (line, character) at document end
// Returns: (u32, u32) - last line (0-based), last column
let (end_line, end_char) = document_end_position(&doc.source_text);
```

### check_naming_convention() - W001 Rule
```rust
// Validates identifier uses kebab-case
// Inputs: name, location, diagnostics
// Outputs: Diagnostic added to vec if violation found
check_naming_convention(name, &expr.loc, &mut diagnostics);
```

### check_call_arity() - W002 Rule
```rust
// Validates function argument count
// Inputs: function_sym, arg_count, location, symbol_table, diagnostics
// Outputs: Diagnostic added if mismatch
check_call_arity(sym_id, args.len(), &expr.loc, &symbol_table, &mut diags);
```

## Important Notes

1. **None Location Handling**: When location is None, defaults to (0, 0) in LSP format
2. **File Path**: SourceLoc.file usually `<unknown>` for LSP documents; actual URI from client
3. **Suggestions Not Sent**: Diagnostic.suggestions populated but not included in LSP output
4. **Severity Codes**: 1=Error, 2=Warning, 3=Info (LSP standard)
5. **Source Identifier**: Always `"elle-lint"` for linter diagnostics
6. **Early Errors**: E0001-E0003 usually have None location (from parsing stages)
7. **Late Warnings**: W001+ usually have location (from linter on ExprWithLoc)

## Testing Diagnostic Creation

```rust
#[test]
fn test_diagnostic_creation() {
    let loc = SourceLoc::from_line_col(5, 2);
    let diag = Diagnostic::new(
        Severity::Warning,
        "W001",
        "naming-kebab-case",
        "identifier should use kebab-case",
        Some(loc),
    );
    assert_eq!(diag.severity, Severity::Warning);
    assert_eq!(diag.code, "W001");
    assert_eq!(diag.location.unwrap().line, 5);
    assert_eq!(diag.location.unwrap().col, 2);
}
```

