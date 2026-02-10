# Elle-LSP Error Handling Analysis - Complete Documentation Index

## Overview

This documentation package provides a comprehensive analysis of how elle-lsp handles and formats error messages with source location information. The analysis covers the diagnostic message structure, SourceLoc infrastructure, LSP protocol integration, and all associated utility functions.

**Analysis Date:** February 10, 2026  
**Scope:** 9 key files across elle compiler and elle-lsp  
**Total Code Analyzed:** 1,869 lines

---

## Documentation Files

### 1. **elle_lsp_error_handling_analysis.md** (595 lines)
**Comprehensive Technical Analysis**

The definitive reference document covering all aspects of error handling in elle-lsp.

**Sections:**
1. Diagnostic Message Structure - Core types and components
2. SourceLoc Infrastructure - Location tracking with 1-based numbering
3. Error Creation and Flow - Diagnostic construction patterns
4. LSP Diagnostic Conversion - Critical protocol boundary conversion
5. Protocol Integration - textDocument/publishDiagnostics flow
6. Error Handling in Compilation Pipeline - 4-stage compilation with error handling
7. File Location Handling - Document URI and file path management
8. Utility Functions for Error Formatting - format_human(), document_end_position()
9. Key Design Patterns - Location-aware errors, coordinate conversion, notifications
10. Document State Management - DocumentState structure and lifecycle
11. Summary of Key Files - Table of files with purposes
12. Error Code Reference - E0001-E0003 and W001-W002 codes
13. Notable Design Characteristics - Key insights and design decisions

**Use this document for:**
- Understanding the full error handling architecture
- Learning how diagnostics flow through the system
- Understanding the 1-based vs 0-based coordinate conversion
- Detailed implementation patterns and design decisions

---

### 2. **elle_lsp_quick_reference.md** (270 lines)
**Quick Reference and Implementation Guide**

A concise, developer-friendly guide for working with error handling code.

**Sections:**
1. File Structure Map - Directory layout and file purposes
2. Critical Conversion Points - 3-step conversion process
3. Diagnostic Conversion - Complete before/after example
4. Error Codes by Stage - Compilation pipeline error codes
5. Diagnostic Flow - Visual flow diagram with annotations
6. SourceLoc Properties - Table of properties and usage
7. Location Access Patterns - Code patterns in different files
8. Severity Mapping - Elle to LSP severity mapping
9. Key Utility Functions - Signatures and descriptions
10. Important Notes - 7 key implementation details
11. Testing - Example test case for diagnostic creation

**Use this document for:**
- Quick lookup during development
- Understanding conversion patterns
- Copy-paste code examples
- Implementation guidelines

---

### 3. **elle_lsp_architecture.md** (496 lines)
**System Architecture and Design**

High-level overview of how the error handling system fits into the broader LSP architecture.

**Sections:**
- Component interaction diagrams
- Data flow through the system
- Module responsibilities
- Compilation and error handling flow

**Use this document for:**
- System-level understanding
- Architectural decisions
- Component relationships

---

### 4. **elle_lsp_exploration.md** (911 lines)
**Detailed Code Exploration**

Raw exploration notes with code snippets and detailed examination of each component.

**Use this document for:**
- In-depth code examination
- Exploration notes and findings
- Detailed component analysis

---

## Key Files in Source Code

### Compiler Library (src/)
```
src/compiler/linter/
├── diagnostics.rs    (120 lines) - Diagnostic struct, Severity enum, format_human()
├── mod.rs           (262 lines) - Linter class, lint_expr()
└── rules.rs         (194 lines) - Linting rules, utilities

src/reader/
└── token.rs         (119 lines) - SourceLoc struct with 1-based numbering
```

### LSP Server (elle-lsp/src/)
```
elle-lsp/src/
├── compiler_state.rs     (246 lines) - Document state, compilation, diagnostics
├── main.rs              (544 lines) - LSP protocol handler, diagnostic conversion
├── definition.rs        (112 lines) - Location conversion pattern
├── references.rs        (161 lines) - Location array building
├── hover.rs            (111 lines) - 1-based ↔ 0-based conversion
└── formatting.rs       (107 lines) - Document utilities
```

---

## Quick Navigation

### By Topic

**Understanding Error Structure:**
- Diagnostic struct → elle_lsp_error_handling_analysis.md § 1
- SourceLoc struct → elle_lsp_error_handling_analysis.md § 2

**Converting to LSP Format:**
- Main conversion → elle_lsp_error_handling_analysis.md § 4
- Quick example → elle_lsp_quick_reference.md § 3
- Before/after data → elle_lsp_quick_reference.md § 3

**Coordinate Conversion (1-based ↔ 0-based):**
- Pattern explanation → elle_lsp_error_handling_analysis.md § 9.2
- Quick reference → elle_lsp_quick_reference.md § 1, 3
- In all handlers → elle_lsp_quick_reference.md § 7

**Compilation Pipeline:**
- Full details → elle_lsp_error_handling_analysis.md § 6
- Flow diagram → elle_lsp_quick_reference.md § 5
- Error codes → elle_lsp_quick_reference.md § 4

**Utility Functions:**
- Detailed → elle_lsp_error_handling_analysis.md § 8
- Quick reference → elle_lsp_quick_reference.md § 8

**Design Patterns:**
- Location-aware errors → elle_lsp_error_handling_analysis.md § 9.1
- LSP conversion → elle_lsp_error_handling_analysis.md § 9.2
- Notification flow → elle_lsp_error_handling_analysis.md § 9.3

### By File

**src/compiler/linter/diagnostics.rs:**
- §1, §8.1 in error_handling_analysis.md
- Severity mapping in quick_reference.md § 8

**src/compiler/linter/mod.rs:**
- §3.2 in error_handling_analysis.md

**src/compiler/linter/rules.rs:**
- §3.1, §8.2 in error_handling_analysis.md
- Key utilities in quick_reference.md § 8

**src/reader/token.rs:**
- §2 in error_handling_analysis.md
- Properties in quick_reference.md § 6

**elle-lsp/src/compiler_state.rs:**
- §6 in error_handling_analysis.md
- §10 in error_handling_analysis.md
- Error codes in quick_reference.md § 4

**elle-lsp/src/main.rs:**
- §4, §5 in error_handling_analysis.md
- Conversion example in quick_reference.md § 3
- Flow in quick_reference.md § 5

**elle-lsp/src/definition.rs, references.rs, hover.rs:**
- §7 in error_handling_analysis.md
- Location patterns in quick_reference.md § 7

**elle-lsp/src/formatting.rs:**
- §8.3 in error_handling_analysis.md

---

## Critical Concepts

### The 1-Based vs 0-Based Problem
- **Elle Compiler:** Uses 1-based line/column numbers in SourceLoc
- **LSP Protocol:** Uses 0-based line/character numbers per spec
- **Solution:** Consistent ±1 conversion at protocol boundary

**When to convert:**
- **Inbound:** LSP 0-based → Elle 1-based (+1)
- **Outbound:** Elle 1-based → LSP 0-based (-1)

See: error_handling_analysis.md § 4, quick_reference.md § 1, 3

### The Diagnostic Flow
```
Code Compilation → Diagnostics Collected → LSP Conversion → Client Display
```

1. Lexing/Reading/Conversion errors (E0001-E0003) - no location
2. Linter warnings (W001+) - may have location from ExprWithLoc
3. All stored in DocumentState.diagnostics
4. Converted to LSP format on request
5. Sent via textDocument/publishDiagnostics

See: error_handling_analysis.md § 6, quick_reference.md § 5

### Location Conversion Pattern
Used consistently across all LSP handlers:

```rust
// Input: LSP position (0-based)
let target_line = line as usize + 1;
let target_col = character as usize + 1;

// Compare with SourceLoc (1-based)
if source_loc.line == target_line { ... }

// Output: Convert back to 0-based
{ "line": source_loc.line - 1, "character": source_loc.col - 1 }
```

See: error_handling_analysis.md § 7.3, quick_reference.md § 3, 7

---

## Implementation Examples

### Creating a Diagnostic
```rust
Diagnostic::new(
    Severity::Warning,
    "W001",
    "naming-kebab-case",
    "identifier 'squareNumber' should use kebab-case",
    Some(SourceLoc::from_line_col(5, 10)),
)
.with_suggestions(vec!["rename to 'square-number'".to_string()])
```

### Converting to LSP
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

### Finding Symbol at Position
```rust
// Input from LSP (0-based)
let target_line = lsp_line as usize + 1;  // Convert to 1-based

// Compare with SourceLoc (1-based)
for usage_loc in &symbol_index.symbol_usages {
    if usage_loc.line == target_line { ... }  // Match!
}

// Output (convert back to 0-based)
{ "line": usage_loc.line - 1, "character": usage_loc.col - 1 }
```

---

## Error Codes

| Code | Severity | Stage | Location | Message Pattern |
|------|----------|-------|----------|-----------------|
| E0001 | Error | Lexing | None | "Lexer error: ..." |
| E0002 | Error | Reading | None | "Reader error: ..." |
| E0003 | Error | Conversion | None | "Conversion error: ..." |
| W001 | Warning | Linting | Optional | "identifier '...' should use kebab-case" |
| W002 | Warning | Linting | Optional | "function '...' expects N argument(s) but got M" |

---

## Key Insights

1. **Layered Errors:** Errors from 4 compilation stages accumulate in single vector

2. **Optional Locations:** Early errors have None, linter errors may have location

3. **Protocol Boundary:** Critical conversion happens in main.rs lines 162-181, 224-243

4. **Suggestions Not Sent:** diagnostic.suggestions populated but not in LSP output (enhancement opportunity)

5. **File Handling:** SourceLoc.file usually `<unknown>`; actual URI from LSP client

6. **Source ID:** Always `"elle-lint"` in diagnostics to identify origin

7. **Severity Mapping:** Error→1, Warning→2, Info→3 (LSP standard)

8. **Coordinate Consistency:** Universal +1/-1 pattern across all handlers

---

## Next Steps for Enhancement

Based on the current implementation, potential enhancements include:

1. **Send Suggestions:** Implement CodeAction to expose diagnostic.suggestions
2. **Better Location Tracking:** Propagate locations through early stages
3. **File Path Resolution:** Use actual file paths instead of `<unknown>`
4. **More Rules:** Implement W003+ for additional linting rules
5. **Related Information:** Add DiagnosticRelatedInformation for context
6. **Grouping:** Support diagnostic grouping and categorization

See: error_handling_analysis.md § 13

---

## How to Use This Documentation

### For Understanding
1. Start with **quick_reference.md** to understand concepts
2. Read **error_handling_analysis.md** § 1-5 for structure
3. Read **error_handling_analysis.md** § 6-9 for implementation details
4. Reference **architecture.md** for system-wide perspective

### For Implementation
1. Reference **quick_reference.md** for code patterns
2. Use **error_handling_analysis.md** for detailed guidelines
3. Look at specific files for actual implementation examples
4. Check error_handling_analysis.md § 11 for file summary table

### For Debugging
1. Check **quick_reference.md** § 5 for diagnostic flow
2. Verify coordinate conversion using § 3 pattern
3. Check **error_handling_analysis.md** § 7.3 for location access
4. Confirm error code mapping using § 12 reference

---

## Document Statistics

| Document | Lines | Sections | Figures | Code Examples |
|----------|-------|----------|---------|----------------|
| error_handling_analysis.md | 595 | 13 | 4 | 25+ |
| quick_reference.md | 270 | 12 | 5 | 15+ |
| architecture.md | 496 | 6 | 8 | 10+ |
| exploration.md | 911 | N/A | 0 | 30+ |
| **Total** | **2,272** | **~35** | **~17** | **80+** |

---

## References

All file paths are absolute paths from /home/adavidoff/git/elle3:

- Compiler Library: `src/compiler/linter/`, `src/reader/`
- LSP Server: `elle-lsp/src/`
- Build Output: `elle-lsp/target/` (excluded from analysis)

---

**Last Updated:** February 10, 2026  
**Analysis Tool:** Elle-LSP Code Explorer  
**Format:** Markdown with code examples

