# Elle-Lint Error Message Handling Analysis

Complete analysis of how elle-lint handles and formats error messages with source location information.

## Documents Included

### 1. **SUMMARY.txt** (Start Here)
Executive summary of findings including:
- Architecture overview
- SourceLoc infrastructure
- Error message structure
- Data flow diagram
- Key implementation patterns
- File locations

### 2. **elle_lint_analysis.md** (Main Reference)
Comprehensive detailed analysis covering:
- Overview of two-tier architecture
- SourceLoc infrastructure details
- Compiler linter diagnostic system
- Elle-lint wrapper layer specifics
- CLI interface
- Error message flow diagram
- Key features and patterns
- Full file path reference
- Example error outputs

### 3. **elle_lint_quick_reference.txt** (Quick Lookup)
Fast reference guide with:
- 9 key files with descriptions
- Architecture summary (at a glance)
- Error message structure
- Formatting examples
- Key patterns
- Implemented rules
- Flow diagram

### 4. **elle_lint_code_examples.md** (Code Reference)
10 practical code examples showing:
- Creating diagnostics with location
- Propagating location through AST
- Severity ordering and filtering
- Formatting outputs (human & JSON)
- Builder pattern usage
- SourceLoc creation
- Optional location handling
- Arity checking rules
- Integration example
- Testing patterns

## Quick Start

### Understanding the Architecture

Elle-lint uses a **two-tier error handling system**:

**Tier 1: Compiler Linter** (`src/compiler/linter/`)
- Core linting logic
- Uses `SourceLoc` for precise location tracking
- Produces `Diagnostic` structs with `Option<SourceLoc>`
- Implements linting rules

**Tier 2: Elle-Lint Wrapper** (`elle-lint/src/`)
- User-facing interface
- Integrates compiler linter
- Provides multiple output formats
- Implements severity filtering
- CLI argument parsing

### Key Files

| File | Purpose |
|------|---------|
| `/src/reader/token.rs` | SourceLoc struct definition |
| `/src/compiler/linter/mod.rs` | Main linter with AST traversal |
| `/src/compiler/linter/diagnostics.rs` | Diagnostic struct and Severity enum |
| `/src/compiler/linter/rules.rs` | Linting rules implementation |
| `/elle-lint/src/lib.rs` | Elle-lint wrapper library |
| `/elle-lint/src/main.rs` | CLI interface |
| `/elle-lint/src/diagnostics.rs` | Enhanced diagnostic formatting |

### SourceLoc Infrastructure

```rust
pub struct SourceLoc {
    pub file: String,  // Source file name
    pub line: usize,   // Line number (1-based)
    pub col: usize,    // Column number (1-based)
}
```

Used throughout the pipeline to track error locations from lexing through rule application.

### Error Message Structure

**Compiler Level:**
- `severity`: Info, Warning, or Error
- `code`: Diagnostic code (e.g., "W001")
- `rule`: Rule name (e.g., "naming-kebab-case")
- `message`: Error description
- `location`: Optional SourceLoc
- `suggestions`: Fix suggestions

**Elle-Lint Level:**
- All of above plus:
- `file`, `line`, `column`: Explicit fields
- `context`: Source code line for display

### Output Formats

**Human-Readable:**
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

**JSON:**
```json
{
  "diagnostics": [{
    "severity": "warning",
    "code": "W001",
    "rule": "naming-kebab-case",
    "message": "identifier 'myVariable' should use kebab-case",
    "line": 5,
    "column": 2,
    "suggestions": ["rename to 'my-variable'"]
  }]
}
```

## Key Findings

### Error Handling Patterns

1. **Builder Pattern** for creating diagnostics with optional suggestions
2. **Option Handling** for graceful degradation without location
3. **Severity Filtering** using `Ord` trait on Severity enum
4. **Location Propagation** through recursive AST traversal
5. **Multiple Output Formats** for different use cases

### Implemented Rules

**W001: naming-kebab-case**
- Validates kebab-case identifiers
- Allows single letters and suffixes (?, !)
- Provides correction suggestions

**W002: arity-mismatch**
- Validates function argument counts
- Uses built-in function arity database
- Covers 30+ built-in functions

### Data Flow

```
Source Code
  ↓ [Lexer] → Tokens with SourceLoc
  ↓ [Reader] → Values
  ↓ [value_to_expr] → Expr
  ↓ [ExprWithLoc] → (Expr, Option<SourceLoc>)
  ↓ [Compiler Linter] → check_expr (recursive)
    ├─ check_naming_convention()
    ├─ check_call_arity()
    └─ Produces: Vec<Diagnostic>
  ↓ [Elle-lint Wrapper] → format_output()
    ├─ Severity filtering
    └─ Format selection
        ├─ Human-readable
        └─ JSON
  ↓ [CLI Output]
    └─ Exit code (0, 1, or 2)
```

## Design Strengths

1. **Separation of Concerns** - Compiler separate from CLI
2. **Precise Location Tracking** - SourceLoc preserved through pipeline
3. **Flexible Output** - Multiple formats with configurable filtering
4. **Extensible Rules** - Easy to add new linting rules
5. **Graceful Degradation** - Works with or without location info

## Usage Examples

### Command Line

```bash
elle-lint test.l                              # Lint single file
elle-lint src/ --format json                  # JSON output
elle-lint script.l --level error              # Only show errors
```

### Programmatic

```rust
let config = LintConfig {
    min_severity: Severity::Warning,
    format: OutputFormat::Human,
};
let mut linter = Linter::new(config);
linter.lint_file(Path::new("test.l"))?;
println!("{}", linter.format_output());
process::exit(linter.exit_code());
```

## Document Navigation

- **Start with:** SUMMARY.txt (5 min read)
- **For details:** elle_lint_analysis.md (comprehensive guide)
- **Quick lookup:** elle_lint_quick_reference.txt (reference)
- **Code examples:** elle_lint_code_examples.md (implementations)

## Directory Structure in Codebase

```
/home/adavidoff/git/elle3/
├── src/
│   ├── reader/
│   │   └── token.rs              # SourceLoc definition
│   └── compiler/
│       └── linter/
│           ├── mod.rs            # Main Linter struct
│           ├── diagnostics.rs    # Diagnostic & Severity
│           └── rules.rs          # Linting rules
└── elle-lint/
    ├── src/
    │   ├── lib.rs               # Wrapper Linter
    │   ├── main.rs              # CLI
    │   ├── diagnostics.rs       # Enhanced Diagnostic
    │   └── rules/               # Rule implementations
    └── tests/
        └── integration_tests.rs # Test suite
```

---

**Analysis Date:** 2026-02-10  
**Repository:** /home/adavidoff/git/elle3  
**Focus:** Elle-lint error message handling and formatting
