# Elle-LSP Implementation Exploration Report

## Executive Summary

The Elle-LSP (Language Server Protocol) implementation is a **foundational but incomplete** LSP server for the Elle Lisp language. It has the core infrastructure in place (~170 lines of main code) but most advanced features are **stubbed/placeholder implementations**. The codebase consists of:

- **3 workspaces**: `elle` (main interpreter), `elle-lsp` (LSP server), `elle-lint` (static analysis)
- **Total LSP code**: ~170 lines across 4 files
- **Total lint code**: ~700 lines across 7 files  
- **Integration point**: LSP depends on `elle-lint` for diagnostics and `elle` for symbol/scope information

---

## 1. LSP Server Location & Structure

### File Locations
```
/home/adavidoff/git/elle2/
├── elle-lsp/                          # LSP server workspace
│   ├── Cargo.toml                     # Declares dependencies: elle, elle-lint, lsp-types, tokio
│   ├── README.md                      # Architecture documentation (249 lines)
│   ├── src/
│   │   ├── main.rs                    # ✅ Core server loop (166 lines) - MAIN ENTRY POINT
│   │   ├── lib.rs                     # ✅ Library interface (10 lines) - JUST STUBS
│   │   ├── protocol.rs                # ❌ EMPTY - Protocol types (2 lines)
│   │   ├── handler.rs                 # ❌ EMPTY - Message handlers (2 lines)
│   │   └── examples/
│   │       └── demo.lisp              # 50-line example with good naming conventions
│   │
│   └── target/release/elle-lsp        # Compiled binary
```

### Dependencies
```toml
elle = { path = ".." }                # Main interpreter for symbol table & AST
elle-lint = { path = "../elle-lint" } # Linter for diagnostics
lsp-types = "0.95"                     # Standard LSP type definitions
serde = "1.0"                          # JSON serialization
serde_json = "1.0"                     # JSON utilities
tokio = "1.35"                         # Async runtime (optional, not used yet)
```

---

## 2. Current LSP Features: Implemented vs Stubbed

### ✅ FULLY IMPLEMENTED

| Feature | Location | Status | Details |
|---------|----------|--------|---------|
| **Initialize** | main.rs:69-90 | ✅ Complete | Sends capabilities for all features (even if stub) |
| **Shutdown** | main.rs:91-97 | ✅ Complete | Graceful shutdown handler |
| **Text Sync (didOpen)** | main.rs:98-115 | ✅ Complete | Stores document URI → text in HashMap |
| **Text Sync (didChange)** | main.rs:116-135 | ✅ Complete | Updates document on change events |
| **Text Sync (didClose)** | main.rs:136-147 | ✅ Complete | Removes document from tracking |
| **Message Parsing** | main.rs:20-60 | ✅ Complete | Reads LSP JSON-RPC over stdin/stdout |
| **Response Formatting** | main.rs:56-58 | ✅ Complete | Sends Content-Length headers + JSON |

### ❌ STUBBED/PLACEHOLDER

| Feature | Location | Status | What's Needed |
|---------|----------|--------|---------------|
| **Hover** | main.rs:148-156 | ⚠️ Stub | Returns generic "Elle Lisp symbol information" |
| **Go to Definition** | README.md:14 | 🔴 Missing | Not implemented at all |
| **Find References** | README.md:15 | 🔴 Missing | Not implemented at all |
| **Code Completion** | main.rs:79-82 | ⚠️ Partial | Response structure declared but handler stub |
| **Diagnostics** | README.md:97 | ⚠️ Partial | Infrastructure for `publishDiagnostics` missing |
| **Symbol Renaming** | README.md:17 | 🔴 Missing | Not implemented |
| **Workspace Diagnostics** | README.md:18 | 🔴 Missing | Only per-document |
| **Semantic Tokens** | README.md:213 | 🔴 Missing | Not implemented |
| **Formatting** | README.md:210 | 🔴 Missing | Not implemented |

### Handler Code Structure (main.rs:63-165)

```rust
fn handle_request(request: &Value, documents: &mut HashMap<String, String>) -> Value {
    let method = request.get("method").and_then(|v| v.as_str())?;
    let id = request.get("id");
    let params = request.get("params");
    
    match method {
        "initialize" => { /* IMPLEMENTED */ }
        "shutdown" => { /* IMPLEMENTED */ }
        "textDocument/didOpen" => { /* IMPLEMENTED */ }
        "textDocument/didChange" => { /* IMPLEMENTED */ }
        "textDocument/didClose" => { /* IMPLEMENTED */ }
        "textDocument/hover" => { /* STUB - return generic text */ }
        _ => { /* STUB - return null */ }
    }
}
```

---

## 3. Architecture & Feature Organization

### Message Flow (LSP Protocol)

```
Client (VS Code, Emacs, etc.)
    ↓
stdin (JSON-RPC messages with Content-Length header)
    ↓
main.rs loop (lines 20-60)
    ├─ Read Content-Length header
    ├─ Read message body
    ├─ Parse JSON
    ├─ Dispatch to handle_request()
    ↓
Response generation (match on method)
    ↓
stdout (Content-Length header + JSON)
    ↓
Client displays results
```

### Document Tracking (Simple but Functional)

```rust
documents: HashMap<String, String> = HashMap::new()
                     └─ URI string → source code text

Operations:
- didOpen:   documents.insert(uri, text)
- didChange: documents.insert(uri, new_text)  // Full sync
- didClose:  documents.remove(uri)
```

**Issue**: No line-by-line tracking, no AST caching, no incremental parsing!

### Capability Declaration (Initialize Handler)

```json
{
  "capabilities": {
    "textDocumentSync": 1,              // Full document sync mode
    "hoverProvider": true,              // Hover implemented (stub)
    "definitionProvider": true,         // Not actually implemented
    "referencesProvider": true,         // Not actually implemented
    "completionProvider": {
      "resolveProvider": true,
      "triggerCharacters": ["("]
    }
  },
  "serverInfo": {
    "name": "Elle Language Server",
    "version": "0.1.0"
  }
}
```

**Problem**: Server declares capabilities it doesn't actually support!

---

## 4. Elle-Lint Integration

### Location
```
/home/adavidoff/git/elle2/elle-lint/
├── Cargo.toml (217 bytes)
├── README.md (340 lines - comprehensive)
├── src/
│   ├── lib.rs (188 lines) - Main Linter struct
│   ├── diagnostics.rs (163 lines) - Diagnostic types & formatting
│   ├── context.rs (211 lines) - Symbol scope management
│   ├── main.rs - CLI entry point
│   └── rules/
│       ├── mod.rs (23 lines)
│       ├── naming.rs (153 lines) ✅ IMPLEMENTED
│       └── arity.rs (17 lines) ⚠️ STUB ONLY
└── tests/ - Integration tests
```

### Linter Usage Pattern (elle_lint/src/lib.rs)

```rust
pub struct Linter {
    config: LintConfig,
    diagnostics: Vec<Diagnostic>,
}

// Usage in LSP would be:
let mut linter = Linter::new(LintConfig::default());
linter.lint_str(document_text, "document.lisp")?;

for diagnostic in linter.diagnostics() {
    // Convert to LSP Diagnostic format
    // Publish via publishDiagnostics notification
}
```

### Currently Implemented Rules

**1. Naming Convention (`naming-kebab-case`)** - WORKING ✅
- Enforces kebab-case for identifiers
- Allows `?` suffix for predicates: `empty?`
- Allows `!` suffix for mutations: `set!`
- Single-letter variables allowed
- Returns Diagnostic with:
  - Severity: Warning
  - Code: W001
  - Line, column, context, suggestions

Example:
```lisp
(define myVariable 10)  # ❌ Error: should be my-variable
(define my-variable 10) # ✅ Correct
```

**2. Arity Validation (`check_arity`)** - STUB ONLY ⚠️
```rust
pub fn check_arity(
    _value: &Value,      // Underscore = unused!
    _filename: &str,
    _line: usize,
    _diagnostics: &mut Vec<Diagnostic>,
    _symbols: &elle::SymbolTable,
) {
    // This is a placeholder for now
    // Full implementation would require tracking function definitions
}
```

### Diagnostic Structure

```rust
pub struct Diagnostic {
    pub severity: Severity,        // Error, Warning, Info
    pub code: String,              // E001, W001, etc.
    pub rule: String,              // "naming-kebab-case"
    pub message: String,           // Human-readable message
    pub file: String,              // Filename
    pub line: usize,               // 1-indexed line
    pub column: usize,             // 1-indexed column
    pub context: String,           // Code snippet
    pub suggestions: Vec<String>,  // Actionable fixes
}
```

**Output Formats**:
- Human-readable: Rust compiler-style error messages
- JSON: For IDE integration

### How LSP Should Use It

**Current**: NOT INTEGRATED
```rust
// In handle_request() for textDocument/didChange:
// TODO: Integration missing!
```

**Should be**:
```rust
fn publish_diagnostics(uri: &str, text: &str) {
    let mut linter = Linter::new(LintConfig::default());
    if linter.lint_str(text, uri).is_ok() {
        let diagnostics = linter.diagnostics()
            .iter()
            .map(|d| LSPDiagnostic {
                range: Range { 
                    start: Position { line: d.line - 1, character: d.column - 1 },
                    end: Position { line: d.line - 1, character: d.column + 10 }
                },
                severity: convert_severity(d.severity),
                code: Some(d.code.clone()),
                message: d.message.clone(),
            })
            .collect();
        
        // Publish via notification
        println!("{{ \"method\": \"textDocument/publishDiagnostics\", ... }}");
    }
}
```

---

## 5. Symbol and Definition Tracking

### Elle's Symbol System

**Location**: `/home/adavidoff/git/elle2/src/symbol.rs`

```rust
pub struct SymbolTable {
    map: FxHashMap<String, SymbolId>,    // name → ID
    names: Vec<String>,                   // ID → name
    macros: FxHashMap<SymbolId, MacroDef>,
    modules: FxHashMap<SymbolId, ModuleDef>,
    current_module: Option<SymbolId>,
}

impl SymbolTable {
    pub fn intern(&mut self, name: &str) -> SymbolId { ... }  // Intern string
    pub fn name(&self, id: SymbolId) -> Option<&str> { ... }  // Get name
    pub fn get(&self, name: &str) -> Option<SymbolId> { ... } // Lookup
}
```

**Key Insight**: Symbols are **fast-to-compare** but **not tracked with locations**!

### Scope Information (for Variable Resolution)

**Compiler Scope** - `/home/adavidoff/git/elle2/src/compiler/scope.rs`

```rust
pub struct VariableBinding {
    pub symbol_id: SymbolId,
    pub binding_type: BindingType,  // Parameter, Local, Captured
    pub depth: usize,                // 0 = current, 1 = parent, etc.
    pub index: usize,                // Position in scope
}

pub struct ScopeFrame {
    pub variables: HashMap<u32, VariableBinding>,
    pub scope_type: ScopeType,       // Global, Function, Block, Loop, Let
    pub depth: usize,                // 0 = global, 1+= nested
}
```

Used during **compilation** to track variable references.

**Runtime Scope** - `/home/adavidoff/git/elle2/src/vm/scope.rs`

```rust
pub struct RuntimeScope {
    pub variables: HashMap<u32, Value>,  // symbol_id → computed value
    pub scope_type: ScopeType,
}

pub struct ScopeStack {
    stack: Vec<RuntimeScope>,  // Stack of scopes during execution
}
```

Used during **execution** to track values.

### AST Representation

**Location**: `/home/adavidoff/git/elle2/src/compiler/ast.rs`

```rust
pub struct ExprWithLoc {
    pub expr: Expr,
    pub loc: Option<SourceLoc>,  // Line & column info!
}

pub enum Expr {
    Literal(Value),
    Var(SymbolId, usize, usize),              // var, depth, index
    GlobalVar(SymbolId),
    Call { func, args, tail },
    Lambda { params, body, captures },
    Define { name: SymbolId, value },
    // ... many more
}
```

**SourceLoc** - `/home/adavidoff/git/elle2/src/reader.rs`

```rust
pub struct SourceLoc {
    pub line: usize,  // 1-indexed
    pub col: usize,   // 1-indexed
}
```

Tracked during lexing/parsing!

### How LSP Could Use This for Features

**For Hover** (show function signature):
```rust
// Get text at position
// Parse to AST with SourceLoc
// Find Expr node at that position
// If it's a Var(symbol_id, _, _):
//   - Get symbol name: symbols.name(symbol_id)
//   - Look up builtin arity: builtin_arity(name)
//   - Return hover info
```

**For Go to Definition**:
```rust
// Need to track where each symbol is defined
// Create a symbol → SourceLoc map during parsing
// Look up definition location for symbol at cursor
// Return Location { uri, range }
```

**For Find References**:
```rust
// Track all uses of each symbol
// Return all SourceLoc where symbol is used
```

**For Code Completion**:
```rust
// Get all symbols in current scope
// Filter by prefix
// Return CompletionItem list
```

---

## 6. What Needs to Be Implemented

### Phase 1: Essential LSP Features (HIGH PRIORITY)

#### 1.1 Diagnostics Publishing ⚠️ CRITICAL

**Missing**: Publishing diagnostics to client

```rust
// Add to main.rs

fn publish_diagnostics(uri: &str, text: &str, writer: &mut dyn Write) -> Result<(), String> {
    let mut linter = elle_lint::Linter::new(elle_lint::LintConfig::default());
    linter.lint_str(text, uri)?;
    
    let diagnostics: Vec<_> = linter
        .diagnostics()
        .iter()
        .map(|d| {
            // Convert elle-lint Diagnostic to LSP Diagnostic
            serde_json::json!({
                "range": {
                    "start": { "line": d.line - 1, "character": d.column - 1 },
                    "end": { "line": d.line - 1, "character": d.column + 5 }
                },
                "severity": match d.severity {
                    elle_lint::diagnostics::Severity::Error => 1,
                    elle_lint::diagnostics::Severity::Warning => 2,
                    elle_lint::diagnostics::Severity::Info => 3,
                },
                "code": &d.code,
                "source": "elle-lint",
                "message": &d.message,
            })
        })
        .collect();
    
    let notification = serde_json::json!({
        "jsonrpc": "2.0",
        "method": "textDocument/publishDiagnostics",
        "params": {
            "uri": uri,
            "diagnostics": diagnostics
        }
    });
    
    let body = notification.to_string();
    writeln!(writer, "Content-Length: {}\r\n\r{}", body.len(), body)?;
    Ok(())
}
```

**Integration Points**:
- Call after `textDocument/didOpen`
- Call after `textDocument/didChange`
- Clear diagnostics on `textDocument/didClose`

#### 1.2 Hover Implementation

**Required**: Symbol information retrieval

```rust
// In handle_request() replace stub:

"textDocument/hover" => {
    if let Some(params) = params {
        if let (Some(uri), Some(pos)) = (
            params.get("textDocument").and_then(|d| d.get("uri")).and_then(|u| u.as_str()),
            params.get("position")
        ) {
            if let Some(doc) = documents.get(uri) {
                if let (Some(line), Some(char)) = (
                    pos.get("line").and_then(|l| l.as_u64()),
                    pos.get("character").and_then(|c| c.as_u64())
                ) {
                    let symbol_info = get_symbol_at(doc, line as usize, char as usize)?;
                    return json!({
                        "jsonrpc": "2.0",
                        "id": id,
                        "result": {
                            "contents": symbol_info.description
                        }
                    });
                }
            }
        }
    }
    json!({})
}
```

**Implementation needed**:
```rust
struct SymbolInfo {
    name: String,
    arity: Option<usize>,
    description: String,
    documentation: Option<String>,
}

fn get_symbol_at(code: &str, line: usize, col: usize) -> Option<SymbolInfo> {
    // 1. Parse code to AST
    // 2. Find Expr node at (line, col) using SourceLoc
    // 3. Extract symbol info
    // 4. Return description
}
```

#### 1.3 Go to Definition

**Required**: Symbol definition tracking

```rust
"textDocument/definition" => {
    // Similar to hover but returns Location instead
    // Need symbol_definitions: HashMap<SymbolId, SourceLoc>
    
    let location = symbol_definitions.get(&symbol_id)?;
    json!({
        "jsonrpc": "2.0",
        "id": id,
        "result": {
            "uri": uri,
            "range": {
                "start": { "line": location.line - 1, "character": location.col - 1 },
                "end": { "line": location.line - 1, "character": location.col + 10 }
            }
        }
    })
}
```

**What's needed**:
- Index all `define` statements during initial parse
- Store symbol → SourceLoc mapping
- Lookup on go-to-definition request

### Phase 2: Enhanced Features (MEDIUM PRIORITY)

#### 2.1 Code Completion

```rust
"textDocument/completion" => {
    // Get word at position
    // Get all symbols in scope
    // Filter by prefix
    // Return CompletionItem[]
}
```

#### 2.2 Find References

```rust
"textDocument/references" => {
    // Similar to go-to-definition but return all usages
    // Need usage tracking: HashMap<SymbolId, Vec<SourceLoc>>
}
```

#### 2.3 Incremental Document Sync

**Current**: Full document sync only
**Improve**: Support `textDocumentSync` with `didChange` events containing text ranges

#### 2.4 Workspace Support

- Handle `didChangeWatchedFiles` to track file changes
- Support multiple open files
- Aggregate diagnostics across workspace

### Phase 3: Polish (LOW PRIORITY)

- [ ] Semantic tokens for syntax highlighting
- [ ] Document formatting
- [ ] Symbol renaming
- [ ] Signature help
- [ ] Call hierarchy

---

## 7. File-by-File Implementation Guide

### `/home/adavidoff/git/elle2/elle-lsp/src/main.rs` (166 lines)

**Current State**: Core message loop + basic handlers

**What to Change**:
1. **Extract message loop to reusable struct**:
   ```rust
   struct LSPServer {
       documents: HashMap<String, String>,
       linter: Linter,
       symbol_table: SymbolTable,
   }
   
   impl LSPServer {
       fn handle_request(&mut self, req: &Value) -> Value { ... }
   }
   ```

2. **Add symbol tracking**:
   ```rust
   struct SymbolIndex {
       definitions: HashMap<SymbolId, SourceLoc>,  // Where defined
       usages: HashMap<SymbolId, Vec<SourceLoc>>,  // Where used
       scopes: HashMap<SourceLoc, ScopeFrame>,     // Scope info
   }
   ```

3. **Implement handler methods**:
   - `handle_hover()` → SymbolInfo
   - `handle_definition()` → Location
   - `handle_references()` → Location[]
   - `handle_completion()` → CompletionItem[]
   - `handle_diagnostics()` → Diagnostic[]

### `/home/adavidoff/git/elle2/elle-lsp/src/lib.rs` (10 lines - EMPTY)

**Should contain**: 
- Public API for embedding LSP in tools
- `pub struct LanguageServer { ... }`
- `pub fn new() -> Self { ... }`

### `/home/adavidoff/git/elle2/elle-lsp/src/protocol.rs` (2 lines - EMPTY)

**Should contain**:
- `LSPPosition`, `LSPRange`, `LSPDiagnostic` type wrappers
- Conversion functions between lsp-types and internal types

### `/home/adavidoff/git/elle2/elle-lsp/src/handler.rs` (2 lines - EMPTY)

**Should contain**:
- `impl LSPServer { pub fn handle_xxx() { ... } }`
- One handler per LSP feature

### `/home/adavidoff/git/elle2/elle-lint/src/rules/arity.rs` (17 lines - STUB)

**Critical to implement**:
```rust
pub fn check_arity(
    value: &Value,
    filename: &str,
    line: usize,
    diagnostics: &mut Vec<Diagnostic>,
    symbols: &elle::SymbolTable,
) {
    // Extract function call: (func arg1 arg2 ...)
    if let Value::Cons(_) = value {
        if let Ok(list) = value.list_to_vec() {
            if !list.is_empty() {
                if let Value::Symbol(func_id) = &list[0] {
                    let name = symbols.name(*func_id).unwrap_or("unknown");
                    let arg_count = list.len() - 1;
                    
                    // Check builtin arity
                    if let Some(expected) = context::builtin_arity(name) {
                        if expected != arg_count {
                            diagnostics.push(Diagnostic::new(
                                Severity::Error,
                                "E003",
                                "arity-mismatch",
                                format!("function '{}' expects {} arguments, got {}",
                                    name, expected, arg_count),
                                filename,
                                line,
                                1,
                                format!("({}...)", name),
                            ));
                        }
                    }
                    
                    // Check user-defined arity (via symbols)
                    // TODO: Need to pass scope context
                }
            }
            
            // Recurse on sub-expressions
            for elem in list.iter().skip(1) {
                check_arity(elem, filename, line, diagnostics, symbols);
            }
        }
    }
}
```

---

## 8. Integration Workflow

### Current State (BROKEN)
```
User edits file in VS Code
         ↓
LSP server receives textDocument/didChange
         ↓
Server updates documents HashMap
         ↓
❌ No diagnostics published
❌ Hover returns stub text
❌ Definition/references not implemented
```

### What It Should Be
```
User edits file in VS Code
         ↓
LSP server receives textDocument/didChange
         ↓
Server updates documents HashMap
         ↓
✅ Parse to AST, build symbol index
         ↓
✅ Run elle-lint, get diagnostics
         ↓
✅ Publish diagnostics via notification
         ↓
✅ User sees errors/warnings in editor
         ↓
User hovers over symbol
         ↓
✅ LSP server looks up in symbol index
         ↓
✅ Returns hover info
         ↓
User Ctrl+Click on symbol
         ↓
✅ LSP server returns definition location
         ↓
✅ Editor jumps to definition
```

---

## 9. Testing Recommendations

### Unit Tests Needed

**For Hover** (`tests/hover.rs`):
```rust
#[test]
fn test_hover_builtin_function() {
    let server = LSPServer::new();
    let hover = server.hover("(+ 1 2)", 0, 1);
    assert!(hover.contains("addition"));
}

#[test]
fn test_hover_user_defined() {
    let server = LSPServer::new();
    server.parse_and_index("(define add-one (fn [x] (+ x 1)))");
    let hover = server.hover("(add-one 5)", 0, 1);
    assert!(hover.contains("add-one"));
}
```

**For Diagnostics** (`tests/diagnostics.rs`):
```rust
#[test]
fn test_diagnostics_naming_violation() {
    let server = LSPServer::new();
    let diags = server.get_diagnostics("(define myVar 10)");
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].rule, "naming-kebab-case");
}

#[test]
fn test_diagnostics_arity_mismatch() {
    let server = LSPServer::new();
    let diags = server.get_diagnostics("(+ 1)");  // + needs 2+ args
    assert!(diags.iter().any(|d| d.rule == "arity-mismatch"));
}
```

**For Go to Definition** (`tests/definition.rs`):
```rust
#[test]
fn test_goto_definition() {
    let server = LSPServer::new();
    server.parse_and_index("(define my-func (fn [x] x))\n(my-func 5)");
    
    let loc = server.definition_at(1, 1);  // Line 2, "my-func"
    assert_eq!(loc.line, 0);
    assert_eq!(loc.col, 8);  // Position of "my-func" in define
}
```

### Integration Tests with VS Code

Use LSP client library:
```bash
cargo test --test integration -- --nocapture
```

---

## 10. Key Findings & Recommendations

### Strengths ✅
1. **Clean LSP message loop** - Proper JSON-RPC parsing
2. **Text synchronization** - Full document tracking works
3. **Foundation for integration** - elle-lint can be plugged in
4. **Good naming rule** - First rule is fully implemented & tested
5. **Clear architecture** - Symbol table, scope system exists in main `elle` crate

### Weaknesses ❌
1. **No symbol indexing** - Can't look up definitions or references
2. **No diagnostics integration** - elle-lint exists but unused
3. **False capability advertising** - Claims features it doesn't implement
4. **No incremental parsing** - Full document parse on every change
5. **No scope context** - Can't resolve variable bindings at cursor
6. **Stub handlers** - Hover, completion, definition all return null/generic
7. **Missing arity rule** - Core lint rule is placeholder only

### Critical Path to MVP

**Minimum Viable Product** (what users need):
1. ✅ Initialize (already works)
2. ✅ Diagnostics (needs elle-lint integration) - ~50 lines
3. ✅ Hover (needs symbol lookup) - ~100 lines
4. ✅ Go to Definition (needs index building) - ~150 lines

**Estimated effort**: 1-2 weeks for one developer to implement MVP

### Next Steps

1. **Immediate** (blocking):
   - Remove false capability claims OR implement stubs
   - Integrate elle-lint for diagnostics
   - Implement basic hover

2. **Short term** (1 sprint):
   - Add symbol indexing on parse
   - Implement go-to-definition
   - Implement find-references
   - Complete arity validation rule

3. **Medium term** (2+ sprints):
   - Code completion
   - Incremental sync
   - Semantic tokens
   - Workspace support

---

## Appendix: Code References

### Key Files to Understand

| File | Lines | Purpose |
|------|-------|---------|
| `elle-lsp/src/main.rs` | 166 | Message loop & handlers |
| `elle/src/symbol.rs` | 139 | Symbol table (fast lookup) |
| `elle/src/reader.rs` | 600+ | Parsing with SourceLoc |
| `elle/src/compiler/scope.rs` | 100+ | Compile-time scope tracking |
| `elle/src/compiler/ast.rs` | 150+ | AST with location info |
| `elle-lint/src/lib.rs` | 188 | Linter main struct |
| `elle-lint/src/diagnostics.rs` | 163 | Diagnostic types |
| `elle-lint/src/rules/naming.rs` | 153 | Example lint rule |

### Key Structures to Use

```rust
// In main elle crate
use elle::{SymbolTable, Reader, Lexer, compile, Value};

// In elle-lint crate
use elle_lint::{Linter, LintConfig, Diagnostic};

// Standard LSP types
use lsp_types::{Position, Range, Location, Diagnostic as LSPDiagnostic};
```

### Example: Minimal Hover Implementation

```rust
fn get_symbol_at_position(code: &str, line: usize, col: usize, 
                          symbols: &SymbolTable) -> Option<String> {
    let mut lexer = Lexer::new(code);
    let mut tokens = Vec::new();
    
    while let Ok(Some(token)) = lexer.next_token() {
        tokens.push(token);
    }
    
    // Find token at position
    for token in tokens {
        if token.loc.line == line + 1 && 
           token.loc.col >= col && token.loc.col < col + 10 {
            return match token.value {
                elle::TokenValue::Symbol(sym_id) => {
                    symbols.name(sym_id).map(|s| s.to_string())
                }
                _ => None
            }
        }
    }
    None
}
```

---

**Report Generated**: February 7, 2026
**Status**: Elle-LSP is Foundation-Ready but Feature-Incomplete
**Priority**: Diagnostics and hover should be implemented first
