# Elle Language Server Protocol Architecture - Technical Deep Dive

## Executive Summary

The Elle codebase is a bytecode-compiled Lisp interpreter in Rust with sophisticated scope tracking, symbol interning, and a register-based VM. The compiler pipeline is well-structured with clear separation between parsing (reader), AST generation (converters), optimization (analysis), and bytecode emission (compile). This document provides detailed findings on the architectural patterns, symbol management, compilation flow, and performance characteristics needed to design an LSP server.

---

## 1. CURRENT LSP SERVER STRUCTURE

### Location
- **Main file**: `/home/adavidoff/git/elle2/elle-lsp/src/main.rs`
- **Library wrapper**: `/home/adavidoff/git/elle2/elle-lsp/src/lib.rs` (2 lines - minimal)
- **Handler**: `/home/adavidoff/git/elle2/elle-lsp/src/handler.rs` (2 lines - stub)
- **Protocol**: `/home/adavidoff/git/elle2/elle-lsp/src/protocol.rs` (2 lines - stub)

### Current Implementation Status

The LSP server is **STUB-LEVEL** - it contains a basic message loop but lacks real implementation:

```rust
// main.rs structure (lines 13-165):
fn main() {
    let mut documents: HashMap<String, String> = HashMap::new();
    let stdin = std::io::stdin();
    let mut reader = BufReader::new(stdin.lock());
    let mut stdout = std::io::stdout();
    
    loop {
        // Read headers until Content-Length
        // Read message body
        // Call handle_request()
    }
}
```

### Capabilities Currently Declared

```json
{
    "capabilities": {
        "textDocumentSync": 1,              // OPEN/CHANGE/CLOSE events
        "hoverProvider": true,              // Basic hover
        "definitionProvider": true,         // Definition lookup
        "referencesProvider": true,         // Find references
        "completionProvider": {
            "resolveProvider": true,
            "triggerCharacters": ["("]
        }
    }
}
```

### Request Handlers Implemented

Currently handles:
- `initialize` - Returns capability list
- `shutdown` - Graceful shutdown
- `textDocument/didOpen` - Store document text
- `textDocument/didChange` - Update document text
- `textDocument/didClose` - Remove document
- `textDocument/hover` - Stub returning generic message
- All others - Return null

### Key Limitation

**No compilation or semantic analysis is performed**. The server only tracks document text in memory without building any index or performing code analysis.

---

## 2. SYMBOL TABLE AND SCOPE SYSTEM

### Symbol ID System

**File**: `/home/adavidoff/git/elle2/src/symbol.rs` (139 lines)

```rust
pub struct SymbolId(pub u32);  // Opaque u32 wrapper

pub struct SymbolTable {
    map: FxHashMap<String, SymbolId>,  // Name → ID
    names: Vec<String>,                // ID → Name
    macros: FxHashMap<SymbolId, Rc<MacroDef>>,
    modules: FxHashMap<SymbolId, Rc<ModuleDef>>,
    current_module: Option<SymbolId>,
}
```

**Performance characteristics**:
- Symbol interning: O(1) lookup via FxHashMap (fast hash)
- Name recovery: O(1) index into Vec
- Macro lookup: O(1)
- Module lookup: O(1)

**Intern pattern** (line 52-61):
```rust
pub fn intern(&mut self, name: &str) -> SymbolId {
    if let Some(&id) = self.map.get(name) {
        return id;  // Cache hit - instant
    }
    
    let id = SymbolId(self.names.len() as u32);
    self.names.push(name.to_string());
    self.map.insert(name.to_string(), id);
    id
}
```

### Scope and Environment System

**File**: `/home/adavidoff/git/elle2/src/compiler/scope.rs` (160+ lines)

#### ScopeType Enum (lines 6-17)
```rust
pub enum ScopeType {
    Global,        // Top-level defines
    Function,      // Lambda scope
    Block,         // let, begin, etc
    Loop,          // while, for bodies
    Let,           // Let-binding scope
}
```

#### VariableBinding Structure (lines 31-45)
```rust
pub struct VariableBinding {
    pub symbol_id: SymbolId,
    pub binding_type: BindingType,  // Parameter/Local/Captured
    pub depth: usize,               // Distance to defining scope
    pub index: usize,               // Position within scope
}
```

#### Scope Frame (lines 49-56)
```rust
pub struct ScopeFrame {
    pub variables: HashMap<u32, VariableBinding>,
    pub scope_type: ScopeType,
    pub depth: usize,              // Relative to global
}
```

#### CompileScope Manager (lines 93-156)
- Maintains a **stack of scope frames**
- `push(ScopeType)` - Enter new scope
- `pop()` - Exit scope
- `define_local(sym_id, binding_type)` - Add variable
- `lookup(sym_id)` - Search stack for variable → (depth, index)
- `is_defined_local(sym_id)` - Check current frame

**Key insight**: The compiler maintains a `CompileScope` during compilation that tracks:
1. Which variables are defined where
2. How deep they are nested
3. What their position is in the scope's environment

This information **flows directly into the Expr** via `Var(SymbolId, depth, index)` nodes.

### Runtime Scope Stack

**File**: `/home/adavidoff/git/elle2/src/vm/scope.rs` (450+ lines)

The VM also maintains a **ScopeStack** at runtime:
- Tracks scopes for runtime variable access
- Supports mutation through `set!`
- Implements scope management instructions (PushScope/PopScope)

---

## 3. COMPILER FLOW AND INFORMATION PIPELINE

### Stage 1: Reading (Reader)

**File**: `/home/adavidoff/git/elle2/src/reader.rs` (600+ lines)

**Entry point** (line 594):
```rust
pub fn read_str(input: &str, symbols: &mut SymbolTable) -> Result<Value, String>
```

**Output**: `Value` (not AST!)

The reader:
1. Lexes input into `TokenWithLoc` (token + `SourceLoc`)
2. Builds `Value` objects while preserving **source locations in tokens**
3. Returns **untyped data** (cons cells, symbols, numbers)

**Tokens preserve location**:
```rust
#[derive(Debug, Clone, PartialEq)]
pub struct TokenWithLoc {
    pub token: Token,
    pub loc: SourceLoc,  // line, col
}
```

**BUT**: Source locations are **NOT PROPAGATED INTO VALUE**
- Values contain no location information
- Location data is lost during read_str

### Stage 2: Conversion to AST

**File**: `/home/adavidoff/git/elle2/src/compiler/converters.rs` (200+ lines)

**Entry point**:
```rust
pub fn value_to_expr(value: &Value, symbols: &mut SymbolTable) -> Result<Expr, String>
```

**Function signature** (important limitation):
```rust
pub fn value_to_expr(value: &Value, symbols: &mut SymbolTable) -> Result<Expr, String>
```

**Problems for LSP**:
1. **No location information passed in** - Can't track what line a symbol came from
2. **No location information created** - `Expr` nodes don't contain `SourceLoc`
3. **Macros are expanded here** - Pattern matching happens at conversion time

**Key transformations**:
- `Value::Symbol(id)` → `Expr::Var/GlobalVar/...` (based on context analysis)
- `Value::Cons(list)` → Function calls or special forms
- Pattern matching via `match_symbol()` function

**What gets resolved**:
- Macro definitions are recognized
- Module system recognized
- Special forms (if, let, lambda, etc.) are identified
- BUT: No semantic analysis yet

### Stage 3: Analysis and Compilation

**Files**:
- `/home/adavidoff/git/elle2/src/compiler/analysis.rs` - Free variable analysis
- `/home/adavidoff/git/elle2/src/compiler/compile.rs` (867 lines) - Bytecode emission
- `/home/adavidoff/git/elle2/src/compiler/capture_resolution.rs` - Closure environment layout

#### Analysis Phase

**Function** (analysis.rs line 8):
```rust
pub fn analyze_capture_usage(
    expr: &Expr,
    local_bindings: &HashSet<SymbolId>,
    candidates: &HashSet<SymbolId>,
) -> HashSet<SymbolId>
```

This determines which variables from parent scopes are actually **used** in a closure body, eliminating dead captures.

#### Capture Resolution Phase

**Key function** (converters.rs line 47):
```rust
fn adjust_var_indices(expr: &mut Expr, captures: &[(SymbolId, usize, usize)], params: &[SymbolId])
```

**This is critical**: During compilation of lambdas:
1. Captures are analyzed (free variables from outer scopes)
2. Variables are re-indexed: `[captures..., parameters...]`
3. Final `Expr::Var(sym, depth, index)` maps to closure environment layout

**Information available at this stage**:
- Which symbols are used in which scopes
- Depth to enclosing scope
- Index within scope
- Parameter vs. captured status

#### Bytecode Emission

**File**: `/home/adavidoff/git/elle2/src/compiler/compile.rs`

**Entry point** (line 861):
```rust
pub fn compile(expr: &Expr) -> Bytecode {
    let mut compiler = Compiler::new();
    compiler.compile_expr(expr, true);
    compiler.bytecode.emit(Instruction::Return);
    compiler.finish()
}
```

**Bytecode structure**:
```rust
pub struct Bytecode {
    pub instructions: Vec<u8>,
    pub constants: Vec<Value>,
    pub inline_caches: HashMap<usize, CacheEntry>,
}
```

**What's lost in bytecode**:
- **All source location information** - No way to map instruction back to source
- **Symbol structure** - Only constants preserve symbol IDs
- **Variable names** - Only symbol IDs are preserved in instructions
- **Scoping details** - Replaced with depth/index tuples

### Stage 4: VM Execution

**Files**:
- `/home/adavidoff/git/elle2/src/vm/mod.rs` (main execution loop)
- `/home/adavidoff/git/elle2/src/vm/core.rs` (VM struct)
- `src/vm/*.rs` (instruction handlers)

**Execution** (vm/mod.rs line 20):
```rust
pub fn execute(&mut self, bytecode: &Bytecode) -> Result<Value, String> {
    self.execute_bytecode(&bytecode.instructions, &bytecode.constants, None)
}
```

**VM Structure** (vm/core.rs line 16):
```rust
pub struct VM {
    pub stack: StackVec,                    // SmallVec with 256 inline elements
    pub globals: HashMap<u32, Value>,      // SymbolId.0 → Value
    pub call_depth: usize,
    pub call_stack: Vec<CallFrame>,
    pub ffi: FFISubsystem,
    pub modules: HashMap<String, HashMap<u32, Value>>,
    pub scope_stack: ScopeStack,
}
```

---

## 4. AST/EXPR STRUCTURE

### ExprWithLoc

**File**: `/home/adavidoff/git/elle2/src/compiler/ast.rs` (lines 1-23)

```rust
pub struct ExprWithLoc {
    pub expr: Expr,
    pub loc: Option<SourceLoc>,   // OPTIONAL - often None!
}
```

**Problem**: Location is optional and often None because `value_to_expr` doesn't have source info.

### Expr Enum (lines 27-200+)

The AST is comprehensive:

```rust
pub enum Expr {
    Literal(Value),                          // Constants
    Var(SymbolId, usize, usize),            // (id, depth, index)
    GlobalVar(SymbolId),                     // Global reference
    If { cond, then, else_ },
    Cond { clauses, else_body },
    Begin(Vec<Expr>),
    Block(Vec<Expr>),
    Call { func, args, tail },              // Function calls
    Lambda { params, body, captures },      // (sym_id, depth, index) tuples
    Let { bindings, body },
    Letrec { bindings, body },
    Set { var, depth, index, value },
    Define { name, value },
    While { cond, body },
    For { var, iter, body },
    Match { value, patterns, default },
    Try { body, catch, finally },
    Throw { value },
    Quote(Box<Expr>),
    Quasiquote(Box<Expr>),
    Unquote(Box<Expr>),
    DefMacro { name, params, body },
    Module { name, exports, body },
    Import { module },
    ModuleRef { module, name },
    // ... more variants
}
```

**Key structure for Var nodes**:
```rust
Var(SymbolId, usize, usize)
    // SymbolId: which variable
    // depth: how many scopes up (0 = current, 1 = parent, etc.)
    // index: position within that scope
```

**For Lambda**:
```rust
Lambda {
    params: Vec<SymbolId>,
    body: Box<Expr>,
    captures: Vec<(SymbolId, usize, usize)>,  // Same format as Var!
}
```

### Information Available at Expr Level

**Available**:
- Symbol IDs and names (via SymbolTable)
- Depth and index for scope location
- Binding type (parameter vs. capture) - visible in Lambda/Var structure
- Function arity (in Lambda)
- Call structure

**NOT Available**:
- Source locations (usually)
- Original formatting
- Comments
- Macro expansions (already expanded)

---

## 5. PRIMITIVES AND GLOBAL FUNCTION REGISTRY

### Registration System

**File**: `/home/adavidoff/git/elle2/src/primitives.rs` (lines 353-361)

```rust
fn register_fn(
    vm: &mut VM,
    symbols: &mut SymbolTable,
    name: &str,
    func: fn(&[Value]) -> Result<Value, String>,
) {
    let sym_id = symbols.intern(name);
    vm.set_global(sym_id.0, Value::NativeFn(func));
}
```

**How it works**:
1. Intern the function name → Get SymbolId
2. Wrap Rust function pointer in `Value::NativeFn`
3. Store in VM's global symbol table: `globals: HashMap<u32, Value>`

### Built-in Functions

**Registration** (lines 70-250):
- **Arithmetic**: +, -, *, /, abs, min, max, mod, %
- **Comparison**: =, <, >, <=, >=
- **List operations**: cons, first, rest, length, append, reverse, nth, take, drop
- **Type checks**: nil?, pair?, number?, symbol?, string?
- **Logic**: not, and, or, xor
- **String operations**: string-length, string-append, substring, split, replace, etc.
- **Vector operations**: vector, vector-length, vector-ref, vector-set
- **Table/Struct operations**: table, struct, table-get, struct-get, etc.
- **Math**: sin, cos, tan, log, exp, pow, sqrt, floor, ceil, round
- **File I/O**: read-file, write-file, file-exists, list-directory, etc.
- **FFI**: load-library, call-c-function, etc.
- **Exception handling**: throw, exception, exception-message
- **Concurrency**: spawn, join, sleep

### Dynamic Globals

The VM's `globals` HashMap stores all global bindings at runtime:
```rust
pub globals: HashMap<u32, Value>  // SymbolId.0 → Value
```

Values stored:
- `Value::NativeFn` - Built-in functions
- `Value::Closure` - User-defined functions
- Any other Value type

---

## 6. SYMBOL EXTRACTION FROM COMPILED STATE

### What Can Be Extracted

**From Expr (after compilation)**:
1. **All referenced symbols** - Appear in Var/GlobalVar/Lambda/etc. nodes
2. **Definition locations** - Define nodes show top-level definitions
3. **Scope information** - depth/index tuples map to scope stack
4. **Call structure** - Call nodes show function-argument relationships
5. **Variable binding relationships** - Let/Lambda bindings explicit
6. **Arity information** - Lambda nodes show parameter count

### Building a Symbol Index During Compilation

**Approach**:
```rust
pub struct SymbolIndex {
    definitions: HashMap<SymbolId, (Vec<SymbolId>, SourceLoc?)>,  // name → (uses, def_loc)
    references: HashMap<SymbolId, Vec<(Expr, SourceLoc?)>>,      // name → uses
    scopes: Vec<ScopeFrame>,                                      // Captured during compile
    builtin_functions: HashSet<SymbolId>,                         // From SymbolTable
}
```

**Can be built by**:
1. Walking the Expr tree after `value_to_expr`
2. Recording all Var/GlobalVar nodes
3. Recording all Define/Lambda/Let nodes
4. Cross-referencing with SymbolTable to get names

**Problem**: No source locations unless we modify the compiler to preserve them.

### Current State Tracking Options

**Option 1: Walk Expr after compilation**
- Pros: Non-invasive, works with current code
- Cons: No source locations, can't map back to original source

**Option 2: Capture during value_to_expr**
- Pros: Can preserve token locations from Reader phase
- Cons: Requires modification to converters.rs

**Option 3: Maintain compilation context**
```rust
pub struct CompilationContext {
    expr_with_loc: ExprWithLoc,
    symbol_table: SymbolTable,
    scope_stack: CompileScope,
    // Can reconstruct most symbol information
}
```

---

## 7. COMPILATION PERFORMANCE

### Benchmark Suite

**File**: `/home/adavidoff/git/elle2/benches/benchmarks.rs` (100+ lines)

Measures:
1. **Parsing** - read_str performance
2. **Symbol interning** - First intern vs. cached lookup
3. **Compilation** - value_to_expr + compile
4. **VM execution** - bytecode evaluation

### Typical Performance (from benchmark structure)

**Parsing** (line 6):
- Simple number: Very fast (one atom)
- List literal: Moderate (linear in element count)
- Nested expr: Moderate (tree depth linear)
- Deep nesting: Potential stack concerns
- Large list 100 elements: Still reasonable

**Symbol interning** (line 46):
- First intern: O(hash + string copy) - micro-ops
- Cached lookup: O(hash lookup) - hash table hit
- 100 unique symbols: Still very fast

**Compilation** (line 82):
- Simple arithmetic: Microseconds
- Conditional: Slightly slower (more complex)
- Lambda/Closure: More overhead (capture analysis)
- Benchmark separates compilation from execution

### Performance Bottlenecks

**Identified from code structure**:

1. **Symbol string allocation** (symbol.rs line 58)
   - Each new symbol copies string into Vec
   - Reused on subsequent interns

2. **Closure capture analysis** (analysis.rs)
   - Walks entire body of lambdas
   - HashSet operations for each variable
   - Linear in closure body size

3. **Variable re-indexing** (converters.rs line 47)
   - Recursive walk of entire Expr tree
   - HashMap lookups for each Var node
   - Not optimized for large functions

4. **Bytecode emission** (compile.rs)
   - Recursive compilation of entire AST
   - No streaming or lazy compilation
   - Entire file compiled before execution

### Memory Footprint

**Per-file state**:
- `Bytecode`:
  - `instructions: Vec<u8>` - Proportional to code complexity
  - `constants: Vec<Value>` - All literals, closures, etc.
  - `inline_caches: HashMap` - For function lookup optimization
  
- Typical file: Few KB of bytecode + constants

**Persistent state** (VM-wide):
- `SymbolTable`: One string per unique symbol (kilobytes for typical program)
- `globals: HashMap`: One Value per global (megabytes for large programs)
- `modules`: Symbol mapping per module

---

## 8. LINTING INFRASTRUCTURE

### Existing Linter

**Files**:
- `/home/adavidoff/git/elle2/elle-lint/src/lib.rs` (194 lines)
- `/home/adavidoff/git/elle2/src/compiler/linter/` - Compiler-integrated linter

### Linting Pipeline

**lib.rs line 57** - `lint_str`:
```rust
pub fn lint_str(&mut self, code: &str, _filename: &str) -> Result<(), String> {
    let mut symbols = SymbolTable::new();
    let mut vm = VM::new();
    register_primitives(&mut vm, &mut symbols);
    init_stdlib(&mut vm, &mut symbols);
    
    // Parse
    let mut lexer = elle::Lexer::new(code);
    let mut tokens = Vec::new();
    loop { /* collect tokens */ }
    
    // Read
    let mut reader = elle::Reader::new(tokens);
    while let Some(result) = reader.try_read(&mut symbols) {
        // Convert to Expr
        let expr = value_to_expr(&value, &mut symbols)?;
        let expr_with_loc = ExprWithLoc::new(expr, None);  // NO LOCATION!
        
        // Lint
        self.compiler_linter.lint_expr(&expr_with_loc, &symbols);
    }
}
```

### Diagnostic System

**File**: `/home/adavidoff/git/elle2/elle-lint/src/diagnostics.rs` (163 lines)

```rust
pub struct Diagnostic {
    pub severity: Severity,           // Info, Warning, Error
    pub code: String,                 // "E001"
    pub rule: String,                 // "undefined-function"
    pub message: String,
    pub file: String,
    pub line: usize,                  // Not always accurate!
    pub column: usize,
    pub context: String,
    pub suggestions: Vec<String>,
}
```

### Compiler Linter

**Location**: `/home/adavidoff/git/elle2/src/compiler/linter/`

Rules currently checked:
- Naming conventions
- Arity validation
- Unused variable detection
- Pattern matching validation

**Can be reused by LSP** for diagnostics!

---

## 9. TECHNICAL FINDINGS AND RECOMMENDATIONS

### Current Limitations for LSP

1. **Source Location Loss**
   - Reader preserves token locations
   - `value_to_expr` drops them
   - Expr nodes usually have no location
   - **Impact**: Can't map errors/symbols to source positions

2. **One-Way Compilation**
   - No way to reverse-engineer source from bytecode
   - Variables stored as (depth, index) tuples
   - Requires maintaining separate symbol table

3. **Macro Expansion**
   - Happens at `value_to_expr` time
   - Can't provide "go to definition" for macros
   - Can't show expanded vs. source view

4. **No AST Caching**
   - Each compilation starts fresh
   - No incremental compilation
   - Full re-parse on every change

### Compilation Performance Characteristics

- **Parsing**: O(n) in code length, very fast
- **Symbol interning**: O(1) amortized
- **Compilation**: O(n) in AST size, includes capture analysis
- **Typical file**: <1ms to compile
- **Memory**: Modest (bytecode + symbol table)

### Design Recommendations for LSP

1. **Preserve Source Locations**
   ```rust
   // Modify converters.rs to accept TokenWithLoc
   pub fn value_to_expr_with_loc(
       value: &Value, 
       tokens: &[TokenWithLoc],  // NEW
       symbols: &mut SymbolTable
   ) -> Result<ExprWithLoc, String>
   ```

2. **Maintain Compilation Context**
   ```rust
   pub struct DocumentState {
       text: String,
       expr_with_loc: ExprWithLoc,
       symbol_table: SymbolTable,     // Could be shared/borrowed
       scope_stack: CompileScope,
       diagnostics: Vec<Diagnostic>,
   }
   ```

3. **Build Symbol Index**
   ```rust
   pub struct SymbolIndex {
       definitions: HashMap<SymbolId, Definition>,
       references: HashMap<SymbolId, Vec<Reference>>,
       completions: Vec<CompletionItem>,
   }
   
   impl SymbolIndex {
       pub fn from_expr(expr: &ExprWithLoc, table: &SymbolTable) -> Self
   }
   ```

4. **Implement Incremental Updates**
   - Only re-compile changed regions
   - Cache symbol table across documents
   - Use global SymbolTable for all documents

5. **LSP Request Handlers**
   ```rust
   async fn handle_hover(doc: &DocumentState, pos: Position) -> Option<HoverInfo> {
       // Find symbol at position via expr walk + location
       // Return type, definition, documentation
   }
   
   async fn handle_definition(doc: &DocumentState, pos: Position) -> Vec<Location> {
       // Walk expr to find Var node
       // Look up Define or Lambda for that symbol
       // Return source location
   }
   
   async fn handle_references(doc: &DocumentState, pos: Position) -> Vec<Location> {
       // Find all Var/GlobalVar nodes with same symbol
       // Filter by position
       // Return locations
   }
   
   async fn handle_completion(doc: &DocumentState, pos: Position) -> Vec<CompletionItem> {
       // Determine context (function position, symbol prefix)
       // Return globals + locals in scope + builtins
   }
   ```

### Key Architectural Insights

1. **Symbol Table is Central**
   - Every ID is looked up there
   - Can be shared across documents
   - Must be mutable during compilation

2. **Scope Information is Explicit**
   - `Expr::Var(sym, depth, index)` encodes scope
   - Can reconstruct scope stack by walking up
   - Can determine variable type from context

3. **Compile Phase is Fast**
   - Typical file: <1ms
   - Can be done on every keystroke
   - Incremental compilation would be optimization, not requirement

4. **Bytecode Loses Information**
   - Don't rely on bytecode for analysis
   - Work with Expr + SymbolTable instead
   - Bytecode only needed for execution

---

## 10. FILE ORGANIZATION REFERENCE

### Core Compiler Files
- `/home/adavidoff/git/elle2/src/reader.rs` - Tokenization and parsing
- `/home/adavidoff/git/elle2/src/compiler/converters.rs` - Value → Expr conversion
- `/home/adavidoff/git/elle2/src/compiler/ast.rs` - Expr definition
- `/home/adavidoff/git/elle2/src/compiler/compile.rs` - Expr → Bytecode
- `/home/adavidoff/git/elle2/src/compiler/scope.rs` - Scope tracking
- `/home/adavidoff/git/elle2/src/compiler/analysis.rs` - Free variable analysis

### Symbol and Type System
- `/home/adavidoff/git/elle2/src/symbol.rs` - SymbolTable
- `/home/adavidoff/git/elle2/src/value.rs` - Value enum
- `/home/adavidoff/git/elle2/src/compiler/bytecode.rs` - Bytecode/Instruction

### Runtime System
- `/home/adavidoff/git/elle2/src/vm/core.rs` - VM struct
- `/home/adavidoff/git/elle2/src/vm/mod.rs` - Execution loop
- `/home/adavidoff/git/elle2/src/vm/scope.rs` - Runtime scope stack

### Primitives and Utilities
- `/home/adavidoff/git/elle2/src/primitives.rs` - Registration system
- `/home/adavidoff/git/elle2/src/primitives/*.rs` - Built-in implementations

### LSP and Linting
- `/home/adavidoff/git/elle2/elle-lsp/src/main.rs` - LSP message loop (needs implementation)
- `/home/adavidoff/git/elle2/elle-lint/src/lib.rs` - Linter wrapper (can be integrated)
- `/home/adavidoff/git/elle2/src/compiler/linter/` - Diagnostic system

---

## 11. QUICK REFERENCE: KEY DATA STRUCTURES

### For LSP Integration

```rust
// What you need to track per document:
pub struct DocumentState {
    pub uri: String,
    pub text: String,
    pub expr_with_loc: ExprWithLoc,        // AST after conversion
    pub symbol_table: Arc<RwLock<SymbolTable>>,  // Shared across docs
    pub diagnostics: Vec<Diagnostic>,
    pub version: u32,
}

// For symbol queries:
pub struct SymbolInfo {
    pub symbol_id: SymbolId,
    pub name: String,                      // From SymbolTable::name()
    pub kind: SymbolKind,                  // Function, Variable, Macro, Module
    pub location: Option<Location>,        // From ExprWithLoc
    pub references: Vec<Location>,
    pub definition: Option<Location>,
}

// For completion:
pub struct CompletionContext {
    pub position: Position,
    pub prefix: String,
    pub in_scope: Vec<SymbolId>,          // Variables available at position
    pub builtins: Vec<SymbolId>,          // From SymbolTable
}
```

---

## Summary Table

| Aspect | Finding | Impact for LSP |
|--------|---------|-----------------|
| **Symbol Storage** | SymbolId(u32) with FxHashMap | Can build fast lookup index |
| **Scope Tracking** | Explicit (depth, index) in Expr | Can compute local scope quickly |
| **Source Locations** | Lost during value_to_expr | Need to modify compiler to preserve |
| **Compilation Speed** | <1ms for typical file | Can compile on every keystroke |
| **Memory Usage** | Modest (kilobytes per file) | Can keep state for all open files |
| **Macro Expansion** | Happens at value_to_expr | Can provide macro expansion in LSP |
| **Capture Information** | Explicit in Lambda nodes | Can determine closure captures |
| **Built-in Functions** | ~100 registered, easily accessible | Can provide completions for all |
| **Diagnostics** | Existing linter infrastructure | Can be reused with minor adaptation |
| **Incremental Compilation** | Not currently supported | Would be optimization, not requirement |

---

**Document generated from codebase analysis**
**Last updated: 2026-02-07**
**Scope: Elle2 Lisp Interpreter - Compiler Architecture**
