# Elle Compiler Architecture Diagrams

## 1. Compilation Pipeline

```
┌──────────────────┐
│  Source Code     │
│  (String)        │
└────────┬─────────┘
         │
         ▼
┌──────────────────────────────────────────────┐
│ READER PHASE (reader.rs)                     │
├──────────────────────────────────────────────┤
│ • Lexer: String → TokenWithLoc               │
│   - Tokenizes input                          │
│   - PRESERVES: line, col in TokenWithLoc     │
│ • Reader: [TokenWithLoc] → Value             │
│   - Builds cons cells, symbols, etc.         │
│   - LOSES: source locations (not in Value)   │
└────────┬─────────────────────────────────────┘
         │ read_str(&str) → Value
         ▼
    ┌─────────────────────────────────────────────┐
    │ VALUE (Untyped Data)                        │
    ├─────────────────────────────────────────────┤
    │ Value::Cons(cons_cells)                     │
    │ Value::Symbol(SymbolId)                     │
    │ Value::Int/Float/String/etc.                │
    │                                              │
    │ ⚠️  NO LOCATION INFO - LOST IN READ PHASE   │
    └────────┬────────────────────────────────────┘
             │
             ▼
┌──────────────────────────────────────────────────┐
│ CONVERSION PHASE (converters.rs)                 │
├──────────────────────────────────────────────────┤
│ • value_to_expr(Value) → Expr                   │
│ • Macro expansion happens here                  │
│ • Pattern matching for special forms            │
│ • Converts Value::Symbol to Var/GlobalVar/etc.  │
│                                                  │
│ PROBLEM: No location info available             │
└────────┬─────────────────────────────────────────┘
         │
         ▼
    ┌─────────────────────────────────────────────┐
    │ EXPR (Untyped AST)                          │
    ├─────────────────────────────────────────────┤
    │ Expr::Var(SymbolId, depth, index)           │
    │ Expr::Lambda {params, body, captures}       │
    │ Expr::Call {func, args, tail}               │
    │ Expr::Let/If/Cond/etc.                      │
    │                                              │
    │ Location: ExprWithLoc.loc = Option<SourceLoc> │
    │           (usually None!)                    │
    └────────┬────────────────────────────────────┘
             │
             ▼
┌──────────────────────────────────────────────────┐
│ ANALYSIS & COMPILATION (compile.rs)             │
├──────────────────────────────────────────────────┤
│ • analyze_capture_usage() - Find free vars      │
│ • adjust_var_indices() - Re-index for closure   │
│   environment: [captures..., parameters...]     │
│ • compile_expr() - Emit bytecode instructions   │
│                                                  │
│ Variable tracking: (depth, index) tuples        │
│ - depth: how many scopes up                     │
│ - index: position in that scope                 │
└────────┬──────────────────────────────────────────┘
         │
         ▼
    ┌──────────────────────────────────────────────┐
    │ BYTECODE (Final Compiled Form)               │
    ├──────────────────────────────────────────────┤
    │ Bytecode {                                    │
    │   instructions: Vec<u8>,  // Low-level ops   │
    │   constants: Vec<Value>,  // Literals, etc.  │
    │   inline_caches: HashMap, // Optimization    │
    │ }                                             │
    │                                               │
    │ ⚠️  ALL SYMBOL INFO LOST AFTER THIS          │
    │     Only instructions and raw values remain  │
    └────────┬──────────────────────────────────────┘
             │
             ▼
┌──────────────────────────────────────────────────┐
│ VM EXECUTION (vm/mod.rs)                         │
├──────────────────────────────────────────────────┤
│ • execute_bytecode() - Main interpreter loop     │
│ • Stack-based execution                         │
│ • Closure environment: Vec<Value>                │
│   ([captures..., parameters...])                 │
│ • Global symbol table: HashMap<u32, Value>      │
└──────────────────────────────────────────────────┘
```

## 2. Symbol Table and Scope System

```
┌─────────────────────────────────────────────────┐
│ SymbolTable (symbol.rs)                         │
├─────────────────────────────────────────────────┤
│                                                  │
│  String ──(intern)──> SymbolId(u32)            │
│  ↓                    ↑                          │
│  map: FxHashMap       names: Vec<String>       │
│  "foo" → SymbolId(0)  [0] → "foo"              │
│  "bar" → SymbolId(1)  [1] → "bar"              │
│                       [2] → "baz"              │
│                                                  │
│  macros: HashMap<SymbolId, MacroDef>           │
│  modules: HashMap<SymbolId, ModuleDef>         │
│                                                  │
└─────────────────────────────────────────────────┘
                       ▲
                       │ Used by all phases
                       │
         ┌─────────────┼─────────────┐
         │             │             │
         ▼             ▼             ▼
      Reader      Compiler         VM
      
      
Scope Tracking During Compilation:
┌─────────────────────────────────────────────────┐
│ CompileScope (scope.rs)                         │
├─────────────────────────────────────────────────┤
│                                                  │
│  frames: Vec<ScopeFrame>  (stack of scopes)    │
│  ├─ [0] Global                                  │
│  ├─ [1] Lambda 1                                │
│  │   ├─ parameters: [x, y]                      │
│  │   └─ locals: [z]                             │
│  └─ [2] Lambda 2 (nested)                       │
│      └─ parameters: [a, b]                      │
│                                                  │
│  When compiling Var(sym_id):                   │
│  • Search frames from top to bottom             │
│  • Return (depth, index) if found               │
│  • depth = how many scopes up                   │
│  • index = position in that scope               │
│                                                  │
│  Example: At depth 2                            │
│    Var(x, 1, 0) means "x from parent scope"    │
│    └─ parent has x at index 0                   │
│                                                  │
└─────────────────────────────────────────────────┘
```

## 3. Expr Structure and Variable References

```
Expr Variants:
┌──────────────────────────────────────────────────┐
│ Variables:                                        │
│  Var(SymbolId, depth, index)                    │
│    └─ Local/captured variables                   │
│  GlobalVar(SymbolId)                            │
│    └─ Global function/constant references       │
│                                                  │
│ Functions:                                       │
│  Lambda {                                        │
│    params: Vec<SymbolId>,                       │
│    body: Box<Expr>,                             │
│    captures: Vec<(SymbolId, depth, index)>,    │
│  }                                               │
│    └─ Captures show what outer variables used  │
│                                                  │
│  Call {                                          │
│    func: Box<Expr>,                             │
│    args: Vec<Expr>,                             │
│    tail: bool,  // Tail call optimization       │
│  }                                               │
│                                                  │
│ Bindings:                                        │
│  Let { bindings, body }                         │
│  Letrec { bindings, body }                      │
│  Lambda (above)                                  │
│                                                  │
│ Control Flow:                                    │
│  If { cond, then, else_ }                       │
│  Cond { clauses, else_body }                    │
│  While { cond, body }                           │
│  For { var, iter, body }                        │
│  Match { value, patterns, default }             │
│                                                  │
│ Other:                                           │
│  Define { name, value }                         │
│  Set { var, depth, index, value }               │
│  Try { body, catch, finally }                   │
│  DefMacro/Module/Import/etc.                    │
│                                                  │
└──────────────────────────────────────────────────┘

Example: (lambda (x y) (+ x y))
│
▼
Lambda {
  params: [SymbolId(x), SymbolId(y)],
  body: Call {
    func: GlobalVar(SymbolId(+)),
    args: [
      Var(SymbolId(x), 0, 0),  // depth=0 (current scope), index=0 (first param)
      Var(SymbolId(y), 0, 1),  // depth=0 (current scope), index=1 (second param)
    ],
    tail: false,
  },
  captures: [],  // No captured variables
}

Environment at runtime: [x_value, y_value]
                        ^index=0  ^index=1
                        ^depth=0 (current scope)
```

## 4. Closure Capture and Environment Layout

```
Source Code:
┌────────────────────────────────────────┐
│ (lambda (x)                            │
│   (lambda (y)                          │
│     (+ x y)))  ; x is captured         │
└────────────────────────────────────────┘

After Compilation:
┌────────────────────────────────────────────────┐
│ Outer Lambda:                                   │
│   params: [x]                                   │
│   body: (inner lambda)                          │
│   captures: []  (x is parameter, not captured)  │
│                                                 │
│   Environment: [x_value]                       │
│                 ^index=0                        │
│                                                 │
│ Inner Lambda:                                   │
│   params: [y]                                   │
│   body: Call(+, [Var(x, 1, 0), Var(y, 0, 1)])  │
│   captures: [(x, 1, 0)]  ← x from outer scope  │
│                                                 │
│   Environment: [x_value, y_value]             │
│                 ^captured(0)  ^param(1)        │
│                 ^depth=1      ^depth=0         │
│                                                 │
└────────────────────────────────────────────────┘

Key: Environment layout is [captures..., parameters...]
     Var indices adjusted during compilation to match this layout
```

## 5. VM Global Symbol Table

```
┌─────────────────────────────────────────────────┐
│ VM Globals: HashMap<u32, Value>                 │
├─────────────────────────────────────────────────┤
│                                                  │
│ Key (u32)          Value                        │
│ ──────────────────────────────────────────────  │
│ SymbolId(+).0  →  NativeFn(prim_add)           │
│ SymbolId(-).0  →  NativeFn(prim_sub)           │
│ SymbolId(*).0  →  NativeFn(prim_mul)           │
│ ...                                              │
│ SymbolId(foo).0 → Closure(...)                  │
│ SymbolId(bar).0 → Int(42)                       │
│ SymbolId(baz).0 → String("hello")               │
│                                                  │
│ Lookup:                                          │
│   1. Symbol name "+" → intern() → SymbolId(0)  │
│   2. globals[0] → Some(NativeFn(...))           │
│   3. Call the function                          │
│                                                  │
└─────────────────────────────────────────────────┘
```

## 6. Information Loss Through Compilation

```
read_str Stage:
┌────────────────────────────────────┐
│ TokenWithLoc {                      │
│   token: Symbol("foo"),             │
│   loc: SourceLoc(5, 10),            │ ◄── LINE & COLUMN PRESERVED
│ }                                   │
└────────────────────────────────────┘
         │
         ▼ Build Value
    ┌────────────────┐
    │ Value::Symbol  │  ◄── LOCATION LOST!
    │   (SymbolId)   │
    └────────────────┘
         │
         ▼ value_to_expr
    ┌──────────────────┐
    │ ExprWithLoc {    │
    │   expr: Var(...) │
    │   loc: None      │  ◄── LOCATION USUALLY NONE
    │ }                │
    └──────────────────┘
         │
         ▼ compile
    ┌──────────────────────┐
    │ Bytecode {           │
    │   instructions: [...] │
    │   constants: [...]   │  ◄── ONLY VALUES REMAIN
    │ }                    │      NO SYMBOL NAMES OR LOCS
    └──────────────────────┘

What Can Still Be Recovered:
• SymbolId values (from constants)
• Scope structure (from Var depth/index)
• Definition locations (from Define nodes in Expr)
• Function arity (from Lambda params)

What Cannot Be Recovered:
• Original source locations (lost at read_str)
• Original formatting
• Comments
• Macro definition sources
```

## 7. Recommended LSP Architecture

```
┌─────────────────────────────────────────────────────────┐
│ LSP Server Main Loop (stdio)                             │
├─────────────────────────────────────────────────────────┤
│ while read_message():                                    │
│   handle_request()                                       │
│   send_response()                                        │
└────────────┬────────────────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────────────────┐
│ Document Manager                                         │
├─────────────────────────────────────────────────────────┤
│ documents: HashMap<URI, DocumentState>                  │
│                                                          │
│ DocumentState {                                          │
│   uri: String,                                           │
│   version: u32,                                          │
│   text: String,                                          │
│   ├─ expr_with_loc: ExprWithLoc,                        │
│   ├─ symbol_table: Arc<RwLock<SymbolTable>>,  SHARED   │
│   ├─ diagnostics: Vec<Diagnostic>,                      │
│   └─ completion_items: Vec<CompletionItem>,             │
│ }                                                        │
└────────────┬────────────────────────────────────────────┘
             │
      ┌──────┴──────┬──────────┬────────────┐
      │             │          │            │
      ▼             ▼          ▼            ▼
   Hover       Definition  References  Completion
   Handler      Handler     Handler     Handler
      │             │          │            │
      └──────┬──────┴────┬─────┴───────┬────┘
             │          │            │
             ▼          ▼            ▼
      ┌────────────────────────────────────┐
      │ Symbol Index & Linter              │
      ├────────────────────────────────────┤
      │ • Find symbols in Expr             │
      │ • Map positions to definitions     │
      │ • Run diagnostics                  │
      │ • Generate completions             │
      └────────────────────────────────────┘

Data Flow for Hover Query:
1. Client: "What's at line 5, col 10?"
2. Get DocumentState for URI
3. Walk ExprWithLoc tree to find symbol at position
4. Look up SymbolId in SymbolTable → name
5. Find Define/Lambda for that symbol
6. Return: type, location, documentation
7. Send HoverInfo response
```

## 8. Compilation Performance Characteristics

```
Typical File Compilation Timeline:

Source Code (1KB)
      │
      ├─ Read Phase: ~100μs
      │  ├─ Lexing: ~50μs
      │  └─ Value construction: ~50μs
      │
      ├─ Conversion Phase: ~200μs
      │  ├─ Pattern matching: ~100μs
      │  └─ Macro expansion: ~100μs
      │
      ├─ Analysis Phase: ~100μs
      │  ├─ Capture analysis: ~60μs
      │  └─ Variable adjustment: ~40μs
      │
      └─ Compilation Phase: ~300μs
         ├─ Bytecode emission: ~200μs
         └─ Constant pool: ~100μs

Total: ~700μs (~1ms) for typical 1KB file

Scales roughly linearly with code size.
Can safely compile on every keystroke.

Memory footprint: ~50KB bytecode + constants per file
```

---

These diagrams show:
1. How code flows through the compilation pipeline
2. Where information is preserved/lost
3. How symbols and scopes are tracked
4. How closures capture variables
5. What the LSP server needs to do
6. Performance characteristics

Key takeaway: The Expr tree (after value_to_expr) contains all the 
symbol information needed for LSP features. Just need to:
1. Preserve source locations through compilation
2. Walk Expr tree to find symbols at positions
3. Leverage SymbolTable for name/definition lookups
