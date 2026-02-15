//! High-level Intermediate Representation (HIR)
//!
//! The HIR is the post-analysis representation where all names are resolved to binding IDs.
//! This is the intermediate step between the Syntax tree (pre-analysis) and the compiler.
//!
//! Key features:
//! - All variable references are resolved to BindingIds or SymbolIds
//! - Capture information for closures is computed
//! - Mutability information for bindings is tracked
//! - Special forms are explicitly represented
//!
//! The compilation pipeline is:
//! ```text
//! Source → Lexer → Token → Parser → Syntax → Expand → Syntax → Analyze → HIR → Compile → Bytecode
//! ```

mod analyze;
mod binding;
mod expr;
mod pattern;

pub use analyze::analyze;
pub use binding::{BindingId, BindingInfo, CaptureInfo, CaptureKind};
pub use expr::{Hir, HirKind, VarRef};
pub use pattern::HirPattern;
