use crate::value::{SymbolId, Value};

/// AST representation after macro expansion and analysis
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Literal value
    Literal(Value),

    /// Variable reference (symbol, depth, index)
    Var(SymbolId, usize, usize),

    /// Global variable reference
    GlobalVar(SymbolId),

    /// If expression
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
    },

    /// Begin (sequence of expressions)
    Begin(Vec<Expr>),

    /// Function call
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
        tail: bool,
    },

    /// Lambda expression
    Lambda {
        params: Vec<SymbolId>,
        body: Box<Expr>,
        captures: Vec<(SymbolId, usize, usize)>,
    },

    /// Let binding
    Let {
        bindings: Vec<(SymbolId, Expr)>,
        body: Box<Expr>,
    },

    /// Set! (mutation)
    Set {
        var: SymbolId,
        depth: usize,
        index: usize,
        value: Box<Expr>,
    },

    /// Define (top-level only)
    Define { name: SymbolId, value: Box<Expr> },
}

impl Expr {
    pub fn is_tail_position(&self) -> bool {
        matches!(
            self,
            Expr::Call { tail: true, .. } | Expr::If { .. } | Expr::Begin(_) | Expr::Let { .. }
        )
    }
}
