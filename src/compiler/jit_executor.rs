// JIT Executor
//
// Executes JIT-compiled code by interfacing with Cranelift infrastructure.
// This bridges the gap between compile-time JIT compilation and runtime execution.

use super::ast::Expr;
use super::cranelift::context::JITContext;
use crate::symbol::SymbolTable;
use crate::value::Value;

/// A cached JIT compilation result
#[derive(Clone)]
pub struct CachedJitCode {
    /// Expression signature (for matching)
    pub expr_hash: u64,
    /// Whether this was successfully compiled
    pub compiled: bool,
}

impl CachedJitCode {
    /// Create a new cached JIT code entry
    pub fn new(expr_hash: u64, compiled: bool) -> Self {
        CachedJitCode {
            expr_hash,
            compiled,
        }
    }

    /// Compute hash of an expression for caching
    pub fn compute_hash(expr: &Expr) -> u64 {
        // Simple hash: use memory address as proxy for now
        // In production, would use proper hash of expression structure
        expr as *const Expr as u64
    }
}

/// JIT Code Executor manages execution of JIT-compiled code
pub struct JitExecutor {
    /// Cache of compiled code
    cache: std::collections::HashMap<u64, CachedJitCode>,
    /// JIT context for compilation
    jit_context: Option<JITContext>,
}

impl JitExecutor {
    /// Create a new JIT executor
    pub fn new() -> Result<Self, String> {
        Ok(JitExecutor {
            cache: std::collections::HashMap::new(),
            jit_context: JITContext::new().ok(),
        })
    }

    /// Try to JIT compile and execute an expression
    pub fn try_jit_execute(
        &mut self,
        expr: &Expr,
        _symbols: &SymbolTable,
    ) -> Result<Option<Value>, String> {
        // Check cache first
        let hash = CachedJitCode::compute_hash(expr);

        if let Some(cached) = self.cache.get(&hash) {
            if !cached.compiled {
                // Already tried and failed
                return Ok(None);
            }
            // For now, cache hit just means we know it's compilable
            // Actual native code execution would happen here
        }

        // Try to compile with Cranelift
        match expr {
            // Literals can be compiled and executed immediately
            Expr::Literal(val) => {
                self.cache.insert(hash, CachedJitCode::new(hash, true));
                Ok(Some(val.clone()))
            }

            // Binary operations - compile if JIT context available
            Expr::Call { .. } if self.jit_context.is_some() => {
                // Mark as attempted
                self.cache.insert(hash, CachedJitCode::new(hash, true));
                Ok(None) // Return None to fall back to bytecode
            }

            // Everything else
            _ => {
                self.cache.insert(hash, CachedJitCode::new(hash, false));
                Ok(None)
            }
        }
    }

    /// Check if JIT context is available
    pub fn has_jit_context(&self) -> bool {
        self.jit_context.is_some()
    }

    /// Get cache statistics
    pub fn cache_stats(&self) -> (usize, usize) {
        let total = self.cache.len();
        let compiled = self.cache.values().filter(|c| c.compiled).count();
        (compiled, total)
    }
}

impl Default for JitExecutor {
    fn default() -> Self {
        Self::new().unwrap_or(JitExecutor {
            cache: std::collections::HashMap::new(),
            jit_context: None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_jit_executor_creation() {
        let executor = JitExecutor::new();
        assert!(executor.is_ok());
    }

    #[test]
    fn test_jit_executor_literal_execution() {
        let mut executor = JitExecutor::new().unwrap();
        let symbols = SymbolTable::new();
        let expr = Expr::Literal(Value::Int(42));

        let result = executor.try_jit_execute(&expr, &symbols);
        assert!(result.is_ok());
        assert!(result.unwrap().is_some());
    }

    #[test]
    fn test_jit_executor_cache() {
        let mut executor = JitExecutor::new().unwrap();
        let symbols = SymbolTable::new();
        let expr1 = Expr::Literal(Value::Int(42));
        let expr2 = Expr::Literal(Value::Int(43));

        let hash1 = CachedJitCode::compute_hash(&expr1);
        let hash2 = CachedJitCode::compute_hash(&expr2);

        executor.try_jit_execute(&expr1, &symbols).ok();
        executor.try_jit_execute(&expr2, &symbols).ok();

        let (compiled, total) = executor.cache_stats();
        assert!(total >= 2);
        assert_ne!(hash1, hash2);
    }

    #[test]
    fn test_jit_executor_has_context() {
        let executor = JitExecutor::new().unwrap();
        // May or may not have context depending on system support
        let _ = executor.has_jit_context();
    }
}
