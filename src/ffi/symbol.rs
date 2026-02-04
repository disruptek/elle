//! Symbol resolution and caching.
//!
//! Caches resolved symbols to avoid repeated lookups.

use super::loader::LibraryHandle;
use super::types::FunctionSignature;
use std::collections::HashMap;

/// Cached function handle.
#[derive(Clone)]
pub struct CachedFunction {
    /// The function pointer
    pub func_ptr: *const std::ffi::c_void,
    /// The function signature
    pub signature: FunctionSignature,
}

/// Symbol resolver with caching.
pub struct SymbolResolver {
    /// Cache: (library_id, symbol_name) -> CachedFunction
    cache: HashMap<(u32, String), CachedFunction>,
}

impl SymbolResolver {
    /// Create a new symbol resolver.
    pub fn new() -> Self {
        SymbolResolver {
            cache: HashMap::new(),
        }
    }

    /// Resolve a symbol in a library, with caching.
    ///
    /// # Arguments
    /// * `lib` - The library to search
    /// * `symbol_name` - The symbol name (e.g., "strlen")
    /// * `signature` - The function signature
    ///
    /// # Returns
    /// * `Ok(function)` - The resolved function
    /// * `Err(message)` - If symbol not found
    pub fn resolve(
        &mut self,
        lib: &LibraryHandle,
        symbol_name: &str,
        signature: FunctionSignature,
    ) -> Result<CachedFunction, String> {
        let key = (lib.id, symbol_name.to_string());

        // Check cache first
        if let Some(cached) = self.cache.get(&key) {
            return Ok(cached.clone());
        }

        // Not in cache, resolve from library
        let func_ptr = lib.get_symbol(symbol_name)?;

        let cached = CachedFunction {
            func_ptr,
            signature,
        };

        // Store in cache
        self.cache.insert(key, cached.clone());

        Ok(cached)
    }

    /// Clear the symbol cache.
    pub fn clear_cache(&mut self) {
        self.cache.clear();
    }

    /// Get cache statistics.
    pub fn cache_size(&self) -> usize {
        self.cache.len()
    }
}

impl Default for SymbolResolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::super::types::CType;
    use super::*;

    #[test]
    fn test_resolver_creation() {
        let resolver = SymbolResolver::new();
        assert_eq!(resolver.cache_size(), 0);
    }

    #[test]
    fn test_cache_clearing() {
        let mut resolver = SymbolResolver::new();
        resolver.clear_cache();
        assert_eq!(resolver.cache_size(), 0);
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn test_symbol_caching() {
        use super::super::loader::load_library;

        let lib = load_library("/lib/x86_64-linux-gnu/libc.so.6")
            .or_else(|_| load_library("/lib64/libc.so.6"))
            .or_else(|_| load_library("libc.so.6"));

        if let Ok(mut lib) = lib {
            lib.id = 1;
            let mut resolver = SymbolResolver::new();

            let sig = FunctionSignature::new(
                "strlen".to_string(),
                vec![CType::Void], // Simplified for test
                CType::Long,
            );

            // First call should hit the library
            let result1 = resolver.resolve(&lib, "strlen", sig.clone());
            assert!(result1.is_ok());
            assert_eq!(resolver.cache_size(), 1);

            // Second call should hit the cache
            let result2 = resolver.resolve(&lib, "strlen", sig);
            assert!(result2.is_ok());
            assert_eq!(resolver.cache_size(), 1);

            // Verify same pointer
            assert_eq!(result1.unwrap().func_ptr, result2.unwrap().func_ptr);
        }
    }
}
