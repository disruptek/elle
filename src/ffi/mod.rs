//! Elle Foreign Function Interface (FFI) subsystem.
//!
//! Enables calling C/C++ functions from Elle Lisp code.
//!
//! # Example
//!
//! ```ignore
//! (load-library "/lib/x86_64-linux-gnu/libc.so.6")
//! (strlen "hello")  ; => 5
//! ```

pub mod bindings;
pub mod call;
pub mod header;
pub mod loader;
pub mod marshal;
pub mod symbol;
pub mod types;

use loader::LibraryHandle;
use std::collections::HashMap;
use symbol::SymbolResolver;

/// The FFI subsystem manages loaded libraries and cached symbols.
pub struct FFISubsystem {
    /// Loaded libraries: id -> handle
    libraries: HashMap<u32, LibraryHandle>,
    /// Next library ID to assign
    next_lib_id: u32,
    /// Symbol resolver with caching
    symbol_resolver: SymbolResolver,
}

impl FFISubsystem {
    /// Create a new FFI subsystem.
    pub fn new() -> Self {
        FFISubsystem {
            libraries: HashMap::new(),
            next_lib_id: 1,
            symbol_resolver: SymbolResolver::new(),
        }
    }

    /// Load a shared library.
    ///
    /// # Arguments
    /// * `path` - Path to library file (.so on Linux)
    ///
    /// # Returns
    /// * `Ok(id)` - Library ID for future reference
    /// * `Err(message)` - If loading fails
    pub fn load_library(&mut self, path: &str) -> Result<u32, String> {
        let mut lib = loader::load_library(path)?;
        let id = self.next_lib_id;
        lib.id = id;
        self.next_lib_id += 1;
        self.libraries.insert(id, lib);
        Ok(id)
    }

    /// Get a loaded library by ID.
    pub fn get_library(&self, id: u32) -> Option<&LibraryHandle> {
        self.libraries.get(&id)
    }

    /// Get a mutable reference to a loaded library.
    pub fn get_library_mut(&mut self, id: u32) -> Option<&mut LibraryHandle> {
        self.libraries.get_mut(&id)
    }

    /// Unload a library (remove from registry).
    pub fn unload_library(&mut self, id: u32) -> Option<LibraryHandle> {
        self.libraries.remove(&id)
    }

    /// Get the symbol resolver.
    pub fn symbol_resolver(&mut self) -> &mut SymbolResolver {
        &mut self.symbol_resolver
    }

    /// List all loaded libraries.
    pub fn loaded_libraries(&self) -> Vec<(u32, String)> {
        self.libraries
            .iter()
            .map(|(id, lib)| (*id, lib.path.clone()))
            .collect()
    }
}

impl Default for FFISubsystem {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ffi_subsystem_creation() {
        let ffi = FFISubsystem::new();
        assert_eq!(ffi.loaded_libraries().len(), 0);
    }

    #[test]
    fn test_ffi_subsystem_default() {
        let ffi = FFISubsystem::default();
        assert_eq!(ffi.loaded_libraries().len(), 0);
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn test_load_library() {
        let mut ffi = FFISubsystem::new();
        let result = ffi
            .load_library("/lib/x86_64-linux-gnu/libc.so.6")
            .or_else(|_| ffi.load_library("/lib64/libc.so.6"))
            .or_else(|_| ffi.load_library("libc.so.6"));

        if let Ok(id) = result {
            assert!(ffi.get_library(id).is_some());
            assert_eq!(ffi.loaded_libraries().len(), 1);
        }
    }

    #[test]
    fn test_unload_library() {
        let mut ffi = FFISubsystem::new();
        let id = 42;

        // Try to unload nonexistent library
        assert!(ffi.unload_library(id).is_none());
    }
}
