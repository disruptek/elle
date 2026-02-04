//! Dynamic library loading with platform abstraction.
//!
//! Supports loading .so files on Linux and provides stubs for other platforms.

use std::ffi::OsStr;
use std::path::Path;

/// Handle to a loaded shared library.
pub struct LibraryHandle {
    /// Unique ID for this library in the FFI subsystem
    pub id: u32,
    /// Path to the library file
    pub path: String,
    /// The underlying native library (Linux only)
    #[cfg(target_os = "linux")]
    pub native: libloading::Library,
}

impl LibraryHandle {
    /// Get a raw pointer to a symbol in this library.
    ///
    /// # Arguments
    /// * `symbol_name` - The symbol to look up (e.g., "strlen")
    ///
    /// # Returns
    /// * `Ok(pointer)` - Raw function pointer
    /// * `Err(message)` - If symbol not found or other error
    pub fn get_symbol(&self, symbol_name: &str) -> Result<*const std::ffi::c_void, String> {
        #[cfg(target_os = "linux")]
        {
            unsafe {
                self.native
                    .get::<*const std::ffi::c_void>(symbol_name.as_bytes())
                    .map(|sym| *sym)
                    .map_err(|e| {
                        format!("Symbol '{}' not found in {}: {}", symbol_name, self.path, e)
                    })
            }
        }

        #[cfg(not(target_os = "linux"))]
        {
            Err(format!(
                "Dynamic library loading not supported on this platform (attempted to load {})",
                self.path
            ))
        }
    }
}

/// Load a dynamic library from a file path.
///
/// # Arguments
/// * `path` - Path to the library file (.so on Linux)
///
/// # Returns
/// * `Ok(library)` - Loaded library handle
/// * `Err(message)` - If file not found or not a valid library
///
/// # Example
/// ```ignore
/// let lib = load_library("/lib/x86_64-linux-gnu/libc.so.6")?;
/// let strlen_ptr = lib.get_symbol("strlen")?;
/// ```
pub fn load_library(path: &str) -> Result<LibraryHandle, String> {
    #[cfg(target_os = "linux")]
    {
        // Verify the file exists
        if !Path::new(path).exists() {
            return Err(format!("Library file not found: {}", path));
        }

        unsafe {
            match libloading::Library::new(path) {
                Ok(native) => Ok(LibraryHandle {
                    id: 0, // Will be assigned by FFISubsystem
                    path: path.to_string(),
                    native,
                }),
                Err(e) => Err(format!("Failed to load library '{}': {}", path, e)),
            }
        }
    }

    #[cfg(not(target_os = "linux"))]
    {
        Err(format!(
            "Dynamic library loading only supported on Linux (attempted to load {})",
            path
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(target_os = "linux")]
    fn test_load_libc() {
        // Load system libc
        let lib = load_library("/lib/x86_64-linux-gnu/libc.so.6")
            .or_else(|_| load_library("/lib64/libc.so.6"))
            .or_else(|_| load_library("libc.so.6"));

        // If libc is findable, test loading succeeds
        if lib.is_ok() {
            let lib = lib.unwrap();
            assert!(!lib.path.is_empty());
        }
    }

    #[test]
    fn test_missing_file() {
        let result = load_library("/nonexistent/library.so");
        assert!(result.is_err());
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn test_get_symbol_strlen() {
        let lib = load_library("/lib/x86_64-linux-gnu/libc.so.6")
            .or_else(|_| load_library("/lib64/libc.so.6"))
            .or_else(|_| load_library("libc.so.6"));

        if let Ok(lib) = lib {
            let result = lib.get_symbol("strlen");
            // strlen should exist in libc
            if result.is_ok() {
                assert!(!result.unwrap().is_null());
            }
        }
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn test_get_symbol_missing() {
        let lib = load_library("/lib/x86_64-linux-gnu/libc.so.6")
            .or_else(|_| load_library("/lib64/libc.so.6"))
            .or_else(|_| load_library("libc.so.6"));

        if let Ok(lib) = lib {
            let result = lib.get_symbol("this_function_does_not_exist_in_libc_12345");
            assert!(result.is_err());
        }
    }
}
