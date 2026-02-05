//! WASM support for FFI
//!
//! This module provides WASM-specific implementations alongside native Linux support.
//! WASM doesn't support dynamic library loading (dlopen/dlsym), so we use a different
//! strategy: pre-compiled WASM modules and JavaScript interop via wasm-bindgen.

#[cfg(target_arch = "wasm32")]
pub mod wasm_impl {
    use crate::Value;

    /// WASM FFI loader
    pub struct WasmFFILoader;

    impl WasmFFILoader {
        /// WASM cannot load .so files; instead links at compile time
        pub fn load_library(_path: &str) -> Result<u32, String> {
            Err("WASM cannot load .so files dynamically. Pre-compile C code with emscripten and link at build time.".to_string())
        }

        /// Call a JavaScript function via wasm-bindgen
        pub fn call_js_function(_name: &str, _args: &[Value]) -> Result<Value, String> {
            Err("JavaScript interop not yet implemented in wasm-bindgen layer".to_string())
        }

        /// Check if running on WASM
        pub fn is_wasm() -> bool {
            true
        }
    }

    /// WASM module handle
    #[derive(Debug, Clone)]
    pub struct WasmModuleHandle {
        pub id: u32,
        pub name: String,
    }

    /// WASM callback (requires special handling)
    pub struct WasmCallback {
        pub id: u32,
    }

    impl WasmCallback {
        /// WASM callbacks are simpler - stored in a table
        pub fn new(id: u32) -> Self {
            WasmCallback { id }
        }

        /// Get callback index
        pub fn index(&self) -> usize {
            self.id as usize
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_wasm_detection() {
            assert!(WasmFFILoader::is_wasm());
        }

        #[test]
        fn test_wasm_module_handle_creation() {
            let handle = WasmModuleHandle {
                id: 1,
                name: "test".to_string(),
            };
            assert_eq!(handle.id, 1);
            assert_eq!(handle.name, "test");
        }

        #[test]
        fn test_wasm_callback_creation() {
            let cb = WasmCallback::new(42);
            assert_eq!(cb.id, 42);
            assert_eq!(cb.index(), 42);
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub mod native_impl {
    use crate::Value;

    /// Native FFI loader (Linux/Unix)
    pub struct NativeFFILoader;

    impl NativeFFILoader {
        /// Load .so file using libloading
        pub fn load_library(path: &str) -> Result<u32, String> {
            // Actual implementation in ffi/loader.rs
            // This is just a stub for the wasm module structure
            Ok(1)
        }

        /// Call a C function via libffi
        pub fn call_c_function(_symbol: &str, _args: &[Value]) -> Result<Value, String> {
            Err("C function calling not yet implemented in wasm stub".to_string())
        }

        /// Check if running on native platform
        pub fn is_wasm() -> bool {
            false
        }
    }

    /// Native library handle
    #[derive(Debug, Clone)]
    pub struct NativeLibraryHandle {
        pub id: u32,
        pub path: String,
    }

    /// Native callback (uses trampolines)
    pub struct NativeCallback {
        pub id: u32,
    }

    impl NativeCallback {
        /// Create a trampoline function for this callback
        pub fn new(id: u32) -> Self {
            NativeCallback { id }
        }

        /// Get callback pointer
        pub fn as_ptr(&self) -> *const std::ffi::c_void {
            self.id as *const std::ffi::c_void
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_native_detection() {
            assert!(!NativeFFILoader::is_wasm());
        }

        #[test]
        fn test_native_library_handle_creation() {
            let handle = NativeLibraryHandle {
                id: 1,
                path: "/usr/lib/libm.so".to_string(),
            };
            assert_eq!(handle.id, 1);
            assert!(handle.path.contains("libm.so"));
        }

        #[test]
        fn test_native_callback_creation() {
            let cb = NativeCallback::new(42);
            assert_eq!(cb.id, 42);
            assert!(!cb.as_ptr().is_null());
        }
    }
}

// Re-export based on target platform
#[cfg(target_arch = "wasm32")]
pub use wasm_impl::*;

#[cfg(not(target_arch = "wasm32"))]
pub use native_impl::*;

/// Platform detection trait
pub trait PlatformDetection {
    fn is_wasm() -> bool;
}

#[cfg(target_arch = "wasm32")]
impl PlatformDetection for WasmFFILoader {
    fn is_wasm() -> bool {
        true
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl PlatformDetection for NativeFFILoader {
    fn is_wasm() -> bool {
        false
    }
}

/// Get platform information string
pub fn platform_info() -> String {
    #[cfg(target_arch = "wasm32")]
    {
        "WASM32 (emscripten/wasm-bindgen)".to_string()
    }
    #[cfg(all(target_arch = "x86_64", target_os = "linux"))]
    {
        "Linux x86-64 (libloading + libffi)".to_string()
    }
    #[cfg(all(target_arch = "aarch64", target_os = "linux"))]
    {
        "Linux ARM64 (libloading + libffi)".to_string()
    }
    #[cfg(target_os = "macos")]
    {
        "macOS (libloading + libffi)".to_string()
    }
    #[cfg(not(any(target_arch = "wasm32", target_os = "linux", target_os = "macos")))]
    {
        "Unknown platform".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_platform_info() {
        let info = platform_info();
        assert!(!info.is_empty());
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn test_native_platform() {
        assert!(!NativeFFILoader::is_wasm());
    }

    #[test]
    #[cfg(target_arch = "wasm32")]
    fn test_wasm_platform() {
        assert!(WasmFFILoader::is_wasm());
    }
}
