//! FFI value types for the Elle runtime

use std::ffi::c_void;

/// FFI library handle
///
/// Wraps a handle ID for a loaded dynamic library.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LibHandle(pub u32);

/// FFI C object handle (opaque pointer to C data)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CHandle {
    /// Raw C pointer
    pub ptr: *const c_void,
    /// Unique ID for this handle
    pub id: u32,
}

impl CHandle {
    /// Create a new C handle
    pub fn new(ptr: *const c_void, id: u32) -> Self {
        CHandle { ptr, id }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lib_handle() {
        let h1 = LibHandle(1);
        let h2 = LibHandle(1);
        let h3 = LibHandle(2);

        assert_eq!(h1, h2);
        assert_ne!(h1, h3);
    }

    #[test]
    fn test_c_handle() {
        let ptr = std::ptr::null();
        let h1 = CHandle::new(ptr, 1);
        let h2 = CHandle::new(ptr, 1);
        let h3 = CHandle::new(ptr, 2);

        assert_eq!(h1, h2);
        assert_ne!(h1, h3);
    }
}
