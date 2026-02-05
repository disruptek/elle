//! C Callbacks - Enable C code to call Elle functions
//!
//! This module provides callback wrappers that allow C libraries to call back into Elle code.
//! Callbacks are registered with metadata about their signature for validation.
//!
//! # Architecture
//!
//! - Callbacks are identified by unique IDs
//! - Callback metadata (arg types, return type) is stored in the FFI subsystem
//! - The actual Elle closure is managed separately by the VM
//! - This avoids threading issues with non-thread-safe types like Rc

use std::sync::atomic::{AtomicU32, Ordering};

use crate::ffi::types::CType;

/// Next callback ID to assign
static NEXT_CALLBACK_ID: AtomicU32 = AtomicU32::new(1);

/// Information about a registered callback (thread-safe metadata)
#[derive(Clone, Debug)]
pub struct CallbackInfo {
    /// Unique ID for this callback
    pub id: u32,
    /// Argument types for validation
    pub arg_types: Vec<CType>,
    /// Return type
    pub return_type: CType,
}

impl CallbackInfo {
    /// Create new callback info
    pub fn new(id: u32, arg_types: Vec<CType>, return_type: CType) -> Self {
        CallbackInfo {
            id,
            arg_types,
            return_type,
        }
    }
}

/// Create a new callback ID and metadata
///
/// # Arguments
/// - `arg_types`: Types of arguments the callback expects
/// - `return_type`: Return type of the callback
///
/// # Returns
/// A callback ID and metadata that can be used for validation
pub fn create_callback(arg_types: Vec<CType>, return_type: CType) -> (u32, CallbackInfo) {
    let id = NEXT_CALLBACK_ID.fetch_add(1, Ordering::SeqCst);
    let info = CallbackInfo::new(id, arg_types, return_type);
    (id, info)
}

/// Create a C callback wrapper that can be passed to C code
pub struct CCallback {
    pub id: u32,
    pub arg_types: Vec<CType>,
    pub return_type: CType,
}

impl CCallback {
    /// Create a new callback wrapper
    pub fn new(id: u32, arg_types: Vec<CType>, return_type: CType) -> Self {
        CCallback {
            id,
            arg_types,
            return_type,
        }
    }

    /// Convert callback ID to a pointer that can be passed to C
    pub fn as_ptr(&self) -> *const std::ffi::c_void {
        self.id as *const std::ffi::c_void
    }

    /// Extract callback ID from a pointer returned by C
    pub fn from_ptr(ptr: *const std::ffi::c_void) -> u32 {
        ptr as usize as u32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_callback_creation() {
        let (id1, info1) = create_callback(vec![CType::Int], CType::Void);
        let (id2, info2) = create_callback(vec![CType::Float], CType::Int);

        assert_ne!(id1, id2);
        assert_eq!(info1.id, id1);
        assert_eq!(info2.id, id2);
        assert_eq!(info1.arg_types, vec![CType::Int]);
        assert_eq!(info2.return_type, CType::Int);
    }

    #[test]
    fn test_callback_pointer_conversion() {
        let callback = CCallback::new(12345, vec![], CType::Void);
        let ptr = callback.as_ptr();
        let id = CCallback::from_ptr(ptr);
        assert_eq!(id, 12345);
    }

    #[test]
    fn test_callback_info_clone() {
        let info = CallbackInfo::new(42, vec![CType::Int, CType::Float], CType::Double);
        let info2 = info.clone();
        assert_eq!(info.id, info2.id);
        assert_eq!(info.arg_types, info2.arg_types);
    }
}
