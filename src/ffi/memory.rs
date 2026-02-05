//! Memory Management for FFI
//!
//! This module provides utilities for tracking memory allocations.
//! Full memory management would be integrated with the VM's GC.

use std::ffi::c_void;

/// Identifies who owns a memory allocation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MemoryOwner {
    /// Elle owns this memory and will free it
    Elle,
    /// C owns this memory, Elle just holds a reference
    C,
    /// Both Elle and C manage it (shared/refcounted)
    Shared,
}

/// Information about a tracked allocation
#[derive(Clone, Debug)]
pub struct AllocationInfo {
    /// Unique allocation ID
    pub id: u32,
    /// Type ID (if known)
    pub type_id: Option<u32>,
    /// Type name (for debugging)
    pub type_name: String,
    /// Size in bytes
    pub size: usize,
    /// Who owns this memory
    pub owner: MemoryOwner,
    /// Reference count (for Shared owner)
    pub ref_count: u32,
    /// Whether this is still valid (not freed)
    pub valid: bool,
}

/// Simple allocation tracker (would be integrated with VM in full implementation)
pub struct AllocationTracker {
    allocations: std::collections::HashMap<*const c_void, AllocationInfo>,
    next_alloc_id: u32,
}

impl AllocationTracker {
    /// Create a new tracker
    pub fn new() -> Self {
        AllocationTracker {
            allocations: std::collections::HashMap::new(),
            next_alloc_id: 1,
        }
    }

    /// Register an allocation
    pub fn register(
        &mut self,
        ptr: *const c_void,
        type_name: &str,
        size: usize,
        owner: MemoryOwner,
    ) -> u32 {
        let id = self.next_alloc_id;
        self.next_alloc_id += 1;

        self.allocations.insert(
            ptr,
            AllocationInfo {
                id,
                type_id: None,
                type_name: type_name.to_string(),
                size,
                owner,
                ref_count: 1,
                valid: true,
            },
        );

        id
    }

    /// Get allocation info
    pub fn get(&self, ptr: *const c_void) -> Option<AllocationInfo> {
        self.allocations.get(&ptr).cloned()
    }

    /// Check if pointer is Elle-owned and can be freed
    pub fn can_free(&self, ptr: *const c_void) -> bool {
        matches!(
            self.allocations.get(&ptr),
            Some(info) if info.valid && info.owner == MemoryOwner::Elle
        )
    }

    /// Check if pointer is null or invalid
    pub fn is_null_or_invalid(&self, ptr: *const c_void) -> bool {
        ptr.is_null() || !self.allocations.get(&ptr).map(|i| i.valid).unwrap_or(true)
    }

    /// Get total allocated memory in bytes
    pub fn total_allocated(&self) -> usize {
        self.allocations
            .values()
            .filter(|info| info.valid)
            .map(|info| info.size)
            .sum()
    }

    /// Get allocation count
    pub fn allocation_count(&self) -> usize {
        self.allocations.values().filter(|info| info.valid).count()
    }
}

impl Default for AllocationTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Simple memory statistics holder
#[derive(Debug, Clone, Copy)]
pub struct MemoryStats {
    /// Total bytes allocated
    pub total_bytes: usize,
    /// Number of allocations
    pub allocation_count: usize,
}

/// Placeholder functions for memory management
/// In a full implementation, these would be integrated with the VM

/// Register a memory allocation
pub fn register_allocation(
    ptr: *const c_void,
    type_name: &str,
    size: usize,
    owner: MemoryOwner,
) -> u32 {
    // In a full implementation, this would update a tracker in the VM
    // For now, just assign a unique ID
    static ALLOC_ID_COUNTER: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(1);
    let id = ALLOC_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    id
}

/// Check if pointer is Elle-owned and can be freed
pub fn can_free_pointer(_ptr: *const c_void) -> bool {
    // In a full implementation, would check the VM's memory tracker
    false
}

/// Check if pointer is null or invalid
pub fn is_pointer_invalid(ptr: *const c_void) -> bool {
    ptr.is_null()
}

/// Get memory statistics
pub fn get_memory_stats() -> (usize, usize) {
    // In a full implementation, would query the VM's memory tracker
    (0, 0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tracker_creation() {
        let tracker = AllocationTracker::new();
        assert_eq!(tracker.allocation_count(), 0);
        assert_eq!(tracker.total_allocated(), 0);
    }

    #[test]
    fn test_tracker_register() {
        let mut tracker = AllocationTracker::new();
        let ptr = 0x12345678 as *const c_void;

        let id = tracker.register(ptr, "TestType", 1024, MemoryOwner::Elle);
        assert!(id > 0);

        let info = tracker.get(ptr).unwrap();
        assert_eq!(info.type_name, "TestType");
        assert_eq!(info.size, 1024);
        assert_eq!(info.owner, MemoryOwner::Elle);
    }

    #[test]
    fn test_memory_ownership() {
        let mut tracker = AllocationTracker::new();
        let ptr1 = 0x11111111 as *const c_void;
        let ptr2 = 0x22222222 as *const c_void;

        tracker.register(ptr1, "Elle-owned", 100, MemoryOwner::Elle);
        tracker.register(ptr2, "C-owned", 200, MemoryOwner::C);

        assert!(tracker.can_free(ptr1));
        assert!(!tracker.can_free(ptr2));
    }

    #[test]
    fn test_pointer_validity() {
        let mut tracker = AllocationTracker::new();
        assert!(tracker.is_null_or_invalid(std::ptr::null()));

        let ptr = 0x12345678 as *const c_void;
        // Unregistered pointers are considered valid (false = not invalid)
        assert!(!tracker.is_null_or_invalid(ptr));

        // Register and then invalidate
        tracker.register(ptr, "Test", 100, MemoryOwner::Elle);
        assert!(!tracker.is_null_or_invalid(ptr));
    }

    #[test]
    fn test_memory_stats() {
        let mut tracker = AllocationTracker::new();
        tracker.register(
            0x11111111 as *const c_void,
            "Alloc1",
            1000,
            MemoryOwner::Elle,
        );
        tracker.register(0x22222222 as *const c_void, "Alloc2", 2000, MemoryOwner::C);

        assert_eq!(tracker.total_allocated(), 3000);
        assert_eq!(tracker.allocation_count(), 2);
    }
}
