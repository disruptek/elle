//! Trait implementations for Value (PartialEq, Eq, Hash).

use std::hash::{Hash, Hasher};

use super::Value;

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use crate::value::heap::{deref, HeapObject};

        // For immediate values, compare bits directly
        if !self.is_heap() && !other.is_heap() {
            return self.0 == other.0;
        }

        // If one is heap and the other isn't, they're not equal
        if self.is_heap() != other.is_heap() {
            return false;
        }

        // Both are heap values - dereference and compare contents
        unsafe {
            let self_obj = deref(*self);
            let other_obj = deref(*other);

            match (self_obj, other_obj) {
                // String comparison
                (HeapObject::String(s1), HeapObject::String(s2)) => s1 == s2,

                // Cons cell comparison
                (HeapObject::Cons(c1), HeapObject::Cons(c2)) => c1 == c2,

                // Array comparison (compare contents)
                (HeapObject::Array(v1), HeapObject::Array(v2)) => {
                    v1.borrow().as_slice() == v2.borrow().as_slice()
                }

                // Table comparison (compare contents)
                (HeapObject::Table(t1), HeapObject::Table(t2)) => *t1.borrow() == *t2.borrow(),

                // Struct comparison (compare contents)
                (HeapObject::Struct(s1), HeapObject::Struct(s2)) => s1 == s2,

                // Closure comparison (compare by reference)
                (HeapObject::Closure(c1), HeapObject::Closure(c2)) => std::rc::Rc::ptr_eq(c1, c2),

                // Tuple comparison (compare contents element-wise)
                (HeapObject::Tuple(t1), HeapObject::Tuple(t2)) => t1 == t2,

                // Buffer comparison (compare contents)
                (HeapObject::Buffer(b1), HeapObject::Buffer(b2)) => *b1.borrow() == *b2.borrow(),

                // Cell comparison (compare contents)
                (HeapObject::Cell(c1, _), HeapObject::Cell(c2, _)) => *c1.borrow() == *c2.borrow(),

                // Float comparison — bitwise, not IEEE, so NaN == NaN (same bits)
                (HeapObject::Float(f1), HeapObject::Float(f2)) => f1.to_bits() == f2.to_bits(),

                // NativeFn comparison (compare by reference)
                (HeapObject::NativeFn(_), HeapObject::NativeFn(_)) => {
                    std::ptr::eq(self_obj as *const _, other_obj as *const _)
                }

                // LibHandle comparison
                (HeapObject::LibHandle(h1), HeapObject::LibHandle(h2)) => h1 == h2,

                // ThreadHandle comparison (compare by reference)
                (HeapObject::ThreadHandle(_), HeapObject::ThreadHandle(_)) => {
                    std::ptr::eq(self_obj as *const _, other_obj as *const _)
                }

                // Fiber comparison (compare by reference)
                (HeapObject::Fiber(_), HeapObject::Fiber(_)) => {
                    std::ptr::eq(self_obj as *const _, other_obj as *const _)
                }

                // Syntax comparison (by reference — same Rc)
                (HeapObject::Syntax(s1), HeapObject::Syntax(s2)) => std::rc::Rc::ptr_eq(s1, s2),

                // Binding comparison (by reference — same heap allocation)
                (HeapObject::Binding(_), HeapObject::Binding(_)) => {
                    std::ptr::eq(self_obj as *const _, other_obj as *const _)
                }

                // FFI signature comparison (structural equality, skip CIF cache)
                (HeapObject::FFISignature(s1, _), HeapObject::FFISignature(s2, _)) => s1 == s2,

                // FFI type descriptor comparison (structural equality)
                (HeapObject::FFIType(t1), HeapObject::FFIType(t2)) => t1 == t2,

                // Managed pointer comparison (by identity, not address)
                (HeapObject::ManagedPointer(_), HeapObject::ManagedPointer(_)) => {
                    std::ptr::eq(self_obj as *const _, other_obj as *const _)
                }

                // External object comparison (by identity — same heap object)
                (HeapObject::External(_), HeapObject::External(_)) => {
                    std::ptr::eq(self_obj as *const _, other_obj as *const _)
                }

                // Parameter comparison (by identity — same heap object)
                (HeapObject::Parameter { .. }, HeapObject::Parameter { .. }) => {
                    std::ptr::eq(self_obj as *const _, other_obj as *const _)
                }

                // Bytes comparison (compare contents)
                (HeapObject::Bytes(b1), HeapObject::Bytes(b2)) => b1 == b2,

                // Blob comparison (compare contents)
                (HeapObject::Blob(b1), HeapObject::Blob(b2)) => *b1.borrow() == *b2.borrow(),

                // Different types are not equal
                _ => false,
            }
        }
    }
}

// NOTE: PartialEq is reflexive for all Value variants:
// - Immediate values: compared by raw bits (always reflexive)
// - Heap structural types: compared by contents (reflexive by induction)
// - Heap identity types: compared by pointer (always reflexive)
// - HeapObject::Float: compared by f64::to_bits() (always reflexive)
//
// The f64::to_bits() comparison means NaN == NaN (same bit pattern),
// which violates IEEE 754 but satisfies Eq's reflexivity requirement.
// This is intentional — set membership requires reflexivity.
impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use crate::value::heap::{deref, HeapObject};

        if !self.is_heap() {
            // Immediate values: raw bits encode the type tag + payload.
            // Same bits ↔ same value, and PartialEq agrees.
            //
            // SSO strings: same content → same bits → same hash.
            // Keywords: interned, same name → same pointer → same bits.
            // Inline floats: same float bits → same Value bits.
            // TAG_NAN floats: NaN/Infinity encoded deterministically.
            self.0.hash(state);
            return;
        }

        unsafe {
            let obj = deref(*self);
            let tag = obj.tag();
            tag.hash(state);

            match obj {
                // Structural content types (immutable)
                HeapObject::String(s) => s.hash(state),
                HeapObject::Cons(c) => c.hash(state),
                HeapObject::Tuple(elems) => elems.hash(state),
                HeapObject::Bytes(b) => b.hash(state),
                HeapObject::Struct(map) => {
                    for (k, v) in map {
                        k.hash(state);
                        v.hash(state);
                    }
                }

                // Structural content types (mutable — hash current contents)
                HeapObject::Array(rc) => {
                    let borrowed = rc.borrow();
                    borrowed.len().hash(state);
                    for v in borrowed.iter() {
                        v.hash(state);
                    }
                }
                HeapObject::Table(rc) => {
                    let borrowed = rc.borrow();
                    borrowed.len().hash(state);
                    for (k, v) in borrowed.iter() {
                        k.hash(state);
                        v.hash(state);
                    }
                }
                HeapObject::Buffer(rc) => rc.borrow().hash(state),
                HeapObject::Blob(rc) => rc.borrow().hash(state),
                HeapObject::Cell(rc, _) => rc.borrow().hash(state),

                // Structural-but-special heap types
                HeapObject::Float(f) => f.to_bits().hash(state),
                HeapObject::LibHandle(id) => id.hash(state),
                HeapObject::FFISignature(sig, _) => sig.hash(state),
                HeapObject::FFIType(desc) => desc.hash(state),

                // Reference-identity types: hash by raw Value bits (encodes pointer).
                // This matches PartialEq which uses pointer identity for these.
                _ => self.0.hash(state),
            }
        }
    }
}

// Debug is implemented in display.rs alongside Display, since both
// share the resolve_name helper for symbol/keyword resolution.
