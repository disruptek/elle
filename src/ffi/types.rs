//! C type system definition and layout calculation.
//!
//! This module defines the C types that Elle can work with via FFI,
//! including size and alignment calculations for the current platform.

use std::fmt;

/// A C type that can be marshaled to/from Elle values.
///
/// # Supported Types
/// - Void
/// - Bool
/// - Char, Short, Int, Long, LongLong
/// - Float, Double
/// - Pointer types (future)
/// - Struct types (future - Phase 2)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CType {
    Void,
    Bool,
    Char,
    SChar,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
    Float,
    Double,
}

impl CType {
    /// Get the size of this type in bytes (x86-64 Linux ABI).
    pub fn size(self) -> usize {
        match self {
            CType::Void => 0,
            CType::Bool => 1,
            CType::Char | CType::SChar | CType::UChar => 1,
            CType::Short | CType::UShort => 2,
            CType::Int | CType::UInt => 4,
            CType::Long | CType::ULong => 8,
            CType::LongLong | CType::ULongLong => 8,
            CType::Float => 4,
            CType::Double => 8,
        }
    }

    /// Get the alignment of this type in bytes (x86-64 Linux ABI).
    pub fn alignment(self) -> usize {
        // x86-64 ABI: alignment matches size for scalar types
        self.size()
    }

    /// Check if this is an integer type.
    pub fn is_integer(self) -> bool {
        matches!(
            self,
            CType::Bool
                | CType::Char
                | CType::SChar
                | CType::UChar
                | CType::Short
                | CType::UShort
                | CType::Int
                | CType::UInt
                | CType::Long
                | CType::ULong
                | CType::LongLong
                | CType::ULongLong
        )
    }

    /// Check if this is a floating-point type.
    pub fn is_float(self) -> bool {
        matches!(self, CType::Float | CType::Double)
    }
}

impl fmt::Display for CType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CType::Void => write!(f, "void"),
            CType::Bool => write!(f, "bool"),
            CType::Char => write!(f, "char"),
            CType::SChar => write!(f, "signed char"),
            CType::UChar => write!(f, "unsigned char"),
            CType::Short => write!(f, "short"),
            CType::UShort => write!(f, "unsigned short"),
            CType::Int => write!(f, "int"),
            CType::UInt => write!(f, "unsigned int"),
            CType::Long => write!(f, "long"),
            CType::ULong => write!(f, "unsigned long"),
            CType::LongLong => write!(f, "long long"),
            CType::ULongLong => write!(f, "unsigned long long"),
            CType::Float => write!(f, "float"),
            CType::Double => write!(f, "double"),
        }
    }
}

/// Function signature for a C function.
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    /// Function name (as it appears in the library)
    pub name: String,
    /// Argument types
    pub args: Vec<CType>,
    /// Return type
    pub return_type: CType,
    /// Whether this is a variadic function (not yet supported)
    pub variadic: bool,
}

impl FunctionSignature {
    /// Create a new function signature.
    pub fn new(name: String, args: Vec<CType>, return_type: CType) -> Self {
        FunctionSignature {
            name,
            args,
            return_type,
            variadic: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_sizes() {
        assert_eq!(CType::Void.size(), 0);
        assert_eq!(CType::Bool.size(), 1);
        assert_eq!(CType::Char.size(), 1);
        assert_eq!(CType::Short.size(), 2);
        assert_eq!(CType::Int.size(), 4);
        assert_eq!(CType::Long.size(), 8);
        assert_eq!(CType::LongLong.size(), 8);
        assert_eq!(CType::Float.size(), 4);
        assert_eq!(CType::Double.size(), 8);
    }

    #[test]
    fn test_type_alignment() {
        assert_eq!(CType::Bool.alignment(), 1);
        assert_eq!(CType::Short.alignment(), 2);
        assert_eq!(CType::Int.alignment(), 4);
        assert_eq!(CType::Long.alignment(), 8);
        assert_eq!(CType::Double.alignment(), 8);
    }

    #[test]
    fn test_type_classification() {
        assert!(CType::Int.is_integer());
        assert!(!CType::Float.is_integer());
        assert!(!CType::Double.is_integer());
        assert!(CType::Float.is_float());
        assert!(CType::Double.is_float());
        assert!(!CType::Int.is_float());
    }
}
