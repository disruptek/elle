//! FFI type descriptors.
//!
//! C types are described by keywords at the Elle level. This module
//! provides the Rust representation and conversion from Elle keywords.

/// Describes a C type for marshalling.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDesc {
    Void,
    Bool,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    Float,
    Double,
    /// Platform-dependent `int`
    Int,
    /// Platform-dependent `unsigned int`
    UInt,
    /// Platform-dependent `long`
    Long,
    /// Platform-dependent `unsigned long`
    ULong,
    /// Platform-dependent `char` (signed on most platforms)
    Char,
    /// `unsigned char`
    UChar,
    /// `short`
    Short,
    /// `unsigned short`
    UShort,
    /// `size_t`
    Size,
    /// `ptrdiff_t`
    SSize,
    /// `void *` — maps to `Value::pointer()` or `nil` for NULL
    Ptr,
    /// `const char *` — marshalled as Elle string (copied)
    Str,
    /// Struct with positional fields
    Struct(StructDesc),
    /// Fixed-size array: element type + count
    Array(Box<TypeDesc>, usize),
}

/// Positional struct descriptor.
///
/// Fields are unnamed and ordered. Created via `ffi/struct` at the Elle level.
#[derive(Debug, Clone, PartialEq)]
pub struct StructDesc {
    pub fields: Vec<TypeDesc>,
}

impl TypeDesc {
    /// Parse a type descriptor from an Elle keyword name.
    ///
    /// Returns `None` for unrecognized keywords.
    pub fn from_keyword(name: &str) -> Option<Self> {
        match name {
            "void" => Some(TypeDesc::Void),
            "bool" => Some(TypeDesc::Bool),
            "i8" => Some(TypeDesc::I8),
            "u8" => Some(TypeDesc::U8),
            "i16" => Some(TypeDesc::I16),
            "u16" => Some(TypeDesc::U16),
            "i32" => Some(TypeDesc::I32),
            "u32" => Some(TypeDesc::U32),
            "i64" => Some(TypeDesc::I64),
            "u64" => Some(TypeDesc::U64),
            "float" => Some(TypeDesc::Float),
            "double" => Some(TypeDesc::Double),
            "int" => Some(TypeDesc::Int),
            "uint" => Some(TypeDesc::UInt),
            "long" => Some(TypeDesc::Long),
            "ulong" => Some(TypeDesc::ULong),
            "char" => Some(TypeDesc::Char),
            "uchar" => Some(TypeDesc::UChar),
            "short" => Some(TypeDesc::Short),
            "ushort" => Some(TypeDesc::UShort),
            "size" => Some(TypeDesc::Size),
            "ssize" => Some(TypeDesc::SSize),
            "ptr" => Some(TypeDesc::Ptr),
            "string" => Some(TypeDesc::Str),
            _ => None,
        }
    }

    /// Size of this type in bytes on the current platform.
    ///
    /// Returns `None` for `Void`.
    pub fn size(&self) -> Option<usize> {
        match self {
            TypeDesc::Void => None,
            TypeDesc::Bool => Some(std::mem::size_of::<std::ffi::c_int>()), // C _Bool
            TypeDesc::I8 | TypeDesc::U8 => Some(1),
            TypeDesc::I16 | TypeDesc::U16 => Some(2),
            TypeDesc::I32 | TypeDesc::U32 => Some(4),
            TypeDesc::I64 | TypeDesc::U64 => Some(8),
            TypeDesc::Float => Some(4),
            TypeDesc::Double => Some(8),
            TypeDesc::Int | TypeDesc::UInt => Some(std::mem::size_of::<std::ffi::c_int>()),
            TypeDesc::Long | TypeDesc::ULong => Some(std::mem::size_of::<std::ffi::c_long>()),
            TypeDesc::Char | TypeDesc::UChar => Some(1),
            TypeDesc::Short | TypeDesc::UShort => Some(std::mem::size_of::<std::ffi::c_short>()),
            TypeDesc::Size => Some(std::mem::size_of::<usize>()),
            TypeDesc::SSize => Some(std::mem::size_of::<isize>()),
            TypeDesc::Ptr | TypeDesc::Str => Some(std::mem::size_of::<*const ()>()),
            TypeDesc::Struct(desc) => {
                // Sum of field sizes with alignment padding
                let mut offset = 0usize;
                for field in &desc.fields {
                    let field_size = field.size()?;
                    let field_align = field.align()?;
                    // Align offset
                    offset = (offset + field_align - 1) & !(field_align - 1);
                    offset += field_size;
                }
                // Align to struct alignment
                if let Some(align) = self.align() {
                    offset = (offset + align - 1) & !(align - 1);
                }
                Some(offset)
            }
            TypeDesc::Array(elem, count) => elem.size().map(|s| s * count),
        }
    }

    /// Alignment of this type in bytes on the current platform.
    ///
    /// Returns `None` for `Void`.
    pub fn align(&self) -> Option<usize> {
        match self {
            TypeDesc::Void => None,
            TypeDesc::Bool => Some(std::mem::align_of::<std::ffi::c_int>()),
            TypeDesc::I8 | TypeDesc::U8 => Some(1),
            TypeDesc::I16 | TypeDesc::U16 => Some(2),
            TypeDesc::I32 | TypeDesc::U32 => Some(4),
            TypeDesc::I64 | TypeDesc::U64 => Some(8),
            TypeDesc::Float => Some(4),
            TypeDesc::Double => Some(8),
            TypeDesc::Int | TypeDesc::UInt => Some(std::mem::align_of::<std::ffi::c_int>()),
            TypeDesc::Long | TypeDesc::ULong => Some(std::mem::align_of::<std::ffi::c_long>()),
            TypeDesc::Char | TypeDesc::UChar => Some(1),
            TypeDesc::Short | TypeDesc::UShort => Some(std::mem::align_of::<std::ffi::c_short>()),
            TypeDesc::Size => Some(std::mem::align_of::<usize>()),
            TypeDesc::SSize => Some(std::mem::align_of::<isize>()),
            TypeDesc::Ptr | TypeDesc::Str => Some(std::mem::align_of::<*const ()>()),
            TypeDesc::Struct(desc) => {
                // Alignment is the max alignment of any field
                desc.fields
                    .iter()
                    .filter_map(|f| f.align())
                    .max()
                    .or(Some(1))
            }
            TypeDesc::Array(elem, _) => elem.align(),
        }
    }
}

/// Reified function signature for FFI calls.
///
/// Created by `ffi/signature`. Contains calling convention, return type,
/// and argument types. Signatures are cached/reused since creating one
/// involves libffi prep work.
#[derive(Debug, Clone, PartialEq)]
pub struct Signature {
    /// Calling convention (currently only `:default`)
    pub convention: CallingConvention,
    /// Return type
    pub ret: TypeDesc,
    /// Argument types
    pub args: Vec<TypeDesc>,
}

/// Calling convention for FFI functions.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CallingConvention {
    /// Platform default calling convention
    Default,
}

impl CallingConvention {
    /// Parse from keyword name.
    pub fn from_keyword(name: &str) -> Option<Self> {
        match name {
            "default" => Some(CallingConvention::Default),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_parsing() {
        assert_eq!(TypeDesc::from_keyword("void"), Some(TypeDesc::Void));
        assert_eq!(TypeDesc::from_keyword("i32"), Some(TypeDesc::I32));
        assert_eq!(TypeDesc::from_keyword("ptr"), Some(TypeDesc::Ptr));
        assert_eq!(TypeDesc::from_keyword("string"), Some(TypeDesc::Str));
        assert_eq!(TypeDesc::from_keyword("size"), Some(TypeDesc::Size));
        assert_eq!(TypeDesc::from_keyword("nonsense"), None);
    }

    #[test]
    fn test_primitive_sizes() {
        assert_eq!(TypeDesc::Void.size(), None);
        assert_eq!(TypeDesc::I8.size(), Some(1));
        assert_eq!(TypeDesc::I16.size(), Some(2));
        assert_eq!(TypeDesc::I32.size(), Some(4));
        assert_eq!(TypeDesc::I64.size(), Some(8));
        assert_eq!(TypeDesc::Float.size(), Some(4));
        assert_eq!(TypeDesc::Double.size(), Some(8));
        assert_eq!(TypeDesc::Ptr.size(), Some(8)); // 64-bit platform
    }

    #[test]
    fn test_struct_size_and_align() {
        // Two i32 fields: no padding needed
        let s = TypeDesc::Struct(StructDesc {
            fields: vec![TypeDesc::I32, TypeDesc::I32],
        });
        assert_eq!(s.size(), Some(8));
        assert_eq!(s.align(), Some(4));

        // i8 + i32: padding after i8
        let s2 = TypeDesc::Struct(StructDesc {
            fields: vec![TypeDesc::I8, TypeDesc::I32],
        });
        assert_eq!(s2.size(), Some(8)); // 1 + 3 padding + 4
        assert_eq!(s2.align(), Some(4));
    }

    #[test]
    fn test_array_size() {
        let a = TypeDesc::Array(Box::new(TypeDesc::I32), 10);
        assert_eq!(a.size(), Some(40));
        assert_eq!(a.align(), Some(4));
    }

    #[test]
    fn test_calling_convention() {
        assert_eq!(
            CallingConvention::from_keyword("default"),
            Some(CallingConvention::Default)
        );
        assert_eq!(CallingConvention::from_keyword("sysv64"), None);
    }
}
