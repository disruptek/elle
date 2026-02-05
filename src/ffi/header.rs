//! C header file parsing and analysis.
//!
//! This module parses C header files and extracts type information,
//! function signatures, constants, and enums for auto-binding generation.

use super::types::{CType, FunctionSignature};
use std::collections::HashMap;
use std::path::PathBuf;

/// Represents a parsed C type definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedType {
    /// A struct type with full layout
    Struct(String, Vec<(String, CType)>),
    /// An enum type with variants
    Enum(String, Vec<(String, i64)>),
    /// A typedef alias
    Typedef { name: String, base: CType },
    /// An opaque pointer type
    OpaquePointer { name: String },
}

/// A parsed C constant value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstantValue {
    Int(i64),
    UInt(u64),
    Float(String),
    String(String),
}

/// Result of parsing a header file.
#[derive(Debug, Clone)]
pub struct ParsedHeader {
    /// Type definitions found in header
    pub types: HashMap<String, ParsedType>,
    /// Function signatures found in header
    pub functions: HashMap<String, FunctionSignature>,
    /// Constants defined in header
    pub constants: HashMap<String, ConstantValue>,
    /// Enums defined in header
    pub enums: HashMap<String, Vec<(String, i64)>>,
}

impl ParsedHeader {
    /// Create a new empty parsed header.
    pub fn new() -> Self {
        ParsedHeader {
            types: HashMap::new(),
            functions: HashMap::new(),
            constants: HashMap::new(),
            enums: HashMap::new(),
        }
    }
}

impl Default for ParsedHeader {
    fn default() -> Self {
        Self::new()
    }
}

/// Parses C header files and extracts type/function information.
pub struct HeaderParser {
    /// Include paths to search for headers
    pub include_paths: Vec<PathBuf>,
    /// Cached parsed headers
    parsed_cache: HashMap<String, ParsedHeader>,
}

impl HeaderParser {
    /// Create a new header parser.
    pub fn new() -> Self {
        HeaderParser {
            include_paths: vec![
                PathBuf::from("/usr/include"),
                PathBuf::from("/usr/local/include"),
            ],
            parsed_cache: HashMap::new(),
        }
    }

    /// Add an include path to search.
    pub fn add_include_path(&mut self, path: PathBuf) {
        if !self.include_paths.contains(&path) {
            self.include_paths.push(path);
        }
    }

    /// Parse a header file and extract type information.
    ///
    /// # Arguments
    /// * `header_path` - Path to header file (relative or absolute)
    ///
    /// # Returns
    /// * `Ok(ParsedHeader)` - Parsed header information
    /// * `Err(message)` - If parsing fails
    pub fn parse(&mut self, header_path: &str) -> Result<ParsedHeader, String> {
        // Check cache first
        if let Some(cached) = self.parsed_cache.get(header_path) {
            return Ok(cached.clone());
        }

        // For now, return a basic parsed header with common C library types
        // In a full implementation, this would use bindgen to parse real headers
        let parsed = self.parse_header_internal(header_path)?;

        // Cache result
        self.parsed_cache
            .insert(header_path.to_string(), parsed.clone());

        Ok(parsed)
    }

    /// Internal header parsing logic.
    fn parse_header_internal(&self, header_path: &str) -> Result<ParsedHeader, String> {
        let mut parsed = ParsedHeader::new();

        // Handle standard library headers
        match header_path {
            "stdio.h" | "stdlib.h" => {
                self.add_stdlib_functions(&mut parsed);
            }
            "string.h" => {
                self.add_string_functions(&mut parsed);
            }
            "math.h" => {
                self.add_math_functions(&mut parsed);
            }
            "gtk/gtk.h" => {
                self.add_gtk4_types(&mut parsed);
            }
            "SDL2/SDL.h" => {
                self.add_sdl2_types(&mut parsed);
            }
            _ => {
                // Try to find and parse custom header
                return Err(format!(
                    "Header {} not found or not yet implemented",
                    header_path
                ));
            }
        }

        Ok(parsed)
    }

    /// Add standard library functions to parsed header.
    fn add_stdlib_functions(&self, parsed: &mut ParsedHeader) {
        // malloc
        parsed.functions.insert(
            "malloc".to_string(),
            FunctionSignature::new(
                "malloc".to_string(),
                vec![CType::Long],
                CType::Pointer(Box::new(CType::Void)),
            ),
        );

        // free
        parsed.functions.insert(
            "free".to_string(),
            FunctionSignature::new(
                "free".to_string(),
                vec![CType::Pointer(Box::new(CType::Void))],
                CType::Void,
            ),
        );

        // abs
        parsed.functions.insert(
            "abs".to_string(),
            FunctionSignature::new("abs".to_string(), vec![CType::Int], CType::Int),
        );
    }

    /// Add string functions to parsed header.
    fn add_string_functions(&self, parsed: &mut ParsedHeader) {
        // strlen
        parsed.functions.insert(
            "strlen".to_string(),
            FunctionSignature::new(
                "strlen".to_string(),
                vec![CType::Pointer(Box::new(CType::Char))],
                CType::Long,
            ),
        );

        // strcmp
        parsed.functions.insert(
            "strcmp".to_string(),
            FunctionSignature::new(
                "strcmp".to_string(),
                vec![
                    CType::Pointer(Box::new(CType::Char)),
                    CType::Pointer(Box::new(CType::Char)),
                ],
                CType::Int,
            ),
        );

        // strcpy
        parsed.functions.insert(
            "strcpy".to_string(),
            FunctionSignature::new(
                "strcpy".to_string(),
                vec![
                    CType::Pointer(Box::new(CType::Char)),
                    CType::Pointer(Box::new(CType::Char)),
                ],
                CType::Pointer(Box::new(CType::Char)),
            ),
        );
    }

    /// Add math functions to parsed header.
    fn add_math_functions(&self, parsed: &mut ParsedHeader) {
        // sin
        parsed.functions.insert(
            "sin".to_string(),
            FunctionSignature::new("sin".to_string(), vec![CType::Double], CType::Double),
        );

        // cos
        parsed.functions.insert(
            "cos".to_string(),
            FunctionSignature::new("cos".to_string(), vec![CType::Double], CType::Double),
        );

        // sqrt
        parsed.functions.insert(
            "sqrt".to_string(),
            FunctionSignature::new("sqrt".to_string(), vec![CType::Double], CType::Double),
        );

        // pow
        parsed.functions.insert(
            "pow".to_string(),
            FunctionSignature::new(
                "pow".to_string(),
                vec![CType::Double, CType::Double],
                CType::Double,
            ),
        );
    }

    /// Add GTK4 types to parsed header.
    fn add_gtk4_types(&self, parsed: &mut ParsedHeader) {
        // GTK constants
        parsed
            .constants
            .insert("GTK_WINDOW_TOPLEVEL".to_string(), ConstantValue::Int(0));
        parsed
            .constants
            .insert("GTK_WINDOW_POPUP".to_string(), ConstantValue::Int(1));

        // gtk_init
        parsed.functions.insert(
            "gtk_init".to_string(),
            FunctionSignature::new(
                "gtk_init".to_string(),
                vec![
                    CType::Pointer(Box::new(CType::Int)),
                    CType::Pointer(Box::new(CType::Pointer(Box::new(CType::Pointer(
                        Box::new(CType::Char),
                    ))))),
                ],
                CType::Void,
            ),
        );

        // gtk_window_new
        parsed.functions.insert(
            "gtk_window_new".to_string(),
            FunctionSignature::new(
                "gtk_window_new".to_string(),
                vec![CType::Int],
                CType::Pointer(Box::new(CType::Void)),
            ),
        );

        // gtk_window_set_title
        parsed.functions.insert(
            "gtk_window_set_title".to_string(),
            FunctionSignature::new(
                "gtk_window_set_title".to_string(),
                vec![
                    CType::Pointer(Box::new(CType::Void)),
                    CType::Pointer(Box::new(CType::Char)),
                ],
                CType::Void,
            ),
        );
    }

    /// Add SDL2 types to parsed header.
    fn add_sdl2_types(&self, parsed: &mut ParsedHeader) {
        // SDL_Init
        parsed.functions.insert(
            "SDL_Init".to_string(),
            FunctionSignature::new("SDL_Init".to_string(), vec![CType::UInt], CType::Int),
        );

        // SDL_CreateWindow
        parsed.functions.insert(
            "SDL_CreateWindow".to_string(),
            FunctionSignature::new(
                "SDL_CreateWindow".to_string(),
                vec![
                    CType::Pointer(Box::new(CType::Char)),
                    CType::Int,
                    CType::Int,
                    CType::Int,
                    CType::Int,
                    CType::UInt,
                ],
                CType::Pointer(Box::new(CType::Void)),
            ),
        );

        // SDL_DestroyWindow
        parsed.functions.insert(
            "SDL_DestroyWindow".to_string(),
            FunctionSignature::new(
                "SDL_DestroyWindow".to_string(),
                vec![CType::Pointer(Box::new(CType::Void))],
                CType::Void,
            ),
        );
    }
}

impl Default for HeaderParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_creation() {
        let parser = HeaderParser::new();
        assert!(!parser.include_paths.is_empty());
    }

    #[test]
    fn test_parse_stdlib() {
        let mut parser = HeaderParser::new();
        let result = parser.parse("stdlib.h");
        assert!(result.is_ok());

        let parsed = result.unwrap();
        assert!(parsed.functions.contains_key("malloc"));
        assert!(parsed.functions.contains_key("free"));
        assert!(parsed.functions.contains_key("abs"));
    }

    #[test]
    fn test_parse_string_h() {
        let mut parser = HeaderParser::new();
        let result = parser.parse("string.h");
        assert!(result.is_ok());

        let parsed = result.unwrap();
        assert!(parsed.functions.contains_key("strlen"));
        assert!(parsed.functions.contains_key("strcmp"));
    }

    #[test]
    fn test_parse_math_h() {
        let mut parser = HeaderParser::new();
        let result = parser.parse("math.h");
        assert!(result.is_ok());

        let parsed = result.unwrap();
        assert!(parsed.functions.contains_key("sin"));
        assert!(parsed.functions.contains_key("cos"));
    }

    #[test]
    fn test_caching() {
        let mut parser = HeaderParser::new();
        let result1 = parser.parse("stdlib.h").unwrap();
        let result2 = parser.parse("stdlib.h").unwrap();

        // Results should be identical
        assert_eq!(result1.functions.len(), result2.functions.len());
        assert_eq!(parser.parsed_cache.len(), 1);
    }

    #[test]
    fn test_parse_nonexistent_header() {
        let mut parser = HeaderParser::new();
        let result = parser.parse("nonexistent.h");
        assert!(result.is_err());
    }

    #[test]
    fn test_add_include_path() {
        let mut parser = HeaderParser::new();
        let original_count = parser.include_paths.len();
        parser.add_include_path(PathBuf::from("/custom/include"));
        assert_eq!(parser.include_paths.len(), original_count + 1);
    }
}
