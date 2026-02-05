//! Error handling and reporting with source locations

use std::fmt;

/// Source code location (line and column)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceLoc {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl SourceLoc {
    /// Create a new source location
    pub fn new(line: usize, col: usize) -> Self {
        SourceLoc { line, col }
    }

    /// Create a location at the beginning of a file
    pub fn start() -> Self {
        SourceLoc { line: 1, col: 1 }
    }
}

/// Runtime error with optional source location
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeError {
    pub message: String,
    pub location: Option<SourceLoc>,
    pub context: Option<String>,
}

impl RuntimeError {
    /// Create a new runtime error
    pub fn new(message: String) -> Self {
        RuntimeError {
            message,
            location: None,
            context: None,
        }
    }

    /// Add location information
    pub fn with_location(mut self, location: SourceLoc) -> Self {
        self.location = Some(location);
        self
    }

    /// Add context information
    pub fn with_context(mut self, context: String) -> Self {
        self.context = Some(context);
        self
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.location {
            Some(loc) => write!(f, "Error at {}: {}", loc, self.message)?,
            None => write!(f, "Error: {}", self.message)?,
        }

        if let Some(ref ctx) = self.context {
            write!(f, "\n  Context: {}", ctx)?;
        }

        Ok(())
    }
}

impl std::error::Error for RuntimeError {}

/// Type mismatch error
pub fn type_mismatch(expected: &str, got: &str) -> String {
    format!("Type error: expected {}, got {}", expected, got)
}

/// Arity mismatch error
pub fn arity_mismatch(expected: usize, got: usize) -> String {
    format!(
        "Argument error: expected {} argument{}, got {}",
        expected,
        if expected == 1 { "" } else { "s" },
        got
    )
}

/// Index out of bounds error
pub fn index_out_of_bounds(index: isize, len: usize) -> String {
    format!(
        "Index error: index {} out of bounds for length {}",
        index, len
    )
}

/// Undefined variable error
pub fn undefined_variable(name: &str) -> String {
    format!("Reference error: undefined variable '{}'", name)
}

/// Division by zero error
pub fn division_by_zero() -> String {
    "Arithmetic error: division by zero".to_string()
}
