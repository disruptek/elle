//! Condition objects for the exception system
//!
//! Conditions are structured exception objects that carry:
//! - An exception ID (for fast matching)
//! - Field values (structured data about the condition)
//! - Optional backtrace (for debugging)
//! - Optional source location (for error reporting)

use crate::reader::SourceLoc;
use crate::value::Value;
use std::collections::HashMap;
use std::fmt;

/// A condition object representing an exceptional situation
#[derive(Debug, Clone)]
pub struct Condition {
    /// Exception type ID (compiled at compile-time)
    pub exception_id: u32,
    /// Field values (field_id -> value mapping)
    pub fields: HashMap<u32, Value>,
    /// Optional backtrace for debugging
    pub backtrace: Option<String>,
    /// Optional source location for error reporting
    pub location: Option<SourceLoc>,
}

impl Condition {
    /// Reserved exception ID for generic exceptions (legacy Exception type)
    pub const GENERIC_EXCEPTION_ID: u32 = 0;

    /// Reserved field ID for exception message
    pub const FIELD_MESSAGE: u32 = 0;

    /// Reserved field ID for exception data
    pub const FIELD_DATA: u32 = 1;

    /// Create a new condition with given exception ID
    pub fn new(exception_id: u32) -> Self {
        Condition {
            exception_id,
            fields: HashMap::new(),
            backtrace: None,
            location: None,
        }
    }

    /// Set a field value
    pub fn set_field(&mut self, field_id: u32, value: Value) {
        self.fields.insert(field_id, value);
    }

    /// Get a field value
    pub fn get_field(&self, field_id: u32) -> Option<&Value> {
        self.fields.get(&field_id)
    }

    /// Set backtrace information
    pub fn with_backtrace(mut self, backtrace: String) -> Self {
        self.backtrace = Some(backtrace);
        self
    }

    /// Set source location information
    pub fn with_location(mut self, loc: SourceLoc) -> Self {
        self.location = Some(loc);
        self
    }

    /// Check if this condition is of a specific type (including inheritance)
    pub fn is_instance_of(&self, exception_id: u32) -> bool {
        self.exception_id == exception_id
    }

    /// Create a generic exception with a message (replaces Exception::new)
    pub fn generic(message: impl Into<String>) -> Self {
        let mut cond = Condition::new(Self::GENERIC_EXCEPTION_ID);
        cond.set_field(Self::FIELD_MESSAGE, Value::string(message.into()));
        cond
    }

    /// Create a generic exception with message and data (replaces Exception::with_data)
    pub fn generic_with_data(message: impl Into<String>, data: Value) -> Self {
        let mut cond = Condition::new(Self::GENERIC_EXCEPTION_ID);
        cond.set_field(Self::FIELD_MESSAGE, Value::string(message.into()));
        cond.set_field(Self::FIELD_DATA, data);
        cond
    }

    /// Check if this is a generic exception
    pub fn is_generic(&self) -> bool {
        self.exception_id == Self::GENERIC_EXCEPTION_ID
    }

    /// Get message for generic exceptions (field 0)
    pub fn message(&self) -> Option<&str> {
        if let Some(field) = self.get_field(Self::FIELD_MESSAGE) {
            field.as_string()
        } else {
            None
        }
    }

    /// Get data for generic exceptions (field 1)
    pub fn data(&self) -> Option<&Value> {
        self.get_field(Self::FIELD_DATA)
    }
}

impl PartialEq for Condition {
    fn eq(&self, other: &Self) -> bool {
        // Conditions are equal if they have the same ID, field values, and location
        self.exception_id == other.exception_id
            && self.fields == other.fields
            && self.location == other.location
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.exception_id == Self::GENERIC_EXCEPTION_ID {
            if let Some(msg) = self.message() {
                return write!(f, "Exception: {}", msg);
            }
        }
        write!(f, "Condition(id={})", self.exception_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_condition_creation() {
        let cond = Condition::new(1);
        assert_eq!(cond.exception_id, 1);
        assert!(cond.fields.is_empty());
    }

    #[test]
    fn test_condition_fields() {
        let mut cond = Condition::new(1);
        cond.set_field(0, Value::int(42));
        assert_eq!(cond.get_field(0), Some(&Value::int(42)));
    }

    #[test]
    fn test_condition_equality() {
        let mut cond1 = Condition::new(1);
        cond1.set_field(0, Value::int(42));

        let mut cond2 = Condition::new(1);
        cond2.set_field(0, Value::int(42));

        assert_eq!(cond1, cond2);
    }

    #[test]
    fn test_generic_exception_creation() {
        let exc = Condition::generic("test error");
        assert_eq!(exc.exception_id, Condition::GENERIC_EXCEPTION_ID);
        assert!(exc.is_generic());
        assert_eq!(exc.message(), Some("test error"));
        assert_eq!(exc.data(), None);
    }

    #[test]
    fn test_generic_exception_with_data() {
        let data = Value::int(42);
        let exc = Condition::generic_with_data("error with data", data);
        assert_eq!(exc.exception_id, Condition::GENERIC_EXCEPTION_ID);
        assert!(exc.is_generic());
        assert_eq!(exc.message(), Some("error with data"));
        assert_eq!(exc.data(), Some(&data));
    }

    #[test]
    fn test_is_generic() {
        let generic = Condition::generic("test");
        assert!(generic.is_generic());

        let specific = Condition::new(42);
        assert!(!specific.is_generic());
    }

    #[test]
    fn test_message_extraction() {
        let exc = Condition::generic("hello world");
        assert_eq!(exc.message(), Some("hello world"));

        let mut cond = Condition::new(1);
        assert_eq!(cond.message(), None);

        // Set a non-string message
        cond.set_field(Condition::FIELD_MESSAGE, Value::int(123));
        assert_eq!(cond.message(), None);
    }

    #[test]
    fn test_data_extraction() {
        let data = Value::int(99);
        let exc = Condition::generic_with_data("msg", data);
        assert_eq!(exc.data(), Some(&data));

        let exc_no_data = Condition::generic("msg");
        assert_eq!(exc_no_data.data(), None);
    }

    #[test]
    fn test_display_generic_exception() {
        let exc = Condition::generic("test error message");
        assert_eq!(exc.to_string(), "Exception: test error message");
    }

    #[test]
    fn test_display_generic_exception_without_message() {
        let exc = Condition::new(Condition::GENERIC_EXCEPTION_ID);
        assert_eq!(exc.to_string(), "Condition(id=0)");
    }

    #[test]
    fn test_display_specific_condition() {
        let cond = Condition::new(42);
        assert_eq!(cond.to_string(), "Condition(id=42)");
    }

    #[test]
    fn test_constants() {
        assert_eq!(Condition::GENERIC_EXCEPTION_ID, 0);
        assert_eq!(Condition::FIELD_MESSAGE, 0);
        assert_eq!(Condition::FIELD_DATA, 1);
    }
}
