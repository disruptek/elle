use crate::reader::SourceLoc;

#[derive(Debug, Clone)]
pub struct LocatedError {
    pub message: String,
    pub loc: Option<SourceLoc>,
    pub context: Option<String>,
}

impl LocatedError {
    pub fn new(message: String, loc: Option<SourceLoc>) -> Self {
        LocatedError {
            message,
            loc,
            context: None,
        }
    }

    pub fn without_loc(message: String) -> Self {
        LocatedError {
            message,
            loc: None,
            context: None,
        }
    }

    pub fn with_context(mut self, context: String) -> Self {
        self.context = Some(context);
        self
    }

    pub fn format_message(&self) -> String {
        let mut msg = match self.loc {
            Some(loc) => format!("{}:{}: {}", loc.line, loc.col, self.message),
            None => self.message.clone(),
        };

        if let Some(ctx) = &self.context {
            msg.push_str(&format!("\n  Context: {}", ctx));
        }

        msg
    }
}

impl std::fmt::Display for LocatedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format_message())
    }
}

impl From<String> for LocatedError {
    fn from(message: String) -> Self {
        LocatedError::without_loc(message)
    }
}

impl From<&str> for LocatedError {
    fn from(message: &str) -> Self {
        LocatedError::without_loc(message.to_string())
    }
}
