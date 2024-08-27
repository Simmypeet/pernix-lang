//! Contains the definition of the [`Diagnostic`] struct and related types.

use crate::{
    log::{Message, Severity, SourceCodeDisplay},
    source_file::Span,
};

/// Implement this trait for a type that can report a diagnostic.
///
/// This trait is typically implemented by error and warning types.
pub trait Report<Param> {
    /// The error type that can be returned when creating a diagnostic.
    type Error;

    /// Creates a diagnostic.
    ///
    /// # Errors
    ///
    /// Returns an error if the diagnostic cannot be created.
    fn report(&self, parameter: Param) -> Result<Diagnostic, Self::Error>;
}

/// A struct containing all the information required to display the diagnostic
/// to the user.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Diagnostic {
    /// The span location where the diagnostic occurred.
    pub span: Span,

    /// The message to display to the user.
    pub message: String,

    /// The severity of the diagnostic.
    pub severity: Severity,

    /// The optional help message to display to the user. This will be
    /// displayed alongside the main message.
    pub help_message: Option<String>,

    /// List of related useful information to display to the user.
    ///
    /// For example, for unimplemented methods, this could be a list of
    /// methods that need to be implemented.
    pub related: Vec<Related>,
}

/// The related information that is displayed alongside the main [`Diagnostic`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Related {
    /// The span location to display the message.
    pub span: Span,

    /// The message to display to the user.
    pub message: String,
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(self.severity, &self.message),
            SourceCodeDisplay::new(&self.span, self.help_message.as_deref())
        )?;
        for related in &self.related {
            write!(
                f,
                "\n{}",
                SourceCodeDisplay::new(&related.span, Some(&related.message))
            )?;
        }

        Ok(())
    }
}
