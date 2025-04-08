//! Contains the definition of the [`Diagnostic`] struct and related types.

/// Implement this trait for a type that can report a diagnostic.
///
/// This trait is typically implemented by error and warning types.
pub trait Report<Param> {
    /// The type of the span used to represent the location of the diagnostic.
    type Span;

    /// Creates a diagnostic.
    fn report(&self, parameter: Param) -> Diagnostic<Self::Span>;
}

/// Enumeration of the severity levels of a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Severity {
    /// An error that prevents the program from compiling.
    Error,

    /// A warning that does not prevent the program from compiling.
    Warning,

    /// An informational message that does not prevent the program from
    /// compiling.
    Info,
}

/// A struct containing all the information required to display the diagnostic
/// to the user.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Diagnostic<S> {
    /// The span location where the diagnostic occurred.
    pub span: S,

    /// The message to display to the user.
    pub message: String,

    /// The label to display to the user. This is typically a short message
    /// that highlights the problem.
    pub label: Option<String>,

    /// The severity of the diagnostic.
    pub severity: Severity,

    /// The optional help message to display to the user. This will be
    /// displayed alongside the main message.
    pub help_message: Option<String>,

    /// List of related useful information to display to the user.
    ///
    /// For example, for unimplemented methods, this could be a list of
    /// methods that need to be implemented.
    pub related: Vec<Related<S>>,
}

/// The related information that is displayed alongside the main [`Diagnostic`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Related<S> {
    /// The span location to display the message.
    pub span: S,

    /// The message to display to the user.
    pub message: String,
}
