//! Contains the definition of the [`Diagnostic`] struct and related types.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::Span;

/// Implement this trait for a type that can report a diagnostic.
///
/// This trait is typically implemented by structs or enums that encodes the
/// error or warning conditions that can occur during the compilation process.
///
/// The reason for having an intermediate structs/enums instead of emitting
/// [`Diagnostic`] directly is to make the errors and warnings generation more
/// decoupled from the outside context. This will be seen greatly useful when
/// working with the incremental compilation, where the context outside may
/// change between compilation runs, but the structs/enums that implement this
/// trait will remain the same.
pub trait Report<Param> {
    /// The type of the span used to represent the location of the diagnostic.
    type Location;

    /// Creates a diagnostic.
    fn report(&self, parameter: Param) -> Diagnostic<Self::Location>;
}

/// Enumeration of the severity levels of a diagnostic.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
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
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Diagnostic<L> {
    /// The span location where the diagnostic occurred.
    pub span: Option<(Span<L>, Option<String>)>,

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
    pub related: Vec<Related<L>>,
}

/// The related information that is displayed alongside the main [`Diagnostic`].
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Related<L> {
    /// The span location to display the message.
    pub span: Span<L>,

    /// The message to display to the user.
    pub message: String,
}
