//! Contains the definition of the [`Diagnostic`] struct and related types.

use std::future::Future;

use bon::Builder;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::Span;
use pernixc_stable_hash::StableHash;

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
    fn report(
        &self,
        parameter: Param,
    ) -> impl Future<Output = Diagnostic<Self::Location>> + Send;
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
    StableHash,
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

/// Represents a region got displayed to the user with additional information
/// supplied.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_new::new,
    Builder,
)]
pub struct Highlight<L> {
    /// Represents a region in the source code involved in the diagnostic
    /// displaying
    pub span: Span<L>,

    /// The additional message to display at the highlighted region.
    #[builder(into)]
    pub message: Option<String>,
}

/// A struct containing all the information required to display the diagnostic
/// to the user.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    Builder,
)]
pub struct Diagnostic<L> {
    /// The span location where the diagnostic occurred.
    pub primary_highlight: Option<Highlight<L>>,

    /// The message to display to the user.
    #[builder(into)]
    pub message: String,

    /// The severity of the diagnostic.
    #[builder(default = Severity::Error)]
    pub severity: Severity,

    /// The optional help message to display to the user. This will be
    /// displayed alongside the main message.
    #[builder(into)]
    pub help_message: Option<String>,

    /// List of related useful information to display to the user.
    ///
    /// For example, for unimplemented methods, this could be a list of
    /// methods that need to be implemented.
    #[builder(default = Vec::new())]
    pub related: Vec<Highlight<L>>,
}
