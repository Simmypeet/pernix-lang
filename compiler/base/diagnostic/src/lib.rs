//! Contains the definition of the [`Diagnostic`] struct and related types.

use std::future::Future;

use bon::Builder;
use pernixc_qbice::TrackedEngine;
// re-export
pub use pernixc_source_file::ByteIndex;
use pernixc_source_file::Span;
use qbice::{Decode, Encode, Identifiable, StableHash};

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
pub trait Report {
    /// Creates a diagnostic.
    fn report<'s, 'e>(
        &'s self,
        parameter: &'e TrackedEngine,
    ) -> impl Future<Output = Rendered<ByteIndex>> + Send + use<'s, 'e, Self>;
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
    Encode,
    Decode,
    StableHash,
)]
pub enum Severity {
    /// An informational message that does not prevent the program from
    /// compiling.
    Info,

    /// A warning that does not prevent the program from compiling.
    Warning,

    /// An error that prevents the program from compiling.
    Error,
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
    Encode,
    Decode,
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

/// A strucut representing a diagnostic message ready to be displayed to the
/// user.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Builder,
    Identifiable,
)]
pub struct Rendered<L> {
    /// The severity of the diagnostic.
    #[builder(default = Severity::Error)]
    pub severity: Severity,

    /// The span location where the diagnostic occurred.
    pub primary_highlight: Option<Highlight<L>>,

    /// The message to display to the user.
    #[builder(into)]
    pub message: String,

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
