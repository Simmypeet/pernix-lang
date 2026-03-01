//! Contains the definition of the [`Diagnostic`] struct and related types.

use std::future::Future;

use bon::{Builder, bon, builder};
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
    span: Span<L>,

    /// The additional message to display at the highlighted region.
    #[builder(into)]
    message: Option<String>,
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
    Identifiable,
)]
pub struct Rendered<L> {
    /// The severity of the diagnostic.
    severity: Severity,

    /// The span location where the diagnostic occurred.
    primary_highlight: Option<Highlight<L>>,

    /// The message to display to the user.
    message: String,

    /// The optional help message to display to the user. This will be
    /// displayed alongside the main message.
    help_message: Option<String>,

    /// List of related useful information to display to the user.
    ///
    /// For example, for unimplemented methods, this could be a list of
    /// methods that need to be implemented.
    related: Vec<Highlight<L>>,

    /// List of related notes to display to the user.
    ///
    /// Each of these notes will be rendered as a separate diagnostic, but
    /// they are still related to the main diagnostic.
    notes: Vec<Note<L>>,
}

#[bon]
impl<L> Rendered<L> {
    /// Creates a builder for constructing a [`Rendered`] diagnostic.
    #[builder(finish_fn = build)]
    pub const fn builder(
        #[builder(default = Severity::Error)] severity: Severity,
        primary_highlight: Option<Highlight<L>>,
        #[builder(into)] message: String,
        #[builder(into)] help_message: Option<String>,
        #[builder(default = Vec::new())] related: Vec<Highlight<L>>,
        #[builder(default = Vec::new())] notes: Vec<Note<L>>,
    ) -> Self {
        Self {
            severity,
            primary_highlight,
            message,
            help_message,
            related,
            notes,
        }
    }
}

/// A struct used to attaching additional information to the main [`Rendered`]
/// diagnostic.
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
pub struct Note<L> {
    /// The span location where the diagnostic occurred.
    primary_highlight: Option<Highlight<L>>,

    /// The message to display to the user.
    #[builder(into)]
    message: String,

    /// The optional help message to display to the user. This will be
    /// displayed alongside the main message.
    #[builder(into)]
    help_message: Option<String>,

    /// List of related useful information to display to the user.
    ///
    /// For example, for unimplemented methods, this could be a list of
    /// methods that need to be implemented.
    #[builder(default = Vec::new())]
    related: Vec<Highlight<L>>,
}
