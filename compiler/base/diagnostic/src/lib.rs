//! Contains the definition of the [`Diagnostic`] struct and related types.

use std::future::Future;

use bon::{Builder, bon};
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

impl<L> Highlight<L> {
    /// Retrieves the span of the highlight.
    #[must_use]
    pub const fn span(&self) -> &Span<L> { &self.span }

    /// Retrieves the message of the highlight, if any.
    #[must_use]
    pub fn message(&self) -> Option<&str> { self.message.as_deref() }
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

    /// The primary highlight of the diagnostic, which is the most important
    /// region to be highlighted to the user.
    group: Group<L>,

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
            group: Group { primary_highlight, message, help_message, related },
            notes,
        }
    }
}

impl<L> Rendered<L> {
    /// Retrieves the severity of the diagnostic.
    #[must_use]
    pub const fn severity(&self) -> Severity { self.severity }

    /// Retrieves the primary highlight of the diagnostic, if any.
    #[must_use]
    pub const fn primary_highlight(&self) -> Option<&Highlight<L>> {
        self.group.primary_highlight.as_ref()
    }

    /// Retrieves the message of the diagnostic.
    #[must_use]
    pub fn message(&self) -> &str { &self.group.message }

    /// Retrieves the help message of the diagnostic, if any.
    #[must_use]
    pub fn help_message(&self) -> Option<&str> {
        self.group.help_message.as_deref()
    }

    /// Retrieves the related highlights of the diagnostic.
    #[must_use]
    pub fn related(&self) -> &[Highlight<L>] { &self.group.related }

    /// Retrieves the related notes of the diagnostic.
    #[must_use]
    pub fn notes(&self) -> &[Note<L>] { &self.notes }

    /// Retrieves the group of the diagnostic, which contains the primary
    /// highlight, the message, the help message, and the related
    /// highlights.
    #[must_use]
    pub const fn group(&self) -> &Group<L> { &self.group }
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
    Identifiable,
)]
pub struct Note<L> {
    group: Group<L>,
}

#[bon]
impl<L> Note<L> {
    /// Creates a builder for constructing a [`Rendered`] diagnostic.
    #[builder(finish_fn = build)]
    pub const fn builder(
        primary_highlight: Option<Highlight<L>>,
        #[builder(into)] message: String,
        #[builder(into)] help_message: Option<String>,
        #[builder(default = Vec::new())] related: Vec<Highlight<L>>,
    ) -> Self {
        Self {
            group: Group { primary_highlight, message, help_message, related },
        }
    }
}

impl<L> Note<L> {
    /// Retrieves the primary highlight of the note, if any.
    #[must_use]
    pub const fn primary_highlight(&self) -> Option<&Highlight<L>> {
        self.group.primary_highlight.as_ref()
    }

    /// Retrieves the message of the note.
    #[must_use]
    pub fn message(&self) -> &str { &self.group.message }

    /// Retrieves the help message of the note, if any.
    #[must_use]
    pub fn help_message(&self) -> Option<&str> {
        self.group.help_message.as_deref()
    }

    /// Retrieves the related highlights of the note.
    #[must_use]
    pub fn related(&self) -> &[Highlight<L>] { &self.group.related }

    /// Retrieves the group of the note, which contains the primary highlight,
    /// the message, the help message, and the related highlights.
    #[must_use]
    pub const fn group(&self) -> &Group<L> { &self.group }
}

/// Represents a group of diagnostics that are related to each other and should
/// be displayed together.
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
)]
pub struct Group<L> {
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
}

impl<L> Group<L> {
    /// Retrieves the primary highlight of the group, if any.
    #[must_use]
    pub const fn primary_highlight(&self) -> Option<&Highlight<L>> {
        self.primary_highlight.as_ref()
    }

    /// Retrieves the message of the group.
    #[must_use]
    pub fn message(&self) -> &str { &self.message }

    /// Retrieves the help message of the group, if any.
    #[must_use]
    pub fn help_message(&self) -> Option<&str> { self.help_message.as_deref() }

    /// Retrieves the related highlights of the group.
    #[must_use]
    pub fn related(&self) -> &[Highlight<L>] { &self.related }
}
