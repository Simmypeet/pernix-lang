//! Diagnostics for instance coherence checking.

use pernixc_diagnostic::{Highlight, Rendered, Report, Severity};
use pernixc_qbice::TrackedEngine;
use pernixc_source_file::ByteIndex;
use pernixc_symbol::{
    kind::Kind, name::get_qualified_name, source_map::to_absolute_span,
    span::get_span,
};
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

/// Enumeration of all diagnostics during instance coherence checking.
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
    derive_more::From,
)]
pub enum Diagnostic {
    /// A member from the trait is missing in the instance implementation.
    MissingMember(MissingMember),

    /// An extraneous member that is not part of the trait was found in the
    /// instance.
    ExtraneousMember(ExtraneousMember),

    /// The kind of the instance member does not match the kind of the trait
    /// member.
    MismatchedMemberKind(MismatchedMemberKind),

    /// A trait member is not visible where the instance is implemented.
    TraitMemberInvisible(TraitMemberInvisible),
}

impl Report for Diagnostic {
    async fn report(&self, engine: &TrackedEngine) -> Rendered<ByteIndex> {
        match self {
            Self::MissingMember(diagnostic) => diagnostic.report(engine).await,
            Self::ExtraneousMember(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::MismatchedMemberKind(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::TraitMemberInvisible(diagnostic) => {
                diagnostic.report(engine).await
            }
        }
    }
}

/// A member from the trait is missing in the instance implementation.
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
    Identifiable,
)]
pub struct MissingMember {
    /// The global ID of the instance symbol.
    pub instance_id: Global<pernixc_symbol::SymbolID>,

    /// The global ID of the trait member that is missing.
    pub trait_member_id: Global<pernixc_symbol::SymbolID>,

    /// The name of the missing member.
    pub member_name: Interned<str>,
}

impl Report for MissingMember {
    async fn report(&self, engine: &TrackedEngine) -> Rendered<ByteIndex> {
        let instance_span = engine.get_span(self.instance_id).await;
        let instance_name = engine.get_qualified_name(self.instance_id).await;
        let trait_member_name = self.member_name.as_ref();

        let primary_highlight = if let Some(span) = instance_span {
            let abs_span = engine.to_absolute_span(&span).await;
            Some(
                Highlight::builder()
                    .span(abs_span)
                    .message(format!(
                        "instance `{instance_name}` is missing member \
                         `{trait_member_name}`"
                    ))
                    .build(),
            )
        } else {
            None
        };

        let mut related_highlights = Vec::new();
        if let Some(trait_member_span) =
            engine.get_span(self.trait_member_id).await
        {
            let abs_span = engine.to_absolute_span(&trait_member_span).await;
            related_highlights.push(
                Highlight::builder()
                    .span(abs_span)
                    .message(format!(
                        "trait member `{trait_member_name}` declared here"
                    ))
                    .build(),
            );
        }

        Rendered::builder()
            .severity(Severity::Error)
            .message(format!(
                "trait member `{trait_member_name}` is not implemented in \
                 instance"
            ))
            .maybe_primary_highlight(primary_highlight)
            .related(related_highlights)
            .build()
    }
}

/// An extraneous member that is not part of the trait was found in the
/// instance.
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
    Identifiable,
)]
pub struct ExtraneousMember {
    /// The global ID of the instance member that is extraneous.
    pub instance_member_id: Global<pernixc_symbol::SymbolID>,

    /// The name of the extraneous member.
    pub member_name: Interned<str>,

    /// The global ID of the trait.
    pub trait_id: Global<pernixc_symbol::SymbolID>,
}

impl Report for ExtraneousMember {
    async fn report(&self, engine: &TrackedEngine) -> Rendered<ByteIndex> {
        let instance_member_span =
            engine.get_span(self.instance_member_id).await;
        let member_name = self.member_name.as_ref();
        let trait_name = engine.get_qualified_name(self.trait_id).await;

        let primary_highlight = if let Some(span) = instance_member_span {
            let abs_span = engine.to_absolute_span(&span).await;
            Some(
                Highlight::builder()
                    .span(abs_span)
                    .message(format!(
                        "member `{member_name}` is not part of trait \
                         `{trait_name}`"
                    ))
                    .build(),
            )
        } else {
            None
        };

        let mut related_highlights = Vec::new();
        if let Some(trait_span) = engine.get_span(self.trait_id).await {
            let abs_span = engine.to_absolute_span(&trait_span).await;
            related_highlights.push(
                Highlight::builder()
                    .span(abs_span)
                    .message(format!("trait `{trait_name}` declared here"))
                    .build(),
            );
        }

        Rendered::builder()
            .severity(Severity::Error)
            .message(format!(
                "extraneous member `{member_name}` not found in trait"
            ))
            .maybe_primary_highlight(primary_highlight)
            .related(related_highlights)
            .build()
    }
}

/// The kind of the instance member does not match the kind of the trait member.
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
    Identifiable,
)]
pub struct MismatchedMemberKind {
    /// The global ID of the instance member.
    pub instance_member_id: Global<pernixc_symbol::SymbolID>,

    /// The global ID of the trait member.
    pub trait_member_id: Global<pernixc_symbol::SymbolID>,

    /// The name of the member.
    pub member_name: Interned<str>,

    /// The expected kind (from the trait).
    pub expected_kind: Kind,

    /// The actual kind (from the instance).
    pub actual_kind: Kind,
}

impl Report for MismatchedMemberKind {
    async fn report(&self, engine: &TrackedEngine) -> Rendered<ByteIndex> {
        let instance_member_span =
            engine.get_span(self.instance_member_id).await;
        let member_name = self.member_name.as_ref();
        let expected_kind_str = self.expected_kind.kind_str();
        let actual_kind_str = self.actual_kind.kind_str();

        let primary_highlight = if let Some(span) = instance_member_span {
            let abs_span = engine.to_absolute_span(&span).await;
            Some(
                Highlight::builder()
                    .span(abs_span)
                    .message(format!(
                        "expected {expected_kind_str}, found {actual_kind_str}"
                    ))
                    .build(),
            )
        } else {
            None
        };

        let mut related_highlights = Vec::new();
        if let Some(trait_member_span) =
            engine.get_span(self.trait_member_id).await
        {
            let abs_span = engine.to_absolute_span(&trait_member_span).await;
            related_highlights.push(
                Highlight::builder()
                    .span(abs_span)
                    .message(format!(
                        "trait member `{member_name}` defined here"
                    ))
                    .build(),
            );
        }

        Rendered::builder()
            .severity(Severity::Error)
            .message(format!(
                "mismatched kind for member `{member_name}`: expected \
                 {expected_kind_str}, found {actual_kind_str}"
            ))
            .maybe_primary_highlight(primary_highlight)
            .related(related_highlights)
            .build()
    }
}

/// A trait member is not visible where the instance is implemented.
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
    Identifiable,
)]
pub struct TraitMemberInvisible {
    /// The global ID of the instance symbol.
    pub instance_id: Global<pernixc_symbol::SymbolID>,

    /// The global ID of the trait member that is invisible.
    pub trait_member_id: Global<pernixc_symbol::SymbolID>,
}

impl Report for TraitMemberInvisible {
    async fn report(&self, engine: &TrackedEngine) -> Rendered<ByteIndex> {
        let instance_span = engine.get_span(self.instance_id).await;
        let trait_member_name =
            engine.get_qualified_name(self.trait_member_id).await;

        let primary_highlight = if let Some(span) = instance_span {
            let abs_span = engine.to_absolute_span(&span).await;
            Some(
                Highlight::builder()
                    .span(abs_span)
                    .message(format!(
                        "the trait member `{trait_member_name}` is not \
                         visible here"
                    ))
                    .build(),
            )
        } else {
            None
        };

        let mut related_highlights = Vec::new();
        if let Some(trait_member_span) =
            engine.get_span(self.trait_member_id).await
        {
            let abs_span = engine.to_absolute_span(&trait_member_span).await;
            related_highlights.push(
                Highlight::builder()
                    .span(abs_span)
                    .message(format!(
                        "the trait member `{trait_member_name}` was declared \
                         here"
                    ))
                    .build(),
            );
        }

        Rendered::builder()
            .severity(Severity::Error)
            .message(format!(
                "the trait member `{trait_member_name}` is not visible and \
                 thus cannot be implemented"
            ))
            .maybe_primary_highlight(primary_highlight)
            .related(related_highlights)
            .help_message(
                "to implement an instance for a trait, all of the trait \
                 members must be visible to the instance",
            )
            .build()
    }
}
