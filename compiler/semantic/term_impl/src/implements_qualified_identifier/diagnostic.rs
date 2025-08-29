use derive_more::From;
use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    kind::get_kind, name::get_qualified_name, parent::get_parent_global,
    source_map::to_absolute_span, span::get_span,
};
use pernixc_target::Global;
use pernixc_term::{display::Display, r#type::Type};

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
    From,
)]
pub enum Diagnostic {
    Resolution(pernixc_resolution::diagnostic::Diagnostic),
    InvalidSymbolForImplements(InvalidSymbolForImplements),
    InvalidTypeForImplements(InvalidTypeForImplements),
    MarkerImplementsNotFinal(MarkerImplementsNotFinal),
    MemberInMarkerImplementationIsNotAllowed(
        MemberInMarkerImplementationIsNotAllowed,
    ),
    TraitMemberNotImplemented(TraitMemberNotImplemented),
    TraitMemberKindMismatch(TraitMemberKindMismatch),
    ExtraneousImplementationMember(ExtraneousImplementationMember),
    InaccessibleTraitMember(InaccessibleTraitMember),
    AdtImplementationCannotBeNegative(AdtImplementationCannotBeNegative),
    AdtImplementationCannotBeFinal(AdtImplementationCannotBeFinal),
}

impl Report<&TrackedEngine> for Diagnostic {
    type Location = ByteIndex;

    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        match self {
            Self::Resolution(diag) => diag.report(parameter).await,
            Self::InvalidSymbolForImplements(diag) => {
                diag.report(parameter).await
            }
            Self::InvalidTypeForImplements(diag) => {
                diag.report(parameter).await
            }
            Self::MarkerImplementsNotFinal(diag) => {
                diag.report(parameter).await
            }
            Self::MemberInMarkerImplementationIsNotAllowed(diag) => {
                diag.report(parameter).await
            }
            Self::TraitMemberNotImplemented(diag) => {
                diag.report(parameter).await
            }
            Self::TraitMemberKindMismatch(diag) => diag.report(parameter).await,
            Self::ExtraneousImplementationMember(diag) => {
                diag.report(parameter).await
            }
            Self::InaccessibleTraitMember(diag) => diag.report(parameter).await,
            Self::AdtImplementationCannotBeNegative(diag) => {
                diag.report(parameter).await
            }
            Self::AdtImplementationCannotBeFinal(diag) => {
                diag.report(parameter).await
            }
        }
    }
}

/// The symbol kinds cannot be implemented.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct InvalidSymbolForImplements {
    /// The qualified identifier span of the resolved symbol.
    pub qualified_identifier_span: RelativeSpan,

    /// The resolved symbol ID that's not valid for implements.
    pub symbol_id: Global<pernixc_symbol::ID>,
}

impl Report<&TrackedEngine> for InvalidSymbolForImplements {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let qualified_name = engine.get_qualified_name(self.symbol_id).await;
        let kind = engine.get_kind(self.symbol_id).await;

        let span =
            engine.to_absolute_span(&self.qualified_identifier_span).await;

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("the symbol cannot be implemented")
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "the symbol `{} {qualified_name}` cannot be \
                         implemented",
                        kind.kind_str()
                    ))
                    .span(span)
                    .build(),
            )
            .help_message(
                "only `trait`, `marker`, `enum`, or `struct` can be \
                 implemented"
                    .to_string(),
            )
            .build()
    }
}

/// The type cannot be implemented.
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
)]
pub struct InvalidTypeForImplements {
    /// The qualified identifier span of the resolved type.
    pub qualified_identifier_span: RelativeSpan,

    /// The resolved type that's not valid for implements.
    pub r#type: Type,
}

impl Report<&TrackedEngine> for InvalidTypeForImplements {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let span =
            engine.to_absolute_span(&self.qualified_identifier_span).await;

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("the type cannot be implemented")
            .primary_highlight(
                Highlight::builder()
                    .message({
                        let mut buf = String::new();
                        buf.push_str("the type `");
                        self.r#type
                            .write_async(engine, &mut buf)
                            .await
                            .unwrap();
                        buf.push_str("` cannot be implemented");

                        buf
                    })
                    .span(span)
                    .build(),
            )
            .help_message(
                "only `enum`, or `struct` can be implemented".to_string(),
            )
            .build()
    }
}

/// The `marker implements` is not `final`
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
)]
pub struct MarkerImplementsNotFinal {
    /// The qualified identifier span of the resolved marker.
    pub qualified_identifier_span: RelativeSpan,
}

impl Report<&TrackedEngine> for MarkerImplementsNotFinal {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let span =
            engine.to_absolute_span(&self.qualified_identifier_span).await;

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("the marker implements is not final")
            .primary_highlight(Highlight::builder().span(span).build())
            .help_message(
                "add the `final` keyword to the `implements`".to_string(),
            )
            .build()
    }
}

/// The marker `implements` can not have any members.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct MemberInMarkerImplementationIsNotAllowed {
    /// The member ID defined in the marker `implements`.
    pub implements_member_id: Global<pernixc_symbol::ID>,
}

impl Report<&TrackedEngine> for MemberInMarkerImplementationIsNotAllowed {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let qualified_name =
            engine.get_qualified_name(self.implements_member_id).await;

        let parent =
            engine.get_parent_global(self.implements_member_id).await.unwrap();

        let parent_span = match engine.get_span(parent).await {
            Some(span) => Some(engine.to_absolute_span(&span).await),
            None => None,
        };

        let span = match engine.get_span(self.implements_member_id).await {
            Some(span) => Some(engine.to_absolute_span(&span).await),
            None => None,
        };

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("member in marker implementation is not allowed")
            .maybe_primary_highlight(span.map(|x| {
                Highlight::builder()
                    .span(x)
                    .message(format!(
                        "`{qualified_name}` cannot be declared in this \
                         `implements`",
                    ))
                    .build()
            }))
            .help_message(
                "marker implementations cannot have members".to_string(),
            )
            .related(
                parent_span
                    .map(|x| {
                        Highlight::builder()
                            .span(x)
                            .message(
                                "this is a marker `implements`".to_string(),
                            )
                            .build()
                    })
                    .into_iter()
                    .collect(),
            )
            .build()
    }
}

/// A required trait member is not implemented.
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
)]
pub struct TraitMemberNotImplemented {
    /// The member IDs in the trait that are not implemented.
    pub unimplemented_trait_member_ids: Vec<Global<pernixc_symbol::ID>>,

    /// The implementation ID where the members should be implemented.
    pub implementation_id: Global<pernixc_symbol::ID>,
}

impl Report<&TrackedEngine> for TraitMemberNotImplemented {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let implementation_span =
            match engine.get_span(self.implementation_id).await {
                Some(span) => Some(engine.to_absolute_span(&span).await),
                None => None,
            };

        // Collect information about all unimplemented members
        let mut member_names = Vec::new();
        let mut related_highlights = Vec::new();

        for &trait_member_id in &self.unimplemented_trait_member_ids {
            let member_name = engine.get_qualified_name(trait_member_id).await;
            let member_kind = engine.get_kind(trait_member_id).await;
            member_names
                .push(format!("{}: `{member_name}`", member_kind.kind_str()));

            if let Some(span) = engine.get_span(trait_member_id).await {
                let trait_member_span = engine.to_absolute_span(&span).await;
                related_highlights.push(
                    Highlight::builder()
                        .span(trait_member_span)
                        .message(format!(
                            "`{} {member_name}` declared here",
                            member_kind.kind_str()
                        ))
                        .build(),
                );
            }
        }

        let message = if self.unimplemented_trait_member_ids.len() == 1 {
            "trait member not implemented".to_string()
        } else {
            "trait members not implemented".to_string()
        };

        let primary_message = if self.unimplemented_trait_member_ids.len() == 1
        {
            format!("missing implementation for {}", member_names[0])
        } else {
            format!("missing implementations for: {}", member_names.join(", "))
        };

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message(message)
            .maybe_primary_highlight(implementation_span.map(|x| {
                Highlight::builder().span(x).message(primary_message).build()
            }))
            .related(related_highlights)
            .build()
    }
}
/// A trait member is implemented with wrong kind (e.g., implementing function
/// as type).
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct TraitMemberKindMismatch {
    /// The trait member ID.
    pub trait_member_id: Global<pernixc_symbol::ID>,

    /// The implementation member ID with wrong kind.
    pub implementation_member_id: Global<pernixc_symbol::ID>,
}

impl Report<&TrackedEngine> for TraitMemberKindMismatch {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let member_name = engine.get_qualified_name(self.trait_member_id).await;
        let trait_kind = engine.get_kind(self.trait_member_id).await;
        let impl_kind = engine.get_kind(self.implementation_member_id).await;

        let impl_span =
            match engine.get_span(self.implementation_member_id).await {
                Some(span) => Some(engine.to_absolute_span(&span).await),
                None => None,
            };

        let trait_span = match engine.get_span(self.trait_member_id).await {
            Some(span) => Some(engine.to_absolute_span(&span).await),
            None => None,
        };

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("trait member kind mismatch")
            .maybe_primary_highlight(impl_span.map(|x| {
                Highlight::builder()
                    .span(x)
                    .message(format!(
                        "expected {}, found {}",
                        trait_kind.kind_str(),
                        impl_kind.kind_str()
                    ))
                    .build()
            }))
            .related(
                trait_span
                    .map(|x| {
                        Highlight::builder()
                            .span(x)
                            .message(format!(
                                "`{} {member_name}` declared here",
                                trait_kind.kind_str()
                            ))
                            .build()
                    })
                    .into_iter()
                    .collect(),
            )
            .build()
    }
}

/// An implementation member that doesn't correspond to any trait member.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct ExtraneousImplementationMember {
    /// The implementation member ID that doesn't correspond to any trait
    /// member.
    pub implementation_member_id: Global<pernixc_symbol::ID>,
}

impl Report<&TrackedEngine> for ExtraneousImplementationMember {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let member_name =
            engine.get_qualified_name(self.implementation_member_id).await;
        let member_kind = engine.get_kind(self.implementation_member_id).await;

        let span = match engine.get_span(self.implementation_member_id).await {
            Some(span) => Some(engine.to_absolute_span(&span).await),
            None => None,
        };

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("extraneous implementation member")
            .maybe_primary_highlight(span.map(|x| {
                Highlight::builder()
                    .span(x)
                    .message(format!(
                        "{} `{member_name}` is not a member of the trait",
                        member_kind.kind_str()
                    ))
                    .build()
            }))
            .help_message(
                "remove this member or add it to the trait definition"
                    .to_string(),
            )
            .build()
    }
}

/// A trait member that is not accessible from the implementation site.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct InaccessibleTraitMember {
    /// The trait member ID that is not accessible.
    pub trait_member_id: Global<pernixc_symbol::ID>,

    /// The implementation member ID that is trying to implement the
    /// inaccessible trait member.
    pub implementation_member_id: Global<pernixc_symbol::ID>,
}

impl Report<&TrackedEngine> for InaccessibleTraitMember {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let member_name = engine.get_qualified_name(self.trait_member_id).await;
        let member_kind = engine.get_kind(self.trait_member_id).await;

        let impl_member_span =
            match engine.get_span(self.implementation_member_id).await {
                Some(span) => Some(engine.to_absolute_span(&span).await),
                None => None,
            };

        let trait_member_span =
            match engine.get_span(self.trait_member_id).await {
                Some(span) => Some(engine.to_absolute_span(&span).await),
                None => None,
            };

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("trait member is not accessible")
            .maybe_primary_highlight(impl_member_span.map(|x| {
                Highlight::builder()
                    .span(x)
                    .message(format!(
                        "cannot implement inaccessible {} `{member_name}`",
                        member_kind.kind_str()
                    ))
                    .build()
            }))
            .related(
                trait_member_span
                    .map(|x| {
                        Highlight::builder()
                            .span(x)
                            .message(format!(
                                "{} `{member_name}` declared here",
                                member_kind.kind_str()
                            ))
                            .build()
                    })
                    .into_iter()
                    .collect(),
            )
            .help_message(
                "make the trait member publicly accessible or move the \
                 implementation to a scope where it is accessible"
                    .to_string(),
            )
            .build()
    }
}

/// ADT implementation cannot be negative.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct AdtImplementationCannotBeNegative {
    /// The implementation ID that is negative.
    pub implementation_id: Global<pernixc_symbol::ID>,
}

impl Report<&TrackedEngine> for AdtImplementationCannotBeNegative {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let span = match engine.get_span(self.implementation_id).await {
            Some(span) => Some(engine.to_absolute_span(&span).await),
            None => None,
        };

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("cannot `delete` an `implements` on `struct` or `enum`")
            .maybe_primary_highlight(span.map(|x| {
                Highlight::builder()
                    .span(x)
                    .message("`implements` must have a body")
                    .build()
            }))
            .help_message(
                "remove the `delete` keyword from the implementation, \
                 negative `implements` is only allowed on `trait` or `marker`"
                    .to_string(),
            )
            .build()
    }
}

/// ADT implementation cannot be final.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct AdtImplementationCannotBeFinal {
    /// The implementation ID that is final.
    pub implementation_id: Global<pernixc_symbol::ID>,
}

impl Report<&TrackedEngine> for AdtImplementationCannotBeFinal {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let span = match engine.get_span(self.implementation_id).await {
            Some(span) => Some(engine.to_absolute_span(&span).await),
            None => None,
        };

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("`implements` on `struct` or `enum` cannot be final")
            .maybe_primary_highlight(span.map(|x| {
                Highlight::builder()
                    .span(x)
                    .message(
                        "`implements` on `struct` or `enum` cannot be final",
                    )
                    .build()
            }))
            .help_message(
                "remove the `final` keyword from the `implements`, `final \
                 implements` is only allowed on `trait` or `marker`"
                    .to_string(),
            )
            .build()
    }
}
