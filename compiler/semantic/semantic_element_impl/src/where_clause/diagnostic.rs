//! Contains the diagnostic information related to building the where clause.

use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_resolution::diagnostic::{
    self as resolution_diagnostic, ForallLifetimeRedefinition,
};
use pernixc_source_file::{ByteIndex, get_source_file_path};
use pernixc_symbol::{
    kind::get_kind, name::get_qualified_name, source_map::to_absolute_span,
};
use pernixc_target::Global;
use pernixc_term::{lifetime, r#type::Type};
use qbice::{Decode, Encode, Identifiable, StableHash};

/// Enumeration of all diagnostics that can be reported during the building of
/// where clause
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
    derive_more::From,
)]
pub enum Diagnostic {
    Resolution(pernixc_resolution::diagnostic::Diagnostic),
    ForallLifetimeRedefinition(ForallLifetimeRedefinition),
    UnexpectedSymbolInPredicate(UnexpectedSymbolInPredicate),
    UnexpectedTypeEqualityPredicate(UnexpectedTypeEqualityPredicate),
    ForallLifetimeIsNotAllowedInOutlivesPredicate(
        ForallLifetimeIsNotAllowedInOutlivesPredicate,
    ),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        match self {
            Self::Resolution(d) => d.report(engine).await,
            Self::ForallLifetimeRedefinition(d) => d.report(engine).await,
            Self::UnexpectedSymbolInPredicate(d) => d.report(engine).await,
            Self::UnexpectedTypeEqualityPredicate(d) => d.report(engine).await,
            Self::ForallLifetimeIsNotAllowedInOutlivesPredicate(d) => {
                d.report(engine).await
            }
        }
    }
}

/// The kinds of the predicate.
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
    Encode,
    Decode,
)]
#[allow(missing_docs)]
pub enum PredicateKind {
    Trait,
    Marker,
    TypeBound,
}

/// The unexpected symbol was found in the predicate.
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
)]
pub struct UnexpectedSymbolInPredicate {
    /// The kind of the predicate that found the unexpected symbol.
    pub predicate_kind: PredicateKind,

    /// The ID of the found symbol.
    pub found_id: Global<pernixc_symbol::ID>,

    /// The span of the qualified identifier of the found symbol.
    pub qualified_identifier_span: RelativeSpan,
}

impl Report for UnexpectedSymbolInPredicate {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let found_symbol_qualified_name =
            engine.get_qualified_name(self.found_id).await;
        let symbol_kind = engine.get_kind(self.found_id).await;

        let expected = match self.predicate_kind {
            PredicateKind::Trait => "trait",
            PredicateKind::Marker => "marker",
            PredicateKind::TypeBound => "either a trait or marker",
        };

        pernixc_diagnostic::Rendered {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.qualified_identifier_span).await,
                Some(format!(
                    "expected {expected} symbol but found `{} \
                     {found_symbol_qualified_name}`",
                    symbol_kind.kind_str()
                )),
            )),
            message: format!(
                "unexpected symbol in the {} predicate",
                match self.predicate_kind {
                    PredicateKind::Trait => "trait",
                    PredicateKind::Marker => "marker",
                    PredicateKind::TypeBound => "type bound",
                },
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The left-hand side of the type equality predicate must be a trait type.
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
)]
pub struct UnexpectedTypeEqualityPredicate {
    /// The span of the invalid left-hand side type.
    pub invalid_lhs_type_span: RelativeSpan,

    /// The span of the type equality predicate.
    pub found_type: Type,
}

impl Report for UnexpectedTypeEqualityPredicate {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        pernixc_diagnostic::Rendered {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.invalid_lhs_type_span).await,
                None,
            )),
            message: "the left-hand side of the type equality predicate must \
                      be a trait associated type"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Forall lifetimes cannot appear in the outlives predicate.
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
)]
pub struct ForallLifetimeIsNotAllowedInOutlivesPredicate {
    /// The span of the term that contains the forall lifetime.
    pub forall_lifetime_span: RelativeSpan,

    /// The list of forall lifetimes found in the term.
    pub forall_lifetimes: Vec<lifetime::Forall>,
}

impl Report for ForallLifetimeIsNotAllowedInOutlivesPredicate {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let mut forall_lifetimes = Vec::new();
        for forall in &self.forall_lifetimes {
            match forall {
                lifetime::Forall::Named(named_forall) => {
                    let source = engine
                        .get_source_file_path(named_forall.span.source_id)
                        .await;
                    let source = engine
                        .query(&pernixc_source_file::Key {
                            path: source,
                            target_id: named_forall.span.source_id.target_id,
                        })
                        .await
                        .unwrap();

                    let abs_span =
                        engine.to_absolute_span(&named_forall.span).await;

                    forall_lifetimes
                        .push(source.content()[abs_span.range()].to_string());
                }

                lifetime::Forall::Generated(generated_forall) => {
                    forall_lifetimes
                        .push(format!("'{}", generated_forall.unique_counter));
                }
            }
        }
        let forall_lifetimes = forall_lifetimes.join(", ");

        pernixc_diagnostic::Rendered {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.forall_lifetime_span).await,
                Some(format!(
                    "the forall lifetime(s) `{forall_lifetimes}` cannot \
                     appear in the outlives predicate",
                )),
            )),
            message: "forall lifetimes cannot appear in the outlives predicate"
                .to_string(),

            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

impl From<resolution_diagnostic::LifetimeParameterNotFound> for Diagnostic {
    fn from(value: resolution_diagnostic::LifetimeParameterNotFound) -> Self {
        Self::Resolution(
            resolution_diagnostic::Diagnostic::LifetimeParameterNotFound(value),
        )
    }
}
