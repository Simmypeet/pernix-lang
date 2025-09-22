//! Contains the dianostic types related to the borrow check analysis.

use enum_as_inner::EnumAsInner;
use pernixc_diagnostic::{ByteIndex, Highlight, Rendered, Report, Severity};
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::source_map::to_absolute_span;
use pernixc_term::{
    display::{Configuration, Display},
    lifetime::Lifetime,
};
use pernixc_type_system::diagnostic::{
    PredicateSatisfiabilityOverflow, UnsatisfiedPredicate,
};

use crate::UniversalRegion;

/// An enumeration of all possible diagnostics that can be produced by the
/// borrow checker.
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
    derive_more::From,
)]
#[allow(missing_docs, clippy::large_enum_variant)]
pub enum Diagnostic {
    TypeSystem(pernixc_type_system::diagnostic::Diagnostic),
    MovedOutWhileBorrowed(MovedOutWhileBorrowed),
    VariableDoesNotLiveLongEnough(VariableDoesNotLiveLongEnough),
    MutablyAccessWhileImmutablyBorrowed(MutablyAccessWhileImmutablyBorrowed),
    AccessWhileMutablyBorrowed(AccessWhileMutablyBorrowed),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, CyclicError> {
        match self {
            Self::TypeSystem(x) => x.report(engine).await,
            Self::MovedOutWhileBorrowed(x) => x.report(engine).await,
            Self::VariableDoesNotLiveLongEnough(x) => x.report(engine).await,
            Self::MutablyAccessWhileImmutablyBorrowed(x) => {
                x.report(engine).await
            }
            Self::AccessWhileMutablyBorrowed(x) => x.report(engine).await,
        }
    }
}

/// An enumeration of what part of the code uses the invalidated
/// borrows/variables.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    StableHash,
    Serialize,
    Deserialize,
)]
pub enum Usage {
    /// The invalidated borrows are later used within the body of local
    /// function/scope.
    Local(RelativeSpan),

    /// The invalidated borrows might be later used by the universal regions
    /// (the caller of the function).
    ByUniversalRegions(Vec<UniversalRegion>),

    /// The invalidated borrows are used in the drop implementation.
    Drop,
}

/// The value is moved out from the variabl while it is borrowed.
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
pub struct MovedOutWhileBorrowed {
    /// The span of the borrow.
    pub borrow_span: RelativeSpan,

    /// The span of the borrow usage.
    pub usage: Usage,

    /// The span where the value is moved out
    pub moved_out_span: RelativeSpan,
}

async fn format_universal_regions(
    engine: &TrackedEngine,
    universal_regions: &[UniversalRegion],
) -> String {
    let elided_lifetimes = universal_regions
        .iter()
        .filter_map(|x| x.as_non_static().and_then(|x| x.as_elided()))
        .copied()
        .zip((0..).map(|x| format!("'{x}").into()))
        .collect::<HashMap<_, _>>();

    let fmt_config =
        Configuration::builder().edlided_lifetimes(&elided_lifetimes).build();

    let mut lifetime_names = Vec::new();

    for region in universal_regions.iter().copied().map(Lifetime::from) {
        lifetime_names.push(
            region
                .write_to_string_with_configuration(engine, &fmt_config)
                .await
                .unwrap(),
        );
    }

    format!(
        "lifetime(s) {} can access the borrow later",
        lifetime_names.join(", ")
    )
}

impl Report for MovedOutWhileBorrowed {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, CyclicError> {
        let mut related = Vec::new();

        if let Usage::Local(span) = &self.usage {
            related.push(
                Highlight::builder()
                    .span(engine.to_absolute_span(span).await)
                    .message("the borrow is used here".to_string())
                    .build(),
            );
        }

        Ok(pernixc_diagnostic::Rendered::builder()
            .message(
                "the value is moved out from the variable while it is borrowed",
            )
            .severity(Severity::Error)
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.moved_out_span).await)
                    .maybe_message(match &self.usage {
                        Usage::Local(_) => None,

                        Usage::ByUniversalRegions(universal_regions) => Some(
                            format_universal_regions(engine, universal_regions)
                                .await,
                        ),

                        Usage::Drop => Some(
                            "the borrow is used in the drop implementation"
                                .to_string(),
                        ),
                    })
                    .build(),
            )
            .related(related)
            .build())
    }
}

/// The variable doesn't live long enough to outlives the given lifetime.
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
pub struct VariableDoesNotLiveLongEnough {
    /// The span where the borrows occurred
    pub borrow_span: RelativeSpan,

    /// The span of the variable declaration.
    pub variable_span: RelativeSpan,

    /// The usage of the borrow that points to the goes out of scope variable.
    pub usage: Usage,
}

impl Report for VariableDoesNotLiveLongEnough {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, CyclicError> {
        let mut related = vec![Highlight::builder()
            .span(engine.to_absolute_span(&self.borrow_span).await)
            .message("the borrow starts here".to_string())
            .build()];

        if let Some(span) = self.usage.as_local() {
            related.push(
                Highlight::builder()
                    .span(engine.to_absolute_span(span).await)
                    .message("the borrow is used here".to_string())
                    .build(),
            );
        }

        Ok(pernixc_diagnostic::Rendered::builder()
            .message("the variable doesn't live long enough")
            .severity(Severity::Error)
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.variable_span).await)
                    .maybe_message(match &self.usage {
                        Usage::Local(_) => None,

                        Usage::ByUniversalRegions(universal_regions) => Some(
                            format_universal_regions(engine, universal_regions)
                                .await,
                        ),

                        Usage::Drop => Some(
                            "the borrow is used in the drop implementation"
                                .to_string(),
                        ),
                    })
                    .build(),
            )
            .related(related)
            .build())
    }
}

/// The mutable access is done while the value is immutably borrowed.
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
pub struct MutablyAccessWhileImmutablyBorrowed {
    /// The span of the mutable access.
    pub mutable_access_span: RelativeSpan,

    /// The span of the prior borrow.
    pub immutable_borrow_span: Option<RelativeSpan>,

    /// The usage span of the prior borrow.
    pub usage: Usage,
}

impl Report for MutablyAccessWhileImmutablyBorrowed {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, CyclicError> {
        let mut related = Vec::new();

        if let Some(span) = &self.immutable_borrow_span {
            related.push(
                Highlight::builder()
                    .span(engine.to_absolute_span(span).await)
                    .message("the borrow starts here".to_string())
                    .build(),
            );
        }

        if let Some(span) = self.usage.as_local() {
            if self.immutable_borrow_span.as_ref() == Some(span) {
                related.push(
                    Highlight::builder()
                        .span(engine.to_absolute_span(span).await)
                        .message(
                            "the borrow is used later in the next iteration"
                                .to_string(),
                        )
                        .build(),
                );
            } else {
                related.push(
                    Highlight::builder()
                        .span(engine.to_absolute_span(span).await)
                        .message("the borrow is used here".to_string())
                        .build(),
                );
            }
        }

        Ok(pernixc_diagnostic::Rendered::builder()
            .message("mutable access is done while it is borrowed")
            .severity(Severity::Error)
            .primary_highlight(
                Highlight::builder()
                    .span(
                        engine
                            .to_absolute_span(&self.mutable_access_span)
                            .await,
                    )
                    .maybe_message(match &self.usage {
                        Usage::Local(_) => None,

                        Usage::ByUniversalRegions(universal_regions) => Some(
                            format_universal_regions(engine, universal_regions)
                                .await,
                        ),

                        Usage::Drop => Some(
                            "the borrow is used in the drop implementation"
                                .to_string(),
                        ),
                    })
                    .build(),
            )
            .related(related)
            .build())
    }
}

/// The access is done while the value is mutably borrowed.
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
pub struct AccessWhileMutablyBorrowed {
    /// The span of the access.
    pub access_span: RelativeSpan,

    /// The span of the prior borrow.
    pub mutable_borrow_span: Option<RelativeSpan>,

    /// The usage span of the prior borrow.
    pub borrow_usage: Usage,
}

impl Report for AccessWhileMutablyBorrowed {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, CyclicError> {
        let mut related = Vec::new();

        if let Some(span) = &self.mutable_borrow_span {
            related.push(
                Highlight::builder()
                    .span(engine.to_absolute_span(span).await)
                    .message("the mutable borrow starts here".to_string())
                    .build(),
            );
        }

        if let Some(span) = self.borrow_usage.as_local() {
            if self.mutable_borrow_span.as_ref() == Some(span) {
                related.push(
                    Highlight::builder()
                        .span(engine.to_absolute_span(span).await)
                        .message(
                            "the borrow is used later in the next iteration"
                                .to_string(),
                        )
                        .build(),
                );
            } else {
                related.push(
                    Highlight::builder()
                        .span(engine.to_absolute_span(span).await)
                        .message("the borrow is used here".to_string())
                        .build(),
                );
            }
        }

        Ok(pernixc_diagnostic::Rendered::builder()
            .message("access is done while it is mutably borrowed")
            .severity(Severity::Error)
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.access_span).await)
                    .maybe_message(match &self.borrow_usage {
                        Usage::Local(_) => None,

                        Usage::ByUniversalRegions(universal_regions) => Some(
                            format_universal_regions(engine, universal_regions)
                                .await,
                        ),

                        Usage::Drop => Some(
                            "the borrow is used in the drop implementation"
                                .to_string(),
                        ),
                    })
                    .build(),
            )
            .related(related)
            .build())
    }
}

impl From<UnsatisfiedPredicate> for Diagnostic {
    fn from(value: UnsatisfiedPredicate) -> Self {
        Self::TypeSystem(value.into())
    }
}

impl From<PredicateSatisfiabilityOverflow> for Diagnostic {
    fn from(value: PredicateSatisfiabilityOverflow) -> Self {
        Self::TypeSystem(value.into())
    }
}
