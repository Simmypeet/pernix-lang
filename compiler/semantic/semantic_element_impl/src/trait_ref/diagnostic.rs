use bon::Builder;
use pernixc_diagnostic::{Highlight, Rendered, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_resolution::diagnostic::ForallLifetimeRedefinition;
use pernixc_symbol::source_map::to_absolute_span;
use qbice::{Decode, Encode, Identifiable, StableHash};

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
#[allow(clippy::large_enum_variant)]
pub enum Diagnostic {
    Resolution(pernixc_resolution::diagnostic::Diagnostic),
    HigherRankedLifetimesInModuleLevelInstance(
        HigherRankedLifetimesInModuleLevelInstance,
    ),
    ForallLifetimeRedefinition(ForallLifetimeRedefinition),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        parameter: &pernixc_qbice::TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<pernixc_diagnostic::ByteIndex> {
        match self {
            Self::Resolution(diagnostic) => diagnostic.report(parameter).await,
            Self::HigherRankedLifetimesInModuleLevelInstance(diagnostic) => {
                diagnostic.report(parameter).await
            }
            Self::ForallLifetimeRedefinition(diagnostic) => {
                diagnostic.report(parameter).await
            }
        }
    }
}

/// Higher-ranked lifetimes are not allowed in module-level instance
/// definitions.
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
)]
pub struct HigherRankedLifetimesInModuleLevelInstance {
    span: RelativeSpan,
}

impl Report for HigherRankedLifetimesInModuleLevelInstance {
    async fn report(
        &self,
        parameter: &pernixc_qbice::TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<pernixc_diagnostic::ByteIndex> {
        Rendered::builder()
            .message(
                "instance cannot implement a trait with higher-ranked \
                 lifetimes",
            )
            .primary_highlight(
                Highlight::builder()
                    .span(parameter.to_absolute_span(&self.span).await)
                    .build(),
            )
            .build()
    }
}
