use pernixc_diagnostic::{ByteIndex, Highlight, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::source_map::to_absolute_span;
use pernixc_term::r#type::Qualifier;

use crate::diagnostic_enum;

diagnostic_enum! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        StableHash,
        Serialize,
        Deserialize,
    )]
    pub enum Diagnostic {
        MismatchedQualifierForReferenceOf(MismatchedQualifierForReferenceOf)
    }
}

/// The given l-value cannot be referenced as the given one.
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
pub struct MismatchedQualifierForReferenceOf {
    /// The span of the reference of expression
    pub reference_of_span: RelativeSpan,

    /// The qualifier of the l-value.
    pub found_qualifier: Qualifier,

    /// The requested qualifier for the reference of expression.
    pub expected_qualifier: Qualifier,

    /// Whether or not the l-value is behind an another reference.
    pub is_behind_reference: bool,
}

impl Report for MismatchedQualifierForReferenceOf {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let message = if self.is_behind_reference {
            format!(
                "the l-value is behind a reference with qualifier `{}` but \
                 the reference of expression expects a qualifier `{}`",
                self.found_qualifier, self.expected_qualifier
            )
        } else {
            format!(
                "the l-value has qualifier `{}` but the reference of \
                 expression expects a qualifier `{}`",
                self.found_qualifier, self.expected_qualifier
            )
        };

        Ok(pernixc_diagnostic::Rendered::builder()
            .message(message)
            .primary_highlight(
                Highlight::builder()
                    .span(
                        engine.to_absolute_span(&self.reference_of_span).await,
                    )
                    .build(),
            )
            .severity(pernixc_diagnostic::Severity::Error)
            .build())
    }
}
