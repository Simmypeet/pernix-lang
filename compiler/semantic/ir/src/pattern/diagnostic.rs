//! Contains the diagnostic related to the pattern binding.

use qbice::storage::intern::Interned;
use pernixc_diagnostic::{ByteIndex, Highlight, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use qbice::{Decode, Encode, StableHash};
use pernixc_symbol::source_map::to_absolute_span;

/// A particular name has already been bound in the given scope.
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
pub struct AlreadyBoundName {
    /// The span of the already bound identifier.
    pub already_bound_identifier_span: RelativeSpan,

    /// The span of the rebinding.
    pub new_binding_span: RelativeSpan,

    /// The name of the identifier.
    pub name: Interned<str>,
}

impl Report for AlreadyBoundName {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        pernixc_diagnostic::Rendered::builder()
            .message(format!(
                "the name `{}` has already been bound in the scope",
                self.name.as_ref()
            ))
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.new_binding_span).await)
                    .build(),
            )
            .related(vec![
                Highlight::builder()
                    .span(
                        engine
                            .to_absolute_span(
                                &self.already_bound_identifier_span,
                            )
                            .await,
                    )
                    .message("the name is already bound here")
                    .build(),
            ])
            .build()
    }
}
