//! Contains the diagnostic related to the pattern binding.

use flexstr::SharedStr;
use pernixc_diagnostic::{Highlight, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_source_file::ByteIndex;
use pernixc_symbol::source_map::to_absolute_span;

/// A particular name has already been bound in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AlreadyBoundName {
    /// The span of the already bound identifier.
    pub already_bound_identifier_span: RelativeSpan,

    /// The span of the rebinding.
    pub new_binding_span: RelativeSpan,

    /// The name of the identifier.
    pub name: SharedStr,
}

impl Report<&TrackedEngine> for AlreadyBoundName {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic::builder()
            .message(format!(
                "the name `{}` has already been bound in the scope",
                self.name
            ))
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.new_binding_span).await)
                    .build(),
            )
            .related(vec![Highlight::builder()
                .span(
                    engine
                        .to_absolute_span(&self.already_bound_identifier_span)
                        .await,
                )
                .message("the name is already bound here")
                .build()])
            .build()
    }
}
