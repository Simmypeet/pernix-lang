use pernixc_diagnostic::{Highlight, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_resolution::qualified_identifier::Resolution;
use pernixc_source_file::ByteIndex;
use pernixc_symbol::{name::get_qualified_name, source_map::to_absolute_span};
use pernixc_target::Global;
use qbice::{Decode, Encode, StableHash};

use crate::diagnostic_enum;

diagnostic_enum! {
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
    #[allow(clippy::large_enum_variant)]
    pub enum Diagnostic {
        ExpectedAssociatedValue(ExpectedAssociatedValue),
        SymbolCannotBeUsedAsAnExpression(SymbolCannotBeUsedAsAnExpression)
    }
}

/// The enum variant expects an associated value but none was provided.
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
pub struct ExpectedAssociatedValue {
    /// The ID of the variant where the associated value is expected.
    pub variant_id: Global<pernixc_symbol::SymbolID>,

    /// The span of the variant.
    pub span: RelativeSpan,
}

impl Report for ExpectedAssociatedValue {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let qualified_identifier =
            parameter.get_qualified_name(self.variant_id).await;
        let abs_span = parameter.to_absolute_span(&self.span).await;

        pernixc_diagnostic::Rendered::builder()
            .message(format!(
                "expected associated value for variant \
                 `{qualified_identifier}`"
            ))
            .primary_highlight(
                Highlight::builder()
                    .span(abs_span)
                    .message("expected associated value here")
                    .build(),
            )
            .severity(pernixc_diagnostic::Severity::Error)
            .help_message(
                "consider adding an associated value by adding `(EXPRESSION)` \
                 after the variant name"
                    .to_string(),
            )
            .build()
    }
}

/// The symbol cannot be used as an expression.
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
pub struct SymbolCannotBeUsedAsAnExpression {
    /// The span of the symbol reference.
    pub span: RelativeSpan,

    /// The ID of the symbol that cannot be used as an expression.
    pub found: Resolution,
}

impl Report for SymbolCannotBeUsedAsAnExpression {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let found_string = self.found.found_string(parameter).await;
        let abs_span = parameter.to_absolute_span(&self.span).await;

        pernixc_diagnostic::Rendered::builder()
            .message(format!(
                "`{found_string}` cannot be used as an expression",
            ))
            .primary_highlight(
                Highlight::builder()
                    .span(abs_span)
                    .message("cannot be used as an expression")
                    .build(),
            )
            .severity(pernixc_diagnostic::Severity::Error)
            .help_message(
                "only variants, constants, functions, parameters, and \
                 variables can be used as expressions"
                    .to_string(),
            )
            .build()
    }
}
