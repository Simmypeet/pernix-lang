use pernixc_diagnostic::{Highlight, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    kind::get_kind, name::get_qualified_name, source_map::to_absolute_span,
};
use pernixc_target::Global;

use crate::diagnostic_enum;

diagnostic_enum! {
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
    Serialize,
    Deserialize,
)]
pub struct ExpectedAssociatedValue {
    /// The ID of the variant where the associated value is expected.
    pub variant_id: Global<pernixc_symbol::ID>,

    /// The span of the variant.
    pub span: RelativeSpan,
}

impl Report for ExpectedAssociatedValue {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let qualified_identifier =
            parameter.get_qualified_name(self.variant_id).await;
        let abs_span = parameter.to_absolute_span(&self.span).await;

        Ok(pernixc_diagnostic::Diagnostic::builder()
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
            .build())
    }
}

/// The symbol cannot be used as an expression.
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
pub struct SymbolCannotBeUsedAsAnExpression {
    /// The span of the symbol reference.
    pub span: RelativeSpan,

    /// The ID of the symbol that cannot be used as an expression.
    pub symbol: Global<pernixc_symbol::ID>,
}

impl Report for SymbolCannotBeUsedAsAnExpression {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let qualified_name = parameter.get_qualified_name(self.symbol).await;
        let abs_span = parameter.to_absolute_span(&self.span).await;
        let kind = parameter.get_kind(self.symbol).await;

        Ok(pernixc_diagnostic::Diagnostic::builder()
            .message(format!(
                "the symbol `{} {qualified_name}` cannot be used as an \
                 expression",
                kind.kind_str()
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
            .build())
    }
}
