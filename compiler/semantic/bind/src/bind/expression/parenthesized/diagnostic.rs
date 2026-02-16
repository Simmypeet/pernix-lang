use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_source_file::ByteIndex;
use pernixc_symbol::source_map::to_absolute_span;
use pernixc_term::{
    constant::Constant,
    display::{Display, InferenceRenderingMap},
    r#type::Type,
};
use qbice::{Decode, Encode, StableHash};

use crate::diagnostic_enum;

diagnostic_enum! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        StableHash,
        Encode,
        Decode,
    )]
    pub enum Diagnostic {
        MoreThanOneUnpackedInTupleExpression(
            MoreThanOneUnpackedInTupleExpression
        )
    }
}

/// The unpack operator can only be used once in a tuple expression.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Encode, Decode)]
pub struct MoreThanOneUnpackedInTupleExpression {
    /// The span of the tuple expression.
    pub span: RelativeSpan,

    /// The type of the tuple expression.
    pub r#type: Type,

    /// The inference rendering map for constants.
    pub constant_inference_map: InferenceRenderingMap<Constant>,

    /// The inference rendering map for types.
    pub type_inference_map: InferenceRenderingMap<Type>,
}

impl Report for MoreThanOneUnpackedInTupleExpression {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let span = engine.to_absolute_span(&self.span).await;

        // Format the tuple type with inference rendering maps
        let mut type_str = String::new();
        let _ = self
            .r#type
            .write_async_with_mapping(
                engine,
                &mut type_str,
                None,
                Some(&self.type_inference_map),
                Some(&self.constant_inference_map),
            )
            .await;

        pernixc_diagnostic::Rendered::builder()
            .severity(Severity::Error)
            .message(
                "the unpack operator can only be used once in a tuple \
                 expression",
            )
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "multiple unpack operators found in tuple expression \
                         of type `{type_str}`"
                    ))
                    .span(span)
                    .build(),
            )
            .help_message(
                "only one unpack operator (`...`) is allowed per tuple \
                 expression",
            )
            .build()
    }
}
