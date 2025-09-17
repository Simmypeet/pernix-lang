use pernixc_diagnostic::{ByteIndex, Highlight, Rendered, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::source_map::to_absolute_span;
use pernixc_term::{
    constant::Constant,
    display::{Display, InferenceRenderingMap},
    r#type::Type,
};

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
        CannotDereference(CannotDereference
        )
    }
}

/// The expression of the given type cannot be dereferenced.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct CannotDereference {
    /// The type of the expression that cannot be dereferenced.
    pub found_type: Type,

    /// The span of the expression with dereference operator.
    pub span: RelativeSpan,

    /// A map for rendering type inference variables.
    pub type_inference_map: InferenceRenderingMap<Type>,

    /// A map for rendering constant inference variables.
    pub constant_inference_map: InferenceRenderingMap<Constant>,
}

impl Report for CannotDereference {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> Result<
        Rendered<ByteIndex>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        let mut message = "cannot dereference expression of type `".to_string();

        self.found_type.write_async_with_mapping(
            parameter,
            &mut message,
            None,
            Some(&self.type_inference_map),
            Some(&self.constant_inference_map),
        );
        message.push('`');

        Ok(Rendered::builder()
            .message(message)
            .primary_highlight(
                Highlight::builder()
                    .span(parameter.to_absolute_span(&self.span).await)
                    .build(),
            )
            .severity(Severity::Error)
            .help_message("only references can be dereferenced")
            .build())
    }
}
