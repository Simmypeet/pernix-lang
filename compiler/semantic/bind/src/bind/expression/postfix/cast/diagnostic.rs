use pernixc_diagnostic::{Highlight, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::source_map::to_absolute_span;
use pernixc_term::{
    constant::Constant,
    display::{Display, InferenceRenderingMap},
    r#type::Type,
};

/// The kind of pointer involved in a pointer type casting operation.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, StableHash, Serialize, Deserialize,
)]
pub enum PointerKind {
    /// A raw pointer (`*T` or `*mut T`).
    RawPointer,

    /// A reference (`&T` or `&mut T`).
    Reference,
}

/// The operand type of pointer type casting is invalid.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct InvalidPointerTypeCasting {
    /// The span of the operand.
    pub span: RelativeSpan,

    /// The type of the operand.
    pub r#type: Type,

    /// The kind of pointer being cast to.
    pub casted_pointer_kind: PointerKind,

    /// Mapping for rendering type inferences
    pub type_inference_map: InferenceRenderingMap<Type>,

    /// Mapping for rendering constant inferences
    pub constant_inference_map: InferenceRenderingMap<Constant>,
}

impl Report for InvalidPointerTypeCasting {
    async fn report(
        &self,
        parameter: &pernixc_query::TrackedEngine,
    ) -> Result<
        pernixc_diagnostic::Rendered<pernixc_diagnostic::ByteIndex>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        let mut message = format!(
            "cannot cast to a `{}` type from an expression of type `",
            match self.casted_pointer_kind {
                PointerKind::RawPointer => "raw pointer",
                PointerKind::Reference => "reference",
            }
        );

        self.r#type
            .write_async_with_mapping(
                parameter,
                &mut message,
                None,
                Some(&self.type_inference_map),
                Some(&self.constant_inference_map),
            )
            .await
            .unwrap();

        message.push('`');

        Ok(pernixc_diagnostic::Rendered::builder()
            .message(message)
            .primary_highlight(
                Highlight::builder()
                    .span(parameter.to_absolute_span(&self.span).await)
                    .build(),
            )
            .severity(pernixc_diagnostic::Severity::Error)
            .help_message(
                "only a `pointer`, `reference`, or `usize` type can be cast \
                 to a pointer type",
            )
            .build())
    }
}

/// The given type cannot be used in cast expression.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct InvalidCastType {
    /// The span of the type reference.
    pub span: RelativeSpan,

    /// The type that cannot be used in cast expression.
    pub r#type: Type,

    /// Mapping for rendering type inferences
    pub type_inference_map: InferenceRenderingMap<Type>,

    /// Mapping for rendering constant inferences
    pub constant_inference_map: InferenceRenderingMap<Constant>,
}

impl Report for InvalidCastType {
    async fn report(
        &self,
        parameter: &pernixc_query::TrackedEngine,
    ) -> Result<
        pernixc_diagnostic::Rendered<pernixc_diagnostic::ByteIndex>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        let mut message = "the type `".to_string();

        self.r#type
            .write_async_with_mapping(
                parameter,
                &mut message,
                None,
                Some(&self.type_inference_map),
                Some(&self.constant_inference_map),
            )
            .await
            .unwrap();

        message.push_str("` cannot be used in cast expression");

        Ok(pernixc_diagnostic::Rendered::builder()
            .message(message)
            .primary_highlight(
                Highlight::builder()
                    .span(parameter.to_absolute_span(&self.span).await)
                    .build(),
            )
            .severity(pernixc_diagnostic::Severity::Error)
            .help_message(
                "only casting between numeric types, casting to/from \
                 pointer-sized integer types, and casting to pointer or \
                 reference types are allowed",
            )
            .build())
    }
}
