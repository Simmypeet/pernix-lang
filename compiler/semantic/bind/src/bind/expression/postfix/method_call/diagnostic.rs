use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_hash::HashSet;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::{name::get_qualified_name, source_map::to_absolute_span};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    display::{Display, InferenceRenderingMap},
    r#type::Type,
};
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

/// A method with the given name could not be found for the given type.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Encode, Decode)]
pub struct MethodCallNotFound {
    /// The name of the method that was not found.
    pub method_name: Interned<str>,

    /// The type of the receiver expression.
    pub receiver_type: Type,

    /// The span of the method identifier in the method call.
    pub method_span: RelativeSpan,

    /// The span of the receiver expression.
    pub receiver_span: RelativeSpan,

    /// The type inference map to use when rendering types.
    pub type_inference_map: InferenceRenderingMap<Type>,

    /// The constant inference map to use when rendering types.
    pub constant_inference_map: InferenceRenderingMap<Constant>,
}

impl Report for MethodCallNotFound {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<pernixc_diagnostic::ByteIndex> {
        let mut message = format!(
            "method `{}` not found for type `",
            self.method_name.as_ref()
        );

        self.receiver_type
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

        let mut expression_message = "this expression has type `".to_string();
        self.receiver_type
            .write_async_with_mapping(
                parameter,
                &mut expression_message,
                None,
                Some(&self.type_inference_map),
                Some(&self.constant_inference_map),
            )
            .await
            .unwrap();

        expression_message.push('`');

        pernixc_diagnostic::Rendered::builder()
            .message(message)
            .primary_highlight(
                Highlight::builder()
                    .span(parameter.to_absolute_span(&self.method_span).await)
                    .message(format!(
                        "method `{}` not found",
                        self.method_name.as_ref()
                    ))
                    .build(),
            )
            .severity(Severity::Error)
            .related(vec![
                Highlight::builder()
                    .span(parameter.to_absolute_span(&self.receiver_span).await)
                    .message(expression_message)
                    .build(),
            ])
            .build()
    }
}

/// A method with the given name matched multiple candidates for the given
/// type.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Encode, Decode)]
pub struct AmbiguousMethodCall {
    /// The name of the method that was found.
    pub method_name: Interned<str>,

    /// The type of the receiver expression.
    pub receiver_type: Type,

    /// The span of the method identifier in the method call.
    pub method_span: RelativeSpan,

    /// The span of the receiver expression.
    pub receiver_span: RelativeSpan,

    /// The candidates that were found.
    pub candidates: HashSet<Global<pernixc_symbol::ID>>,

    /// The type inference map to use when rendering types.
    pub type_inference_map: InferenceRenderingMap<Type>,

    /// The constant inference map to use when rendering types.
    pub constant_inference_map: InferenceRenderingMap<Constant>,
}

impl Report for AmbiguousMethodCall {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<pernixc_diagnostic::ByteIndex> {
        let mut message = format!(
            "ambiguous method call `{}` for type `",
            self.method_name.as_ref()
        );

        self.receiver_type
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

        let mut expression_message = "this expression has type `".to_string();
        self.receiver_type
            .write_async_with_mapping(
                parameter,
                &mut expression_message,
                None,
                Some(&self.type_inference_map),
                Some(&self.constant_inference_map),
            )
            .await
            .unwrap();

        expression_message.push('`');

        let mut related = vec![
            Highlight::builder()
                .span(parameter.to_absolute_span(&self.receiver_span).await)
                .message(expression_message)
                .build(),
        ];

        for candidate in &self.candidates {
            let qualified_name = parameter.get_qualified_name(*candidate).await;
            related.push(
                Highlight::builder()
                    .span(parameter.to_absolute_span(&self.method_span).await)
                    .message(format!("candidate: `{qualified_name}`"))
                    .build(),
            );
        }

        pernixc_diagnostic::Rendered::builder()
            .message(message)
            .primary_highlight(
                Highlight::builder()
                    .span(parameter.to_absolute_span(&self.method_span).await)
                    .message(format!(
                        "ambiguous method call `{}`",
                        self.method_name.as_ref()
                    ))
                    .build(),
            )
            .severity(Severity::Error)
            .related(related)
            .build()
    }
}
