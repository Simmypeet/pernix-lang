use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_semantic_element::implements_arguments::get_implements_argument;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    kind::get_kind, name::get_qualified_name, source_map::to_absolute_span,
    span::get_span,
};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    display::{Display, InferenceRenderingMap},
    generic_arguments::GenericArguments,
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
        SymbolIsNotCallable(SymbolIsNotCallable),
        VariantDoesntHaveAssociatedValue(VariantDoesntHaveAssociatedValue),
        ExtraneousArgumentsToAssociatedValue(
            ExtraneousArgumentsToAssociatedValue
        ),
        VariantAssociatedValueExpected(VariantAssociatedValueExpected),
        MismatchedArgumentsCount(MismatchedArgumentsCount),
        MismatchedImplementationArguments(
            MismatchedImplementationArguments
        ),
    }
}

/// The given symbol cannot be called.
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
pub struct SymbolIsNotCallable {
    /// The ID of the symbol that cannot be called.
    pub symbol_id: Global<pernixc_symbol::ID>,

    /// The span of the call.
    pub span: RelativeSpan,
}

impl Report for SymbolIsNotCallable {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let span = engine.to_absolute_span(&self.span).await;
        let kind = engine.get_kind(self.symbol_id).await;
        let qualified_name = engine.get_qualified_name(self.symbol_id).await;

        Ok(pernixc_diagnostic::Diagnostic::builder()
            .message(format!(
                "the symbol `{} {qualified_name}` cannot be called",
                kind.kind_str()
            ))
            .primary_highlight(
                Highlight::builder().span(span).message("not callable").build(),
            )
            .severity(Severity::Error)
            .help_message(
                "only functions or enum with associated value can be called",
            )
            .build())
    }
}

/// The given variant doesn't have an associated value.
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
pub struct VariantDoesntHaveAssociatedValue {
    /// The ID of the variant that doesn't have an associated value but was
    /// called as if it did.
    pub variant_id: Global<pernixc_symbol::ID>,

    /// The span of the call.
    pub span: RelativeSpan,

    /// The number of arguments supplied in the call.
    pub supplied_count: usize,
}

impl Report for VariantDoesntHaveAssociatedValue {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let span = engine.to_absolute_span(&self.span).await;
        let qualified_name = engine.get_qualified_name(self.variant_id).await;

        Ok(pernixc_diagnostic::Diagnostic::builder()
            .message(format!(
                "the variant `{qualified_name}` doesn't have an associated \
                 value",
            ))
            .primary_highlight(
                Highlight::builder().span(span).message("not callable").build(),
            )
            .severity(Severity::Error)
            .help_message(if self.supplied_count == 0 {
                "remove the parentheses to use the variant as a value"
            } else if self.supplied_count == 1 {
                "remove the argument and the parentheses to use the variant as \
                 a value"
            } else {
                "remove the arguments and the parentheses to use the variant \
                 as a value"
            })
            .build())
    }
}

/// The variant expects an associated value but was called without one.
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
pub struct VariantAssociatedValueExpected {
    /// The ID of the variant that expects an associated value but was called
    /// without one.
    pub variant_id: Global<pernixc_symbol::ID>,

    /// The span of the call.
    pub span: RelativeSpan,
}

impl Report for VariantAssociatedValueExpected {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let span = engine.to_absolute_span(&self.span).await;
        let qualified_name = engine.get_qualified_name(self.variant_id).await;

        Ok(pernixc_diagnostic::Diagnostic::builder()
            .message(format!(
                "the variant `{qualified_name}` expects an associated value",
            ))
            .primary_highlight(
                Highlight::builder()
                    .span(span)
                    .message("associated value expected")
                    .build(),
            )
            .severity(Severity::Error)
            .help_message(format!(
                "supply the associated value using the syntax \
                 `{qualified_name}(value)`"
            ))
            .build())
    }
}

/// More than one argument was supplied to a variant.
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
pub struct ExtraneousArgumentsToAssociatedValue {
    /// The ID of the variant that was supplied with too many arguments.
    pub variant_id: Global<pernixc_symbol::ID>,

    /// The span of the call.
    pub span: RelativeSpan,

    /// The number of arguments supplied in the call.
    pub supplied_count: usize,
}

impl Report for ExtraneousArgumentsToAssociatedValue {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let span = engine.to_absolute_span(&self.span).await;
        let qualified_name = engine.get_qualified_name(self.variant_id).await;

        Ok(pernixc_diagnostic::Diagnostic::builder()
            .message(format!(
                "the variant `{qualified_name}` was supplied with too many \
                 arguments",
            ))
            .primary_highlight(
                Highlight::builder()
                    .span(span)
                    .message("too many arguments")
                    .build(),
            )
            .severity(Severity::Error)
            .help_message(
                "only one argument is allowed for the associated value",
            )
            .build())
    }
}

/// The number of arguments supplied to a function does not match the number
/// of parameters the function expects.
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
pub struct MismatchedArgumentsCount {
    /// The ID of the function that was called with the wrong number of
    /// arguments.
    pub function_id: Global<pernixc_symbol::ID>,

    /// The number of arguments the function expects.
    pub expected: usize,

    /// The number of arguments that were actually supplied.
    pub supplied: usize,

    /// The span of the call.
    pub span: RelativeSpan,
}

impl Report for MismatchedArgumentsCount {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let span = engine.to_absolute_span(&self.span).await;
        let qualified_name = engine.get_qualified_name(self.function_id).await;

        Ok(pernixc_diagnostic::Diagnostic::builder()
            .message(format!(
                "the function `{qualified_name}` was called with the wrong \
                 number of arguments",
            ))
            .primary_highlight(
                Highlight::builder()
                    .span(span)
                    .message("wrong number of arguments")
                    .build(),
            )
            .severity(Severity::Error)
            .build())
    }
}

/// The generic arguments are not compatible with the generic arguments defined
/// in the implementation.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct MismatchedImplementationArguments {
    /// The ID of the ADT implementation where the generic arguments are
    /// mismatched.
    pub implementation_id: Global<pernixc_symbol::ID>,

    /// The generic arguments found in the implementation.
    pub found_generic_arguments: GenericArguments,

    /// The span of the instantiation that causes the mismatch.
    pub instantiation_span: RelativeSpan,

    /// The inference rendering map for constants.
    pub constant_inference_map: InferenceRenderingMap<Constant>,

    /// The inference rendering map for types.
    pub type_inference_map: InferenceRenderingMap<Type>,
}

impl Report for MismatchedImplementationArguments {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let impl_span =
            if let Some(span) = engine.get_span(self.implementation_id).await {
                Some(engine.to_absolute_span(&span).await)
            } else {
                None
            };

        let impl_arguments = engine
            .get_implements_argument(self.implementation_id)
            .await
            .unwrap()
            .unwrap();

        Ok(pernixc_diagnostic::Diagnostic::builder()
            .message(
                "the generic arguments are not compatible with the generic \
                 arguments defined in the implementation",
            )
            .primary_highlight(
                Highlight::builder()
                    .span(
                        engine.to_absolute_span(&self.instantiation_span).await,
                    )
                    .message({
                        let mut string = String::new();

                        string.push_str("the generic arguments supplied was `");

                        self.found_generic_arguments
                            .write_async_with_mapping(
                                engine,
                                &mut string,
                                None,
                                Some(&self.type_inference_map),
                                Some(&self.constant_inference_map),
                            )
                            .await
                            .unwrap();
                        string.push_str("` aren't compatible with `");
                        impl_arguments
                            .write_async(engine, &mut string)
                            .await
                            .unwrap();
                        string.push('`');

                        string
                    })
                    .build(),
            )
            .related(
                impl_span
                    .as_ref()
                    .map(|span| {
                        Highlight::new(
                            *span,
                            Some(
                                "the implementation is defined here"
                                    .to_string(),
                            ),
                        )
                    })
                    .into_iter()
                    .collect(),
            )
            .build())
    }
}
