//! Diagnostics emitted during the binding phase of the compiler.

use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    name::get_qualified_name, source_map::to_absolute_span, span::get_span,
};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    display::{Display, InferenceRenderingMap},
    generic_parameters::{
        get_generic_parameters, ConstantParameterID, TypeParameterID,
    },
    r#type::Type,
};

use crate::{binder, pattern};

diagnostic_enum! {
    /// An enumeration of all diagnostics that can occur during binding the
    /// IR phase.
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        StableHash,
        Serialize,
        Deserialize,
        derive_more::From,
    )]
    #[allow(clippy::large_enum_variant, missing_docs)]
    pub enum Diagnostic {
        Resolution(pernixc_resolution::diagnostic::Diagnostic),
        TypeSystem(pernixc_type_system::diagnostic::Diagnostic),
        TypeCheck(binder::type_check::diagnostic::Diagnostic),
        ExpectedLValue(ExpectedLValue),
        PatternBinding(pattern::bind::diagnostic::Diagnostic),
        PatternInsertNameBinding(
            pattern::insert_name_binding::diagnostic::Diagnostic
        ),
        QualifiedIdentifier(
            expression::qualified_identifier::diagnostic::Diagnostic
        ),
        FunctionCall(expression::function_call::diagnostic::Diagnostic),
        Parenthesized(expression::parenthesized::diagnostic::Diagnostic),
        Numeric(expression::numeric::diagnostic::Diagnostic),
        Struct(expression::r#struct::diagnostic::Diagnostic),
        Postfix(expression::postfix::diagnostic::Diagnostic),
        Borrow(expression::borrow::diagnostic::Diagnostic),
        Dereference(expression::dereference::diagnostic::Diagnostic),
        UnsafeRequired(expression::diagnostic::UnsafeRequired),
        AssignToNonMutable(AssignToNonMutable),
        InvalidTypeInBinaryOperator(InvalidTypeInBinaryOperator),
        NotAllFlowPathsReturnAValue(NotAllFlowPathsReturnAValue),
        ReturnIsNotAllowed(ReturnIsNotAllowed),
        TypeAnnotationRequired(TypeAnnotationRequired),
        ConstantAnnotationRequired(ConstantAnnotationRequired),
    }
}

/// A convenience macro for creating diagnostic enums and implementing the
/// `Report` trait for them.
#[macro_export]
macro_rules! diagnostic_enum {
    {
        $( #[$enum_meta:meta] )*
        $enum_vis:vis enum $ident:ident {
            $(
                $variant:ident($ty:ty)
            ),* $(,)?
        }
    } => {
        $( #[$enum_meta] )*
        $enum_vis enum $ident {
            $(
                $variant($ty),
            )*
        }

        impl pernixc_diagnostic::Report for Diagnostic {
            async fn report(
                &self,
                engine: &pernixc_query::TrackedEngine,
            ) -> Result<
                pernixc_diagnostic::Rendered<pernixc_diagnostic::ByteIndex>,
                pernixc_query::runtime::executor::CyclicError
            > {
                match self {
                    $(
                        Diagnostic::$variant(inner) => inner.report(engine).await,
                    )*
                }
            }
        }
    };
}

pub use diagnostic_enum;

use crate::bind::expression;

/// Expected an l-value but found an r-value.
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
pub struct ExpectedLValue {
    /// The span of the r-value.
    pub expression_span: RelativeSpan,
}

impl Report for ExpectedLValue {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Rendered::builder()
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.expression_span).await)
                    .build(),
            )
            .severity(Severity::Error)
            .message("expected an l-value")
            .help_message(
                "an l-value refers to an expression that designates a storage \
                 location,
                        for example, a variable or a dereferenced pointer",
            )
            .build())
    }
}

/// Attempts to assign to a non-mutable l-value.
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
pub struct AssignToNonMutable {
    /// The span of the assignment.
    pub span: RelativeSpan,
}

impl Report for AssignToNonMutable {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Rendered::builder()
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.span).await)
                    .build(),
            )
            .severity(Severity::Error)
            .message("cannot assign to non-mutable l-value")
            .help_message(
                "only mutable l-values can be assigned to; consider declaring \
                 the variable as `mut`",
            )
            .build())
    }
}

/// A kind of binary operator.
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
#[allow(missing_docs)]
pub enum BinaryOperatorKind {
    Arithmetic,
    Relational,
    Bitwise,
}

/// The type of the left-hand side of a binary operator is invalid.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct InvalidTypeInBinaryOperator {
    /// The span of the left-hand side expression.
    pub lhs_span: RelativeSpan,

    /// The kind of the operator.
    pub operator_kind: BinaryOperatorKind,

    /// The type of the left-hand side expression.
    pub lhs_type: Type,

    /// Mapping for rendering type inferences
    pub type_inference_map: InferenceRenderingMap<Type>,

    /// Mapping for rendering constant inferences
    pub constant_inference_map: InferenceRenderingMap<Constant>,
}

impl Report for InvalidTypeInBinaryOperator {
    async fn report(
        &self,
        parameter: &pernixc_query::TrackedEngine,
    ) -> Result<
        pernixc_diagnostic::Rendered<pernixc_diagnostic::ByteIndex>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        let mut message = match self.operator_kind {
            BinaryOperatorKind::Arithmetic => {
                "the left-hand side of an arithmetic operator must be a number \
                 or pointer (via pointer arithmetic), but found `"
                    .to_string()
            }
            BinaryOperatorKind::Relational => "the left-hand side of a \
                                               relational operator must be a \
                                               number or a `bool`, but found `"
                .to_string(),
            BinaryOperatorKind::Bitwise => "the left-hand side of a bitwise \
                                            operator must be an integer, but \
                                            found `"
                .to_string(),
        };

        self.lhs_type
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
                    .span(parameter.to_absolute_span(&self.lhs_span).await)
                    .build(),
            )
            .severity(pernixc_diagnostic::Severity::Error)
            .help_message(match self.operator_kind {
                BinaryOperatorKind::Arithmetic => {
                    "language only supports numeric arithmetic and pointer \
                     arithmetic"
                }
                BinaryOperatorKind::Relational => {
                    "only a number or `bool` type can be used with relational \
                     operators"
                }
                BinaryOperatorKind::Bitwise => {
                    "only an integer type can be used with bitwise operators"
                }
            })
            .build())
    }
}

/// Not all control flow paths in a function return a value.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, StableHash, Serialize, Deserialize,
)]
pub struct NotAllFlowPathsReturnAValue {
    /// The ID of the callable that does not return a value on all paths.
    pub callable_id: Global<pernixc_symbol::ID>,
}

impl Report for NotAllFlowPathsReturnAValue {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let span = engine
            .to_absolute_span(&engine.get_span(self.callable_id).await.unwrap())
            .await;

        let qualified_name = engine.get_qualified_name(self.callable_id).await;

        Ok(pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::builder().span(span).build())
            .severity(Severity::Error)
            .message(format!(
                "not all control flow paths in function `{qualified_name}` \
                 return a value"
            ))
            .help_message(
                "consider adding a return statement to all control flow paths",
            )
            .build())
    }
}

/// Return expression is used outside the function.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, StableHash, Serialize, Deserialize,
)]
pub struct ReturnIsNotAllowed {
    /// The span of the return expression.
    pub return_span: RelativeSpan,
}

impl Report for ReturnIsNotAllowed {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Rendered::builder()
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.return_span).await)
                    .build(),
            )
            .severity(Severity::Error)
            .message("`return` is only allowed inside a function")
            .help_message("consider removing the `return` statement")
            .build())
    }
}

/// A type annotation is required for a generic type parameter.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, StableHash, Serialize, Deserialize,
)]
pub struct TypeAnnotationRequired {
    /// The span where the type annotation is required.
    pub span: RelativeSpan,

    /// The generic parameter that requires a type annotation.
    pub generic_parameter: TypeParameterID,
}

impl Report for TypeAnnotationRequired {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let qualified_name =
            engine.get_qualified_name(self.generic_parameter.parent_id).await;

        let generic_parameters = engine
            .get_generic_parameters(self.generic_parameter.parent_id)
            .await?;

        let ty_parameter =
            &generic_parameters.types()[self.generic_parameter.id];

        let message = format!(
            "type annotation is required for generic parameter `{}` of `{}`",
            ty_parameter.name, qualified_name
        );

        Ok(pernixc_diagnostic::Rendered::builder()
            .message(message)
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.span).await)
                    .build(),
            )
            .severity(pernixc_diagnostic::Severity::Error)
            .help_message(format!(
                "consider adding a type annotation using syntax such as \
                 `{qualified_name}[Type]`"
            ))
            .build())
    }
}

/// A constant argument annotation is required for a generic constant parameter.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, StableHash, Serialize, Deserialize,
)]
pub struct ConstantAnnotationRequired {
    /// The span where the type annotation is required.
    pub span: RelativeSpan,

    /// The generic parameter that requires a type annotation.
    pub generic_parameter: ConstantParameterID,
}

impl Report for ConstantAnnotationRequired {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let qualified_name =
            engine.get_qualified_name(self.generic_parameter.parent_id).await;

        let generic_parameters = engine
            .get_generic_parameters(self.generic_parameter.parent_id)
            .await?;

        let const_parameter =
            &generic_parameters.constants()[self.generic_parameter.id];

        let message = format!(
            "constant annotation is required for generic parameter `{}` of \
             `{}`",
            const_parameter.name, qualified_name
        );

        Ok(pernixc_diagnostic::Rendered::builder()
            .message(message)
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.span).await)
                    .build(),
            )
            .severity(pernixc_diagnostic::Severity::Error)
            .help_message(format!(
                "consider adding a constant annotation using syntax such as \
                 `{qualified_name}[ {{ EXPR }} ]`"
            ))
            .build())
    }
}
