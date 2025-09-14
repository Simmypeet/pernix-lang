#![allow(missing_docs)]

use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::source_map::to_absolute_span;
use pernixc_term::{
    constant::Constant,
    display::{Display, InferenceRenderingMap},
    r#type::Type,
};

use crate::{binder::type_check::Expected, diagnostic_enum};

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
        CyclicInference(CyclicInference),
        MismatchedType(MismatchedType),
    }
}

/// Found a cyclic inference in the type checking.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct CyclicInference {
    /// The type that is involved in the cyclic inference.
    pub first: Type,

    /// The type that is involved in the cyclic inference.
    pub second: Type,

    /// The span where the type check occurred.
    pub span: RelativeSpan,

    /// The inference rendering map for constants.
    pub constant_inference_map: InferenceRenderingMap<Constant>,

    /// The inference rendering map for types.
    pub type_inference_map: InferenceRenderingMap<Type>,
}

impl Report for CyclicInference {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let span = engine.to_absolute_span(&self.span).await;

        // Format types with inference rendering maps
        let mut first_type_str = String::new();
        let mut second_type_str = String::new();

        let _ = self
            .first
            .write_async_with_mapping(
                engine,
                &mut first_type_str,
                None,
                Some(&self.type_inference_map),
                Some(&self.constant_inference_map),
            )
            .await;

        let _ = self
            .second
            .write_async_with_mapping(
                engine,
                &mut second_type_str,
                None,
                Some(&self.type_inference_map),
                Some(&self.constant_inference_map),
            )
            .await;

        Ok(pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("cyclic inference detected in type checking")
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "cannot infer type due to circular dependency between \
                         '{first_type_str}' and '{second_type_str}'"
                    ))
                    .span(span)
                    .build(),
            )
            .help_message(
                "try adding explicit type annotations to break the cycle",
            )
            .build())
    }
}

/// Mismatched type error.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct MismatchedType {
    /// The expected type.
    pub expected_type: Expected,

    /// The found type.
    pub found_type: Type,

    /// The sapn where the type check occurred.
    pub span: RelativeSpan,

    /// The inference rendering map for constants.
    pub constant_inference_map: InferenceRenderingMap<Constant>,

    /// The inference rendering map for types.
    pub type_inference_map: InferenceRenderingMap<Type>,
}

impl Report for MismatchedType {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let span = engine.to_absolute_span(&self.span).await;

        // Format expected type with inference rendering maps
        let mut expected_str = String::new();
        match &self.expected_type {
            Expected::Known(ty) => {
                let _ = ty
                    .write_async_with_mapping(
                        engine,
                        &mut expected_str,
                        None,
                        Some(&self.type_inference_map),
                        Some(&self.constant_inference_map),
                    )
                    .await;
            }
            Expected::Constraint(constraint_ty) => {
                // constraint::Type implements Display, so we can format it
                // directly
                use std::fmt::Write;
                let _ = write!(expected_str, "{constraint_ty}");
            }
        }

        // Format found type with inference rendering maps
        let mut found_str = String::new();
        let _ = self
            .found_type
            .write_async_with_mapping(
                engine,
                &mut found_str,
                None,
                Some(&self.type_inference_map),
                Some(&self.constant_inference_map),
            )
            .await;

        Ok(pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message(format!(
                "mismatched types: expected `{expected_str}`, found \
                 `{found_str}`"
            ))
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "expected `{expected_str}`, found `{found_str}`"
                    ))
                    .span(span)
                    .build(),
            )
            .help_message(
                "ensure the expression type matches the expected type",
            )
            .build())
    }
}
