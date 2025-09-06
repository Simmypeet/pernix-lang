use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::r#type::Type;

use crate::{
    binder::{
        report_as_type_calculating_overflow, report_as_type_check_overflow,
        type_check::diagnostic::{CyclicInference, MismatchedType},
        Binder, UnrecoverableError,
    },
    diagnostic::Diagnostic,
    inference_context::{
        constraint::{self, Constraint},
        UnifyError,
    },
};

pub mod diagnostic;

/// An enumeration of either a known type or an inferring type.
///
/// This is used for type checking and type inference.
#[derive(
    Debug,
    Clone,
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
pub enum Expected {
    Known(Type),
    Constraint(constraint::Type),
}

impl Binder<'_> {
    /// Performs type checking on the given `ty` and returns a diagnostic if
    /// there is an error.
    pub async fn type_check_as_diagnostic(
        &mut self,
        ty: &Type,
        expected_ty: Expected,
        type_check_span: RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Option<Diagnostic>, UnrecoverableError> {
        let environment = self.create_environment();

        // simplify the types
        let simplified_ty =
            environment.simplify(ty.clone()).await.map_err(|x| {
                x.report_as_type_calculating_overflow(
                    type_check_span.clone(),
                    handler,
                )
            })?;

        match expected_ty {
            Expected::Known(expected_ty) => {
                let simplified_expected =
                    environment.simplify(expected_ty).await.map_err(|x| {
                        x.report_as_type_calculating_overflow(
                            type_check_span.clone(),
                            handler,
                        )
                    })?;

                let error: Option<Diagnostic> = match self
                    .inference_context
                    .unify(
                        &simplified_ty.result,
                        &simplified_expected.result,
                        &self.premise,
                        self.engine,
                    )
                    .await
                {
                    Ok(()) => None,

                    Err(
                        UnifyError::CyclicTypeInference(_)
                        | UnifyError::CyclicConstantInference(_),
                    ) => Some(
                        diagnostic::Diagnostic::CyclicInference(
                            CyclicInference {
                                first: simplified_ty.result.clone(),
                                second: simplified_expected.result.clone(),
                                span: type_check_span,
                                type_inference_map: self
                                    .inference_context
                                    .type_table()
                                    .get_inference_rendering_map(),
                                constant_inference_map: self
                                    .inference_context
                                    .const_table()
                                    .get_inference_rendering_map(),
                            },
                        )
                        .into(),
                    ),

                    Err(UnifyError::TypeSystem(type_system_error)) => {
                        return Err(type_system_error
                            .report_as_type_check_overflow(
                                type_check_span.clone(),
                                handler,
                            ));
                    }

                    Err(
                        UnifyError::IncompatibleTypes { .. }
                        | UnifyError::IncompatibleConstants { .. }
                        | UnifyError::UnsatisfiedConstraint(_)
                        | UnifyError::CombineConstraint(_),
                    ) => Some(
                        diagnostic::Diagnostic::MismatchedType(
                            MismatchedType {
                                expected_type: Expected::Known(
                                    simplified_expected.result.clone(),
                                ),
                                found_type: simplified_ty.result.clone(),
                                span: type_check_span,
                                type_inference_map: self
                                    .inference_context
                                    .type_table()
                                    .get_inference_rendering_map(),
                                constant_inference_map: self
                                    .inference_context
                                    .const_table()
                                    .get_inference_rendering_map(),
                            },
                        )
                        .into(),
                    ),
                };

                // report the error
                Ok(error)
            }
            Expected::Constraint(constraint) => {
                let result = if let Type::Inference(inference_var) =
                    &simplified_ty.result
                {
                    self.inference_context
                        .unify_with_constraint(*inference_var, constraint)
                        .is_ok()
                } else {
                    constraint.satisfies(&simplified_ty.result)
                };

                // report the error
                if result {
                    Ok(None)
                } else {
                    Ok(Some(
                        diagnostic::Diagnostic::MismatchedType(
                            MismatchedType {
                                expected_type: Expected::Constraint(constraint),
                                found_type: simplified_ty.result.clone(),
                                span: type_check_span,
                                type_inference_map: self
                                    .inference_context
                                    .type_table()
                                    .get_inference_rendering_map(),
                                constant_inference_map: self
                                    .inference_context
                                    .const_table()
                                    .get_inference_rendering_map(),
                            },
                        )
                        .into(),
                    ))
                }
            }
        }
    }

    /// Performs type checking on the given `ty`.
    ///
    /// This function performs type inference as well as type checking. Any
    /// error, error found will make the binder suboptimal.
    ///
    /// # Parameters
    ///
    /// - `ty`: The type to check.
    /// - `expected_ty`: The type or constraint that `ty` should satisfy.
    /// - `type_check_span`: The span of the type check. This is used for error
    ///   reoprting.
    /// - `handler`: The handler to report errors to.
    ///
    /// # Panics
    ///
    /// This function panics if an unregistered inference variable is found.
    pub async fn type_check(
        &mut self,
        ty: &Type,
        expected_ty: Expected,
        type_check_span: RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<bool, UnrecoverableError> {
        let error = self
            .type_check_as_diagnostic(ty, expected_ty, type_check_span, handler)
            .await?;

        error.map_or(Ok(true), |error| {
            handler.receive(error);
            Ok(false)
        })
    }
}
