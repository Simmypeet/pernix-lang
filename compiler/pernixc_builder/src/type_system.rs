//! Contains the extension trait for the environment for interacting with the
//! type system and the term resolution.

use pernixc_abort::Abort;
use pernixc_handler::Handler;
use pernixc_source_file::Span;
use pernixc_semantic::diagnostic::Diagnostic;
use pernixc_term::{predicate::Predicate, r#type::Type, Model};
use pernixc_type_system::{
    diagnostic::UnsatisfiedPredicate, environment::Environment,
    normalizer::Normalizer, simplify::Simplify, Error, LifetimeConstraint,
};

/// An extension trait for the environment for interacting with the type system
/// and the term resolution. This allows simpler uses of the type system.
pub trait EnvironmentExt {
    /// The model type of the environment.
    type Model: Model;

    /// Simplifies the type term followed by checking the symbolic lifetime
    /// constraints. The errors will be reported to the handler.
    fn simplify_and_check_lifetime_constraints(
        &self,
        ty: &Type<Self::Model>,
        type_span: &Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Type<Self::Model>;

    /// Performs symbolic lifetime constraint checking.
    fn check_lifetime_constraints<'a>(
        &self,
        lifetime_constraints: impl IntoIterator<
            Item = &'a LifetimeConstraint<Self::Model>,
        >,
        span: &Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    );
}

impl<M: Model, N: Normalizer<M>> EnvironmentExt for Environment<'_, M, N>
where
    M::LifetimeInference: pernixc_semantic::Display,
    M::TypeInference: pernixc_semantic::Display,
    M::ConstantInference: pernixc_semantic::Display,
{
    type Model = M;

    fn simplify_and_check_lifetime_constraints(
        &self,
        ty: &Type<Self::Model>,
        type_span: &Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Type<M> {
        match self.query(&Simplify(ty.clone())) {
            Ok(Some(result)) => {
                self.check_lifetime_constraints(
                    &result.constraints,
                    type_span,
                    handler,
                );

                result.result.clone()
            }

            Err(Error::Abort(Abort)) | Ok(None) => ty.clone(),

            Err(Error::Overflow(error)) => {
                error.report_as_type_calculating_overflow(
                    type_span.clone(),
                    handler,
                );

                ty.clone()
            }
        }
    }

    fn check_lifetime_constraints<'a>(
        &self,
        lifetime_constraints: impl IntoIterator<
            Item = &'a LifetimeConstraint<Self::Model>,
        >,
        span: &Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        for constraint in lifetime_constraints {
            match constraint {
                LifetimeConstraint::LifetimeOutlives(outlives) => {
                    match self.query(outlives) {
                        Err(Error::Abort(Abort)) | Ok(Some(_)) => {}

                        Ok(None) => {
                            handler.receive(Box::new(UnsatisfiedPredicate {
                                predicate: Predicate::LifetimeOutlives(
                                    outlives.clone(),
                                ),
                                instantiation_span: span.clone(),
                                predicate_declaration_span: None,
                            }));
                        }
                        Err(Error::Overflow(error)) => {
                            error.report_as_undecidable_predicate(
                                Predicate::LifetimeOutlives(outlives.clone()),
                                None,
                                span.clone(),
                                handler,
                            );
                        }
                    }
                }
            }
        }
    }
}
