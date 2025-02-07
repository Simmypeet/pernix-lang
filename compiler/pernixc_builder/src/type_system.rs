//! Contains the extension trait for the environment for interacting with the
//! type system and the term resolution.

use pernixc_handler::Handler;
use pernixc_source_file::Span;
use pernixc_table::{diagnostic::Diagnostic, query::CyclicDependencyError};
use pernixc_term::{predicate::Predicate, r#type::Type, Model};
use pernixc_type_system::{
    diagnostic::{OverflowOperation, UnsatisfiedPredicate},
    environment::Environment,
    normalizer::Normalizer,
    simplify::Simplify,
    AbruptError, LifetimeConstraint,
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
    M::LifetimeInference: pernixc_table::Display,
    M::TypeInference: pernixc_table::Display,
    M::ConstantInference: pernixc_table::Display,
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

            Err(AbruptError::CyclicDependency(CyclicDependencyError))
            | Ok(None) => ty.clone(),

            Err(AbruptError::Overflow(error)) => {
                handler.receive(Box::new(error.into_diagnostic(
                    OverflowOperation::TypeOf,
                    type_span.clone(),
                )));

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
                        Err(AbruptError::CyclicDependency(
                            CyclicDependencyError,
                        ))
                        | Ok(Some(_)) => {}

                        Ok(None) => {
                            handler.receive(Box::new(UnsatisfiedPredicate {
                                predicate: Predicate::LifetimeOutlives(
                                    outlives.clone(),
                                ),
                                instantiation_span: span.clone(),
                                predicate_declaration_span: None,
                            }));
                        }
                        Err(AbruptError::Overflow(error)) => {
                            handler.receive(Box::new(error.into_diagnostic(
                                OverflowOperation::TypeCheck,
                                span.clone(),
                            )));
                        }
                    }
                }
            }
        }
    }
}
