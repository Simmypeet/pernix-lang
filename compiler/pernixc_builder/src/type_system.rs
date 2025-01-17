//! Contains the extension trait for the environment for interacting with the
//! type system and the term resolution.

use std::collections::BTreeSet;

use diagnostic::{OverflowOperation, TypeSystemOverflow, UnsatisfiedPredicate};
use pernixc_handler::Handler;
use pernixc_source_file::Span;
use pernixc_table::{
    component::SymbolKind, diagnostic::Diagnostic, query, GlobalID, Table,
};
use pernixc_term::{
    predicate::Predicate, r#type::Type, where_clause::WhereClause, Model,
};
use pernixc_type_system::{
    environment::{Environment, Premise},
    normalizer::Normalizer,
    simplify::Simplify,
    AbruptError, LifetimeConstraint,
};

pub mod diagnostic;

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

            Ok(None) => ty.clone(),

            Err(AbruptError::CyclicDependency(error)) => {
                handler.receive(Box::new(error));
                ty.clone()
            }

            Err(AbruptError::Overflow(error)) => {
                handler.receive(Box::new(TypeSystemOverflow::new(
                    OverflowOperation::TypeOf,
                    type_span.clone(),
                    error,
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
                        Ok(Some(_)) => { /* do nothing */ }
                        Ok(None) => {
                            handler.receive(Box::new(UnsatisfiedPredicate {
                                predicate: Predicate::LifetimeOutlives(
                                    outlives.clone(),
                                ),
                                instantiation_span: span.clone(),
                                predicate_declaration_span: None,
                            }));
                        }
                        Err(AbruptError::CyclicDependency(error)) => {
                            handler.receive(Box::new(error));
                        }
                        Err(AbruptError::Overflow(error)) => {
                            handler.receive(Box::new(TypeSystemOverflow::new(
                                OverflowOperation::TypeCheck,
                                span.clone(),
                                error,
                            )));
                        }
                    }
                }
            }
        }
    }
}

/// An extension trait for the table for interacting with the type system .
pub trait TableExt {
    /// Retrieves the active premise of the current site.
    ///
    /// # Parameters
    ///
    /// - `current_site`: The current site to get the active premise of.
    /// - `handler`: The handler to report the cyclic dependency error to.
    fn get_active_premise<M: Model>(
        &self,
        current_site: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Premise<M>;
}

impl TableExt for Table {
    fn get_active_premise<M: Model>(
        &self,
        current_site: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Premise<M> {
        let mut premise = Premise {
            predicates: BTreeSet::new(),
            query_site: Some(current_site),
        };

        for id in self.scope_walker(current_site) {
            let current_id = GlobalID::new(current_site.target_id, id);
            let kind = *self.get::<SymbolKind>(current_id).unwrap();

            if !kind.has_where_clause() {
                continue;
            }

            match self.query::<WhereClause>(current_id) {
                Ok(where_clause) => {
                    premise.predicates.extend(
                        where_clause.predicates.iter().map(|x| {
                            Predicate::from_other_model(x.predicate.clone())
                        }),
                    );
                }

                Err(query::Error::CyclicDependency(error)) => {
                    handler.receive(Box::new(error));
                }

                err @ Err(
                    query::Error::SymbolNotFoundOrInvalidComponent
                    | query::Error::NoBuilderFound,
                ) => {
                    panic!("unexpected error {err:?}");
                }
            }
        }

        premise
    }
}
