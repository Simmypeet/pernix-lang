//! Contains the logic for simplifying the type term.

use std::{collections::BTreeSet, sync::Arc};

use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::variance::Variance;
use pernixc_term::{r#type::Type, visitor::AsyncMutable};

use crate::{
    OverflowError, Succeeded,
    diagnostic::Diagnostic,
    environment::{BoxedFuture, Environment, Query},
    lifetime_constraint::LifetimeConstraint,
    normalizer::Normalizer,
    term::Term,
};

struct Visitor<'e, N: Normalizer> {
    environment: &'e Environment<'e, N>,
    lifetime_constraints: BTreeSet<LifetimeConstraint>,
    abrupt_error: Option<OverflowError>,
}

impl<U: Term, N: Normalizer> AsyncMutable<U> for Visitor<'_, N> {
    async fn visit(&mut self, term: &mut U, _: U::Location) -> bool {
        if self.abrupt_error.is_some() {
            return false;
        }

        match self.environment.query(&Simplify(term.clone())).await {
            Ok(succeeded) => {
                *term = succeeded.result.clone();
                self.lifetime_constraints
                    .extend(succeeded.constraints.iter().cloned());

                true
            }

            Err(err) => {
                self.abrupt_error = Some(err);
                false
            }
        }
    }
}

/// A query for simplifying a term by recursively applying the normalization and
/// trait member equality.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Simplify<T: Term>(pub T);

impl<T: Term> Query for Simplify<T> {
    type InProgress = ();
    type Result = Arc<Succeeded<T>>;

    #[allow(clippy::too_many_lines)]
    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
    ) -> BoxedFuture<'x, Self::Result> {
        Box::pin(async move {
            // recursively simplify the term
            let mut new_term = self.0.clone();
            let mut visitor = Visitor {
                environment,
                lifetime_constraints: BTreeSet::new(),
                abrupt_error: None,
            };

            let _ = new_term.accept_one_level_async_mut(&mut visitor).await;
            if let Some(error) = visitor.abrupt_error {
                return Err(error);
            }

            // simplify by equality
            if self.0.as_instance_associated().is_some() {
                let mut equivalent: Option<Succeeded<T>> = None;

                for predicate in
                    environment.premise().predicates.iter().filter_map(
                        T::as_instance_associated_equality_predicate,
                    )
                {
                    let lhs: T = predicate.lhs.clone().into();

                    // check if unifiable
                    let Some(unifier) = environment
                        .subtypes(
                            self.0.clone(),
                            lhs.clone(),
                            Variance::Invariant,
                        )
                        .await?
                    else {
                        continue;
                    };

                    // recursively simplify the equivalent
                    let new_equiv =
                        environment.query(&Self(predicate.rhs.clone())).await?;

                    let new_equiv = Succeeded::with_constraints(
                        new_equiv.result.clone(),
                        new_equiv
                            .constraints
                            .iter()
                            .chain(&unifier.constraints)
                            .cloned()
                            .collect(),
                    );

                    if let Some(existing_equiv) = &mut equivalent {
                        if *existing_equiv != new_equiv {
                            equivalent = None;
                            break;
                        }
                    } else {
                        equivalent = Some(new_equiv);
                    }
                }

                if let Some(mut equivalent) = equivalent {
                    equivalent.constraints.extend(visitor.lifetime_constraints);
                    return Ok(Arc::new(equivalent));
                }
            }

            {
                if let Some(normalization) =
                    new_term.normalize(environment).await?
                {
                    let new_equiv =
                        environment.query(&Self(normalization.result)).await?;

                    let mut new_equiv = Succeeded::with_constraints(
                        new_equiv.result.clone(),
                        new_equiv
                            .constraints
                            .iter()
                            .chain(&normalization.constraints)
                            .cloned()
                            .collect(),
                    );

                    new_equiv.constraints.extend(visitor.lifetime_constraints);
                    new_equiv.constraints.extend(normalization.constraints);

                    return Ok(Arc::new(new_equiv));
                }
            }

            Ok(Arc::new(Succeeded::with_constraints(
                new_term,
                visitor.lifetime_constraints,
            )))
        })
    }

    fn on_cyclic(
        &self,
        (): Self::InProgress,
        (): Self::InProgress,
        _: &[crate::environment::Call<
            crate::environment::DynArc,
            crate::environment::DynArc,
        >],
    ) -> Self::Result {
        Arc::new(Succeeded::new(self.0.clone()))
    }
}

impl<N: Normalizer> Environment<'_, N> {
    /// Shorthand for querying the [`Simplify`].
    pub async fn simplify<T: Term>(
        &self,
        term: T,
    ) -> Result<Arc<Succeeded<T>>, OverflowError> {
        self.query(&Simplify(term)).await
    }

    /// Simplifies a type and checks its lifetime constraints.
    pub async fn simplify_and_check_lifetime_constraints(
        &self,
        ty: &Type,
        type_span: &RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> Type {
        match self.query(&Simplify(ty.clone())).await {
            Ok(result) => {
                self.check_lifetime_constraints(
                    &result.constraints,
                    type_span,
                    handler,
                )
                .await;

                result.result.clone()
            }

            Err(error) => {
                error.report_as_type_calculating_overflow(*type_span, handler);

                pernixc_term::r#type::Type::Error(pernixc_term::error::Error)
            }
        }
    }
}

#[cfg(test)]
mod test;
