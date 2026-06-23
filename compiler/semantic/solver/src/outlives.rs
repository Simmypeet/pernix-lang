use std::future::Future;

use pernixc_type::{
    predicate::{Outlives, Predicate},
    r#type::{
        Type,
        constructor::{Constructor, Lifetime},
    },
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{OverflowError, Provisional, Solve, Solver},
};

impl Solve for Outlives {
    type Result = bool;

    async fn solve(
        &self,
        solver: &mut Solver<'_>,
    ) -> Result<Self::Result, OverflowError> {
        solver.solve_outlives(self).await
    }

    fn provisional_result(&self) -> Provisional<Self::Result> {
        Provisional::Continue(false)
    }
}

impl Solver<'_> {
    /// Returns whether `outlives` is derivable from the current premise.
    ///
    /// A `false` result means that no proof was found. It does not establish
    /// the negation of the relation.
    pub async fn outlives(
        &mut self,
        outlives: &Outlives,
    ) -> Result<bool, OverflowError> {
        self.solve(outlives).await
    }

    #[allow(clippy::manual_async_fn)]
    fn solve_outlives<'a>(
        &'a mut self,
        outlives: &'a Outlives,
    ) -> impl Future<Output = Result<bool, OverflowError>> + Send + 'a {
        async move {
            // The outlives relation is reflexive.
            if outlives.lesser() == outlives.greater() {
                return Ok(true);
            }

            // `'static` outlives every lifetime.
            if is_static_lifetime(outlives.lesser()) {
                return Ok(true);
            }

            // Search the premise graph for a direct or transitive proof.
            if self.outlives_from_premise(outlives).await? {
                return Ok(true);
            }

            match &**outlives.lesser() {
                Type::Application(application) => {
                    match application.constructor() {
                        Constructor::InstanceAssociated(_) => {
                            self.instance_associated_outlives(outlives).await
                        }

                        Constructor::Lifetime(_) => Ok(false),

                        Constructor::Primitive(_)
                        | Constructor::Reference(_)
                        | Constructor::Symbolic(_)
                        | Constructor::Tuple(_)
                        | Constructor::FunctionPointer(_)
                        | Constructor::AnonymousTraitInstance(_) => {
                            self.all_components_outlive(
                                outlives_components(outlives.lesser()),
                                outlives.greater(),
                            )
                            .await
                        }
                    }
                }

                Type::GenericParameter(_)
                | Type::InferenceVariable(_)
                | Type::BoundVariable(_)
                | Type::SkolemizedVariable(_) => Ok(false),
            }
        }
    }

    async fn outlives_from_premise(
        &mut self,
        goal: &Outlives,
    ) -> Result<bool, OverflowError> {
        let lesser_is_lifetime =
            self.kind_of(goal.lesser()).await.is_lifetime();

        for predicate in self.premise_predicates() {
            let Predicate::Outlives(premise) = predicate else {
                continue;
            };

            assert!(
                !contains_inference_variable(premise.lesser())
                    && !contains_inference_variable(premise.greater()),
                "outlives premises must not contain inference variables"
            );

            for component in outlives_components(premise.lesser()) {
                if lesser_is_lifetime {
                    // Lifetime premises form graph edges. Matching distinct
                    // lifetimes with `match_types` would emit equality edges
                    // in both directions and make transitive lookup blow up.
                    if component != *goal.lesser() {
                        continue;
                    }
                } else {
                    let Some((substitution, constraints)) =
                        self.match_types(&component, goal.lesser()).await
                    else {
                        continue;
                    };

                    // Outlives premises are fully symbolic givens, not
                    // patterns. In particular, they must not contain
                    // inference variables that `match_types` could bind
                    // without returning that binding to the caller of this
                    // boolean query.
                    assert!(
                        substitution.iter().next().is_none(),
                        "outlives premises must not contain inference \
                         variables"
                    );

                    if !self.all_constraints_hold(constraints).await? {
                        continue;
                    }
                }

                if Box::pin(self.solve(&Outlives::new(
                    premise.greater().clone(),
                    goal.greater().clone(),
                )))
                .await?
                {
                    return Ok(true);
                }
            }
        }

        Ok(false)
    }

    async fn instance_associated_outlives(
        &mut self,
        outlives: &Outlives,
    ) -> Result<bool, OverflowError> {
        if let Some((reduced, constraints)) =
            self.reduce_type(outlives.lesser().clone()).await?
            && self.all_constraints_hold(constraints).await?
            && Box::pin(
                self.solve(&Outlives::new(reduced, outlives.greater().clone())),
            )
            .await?
        {
            return Ok(true);
        }

        let Type::Application(application) = &**outlives.lesser() else {
            unreachable!("the caller only passes instance-associated types");
        };

        let components = application
            .arguments()
            .iter()
            .flat_map(outlives_components)
            .collect::<Vec<_>>();

        self.all_components_outlive(components, outlives.greater()).await
    }

    async fn all_components_outlive(
        &mut self,
        components: impl IntoIterator<Item = Interned<Type>>,
        greater: &Interned<Type>,
    ) -> Result<bool, OverflowError> {
        for component in components {
            if !Box::pin(self.solve(&Outlives::new(component, greater.clone())))
                .await?
            {
                return Ok(false);
            }
        }

        Ok(true)
    }

    async fn all_constraints_hold(
        &mut self,
        constraints: Constraints,
    ) -> Result<bool, OverflowError> {
        for constraint in constraints {
            if !Box::pin(self.solve(&constraint)).await? {
                return Ok(false);
            }
        }

        Ok(true)
    }
}

fn outlives_components(ty: &Interned<Type>) -> Vec<Interned<Type>> {
    let mut components = Vec::new();
    push_outlives_components(ty, 0, &mut components);
    components
}

fn push_outlives_components(
    ty: &Interned<Type>,
    binder_depth: usize,
    components: &mut Vec<Interned<Type>>,
) {
    match &**ty {
        Type::GenericParameter(_)
        | Type::InferenceVariable(_)
        | Type::SkolemizedVariable(_) => components.push(ty.clone()),

        Type::BoundVariable(variable) => {
            assert!(
                variable.depth() < binder_depth,
                "outlives component traversal found an escaping bound variable"
            );
        }

        Type::Application(application) => match application.constructor() {
            Constructor::Lifetime(_) | Constructor::InstanceAssociated(_) => {
                components.push(ty.clone());
            }

            Constructor::FunctionPointer(_) => {
                for argument in application.arguments().iter() {
                    push_outlives_components(
                        argument,
                        binder_depth + 1,
                        components,
                    );
                }
            }

            Constructor::Primitive(_)
            | Constructor::Reference(_)
            | Constructor::Symbolic(_)
            | Constructor::Tuple(_)
            | Constructor::AnonymousTraitInstance(_) => {
                for argument in application.arguments().iter() {
                    push_outlives_components(
                        argument,
                        binder_depth,
                        components,
                    );
                }
            }
        },
    }
}

const fn is_static_lifetime(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Application(application)
            if matches!(
                application.constructor(),
                Constructor::Lifetime(Lifetime::Static)
            )
    )
}

fn contains_inference_variable(ty: &Interned<Type>) -> bool {
    match &**ty {
        Type::InferenceVariable(_) => true,
        Type::Application(application) => {
            application.arguments().iter().any(contains_inference_variable)
        }
        Type::GenericParameter(_)
        | Type::BoundVariable(_)
        | Type::SkolemizedVariable(_) => false,
    }
}

#[cfg(test)]
mod test;
