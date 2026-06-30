use pernixc_qbice::Interner;
use pernixc_type::{
    substitution::{Substitutable, Substitution},
    r#type::{
        Type2, constructor::DestructureOptions, inference::InferenceVariable,
    },
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{DoOccurCheck, OverflowError, Provisional, Solve, Solver},
};

#[cfg(test)]
mod test;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unify {
    left: Interned<Type2>,
    right: Interned<Type2>,
}

impl Unify {
    #[must_use]
    pub const fn new(left: Interned<Type2>, right: Interned<Type2>) -> Self {
        Self { left, right }
    }

    #[must_use]
    pub const fn left(&self) -> &Interned<Type2> { &self.left }

    #[must_use]
    pub const fn right(&self) -> &Interned<Type2> { &self.right }
}

impl Substitutable for Unify {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self> {
        match (
            self.left.apply(subst, interner),
            self.right.apply(subst, interner),
        ) {
            (Some(left), Some(right)) => Some(Self { left, right }),
            (Some(left), None) => {
                Some(Self { left, right: self.right.clone() })
            }
            (None, Some(right)) => {
                Some(Self { left: self.left.clone(), right })
            }
            (None, None) => None,
        }
    }
}

impl Solver<'_> {
    pub async fn unify(
        &mut self,
        left: Interned<Type>,
        right: Interned<Type>,
    ) -> Result<Option<(Substitution, Constraints)>, OverflowError> {
        self.solve(&Unify::new(left, right)).await
    }
}

impl Solve for Unify {
    type Result = Option<(Substitution, Constraints)>;

    async fn solve(
        &self,
        solver: &mut Solver<'_>,
    ) -> Result<Self::Result, OverflowError> {
        solver.solve_unify_impl(self).await
    }

    fn provisional_result(&self) -> Provisional<Self::Result> {
        Provisional::Continue(None)
    }
}

enum BindInferenceVariable {
    Bound(Substitution, Constraints),
    Failed,
    NotApplicable,
}

impl Solver<'_> {
    #[allow(clippy::manual_async_fn)]
    fn solve_unify_impl(
        &mut self,
        unify: &Unify,
    ) -> impl Future<
        Output = Result<Option<(Substitution, Constraints)>, OverflowError>,
    > + Send {
        async move {
            if unify.left == unify.right {
                return Ok(Some((Substitution::new(), Constraints::default())));
            }

            match self.bind_inference_variable_unify(unify).await? {
                BindInferenceVariable::Bound(substitution, constraints) => {
                    return Ok(Some((substitution, constraints)));
                }
                BindInferenceVariable::Failed => return Ok(None),
                BindInferenceVariable::NotApplicable => {}
            }

            if !unify.left.is_bound_variable()
                && !unify.right.is_bound_variable()
                && self.kind_of(&unify.left).await.is_lifetime()
                && self.kind_of(&unify.right).await.is_lifetime()
            {
                return Ok(Some((
                    Substitution::new(),
                    Constraints::lifetimes_eq(
                        unify.left.clone(),
                        unify.right.clone(),
                    ),
                )));
            }

            match (&*unify.left, &*unify.right) {
                (Type2::Application(left_ap), Type2::Application(right_ap)) => {
                    let Some(iter) = left_ap.destructure(
                        right_ap,
                        DestructureOptions::require_equal_binders(),
                        self.engine(),
                    ) else {
                        return Box::pin(self.try_reduce_unify(unify)).await;
                    };

                    let mut substitution = Substitution::new();
                    let mut constraints = Constraints::default();

                    for (left, right) in iter {
                        let left =
                            left.apply_or_clone(&substitution, self.engine());
                        let right =
                            right.apply_or_clone(&substitution, self.engine());

                        let Some((mut step_substitution, new_constraints)) =
                            Box::pin(self.solve(&Unify::new(left, right)))
                                .await?
                        else {
                            return Box::pin(self.try_reduce_unify(unify))
                                .await;
                        };

                        constraints = constraints.union_into(new_constraints);
                        step_substitution.compose(substitution, self.engine());
                        substitution = step_substitution;
                    }

                    Ok(Some((substitution, constraints)))
                }

                (
                    Type2::BoundVariable(_)
                    | Type2::GenericParameter(_)
                    | Type2::SkolemizedVariable(_),
                    _,
                )
                | (
                    _,
                    Type2::BoundVariable(_)
                    | Type2::GenericParameter(_)
                    | Type2::SkolemizedVariable(_),
                ) => Box::pin(self.try_reduce_unify(unify)).await,

                (Type2::InferenceVariable(_), _)
                | (_, Type2::InferenceVariable(_)) => {
                    unreachable!("inference variables are handled earlier")
                }
            }
        }
    }

    async fn bind_inference_variable_unify(
        &mut self,
        unify: &Unify,
    ) -> Result<BindInferenceVariable, OverflowError> {
        if let Type2::InferenceVariable(infer_var) = &*unify.left {
            return self
                .bind_inference_variable_to_unify_target(
                    *infer_var,
                    unify.right.clone(),
                )
                .await;
        }

        if let Type2::InferenceVariable(infer_var) = &*unify.right {
            return self
                .bind_inference_variable_to_unify_target(
                    *infer_var,
                    unify.left.clone(),
                )
                .await;
        }

        Ok(BindInferenceVariable::NotApplicable)
    }

    async fn bind_inference_variable_to_unify_target(
        &mut self,
        infer_var: InferenceVariable,
        target: Interned<Type2>,
    ) -> Result<BindInferenceVariable, OverflowError> {
        if !self
            .can_bind_inference_variable_to_type(
                infer_var,
                &target,
                DoOccurCheck::Yes,
            )
            .await
        {
            return Ok(BindInferenceVariable::Failed);
        }

        Ok(BindInferenceVariable::Bound(
            Substitution::singleton(infer_var, target),
            Constraints::default(),
        ))
    }

    async fn try_reduce_unify(
        &mut self,
        unify: &Unify,
    ) -> Result<Option<(Substitution, Constraints)>, OverflowError> {
        if let Some((reduced_left, constrs)) =
            self.reduce_type(unify.left.clone()).await?
            && let Some((subst, new_constrs)) = self
                .solve(&Unify::new(reduced_left, unify.right.clone()))
                .await?
        {
            return Ok(Some((subst, constrs.union_into(new_constrs))));
        }

        if let Some((reduced_right, constrs)) =
            self.reduce_type(unify.right.clone()).await?
            && let Some((subst, new_constrs)) = self
                .solve(&Unify::new(unify.left.clone(), reduced_right))
                .await?
        {
            return Ok(Some((subst, constrs.union_into(new_constrs))));
        }

        Ok(None)
    }

    pub async fn resolve_unifications(
        &mut self,
        mut unifications: Vec<Unify>,
    ) -> Result<(Substitution, Vec<Unify>, Constraints), OverflowError> {
        let mut constraints = Constraints::default();
        let mut substitution = Substitution::new();

        loop {
            let mut residual_unifications = Vec::new();
            let mut has_progress = false;

            while let Some(unify) = unifications.pop() {
                let Some((mut step_substitution, new_constraints)) =
                    Box::pin(self.solve(&unify)).await?
                else {
                    residual_unifications.push(unify);
                    continue;
                };

                has_progress = true;
                constraints = constraints.union_into(new_constraints);

                step_substitution.compose(substitution, self.engine());
                substitution = step_substitution;
            }

            if residual_unifications.is_empty() {
                return Ok((substitution, Vec::new(), constraints));
            }

            if !has_progress {
                return Ok((substitution, residual_unifications, constraints));
            }

            for residual_unify in &mut residual_unifications {
                *residual_unify =
                    residual_unify.apply_or_clone(&substitution, self.engine());
            }

            unifications = residual_unifications;
        }
    }
}
