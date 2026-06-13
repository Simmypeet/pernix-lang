use std::convert::Infallible;

use pernixc_type::{
    generic_parameters::GenericParameterID,
    predicate::Subtype,
    substitution::{Substitutable, Substitution},
    r#type::{
        Type,
        constructor::{
            Application, Constructor,
            rewrite::{
                AsyncTypeRewriter, RewriteContext, rewrite_type_or_clone_async,
            },
        },
        context::TyContext,
        inference::InferenceVariable,
        kind::TyKind,
        skolem::SkolemizedVariable,
    },
    variance::Variance,
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{
        Agree, DoOccurCheck, OverflowError, Provisional, Solve, Solver,
        universe::UniverseIndex,
    },
};

mod application;

#[cfg(test)]
mod test;

pub type Step = (Substitution, Vec<Subtype>, Constraints);

enum BindInferenceVariableSubtype {
    Bound(Step),
    Failed,
    NotApplicable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum InferenceVariableSubtypeSide {
    Lesser,
    Greater,
}

impl Agree for Step {
    fn agree(&self, other: &Self) -> bool {
        let (subst1, subtypes1, constrs1) = self;
        let (subst2, subtypes2, constrs2) = other;
        Agree::agree(subst1, subst2)
            && subtypes1 == subtypes2
            && Agree::agree(constrs1, constrs2)
    }
}

impl Solve for Subtype {
    type Result = Option<Step>;

    async fn solve(
        &self,
        solver: &mut Solver<'_>,
    ) -> Result<Self::Result, OverflowError> {
        solver.solve_impl(self).await
    }

    fn provisional_result(&self) -> Provisional<Self::Result> {
        Provisional::Continue(None)
    }
}

impl Solver<'_> {
    // due to the weird cyclic query error in rustc, we have to define the
    // implementation with `+ Send` bounds
    #[allow(clippy::manual_async_fn)]
    fn solve_impl(
        &mut self,
        subtype: &Subtype,
    ) -> impl Future<Output = Result<Option<Step>, OverflowError>> + Send {
        async move {
            // if they are syntactically equal, we are done.
            if subtype.lesser() == subtype.greater() {
                return Ok(Some((
                    Substitution::new(),
                    Vec::new(),
                    Constraints::default(),
                )));
            }

            match self.bind_inference_variable_subtype(subtype).await? {
                BindInferenceVariableSubtype::Bound(step) => {
                    return Ok(Some(step));
                }
                BindInferenceVariableSubtype::Failed => return Ok(None),
                BindInferenceVariableSubtype::NotApplicable => {}
            }

            // if they are both lifetimes, return constraints according to the
            // variance
            if !subtype.lesser().is_bound_variable()
                && !subtype.greater().is_bound_variable()
                && self.kind_of(subtype.lesser()).await.is_lifetime()
                && self.kind_of(subtype.greater()).await.is_lifetime()
            {
                let constraints = match subtype.variance() {
                    Variance::Covariant => Constraints::lifetimes_outlives(
                        subtype.lesser().clone(),
                        subtype.greater().clone(),
                    ),
                    Variance::Contravariant => Constraints::lifetimes_outlives(
                        subtype.greater().clone(),
                        subtype.lesser().clone(),
                    ),
                    Variance::Invariant => Constraints::lifetimes_eq(
                        subtype.lesser().clone(),
                        subtype.greater().clone(),
                    ),
                    Variance::Bivariant => Constraints::default(),
                };

                return Ok(Some((
                    Substitution::new(),
                    Vec::new(),
                    constraints,
                )));
            }

            match (&**subtype.lesser(), &**subtype.greater()) {
                // this is a tough one
                (Type::Application(left_ap), Type::Application(right_ap)) => {
                    self.handle_application(
                        subtype.lesser(),
                        subtype.greater(),
                        left_ap,
                        right_ap,
                        subtype.variance(),
                    )
                    .await
                }

                (_, _) => {
                    Box::pin(self.try_reduce(
                        subtype.lesser(),
                        subtype.greater(),
                        subtype.variance(),
                    ))
                    .await
                }
            }
        }
    }

    async fn bind_inference_variable_subtype(
        &mut self,
        subtype: &Subtype,
    ) -> Result<BindInferenceVariableSubtype, OverflowError> {
        let Some((infer_var, binding_target, side)) =
            self.inference_variable_subtype_binding_target(subtype).await
        else {
            return Ok(BindInferenceVariableSubtype::NotApplicable);
        };

        self.bind_inference_variable_to_target(
            infer_var,
            binding_target,
            side,
            subtype.variance(),
        )
        .await
    }

    async fn inference_variable_subtype_binding_target(
        &mut self,
        subtype: &Subtype,
    ) -> Option<(InferenceVariable, Interned<Type>, InferenceVariableSubtypeSide)>
    {
        if let Type::InferenceVariable(infer_var) = &**subtype.lesser()
            && self.can_bind_inference_variable_to(subtype.greater()).await
        {
            return Some((
                *infer_var,
                subtype.greater().clone(),
                InferenceVariableSubtypeSide::Lesser,
            ));
        }

        if let Type::InferenceVariable(infer_var) = &**subtype.greater()
            && self.can_bind_inference_variable_to(subtype.lesser()).await
        {
            return Some((
                *infer_var,
                subtype.lesser().clone(),
                InferenceVariableSubtypeSide::Greater,
            ));
        }

        None
    }

    async fn can_bind_inference_variable_to(
        &mut self,
        target: &Interned<Type>,
    ) -> bool {
        !target.is_bound_variable()
            && !target.is_inference_variable()
            && !self.kind_of(target).await.is_lifetime()
    }

    async fn bind_inference_variable_to_target(
        &mut self,
        infer_var: InferenceVariable,
        binding_target: Interned<Type>,
        side: InferenceVariableSubtypeSide,
        variance: Variance,
    ) -> Result<BindInferenceVariableSubtype, OverflowError> {
        if !self
            .can_bind_inference_variable_to_type(
                infer_var,
                &binding_target,
                DoOccurCheck::Yes,
            )
            .await
        {
            return Ok(BindInferenceVariableSubtype::Failed);
        }

        let Type::Application(_) = &*binding_target else {
            let subst = Substitution::singleton(infer_var, binding_target);

            return Ok(BindInferenceVariableSubtype::Bound((
                subst,
                Vec::new(),
                Constraints::default(),
            )));
        };

        let binding_universe = self.get_inference_variable_universe(infer_var);
        let intermediate_application = self
            .freshen_application_inference_variables(
                &binding_target,
                binding_universe,
            )
            .await;

        let subst = Substitution::singleton(
            infer_var,
            intermediate_application.clone(),
        );

        let subtype_problem = match side {
            InferenceVariableSubtypeSide::Lesser => Subtype::new(
                intermediate_application.clone(),
                binding_target,
                variance,
            ),
            InferenceVariableSubtypeSide::Greater => Subtype::new(
                binding_target,
                intermediate_application.clone(),
                variance,
            ),
        };

        let Some((mut new_subst, subtypes, constraints)) =
            Box::pin(self.solve(&subtype_problem)).await?
        else {
            return Ok(BindInferenceVariableSubtype::Failed);
        };

        self.compose_subst(&mut new_subst, subst);

        Ok(BindInferenceVariableSubtype::Bound((
            new_subst,
            subtypes,
            constraints,
        )))
    }

    async fn freshen_application_inference_variables(
        &mut self,
        ty: &Interned<Type>,
        universe: UniverseIndex,
    ) -> Interned<Type> {
        let engine = self.engine();
        let mut rewriter =
            FreshInferenceVariableRewriter { solver: self, universe };

        rewrite_type_or_clone_async(ty, &mut rewriter, engine)
            .await
            .unwrap_or_else(|err| match err {})
    }
}

struct FreshInferenceVariableRewriter<'solver, 'engine> {
    solver: &'solver mut Solver<'engine>,
    universe: UniverseIndex,
}

impl AsyncTypeRewriter for FreshInferenceVariableRewriter<'_, '_> {
    type Error = Infallible;

    async fn rewrite_application(
        &mut self,
        application: &Application,
        _: RewriteContext,
    ) -> Result<Option<Interned<Type>>, Self::Error> {
        if let Constructor::Lifetime(_) = application.constructor() {
            return Ok(Some(self.fresh_inference_variable(TyKind::Lifetime)));
        }

        Ok(None)
    }

    async fn rewrite_inference_variable(
        &mut self,
        variable: InferenceVariable,
        _: RewriteContext,
    ) -> Result<Option<Interned<Type>>, Self::Error> {
        let kind = self.solver.get_inference_variable_kind(&variable);

        Ok(Some(self.fresh_inference_variable(kind)))
    }

    async fn rewrite_generic_parameter(
        &mut self,
        id: GenericParameterID,
        _: RewriteContext,
    ) -> Result<Option<Interned<Type>>, Self::Error> {
        let ty = Type::GenericParameter(id);

        Ok(self
            .solver
            .kind_of(&ty)
            .await
            .is_lifetime()
            .then(|| self.fresh_inference_variable(TyKind::Lifetime)))
    }

    async fn rewrite_skolemized_variable(
        &mut self,
        variable: SkolemizedVariable,
        _: RewriteContext,
    ) -> Result<Option<Interned<Type>>, Self::Error> {
        let kind = self.solver.get_skolemized_variable_kind(&variable);

        Ok(kind.is_lifetime().then(|| self.fresh_inference_variable(kind)))
    }
}

impl FreshInferenceVariableRewriter<'_, '_> {
    fn fresh_inference_variable(&mut self, kind: TyKind) -> Interned<Type> {
        let fresh_var = self
            .solver
            .fresh_inference_variable_in_universe(kind, self.universe);

        self.solver.intern(Type::InferenceVariable(fresh_var))
    }
}

impl Solver<'_> {
    async fn try_reduce(
        &mut self,
        lesser: &Interned<Type>,
        greater: &Interned<Type>,
        variance: Variance,
    ) -> Result<Option<Step>, OverflowError> {
        // lazily reduce the lesser type and try again.
        if let Some((reduced_lesser, constrs)) =
            self.reduce_type(lesser.clone()).await?
            && let Some((subst, sub_problem, new_constrs)) = self
                .solve(&Subtype::new(reduced_lesser, greater.clone(), variance))
                .await?
        {
            return Ok(Some((
                subst,
                sub_problem,
                constrs.union_into(new_constrs),
            )));
        }

        // lazily reduce the greater type and try again.
        if let Some((reduced_greater, constrs)) =
            self.reduce_type(greater.clone()).await?
            && let Some((subst, sub_problem, new_constrs)) = self
                .solve(&Subtype::new(lesser.clone(), reduced_greater, variance))
                .await?
        {
            return Ok(Some((
                subst,
                sub_problem,
                constrs.union_into(new_constrs),
            )));
        }

        // otherwise, we have no information to learn from this
        Ok(None)
    }

    /// Exhaustively resolves a batch of subtype constraints.
    ///
    /// Each subtype constraint is solved into a step, then the learned
    /// substitution and emitted constraints are accumulated. Constraints that
    /// cannot make progress in the current round are returned in the `Step`
    /// instead of causing the whole resolution to fail. If a round learns
    /// something, those residual subtype constraints are rewritten once with
    /// the accumulated substitution before the next round starts.
    pub async fn resolve_subtypes(
        &mut self,
        mut subtypes: Vec<Subtype>,
    ) -> Result<Step, OverflowError> {
        let mut constraints = Constraints::default();
        let mut substitution = Substitution::new();

        loop {
            let mut residual_subtypes = Vec::new();
            let mut has_progress = false;

            while let Some(subtype) = subtypes.pop() {
                // A stuck subtype is kept for the next round. It may become
                // solvable after this round's substitutions are composed.
                let Some((
                    mut step_substitution,
                    new_subtypes,
                    new_constraints,
                )) = Box::pin(self.solve(&subtype)).await?
                else {
                    residual_subtypes.push(subtype);
                    continue;
                };

                has_progress = true;

                // Lifetime constraints are accumulated as emitted; callers
                // receive the learned substitution separately.
                constraints = constraints.union_into(new_constraints);

                // Newly decomposed subtype problems are solved in the same
                // batch without rewriting them by the step substitution.
                subtypes.extend(new_subtypes);

                // Preserve composition order so the returned substitution
                // represents everything learned by all successful steps.
                self.compose_subst(&mut step_substitution, substitution);
                substitution = step_substitution;
            }

            if residual_subtypes.is_empty() {
                return Ok((substitution, Vec::new(), constraints));
            }

            if !has_progress {
                return Ok((substitution, residual_subtypes, constraints));
            }

            // Normalize stuck constraints once per round, not after each
            // individual step, to keep the worklist updates batched.
            for residual_subtype in &mut residual_subtypes {
                *residual_subtype = residual_subtype
                    .apply_or_clone(&substitution, self.engine());
            }

            subtypes = residual_subtypes;
        }
    }
}
