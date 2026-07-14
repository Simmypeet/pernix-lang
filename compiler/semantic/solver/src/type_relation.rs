use pernixc_type::{
    predicate::Subtype,
    substitution::{Substitutable, Substitution},
    r#type::{Type2, inference::InferenceVariable},
    variance::Variance2,
};
use qbice::{Identifiable, StableHash, storage::intern::Interned};

use crate::{
    constraints::Constraints,
    solver::{Agree, DoOccurCheck, OverflowError, Provisional, Solve, Solver},
};

mod application;
mod generalization;

#[cfg(test)]
mod test;

pub type Step = (Substitution, Interned<[TypeRelation]>, Constraints);

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, StableHash,
)]
struct RelationFlags {
    variance: Variance2,
    lesser_rigid_inference: bool,
    greater_rigid_inference: bool,
    reduce: bool,
}

impl RelationFlags {
    #[must_use]
    pub const fn new(variance: Variance2) -> Self {
        Self {
            variance,
            lesser_rigid_inference: false,
            greater_rigid_inference: false,
            reduce: true,
        }
    }

    #[must_use]
    pub const fn variance(self) -> Variance2 { self.variance }

    #[must_use]
    pub const fn lesser_rigid_inference(self) -> bool {
        self.lesser_rigid_inference
    }

    #[must_use]
    pub const fn greater_rigid_inference(self) -> bool {
        self.greater_rigid_inference
    }

    #[must_use]
    pub const fn reduce(self) -> bool { self.reduce }

    #[must_use]
    pub const fn with_variance(mut self, variance: Variance2) -> Self {
        self.variance = variance;
        self
    }

    #[must_use]
    pub const fn with_lesser_rigid_inference(
        mut self,
        lesser_rigid_inference: bool,
    ) -> Self {
        self.lesser_rigid_inference = lesser_rigid_inference;
        self
    }

    #[must_use]
    pub const fn with_greater_rigid_inference(
        mut self,
        greater_rigid_inference: bool,
    ) -> Self {
        self.greater_rigid_inference = greater_rigid_inference;
        self
    }

    #[must_use]
    pub const fn with_reduce(mut self, reduce: bool) -> Self {
        self.reduce = reduce;
        self
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, StableHash, Identifiable,
)]
pub struct TypeRelation {
    left: Interned<Type2>,
    right: Interned<Type2>,
    flags: RelationFlags,
}

impl TypeRelation {
    #[must_use]
    pub const fn new(
        left: Interned<Type2>,
        right: Interned<Type2>,
        variance: Variance2,
    ) -> Self {
        Self::new_with_flags(left, right, RelationFlags::new(variance))
    }

    #[must_use]
    pub const fn new_rigid(
        left: Interned<Type2>,
        right: Interned<Type2>,
        variance: Variance2,
    ) -> Self {
        Self::new_with_flags(
            left,
            right,
            RelationFlags::new(variance)
                .with_lesser_rigid_inference(true)
                .with_greater_rigid_inference(true),
        )
    }

    #[must_use]
    pub const fn new_with_rigidity(
        left: Interned<Type2>,
        right: Interned<Type2>,
        variance: Variance2,
        lesser_rigid_inference: bool,
        greater_rigid_inference: bool,
    ) -> Self {
        Self::new_with_flags(
            left,
            right,
            RelationFlags::new(variance)
                .with_lesser_rigid_inference(lesser_rigid_inference)
                .with_greater_rigid_inference(greater_rigid_inference),
        )
    }

    #[must_use]
    pub const fn new_matching(
        head: Interned<Type2>,
        subject: Interned<Type2>,
    ) -> Self {
        Self::new_with_flags(
            head,
            subject,
            RelationFlags::new(Variance2::Invariant)
                .with_greater_rigid_inference(true)
                .with_reduce(false),
        )
    }

    #[must_use]
    const fn new_with_flags(
        left: Interned<Type2>,
        right: Interned<Type2>,
        flags: RelationFlags,
    ) -> Self {
        Self { left, right, flags }
    }

    #[must_use]
    pub const fn invariant(
        left: Interned<Type2>,
        right: Interned<Type2>,
    ) -> Self {
        Self::new(left, right, Variance2::Invariant)
    }

    #[must_use]
    pub const fn left(&self) -> &Interned<Type2> { &self.left }

    #[must_use]
    pub const fn right(&self) -> &Interned<Type2> { &self.right }

    #[must_use]
    pub const fn lesser(&self) -> &Interned<Type2> { &self.left }

    #[must_use]
    pub const fn greater(&self) -> &Interned<Type2> { &self.right }

    #[must_use]
    pub const fn variance(&self) -> Variance2 { self.flags.variance() }

    #[must_use]
    pub const fn lesser_rigid_inference(&self) -> bool {
        self.flags.lesser_rigid_inference()
    }

    #[must_use]
    pub const fn greater_rigid_inference(&self) -> bool {
        self.flags.greater_rigid_inference()
    }

    #[must_use]
    pub const fn reduce(&self) -> bool { self.flags.reduce() }

    #[must_use]
    const fn flags(&self) -> RelationFlags { self.flags }

    #[must_use]
    pub const fn with_reduce(mut self, reduce: bool) -> Self {
        self.flags = self.flags.with_reduce(reduce);
        self
    }

    #[must_use]
    pub fn into_subtype(self) -> Subtype {
        Subtype::new(self.left, self.right, self.flags.variance())
    }
}

impl From<Subtype> for TypeRelation {
    fn from(subtype: Subtype) -> Self {
        Self::new(
            subtype.lesser().clone(),
            subtype.greater().clone(),
            subtype.variance(),
        )
    }
}

impl Substitutable for TypeRelation {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl pernixc_qbice::Interner,
    ) -> Option<Self> {
        match (
            self.left.apply(subst, interner),
            self.right.apply(subst, interner),
        ) {
            (Some(left), Some(right)) => {
                Some(Self { left, right, flags: self.flags })
            }
            (Some(left), None) => Some(Self {
                left,
                right: self.right.clone(),
                flags: self.flags,
            }),
            (None, Some(right)) => {
                Some(Self { left: self.left.clone(), right, flags: self.flags })
            }
            (None, None) => None,
        }
    }
}

enum BindInferenceVariableRelation {
    Bound(Step),
    Failed,
    NotApplicable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum InferenceVariableRelationSide {
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

impl Solve for TypeRelation {
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
        relation: &TypeRelation,
    ) -> impl Future<Output = Result<Option<Step>, OverflowError>> + Send {
        async move {
            assert!(
                !relation.lesser().is_bound_variable()
                    && !relation.greater().is_bound_variable(),
                "found un-instantiated bound variable in type relation:"
            );

            // if they are syntactically equal, we are done.
            if relation.lesser() == relation.greater() {
                return Ok(Some((
                    Substitution::new(),
                    self.engine().intern_unsized([]),
                    Constraints::default(),
                )));
            }

            match self.bind_inference_variable_relation(relation).await? {
                BindInferenceVariableRelation::Bound(step) => {
                    return Ok(Some(step));
                }
                BindInferenceVariableRelation::Failed => return Ok(None),
                BindInferenceVariableRelation::NotApplicable => {}
            }

            // if they are both lifetimes, return constraints according to the
            // variance

            // NOTE: we don't need universe nameability checks here because
            // "leak-checker" will take care of that, which requires more
            // semantic information.
            if self.kind_of(relation.lesser()).await.is_lifetime()
                && self.kind_of(relation.greater()).await.is_lifetime()
            {
                let constraints = match relation.variance() {
                    Variance2::Covariant => Constraints::lifetimes_outlives(
                        relation.lesser().clone(),
                        relation.greater().clone(),
                    ),
                    Variance2::Contravariant => {
                        Constraints::lifetimes_outlives(
                            relation.greater().clone(),
                            relation.lesser().clone(),
                        )
                    }
                    Variance2::Invariant => Constraints::lifetimes_eq(
                        relation.lesser().clone(),
                        relation.greater().clone(),
                    ),
                    Variance2::Bivariant => Constraints::default(),
                };

                return Ok(Some((
                    Substitution::new(),
                    self.engine().intern_unsized([]),
                    constraints,
                )));
            }

            match (&**relation.lesser(), &**relation.greater()) {
                // this is a tough one
                (Type2::Application(left_ap), Type2::Application(right_ap)) => {
                    self.handle_application(relation, left_ap, right_ap).await
                }

                (_, _) => {
                    Box::pin(self.try_reduce(
                        relation.lesser(),
                        relation.greater(),
                        relation.flags(),
                    ))
                    .await
                }
            }
        }
    }

    async fn bind_inference_variable_relation(
        &mut self,
        relation: &TypeRelation,
    ) -> Result<BindInferenceVariableRelation, OverflowError> {
        let Some((infer_var, binding_target, side)) =
            self.inference_variable_relation_binding_target(relation).await
        else {
            return Ok(BindInferenceVariableRelation::NotApplicable);
        };

        self.bind_inference_variable_to_target(
            infer_var,
            binding_target,
            side,
            relation.flags(),
        )
        .await
    }

    async fn inference_variable_relation_binding_target(
        &mut self,
        relation: &TypeRelation,
    ) -> Option<(
        InferenceVariable,
        Interned<Type2>,
        InferenceVariableRelationSide,
    )> {
        if !relation.lesser_rigid_inference()
            && let Type2::InferenceVariable(infer_var) = &**relation.lesser()
            && self
                .can_bind_inference_variable_to(
                    relation.greater(),
                    relation.variance(),
                )
                .await
        {
            return Some((
                *infer_var,
                relation.greater().clone(),
                InferenceVariableRelationSide::Lesser,
            ));
        }

        if !relation.greater_rigid_inference()
            && let Type2::InferenceVariable(infer_var) = &**relation.greater()
            && self
                .can_bind_inference_variable_to(
                    relation.lesser(),
                    relation.variance(),
                )
                .await
        {
            return Some((
                *infer_var,
                relation.lesser().clone(),
                InferenceVariableRelationSide::Greater,
            ));
        }

        None
    }

    async fn can_bind_inference_variable_to(
        &mut self,
        target: &Interned<Type2>,
        variance: Variance2,
    ) -> bool {
        // NOTE: invariant (strictly-equal) allows merging two inference
        // variables
        (variance == Variance2::Invariant)
            || (
                // NOTE: we also don't force two inference variables to unify in
                // non-invariant type relations.
                !target.is_inference_variable()
                // NOTE: in non-invariant type relations, we NEVER construct a
                // substitution that maps a inference variable of lifetime kind.
                && !self.kind_of(target).await.is_lifetime()
            )
    }

    async fn bind_with_check(
        &mut self,
        infer_var: InferenceVariable,
        binding_target: Interned<Type2>,
    ) -> BindInferenceVariableRelation {
        if !self
            .can_bind_inference_variable_to_type(
                infer_var,
                &binding_target,
                DoOccurCheck::Yes,
            )
            .await
        {
            return BindInferenceVariableRelation::Failed;
        }

        BindInferenceVariableRelation::Bound((
            Substitution::singleton(infer_var, binding_target),
            self.engine().intern_unsized([]),
            Constraints::default(),
        ))
    }

    async fn bind_inference_variable_to_target(
        &mut self,
        infer_var: InferenceVariable,
        binding_target: Interned<Type2>,
        side: InferenceVariableRelationSide,
        flags: RelationFlags,
    ) -> Result<BindInferenceVariableRelation, OverflowError> {
        if flags.variance() == Variance2::Invariant {
            return Ok(self.bind_with_check(infer_var, binding_target).await);
        }

        let Type2::Application(_) = &*binding_target else {
            return Ok(self.bind_with_check(infer_var, binding_target).await);
        };

        let binding_universe = infer_var.universe_index();

        // NOTE: given `?T <: &'static int32 @ Covariant`, we first perform a
        // generalization by replacing all the lifetimes in `&'static int32`
        // with fresh inference variables.
        //
        // Making `?T := &'?0 int32`, then we solve the relation
        // `&'?0 int32 <: &'static int32 @ Covariant` in the next step.
        //
        // Interestingly, we allow this binding `?T@U0 <: &'!P@U1 int32` to
        // succeed even though `U1` is not nameable in `U0`. This is because we
        // will generalize `?T@U0` to `&'?0@U0 int32` and then solve the
        // relation `&'?0@U0 int32 <: &'!P@U1 int32`. Whether this relation is
        // solvable depends on the "leak-checker".
        let intermediate_application = self
            .generalize_application_inference_variables(
                &binding_target,
                binding_universe,
            )
            .await;

        if !self
            .can_bind_inference_variable_to_type(
                infer_var,
                &intermediate_application,
                DoOccurCheck::Yes,
            )
            .await
        {
            return Ok(BindInferenceVariableRelation::Failed);
        }

        let subst = Substitution::singleton(
            infer_var,
            intermediate_application.clone(),
        );

        let relation_problem = match side {
            InferenceVariableRelationSide::Lesser => {
                TypeRelation::new_with_flags(
                    intermediate_application.clone(),
                    binding_target,
                    flags,
                )
            }
            InferenceVariableRelationSide::Greater => {
                TypeRelation::new_with_flags(
                    binding_target,
                    intermediate_application.clone(),
                    flags,
                )
            }
        };

        let Some((mut new_subst, relations, constraints)) =
            Box::pin(self.solve(&relation_problem)).await?
        else {
            return Ok(BindInferenceVariableRelation::Failed);
        };

        new_subst.compose(subst, self.engine());

        Ok(BindInferenceVariableRelation::Bound((
            new_subst,
            relations,
            constraints,
        )))
    }
}

impl Solver<'_> {
    async fn try_reduce(
        &mut self,
        lesser: &Interned<Type2>,
        greater: &Interned<Type2>,
        flags: RelationFlags,
    ) -> Result<Option<Step>, OverflowError> {
        if !flags.reduce() {
            return Ok(None);
        }

        // lazily reduce the lesser type and try again.
        if let Some((reduced_lesser, constrs)) =
            self.reduce_type(lesser.clone()).await?
            && let Some((subst, sub_problem, new_constrs)) = self
                .solve(&TypeRelation::new_with_flags(
                    reduced_lesser,
                    greater.clone(),
                    flags,
                ))
                .await?
        {
            let final_constrs = constrs
                .apply_or_self(&subst, self.engine())
                .union_into(new_constrs);

            return Ok(Some((subst, sub_problem, final_constrs)));
        }

        // lazily reduce the greater type and try again.
        if let Some((reduced_greater, constrs)) =
            self.reduce_type(greater.clone()).await?
            && let Some((subst, sub_problem, new_constrs)) = self
                .solve(&TypeRelation::new_with_flags(
                    lesser.clone(),
                    reduced_greater,
                    flags,
                ))
                .await?
        {
            let final_constrs = constrs
                .apply_or_self(&subst, self.engine())
                .union_into(new_constrs);

            return Ok(Some((subst, sub_problem, final_constrs)));
        }

        // otherwise, we have no information to learn from this
        Ok(None)
    }

    /// Exhaustively resolves a batch of type relations.
    ///
    /// Each relation is solved into a step, then the learned
    /// substitution and emitted constraints are accumulated. Constraints that
    /// cannot make progress in the current round are returned in the `Step`
    /// instead of causing the whole resolution to fail. If a round learns
    /// something, those residual relations are rewritten once with
    /// the accumulated substitution before the next round starts.
    pub async fn resolve_type_relations(
        &mut self,
        mut type_relations: Vec<TypeRelation>,
    ) -> Result<Step, OverflowError> {
        let mut constraints = Constraints::default();
        let mut substitution = Substitution::new();

        loop {
            let mut residual_relations = Vec::new();
            let mut has_progress = false;

            while let Some(relation) = type_relations.pop() {
                // A stuck relation is kept for the next round. It may become
                // solvable after this round's substitutions are composed.
                let Some((
                    mut step_substitution,
                    new_relations,
                    new_constraints,
                )) = Box::pin(self.solve(&relation)).await?
                else {
                    residual_relations.push(relation);
                    continue;
                };

                has_progress = true;

                // Lifetime constraints are accumulated as emitted; callers
                // receive the learned substitution separately.
                constraints = constraints.union_into(new_constraints);

                // Relations still in the active worklist must observe each
                // successful step immediately so repeated inference variables
                // are solved consistently within the same batch.
                for relation in &mut type_relations {
                    *relation = relation
                        .apply_or_clone(&step_substitution, self.engine());
                }
                type_relations.extend(new_relations.iter().cloned());

                // Preserve composition order so the returned substitution
                // represents everything learned by all successful steps.
                step_substitution.compose(substitution, self.engine());
                substitution = step_substitution;
            }

            if residual_relations.is_empty() || !has_progress {
                // NOTE: we have to apply the substitution to the constraints
                // because the constraints may contain inference variables that
                // have been unified with other types in the substitution.
                constraints =
                    constraints.apply_or_self(&substitution, self.engine());

                return Ok((
                    substitution,
                    self.engine().intern_unsized(residual_relations),
                    constraints,
                ));
            }

            // Normalize stuck constraints once per round, not after each
            // individual step, to keep the worklist updates batched.
            for residual_relation in &mut residual_relations {
                *residual_relation = residual_relation
                    .apply_or_clone(&substitution, self.engine());
            }

            type_relations = residual_relations;
        }
    }

    pub async fn resolve_subtypes(
        &mut self,
        subtypes: Vec<Subtype>,
    ) -> Result<(Substitution, Vec<Subtype>, Constraints), OverflowError> {
        let (substitution, residual_type_relations, constraints) = self
            .resolve_type_relations(
                subtypes.into_iter().map(TypeRelation::from).collect(),
            )
            .await?;

        Ok((
            substitution,
            residual_type_relations
                .iter()
                .cloned()
                .map(TypeRelation::into_subtype)
                .collect(),
            constraints,
        ))
    }
}
