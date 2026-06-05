use std::convert::AsRef;

use pernixc_type::{
    predicate::Subtype,
    substitution::Substitution,
    r#type::{
        Type,
        constructor::{Application, Constructor, Mutability},
    },
    variance::Variance,
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{Agree, DoOccurCheck, OverflowError, Provisional, Solve, Solver},
};

pub type Step = (Substitution, Vec<Subtype>, Constraints);

enum BindInferenceVariableSubtype {
    Bound(Step),
    Failed,
    NotApplicable,
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

            match self.bind_inference_variable_subtype(subtype).await {
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
    ) -> BindInferenceVariableSubtype {
        // if one of them is an inference variable, directly map it to the
        // other type.
        let target = match (&**subtype.lesser(), &**subtype.greater()) {
            (Type::InferenceVariable(infer_var), x)
            | (x, Type::InferenceVariable(infer_var))
                if !x.is_bound_variable()
                    && !x.is_inference_variable()
                    && !self.kind_of(x).await.is_lifetime() =>
            {
                let target = if subtype.lesser().is_inference_variable() {
                    subtype.greater().clone()
                } else {
                    subtype.lesser().clone()
                };

                (*infer_var, target)
            }

            (_, _) => return BindInferenceVariableSubtype::NotApplicable,
        };

        let Some(subst) =
            self.map_variable(target.0, target.1, DoOccurCheck::Yes).await
        else {
            return BindInferenceVariableSubtype::Failed;
        };

        BindInferenceVariableSubtype::Bound((
            subst,
            Vec::new(),
            Constraints::default(),
        ))
    }

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

    async fn handle_application(
        &mut self,
        lesser: &Interned<Type>,
        greater: &Interned<Type>,
        lesser_ap: &Application,
        greater_ap: &Application,
        variance: Variance,
    ) -> Result<Option<Step>, OverflowError> {
        let Some(iter) = self.destructure(lesser_ap, greater_ap) else {
            // can't destructure, so try reducing and trying again
            return Box::pin(self.try_reduce(lesser, greater, variance)).await;
        };

        self.new_universe(
            async |solver| -> Result<Option<Step>, OverflowError> {
                let lesser_inst = lesser_ap
                    .binder()
                    .map(|x| solver.create_inference_instantiations(x.kinds()));
                let greater_inst = greater_ap
                    .binder()
                    .map(|x| solver.create_skolem_instantiations(x.kinds()));

                let mut new_iter = iter.map(|(l, g)| {
                    (
                        lesser_inst.as_ref().map_or_else(
                            || l.clone(),
                            |insts| solver.instantiate(&l, insts),
                        ),
                        greater_inst.as_ref().map_or_else(
                            || g.clone(),
                            |insts| solver.instantiate(&g, insts),
                        ),
                    )
                });

                match lesser_ap.constructor() {
                    Constructor::AnonymousTraitInstance(_)
                    | Constructor::Primitive(_) => {
                        assert!(
                            lesser_ap.arguments().is_empty()
                                && greater_ap.arguments().is_empty()
                        );

                        Ok(Some((
                            Substitution::new(),
                            Vec::new(),
                            Constraints::default(),
                        )))
                    }

                    Constructor::Lifetime(lifetime) => {
                        unreachable!(
                            "should've been caught by the lifetime case"
                        )
                    }

                    Constructor::Reference(reference) => {
                        let (lt_l, lt_g) =
                            new_iter.next().expect("expect lifetime component");
                        let (ty_l, ty_g) =
                            new_iter.next().expect("expect type component");

                        assert!(new_iter.next().is_none());

                        Box::pin(
                            solver.handle_subtype_of_arguments(
                                [
                                    (lt_l, lt_g, Variance::Covariant),
                                    (
                                        ty_l,
                                        ty_g,
                                        if reference.mutability()
                                            == Mutability::Mutable
                                        {
                                            Variance::Invariant
                                        } else {
                                            Variance::Covariant
                                        },
                                    ),
                                ]
                                .into_iter(),
                                lesser_inst.as_ref().map(AsRef::as_ref),
                                greater_inst.as_ref().map(AsRef::as_ref),
                            ),
                        )
                        .await
                    }

                    Constructor::Symbolic(symbolic) => todo!(),

                    Constructor::Tuple(tuple) => todo!(),

                    Constructor::FunctionPointer(function_pointer) => todo!(),

                    Constructor::AnonymousTraitInstance(
                        anonymous_trait_instance,
                    ) => todo!(),

                    Constructor::InstanceAssociated(instance_associated) => {
                        todo!()
                    }
                }
            },
        )
        .await?;

        todo!()
    }

    async fn handle_subtype_of_arguments(
        &mut self,
        pairs: impl Iterator<Item = (Interned<Type>, Interned<Type>, Variance)>,
        lesser_inst: Option<&[Interned<Type>]>,
        greater_inst: Option<&[Interned<Type>]>,
    ) -> Result<Option<Step>, OverflowError> {
        // if has no higher-ranked stuffs invovled, break down the problem into
        // smaller subproblems
        if lesser_inst.is_none_or(<[_]>::is_empty)
            && greater_inst.is_none_or(<[_]>::is_empty)
        {
            return Ok(Some((
                Substitution::new(),
                pairs.map(|(l, r, v)| Subtype::new(l, r, v)).collect(),
                Constraints::default(),
            )));
        }

        let mut subst = Substitution::new();
        let mut constrs = Constraints::default();

        for (lesser_arg, greater_arg, variance) in pairs {
            let substed_lesser = self.apply_or_self(&subst, lesser_arg);
            let substed_greater = self.apply_or_self(&subst, greater_arg);

            let Some((mut new_subst, _, new_constrs)) = self
                .solve(&Subtype::new(substed_lesser, substed_greater, variance))
                .await?
            else {
                return Ok(None);
            };

            self.compose_subst(&mut new_subst, subst);
            subst = new_subst;

            constrs = constrs.union_into(new_constrs);
        }
        // apply substitution to the constraints before returning.
        constrs = self.apply_or_self(&subst, constrs);

        Ok(Some((subst, todo!(), constrs)))
    }
}
