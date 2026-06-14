use pernixc_hash::{FxHashMap, FxHashSet};
use pernixc_type::{
    predicate::Outlives,
    substitution::{Substitutable, Substitution, Variable},
    r#type::{
        Type,
        bound::Instantiate,
        constructor::{Application, Constructor, Lifetime},
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
    solver::{OverflowError, Solver, universe::UniverseIndex},
    subtype::Step,
};

#[derive(Debug, Clone, Copy)]
enum HrtbInstantiation {
    LesserInferenceGreaterSkolem,
    LesserSkolemGreaterInference,
}

impl Solver<'_> {
    pub(super) async fn handle_hrtb_application(
        &mut self,
        lesser_ap: &Application,
        greater_ap: &Application,
        arguments: &[(Interned<Type>, Interned<Type>)],
        variance: Variance,
    ) -> Result<Option<Step>, OverflowError> {
        match variance {
            Variance::Covariant | Variance::Contravariant => {
                let instantiation = match variance {
                    Variance::Covariant => {
                        HrtbInstantiation::LesserInferenceGreaterSkolem
                    }
                    Variance::Contravariant => {
                        HrtbInstantiation::LesserSkolemGreaterInference
                    }
                    Variance::Invariant | Variance::Bivariant => {
                        unreachable!(
                            "invariant and bivariant are handled separately"
                        )
                    }
                };

                self.handle_hrtb_application_run(
                    lesser_ap,
                    greater_ap,
                    arguments,
                    variance,
                    instantiation,
                )
                .await
            }

            Variance::Invariant => {
                let Some((mut substitution, residual_subtypes, constraints)) =
                    Box::pin(self.handle_hrtb_application_run(
                        lesser_ap,
                        greater_ap,
                        arguments,
                        Variance::Invariant,
                        HrtbInstantiation::LesserInferenceGreaterSkolem,
                    ))
                    .await?
                else {
                    return Ok(None);
                };

                assert!(residual_subtypes.is_empty());

                let engine = self.engine();
                let substituted_arguments = arguments
                    .iter()
                    .map(|(lesser, greater)| {
                        (
                            lesser.apply_or_clone(&substitution, engine),
                            greater.apply_or_clone(&substitution, engine),
                        )
                    })
                    .collect::<Vec<_>>();

                let Some((
                    mut other_substitution,
                    other_residual_subtypes,
                    other_constraints,
                )) = Box::pin(self.handle_hrtb_application_run(
                    lesser_ap,
                    greater_ap,
                    &substituted_arguments,
                    Variance::Invariant,
                    HrtbInstantiation::LesserSkolemGreaterInference,
                ))
                .await?
                else {
                    return Ok(None);
                };

                assert!(other_residual_subtypes.is_empty());

                self.compose_subst(&mut other_substitution, substitution);
                substitution = other_substitution;

                Ok(Some((
                    substitution,
                    Vec::new(),
                    constraints.union_into(other_constraints),
                )))
            }

            Variance::Bivariant => Ok(Some((
                Substitution::new(),
                Vec::new(),
                Constraints::default(),
            ))),
        }
    }

    async fn handle_hrtb_application_run(
        &mut self,
        lesser_ap: &Application,
        greater_ap: &Application,
        arguments: &[(Interned<Type>, Interned<Type>)],
        variance: Variance,
        instantiation: HrtbInstantiation,
    ) -> Result<Option<Step>, OverflowError> {
        self.new_universe(async |solver| {
            let run_universe = solver.current_universe();
            let (lesser_inst, greater_inst) = match instantiation {
                HrtbInstantiation::LesserInferenceGreaterSkolem => (
                    lesser_ap.binder().map(|x| {
                        solver.create_inference_instantiations(x.kinds())
                    }),
                    greater_ap.binder().map(|x| {
                        solver.create_skolem_instantiations(x.kinds())
                    }),
                ),
                HrtbInstantiation::LesserSkolemGreaterInference => (
                    lesser_ap.binder().map(|x| {
                        solver.create_skolem_instantiations(x.kinds())
                    }),
                    greater_ap.binder().map(|x| {
                        solver.create_inference_instantiations(x.kinds())
                    }),
                ),
            };

            let engine = solver.engine();

            let step = Box::pin(solver.handle_application_arguments(
                lesser_ap,
                arguments.iter().map(|(l, g)| {
                    (
                        lesser_inst.as_ref().map_or_else(
                            || l.clone(),
                            |insts| l.instantiate(insts, engine),
                        ),
                        greater_inst.as_ref().map_or_else(
                            || g.clone(),
                            |insts| g.instantiate(insts, engine),
                        ),
                    )
                }),
                lesser_inst.as_deref(),
                greater_inst.as_deref(),
                variance,
            ))
            .await?;

            let Some(step) = step else { return Ok(None) };

            Ok(solver.clean_hrtb_step(step, run_universe))
        })
        .await
    }

    fn clean_hrtb_step(
        &mut self,
        (mut substitution, residual_subtypes, constraints): Step,
        run_universe: UniverseIndex,
    ) -> Option<Step> {
        let engine = self.engine();
        let constraints = constraints.apply_or_clone(&substitution, engine);

        if residual_subtypes.is_empty() {
            if !self.hrtb_leak_check(&constraints, run_universe) {
                return None;
            }
        } else {
            return None;
        }

        let constraints =
            self.clean_hrtb_constraints(&constraints, run_universe);

        substitution.retain(|variable, ty| {
            !self.is_internal_substitution_variable(variable, run_universe)
                && !self.contains_internal_hrtb_variable(ty, run_universe)
        });

        Some((substitution, Vec::new(), constraints))
    }

    fn hrtb_leak_check(
        &self,
        constraints: &Constraints,
        run_universe: UniverseIndex,
    ) -> bool {
        let graph = Self::constraint_graph(constraints);

        for start in graph.keys() {
            let Type::SkolemizedVariable(skolem) = &**start else {
                continue;
            };

            if !self.is_internal_lifetime_skolem(*skolem, run_universe) {
                continue;
            }

            let mut seen = FxHashSet::default();
            let mut stack = vec![start.clone()];

            while let Some(next) = stack.pop() {
                if !seen.insert(next.clone()) {
                    continue;
                }

                if next != *start {
                    match &*next {
                        Type::SkolemizedVariable(other_skolem)
                            if self.is_internal_lifetime_skolem(
                                *other_skolem,
                                run_universe,
                            ) && other_skolem != skolem =>
                        {
                            return false;
                        }

                        ty if !self
                            .is_internal_hrtb_variable(ty, run_universe) =>
                        {
                            return false;
                        }

                        Type::GenericParameter(_)
                        | Type::InferenceVariable(_)
                        | Type::BoundVariable(_)
                        | Type::SkolemizedVariable(_)
                        | Type::Application(_) => {}
                    }
                }

                if let Some(edges) = graph.get(&next) {
                    stack.extend(edges.iter().cloned());
                }
            }
        }

        true
    }

    fn clean_hrtb_constraints(
        &self,
        constraints: &Constraints,
        run_universe: UniverseIndex,
    ) -> Constraints {
        let graph = Self::constraint_graph(constraints);
        let mut cleaned = Constraints::new();
        let static_lifetime = self.static_lifetime();

        for source in graph.keys() {
            let mut seen = FxHashSet::default();
            let mut stack = vec![source.clone()];

            while let Some(next) = stack.pop() {
                if !seen.insert(next.clone()) {
                    continue;
                }

                if next != *source {
                    self.push_clean_hrtb_constraint(
                        &mut cleaned,
                        source.clone(),
                        next.clone(),
                        static_lifetime.clone(),
                        run_universe,
                    );
                }

                if (self
                    .is_internal_lifetime_inference_type(&next, run_universe)
                    || next == *source)
                    && let Some(edges) = graph.get(&next)
                {
                    stack.extend(edges.iter().cloned());
                }
            }
        }

        cleaned
    }

    fn push_clean_hrtb_constraint(
        &self,
        cleaned: &mut Constraints,
        lesser: Interned<Type>,
        greater: Interned<Type>,
        static_lifetime: Interned<Type>,
        run_universe: UniverseIndex,
    ) {
        if self.is_internal_lifetime_inference_type(&lesser, run_universe)
            || self.is_internal_lifetime_inference_type(&greater, run_universe)
        {
            return;
        }

        if self.is_internal_lifetime_skolem_type(&greater, run_universe)
            && !self.contains_internal_hrtb_variable(&lesser, run_universe)
        {
            cleaned.extend([Outlives::new(lesser, static_lifetime)]);
            return;
        }

        if self.contains_internal_hrtb_variable(&lesser, run_universe)
            || self.contains_internal_hrtb_variable(&greater, run_universe)
        {
            return;
        }

        cleaned.extend([Outlives::new(lesser, greater)]);
    }

    fn constraint_graph(
        constraints: &Constraints,
    ) -> FxHashMap<Interned<Type>, FxHashSet<Interned<Type>>> {
        let mut graph = FxHashMap::<Interned<Type>, FxHashSet<_>>::default();

        for constraint in constraints.clone() {
            graph
                .entry(constraint.lesser().clone())
                .or_default()
                .insert(constraint.greater().clone());
        }

        graph
    }

    fn is_internal_substitution_variable(
        &self,
        variable: Variable,
        run_universe: UniverseIndex,
    ) -> bool {
        match variable {
            Variable::Inference(variable) => {
                self.is_internal_lifetime_inference(variable, run_universe)
            }
            Variable::Generic(_) => false,
        }
    }

    fn contains_internal_hrtb_variable(
        &self,
        ty: &Interned<Type>,
        run_universe: UniverseIndex,
    ) -> bool {
        match &**ty {
            Type::InferenceVariable(variable) => {
                self.is_internal_lifetime_inference(*variable, run_universe)
            }
            Type::SkolemizedVariable(variable) => {
                self.is_internal_lifetime_skolem(*variable, run_universe)
            }
            Type::Application(application) => {
                application.arguments().iter().any(|argument| {
                    self.contains_internal_hrtb_variable(argument, run_universe)
                })
            }
            Type::GenericParameter(_) | Type::BoundVariable(_) => false,
        }
    }

    fn is_internal_hrtb_variable(
        &self,
        ty: &Type,
        run_universe: UniverseIndex,
    ) -> bool {
        match ty {
            Type::InferenceVariable(variable) => {
                self.is_internal_lifetime_inference(*variable, run_universe)
            }
            Type::SkolemizedVariable(variable) => {
                self.is_internal_lifetime_skolem(*variable, run_universe)
            }
            Type::GenericParameter(_)
            | Type::BoundVariable(_)
            | Type::Application(_) => false,
        }
    }

    fn is_internal_lifetime_inference_type(
        &self,
        ty: &Interned<Type>,
        run_universe: UniverseIndex,
    ) -> bool {
        match &**ty {
            Type::InferenceVariable(variable) => {
                self.is_internal_lifetime_inference(*variable, run_universe)
            }
            Type::GenericParameter(_)
            | Type::BoundVariable(_)
            | Type::SkolemizedVariable(_)
            | Type::Application(_) => false,
        }
    }

    fn is_internal_lifetime_skolem_type(
        &self,
        ty: &Interned<Type>,
        run_universe: UniverseIndex,
    ) -> bool {
        match &**ty {
            Type::SkolemizedVariable(variable) => {
                self.is_internal_lifetime_skolem(*variable, run_universe)
            }
            Type::GenericParameter(_)
            | Type::InferenceVariable(_)
            | Type::BoundVariable(_)
            | Type::Application(_) => false,
        }
    }

    fn is_internal_lifetime_inference(
        &self,
        variable: InferenceVariable,
        run_universe: UniverseIndex,
    ) -> bool {
        self.get_inference_variable_universe(variable) == run_universe
            && self.get_inference_variable_kind(&variable) == TyKind::Lifetime
    }

    fn is_internal_lifetime_skolem(
        &self,
        variable: SkolemizedVariable,
        run_universe: UniverseIndex,
    ) -> bool {
        self.get_skolemized_variable_universe(variable) == run_universe
            && self.get_skolemized_variable_kind(&variable) == TyKind::Lifetime
    }

    fn static_lifetime(&self) -> Interned<Type> {
        self.intern(Type::Application(Application::new(
            Constructor::Lifetime(Lifetime::Static),
            self.engine().intern_unsized(Vec::<Interned<Type>>::new()),
        )))
    }
}
