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

type ConstraintGraph = FxHashMap<Interned<Type>, FxHashSet<Interned<Type>>>;

#[derive(Debug, Clone, Copy)]
enum HrtbInstantiation {
    // The subtype side is existential and the supertype side is universal:
    // `for<a> T[a] <: for<b> U[b]` becomes `T[?a] <: U[!b]`.
    LesserInferenceGreaterSkolem,

    // Used for contravariant positions and the second invariant pass, where
    // the subtype/supertype roles are observed through the flipped variance.
    LesserSkolemGreaterInference,
}

struct HrtbRun {
    step: Step,
    universe: UniverseIndex,
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

                let Some(run) = self
                    .handle_hrtb_application_run(
                        lesser_ap,
                        greater_ap,
                        arguments,
                        variance,
                        instantiation,
                    )
                    .await?
                else {
                    return Ok(None);
                };

                Ok(self.clean_hrtb_step(run.step, &[run.universe]))
            }

            Variance::Invariant => {
                Box::pin(self.handle_invariant_hrtb_application(
                    lesser_ap, greater_ap, arguments,
                ))
                .await
            }

            Variance::Bivariant => Ok(Some((
                Substitution::new(),
                Vec::new(),
                Constraints::default(),
            ))),
        }
    }

    async fn handle_invariant_hrtb_application(
        &mut self,
        lesser_ap: &Application,
        greater_ap: &Application,
        arguments: &[(Interned<Type>, Interned<Type>)],
    ) -> Result<Option<Step>, OverflowError> {
        // Invariant HRTB must prove both directions, but each proof is still an
        // invariant argument solve. Only binder polarity is swapped between the
        // two runs.
        let Some(first_run) = Box::pin(self.handle_hrtb_application_run(
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

        assert!(first_run.step.1.is_empty());

        // The second direction sees any external knowledge learned by the
        // first direction, but gets fresh HRTB variables of its own.
        let first_external_substitution = self
            .external_hrtb_substitution(first_run.step.0.clone(), &[
                first_run.universe
            ]);
        let engine = self.engine();
        let substituted_arguments = arguments
            .iter()
            .map(|(lesser, greater)| {
                (
                    lesser.apply_or_clone(&first_external_substitution, engine),
                    greater
                        .apply_or_clone(&first_external_substitution, engine),
                )
            })
            .collect::<Vec<_>>();

        let Some(second_run) = Box::pin(self.handle_hrtb_application_run(
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

        assert!(second_run.step.1.is_empty());

        let (first_substitution, _first_residual_subtypes, first_constraints) =
            first_run.step;
        let (
            mut second_substitution,
            _second_residual_subtypes,
            second_constraints,
        ) = second_run.step;

        self.compose_subst(&mut second_substitution, first_substitution);

        let step = (
            second_substitution,
            Vec::new(),
            first_constraints.union_into(second_constraints),
        );
        let universes = [first_run.universe, second_run.universe];

        Ok(self.clean_hrtb_step(step, &universes))
    }

    async fn handle_hrtb_application_run(
        &mut self,
        lesser_ap: &Application,
        greater_ap: &Application,
        arguments: &[(Interned<Type>, Interned<Type>)],
        variance: Variance,
        instantiation: HrtbInstantiation,
    ) -> Result<Option<HrtbRun>, OverflowError> {
        self.new_universe(async |solver| {
            // The fresh universe labels every inference/skolem variable created
            // for this run so leak checking and cleanup can recognize them.
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
                variance,
                true,
            ))
            .await?;

            let Some(step) = step else { return Ok(None) };

            Ok(Some(HrtbRun { step, universe: run_universe }))
        })
        .await
    }

    fn clean_hrtb_step(
        &mut self,
        (mut substitution, residual_subtypes, constraints): Step,
        run_universes: &[UniverseIndex],
    ) -> Option<Step> {
        if !residual_subtypes.is_empty() {
            return None;
        }

        let engine = self.engine();
        // Substitutions can turn `?x: !s` into `R: !s`; leak checking must see
        // the final constraints, not the pre-substitution obligations.
        let constraints = constraints.apply_or_clone(&substitution, engine);
        let graph = Self::constraint_graph(&constraints);

        if !self.hrtb_leak_check(&graph, run_universes) {
            return None;
        }

        let constraints = self.clean_hrtb_constraints(&graph, run_universes);

        // HRTB variables are local proof artifacts. A successful step must not
        // expose them through the substitution map returned to callers.
        substitution.retain(|variable, ty| {
            !self.is_internal_substitution_variable(variable, run_universes)
                && !self.contains_internal_hrtb_variable(ty, run_universes)
        });

        Some((substitution, Vec::new(), constraints))
    }

    fn external_hrtb_substitution(
        &self,
        mut substitution: Substitution,
        run_universes: &[UniverseIndex],
    ) -> Substitution {
        substitution.retain(|variable, ty| {
            !self.is_internal_substitution_variable(variable, run_universes)
                && !self.contains_internal_hrtb_variable(ty, run_universes)
        });

        substitution
    }

    fn hrtb_leak_check(
        &self,
        graph: &ConstraintGraph,
        run_universes: &[UniverseIndex],
    ) -> bool {
        // Edges are directed as written by `Outlives::new(lesser, greater)`.
        // A bound skolem may only reach itself or inference variables created
        // for the same HRTB proof. Anything else leaks the universal lifetime.
        for start in graph.keys() {
            let Type::SkolemizedVariable(skolem) = &**start else {
                continue;
            };

            if !self.is_internal_lifetime_skolem(*skolem, run_universes) {
                continue;
            }

            let mut seen = FxHashSet::default();
            let mut stack = vec![start.clone()];

            while let Some(next) = stack.pop() {
                if !seen.insert(next.clone()) {
                    continue;
                }

                if next != *start
                    && !self.is_internal_lifetime_inference_type(
                        &next,
                        run_universes,
                    )
                {
                    return false;
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
        graph: &ConstraintGraph,
        run_universes: &[UniverseIndex],
    ) -> Constraints {
        let mut cleaned = Constraints::new();
        let static_lifetime = self.static_lifetime();

        for source in graph.keys() {
            // Compress paths through internal inference variables so removing
            // `?x` from `A -> ?x -> B` still leaves the observable `A -> B`.
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
                        run_universes,
                    );
                }

                if (self
                    .is_internal_lifetime_inference_type(&next, run_universes)
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
        run_universes: &[UniverseIndex],
    ) {
        if self.is_internal_lifetime_inference_type(&lesser, run_universes)
            || self.is_internal_lifetime_inference_type(&greater, run_universes)
        {
            return;
        }

        // `R: !s` is the observable remnant of proving an external lifetime
        // outlives every choice of the bound lifetime. That is equivalent to
        // requiring `R: 'static`.
        if self.is_internal_lifetime_skolem_type(&greater, run_universes)
            && !self.contains_internal_hrtb_variable(&lesser, run_universes)
        {
            cleaned.extend([Outlives::new(lesser, static_lifetime)]);
            return;
        }

        if self.contains_internal_hrtb_variable(&lesser, run_universes)
            || self.contains_internal_hrtb_variable(&greater, run_universes)
        {
            return;
        }

        cleaned.extend([Outlives::new(lesser, greater)]);
    }

    fn constraint_graph(constraints: &Constraints) -> ConstraintGraph {
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
        run_universes: &[UniverseIndex],
    ) -> bool {
        match variable {
            Variable::Inference(variable) => {
                self.is_internal_lifetime_inference(variable, run_universes)
            }
            Variable::Generic(_) => false,
        }
    }

    fn contains_internal_hrtb_variable(
        &self,
        ty: &Interned<Type>,
        run_universes: &[UniverseIndex],
    ) -> bool {
        match &**ty {
            Type::InferenceVariable(variable) => {
                self.is_internal_lifetime_inference(*variable, run_universes)
            }
            Type::SkolemizedVariable(variable) => {
                self.is_internal_lifetime_skolem(*variable, run_universes)
            }
            Type::Application(application) => {
                application.arguments().iter().any(|argument| {
                    self.contains_internal_hrtb_variable(
                        argument,
                        run_universes,
                    )
                })
            }
            Type::GenericParameter(_) | Type::BoundVariable(_) => false,
        }
    }

    fn is_internal_lifetime_inference_type(
        &self,
        ty: &Interned<Type>,
        run_universes: &[UniverseIndex],
    ) -> bool {
        match &**ty {
            Type::InferenceVariable(variable) => {
                self.is_internal_lifetime_inference(*variable, run_universes)
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
        run_universes: &[UniverseIndex],
    ) -> bool {
        match &**ty {
            Type::SkolemizedVariable(variable) => {
                self.is_internal_lifetime_skolem(*variable, run_universes)
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
        run_universes: &[UniverseIndex],
    ) -> bool {
        run_universes.contains(&self.get_inference_variable_universe(variable))
            && self.get_inference_variable_kind(&variable) == TyKind::Lifetime
    }

    fn is_internal_lifetime_skolem(
        &self,
        variable: SkolemizedVariable,
        run_universes: &[UniverseIndex],
    ) -> bool {
        run_universes.contains(&self.get_skolemized_variable_universe(variable))
            && self.get_skolemized_variable_kind(&variable) == TyKind::Lifetime
    }

    fn static_lifetime(&self) -> Interned<Type> {
        self.intern(Type::Application(Application::new(
            Constructor::Lifetime(Lifetime::Static),
            self.engine().intern_unsized(Vec::<Interned<Type>>::new()),
        )))
    }
}
