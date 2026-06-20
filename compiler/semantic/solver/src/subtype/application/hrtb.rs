use pernixc_hash::{FxHashMap, FxHashSet};
use pernixc_type::{
    predicate::Outlives,
    substitution::{Substitutable, Substitution},
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

use super::ResolveStrategy;
use crate::{
    constraints::Constraints,
    solver::{OverflowError, Solver},
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
    substitution: Substitution,
    constraints: Constraints,
    variables: HrtbVariables,
}

#[derive(Debug, Clone, Default)]
struct HrtbVariables {
    inference_lifetimes: FxHashSet<InferenceVariable>,
    skolem_lifetimes: FxHashSet<SkolemizedVariable>,
}

impl HrtbVariables {
    fn union_into(mut self, other: Self) -> Self {
        self.inference_lifetimes.extend(other.inference_lifetimes);
        self.skolem_lifetimes.extend(other.skolem_lifetimes);
        self
    }

    fn contains_internal_hrtb_variable(&self, ty: &Interned<Type>) -> bool {
        match &**ty {
            Type::InferenceVariable(variable) => {
                self.inference_lifetimes.contains(variable)
            }
            Type::SkolemizedVariable(variable) => {
                self.skolem_lifetimes.contains(variable)
            }
            Type::Application(application) => application
                .arguments()
                .iter()
                .any(|argument| self.contains_internal_hrtb_variable(argument)),
            Type::GenericParameter(_) | Type::BoundVariable(_) => false,
        }
    }

    fn is_internal_lifetime_inference_type(&self, ty: &Interned<Type>) -> bool {
        match &**ty {
            Type::InferenceVariable(variable) => {
                self.inference_lifetimes.contains(variable)
            }
            Type::GenericParameter(_)
            | Type::BoundVariable(_)
            | Type::SkolemizedVariable(_)
            | Type::Application(_) => false,
        }
    }

    fn is_internal_lifetime_skolem_type(&self, ty: &Interned<Type>) -> bool {
        match &**ty {
            Type::SkolemizedVariable(variable) => {
                self.skolem_lifetimes.contains(variable)
            }
            Type::GenericParameter(_)
            | Type::InferenceVariable(_)
            | Type::BoundVariable(_)
            | Type::Application(_) => false,
        }
    }
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
            // for the contravariant and covariant cases, a single run with
            // appropriate instantiation is sufficient.
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

                Ok(self.clean_hrtb_step(
                    run.substitution,
                    &run.constraints,
                    &run.variables,
                ))
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

    /// Runs higher-ranked subtyping proof for Invariant ambient variance. This
    /// requires proving both directions of the subtyping relationship by
    /// running `handle_hrtb_application_run` twice with opposite instantiation
    /// strategies, then combining the results.
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

        let engine = self.engine();
        let first_substitution = first_run.substitution;

        let substituted_arguments = arguments
            .iter()
            .map(|(lesser, greater)| {
                (
                    lesser.apply_or_clone(&first_substitution, engine),
                    greater.apply_or_clone(&first_substitution, engine),
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

        // Residual subtypes are guaranteed to be empty
        let mut second_substitution = second_run.substitution;

        self.compose_subst(&mut second_substitution, first_substitution);

        let variables = first_run.variables.union_into(second_run.variables);
        let constraints =
            first_run.constraints.union_into(second_run.constraints);

        Ok(self.clean_hrtb_step(second_substitution, &constraints, &variables))
    }

    /// Runs the higher-ranked subtyping proof for the given application and
    /// arguments. Depending on the `instantiation` strategy, lesser/greater
    /// binders are instantiated with either inference variables or skolem
    /// variables. Then the set of subtypes is solved with the instantiated
    /// arguments with the result as [`HrtbRun`] if successful.
    async fn handle_hrtb_application_run(
        &mut self,
        lesser_ap: &Application,
        greater_ap: &Application,
        arguments: &[(Interned<Type>, Interned<Type>)],
        variance: Variance,
        instantiation: HrtbInstantiation,
    ) -> Result<Option<HrtbRun>, OverflowError> {
        self.new_universe(async |solver| {
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
            let variables = solver.hrtb_variables_from_instantiations(
                lesser_inst
                    .iter()
                    .flatten()
                    .chain(greater_inst.iter().flatten()),
            );

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
                ResolveStrategy::ResolveImmediately,
            ))
            .await?;

            let Some((substitution, residual, constraints)) = step else {
                return Ok(None);
            };

            if !residual.is_empty() {
                return Ok(None);
            }

            Ok(Some(HrtbRun { substitution, constraints, variables }))
        })
        .await
    }

    /// Run leak check and clean up the resulting constraints from the HRTB
    /// proof run
    fn clean_hrtb_step(
        &mut self,
        substitution: Substitution,
        constraints: &Constraints,
        variables: &HrtbVariables,
    ) -> Option<Step> {
        // NOTE: You might think that we should apply the substitutions on the
        // constraints before the leack check first, but we actually don't need
        // it because:
        //
        // 1a. Currently, in **Subtyping Relation**, lifetimes can never be
        //    mapped by substitution. However, in other relations like **Match**
        //    still does, but it's not relevant here.
        // 2a. The `Constraints` generated here can only be in the form of
        //    `lifetime: lifetime` and since (1a) holds, the substitution won't
        //    change the shape of constraints.
        //
        // Proof? Trust me bro :-)
        let graph = Self::constraint_graph(constraints);

        if !Self::hrtb_leak_check(&graph, variables) {
            return None;
        }

        let constraints = self.clean_hrtb_constraints(&graph, variables);

        // NOTE: here we directly return the original substitution without
        // eliminating the internal variables, because:
        //
        // 1b. We currently assume that all higher-ranked variables are
        //    lifetimes, even though we have infrastructure to support other
        //    kinds. If we happens to change this assumption in the future, we
        //    might need to revisit this decision.
        // 2b. The internal higher-ranked variables will never be in the
        //    codomain of the substitution, because:
        //    2.1b Since (1b) and (1a) holds, it means that all domains of the
        //      substitution will always have root universe.
        //    2.2b Therefore, internal higher-ranked variables that are created
        //      in a higher universe will never be substituted due to universe
        //      checks.
        // 3b. Therefore, the subsitution will never mention any internal
        //    higher-ranked variables, and thus we can safely return it without
        //    eliminating them.

        Some((substitution, Vec::new(), constraints))
    }

    fn hrtb_leak_check(
        graph: &ConstraintGraph,
        variables: &HrtbVariables,
    ) -> bool {
        // Edges are directed as written by `Outlives::new(lesser, greater)`.
        // A bound skolem may only reach itself or inference variables created
        // for the same HRTB proof. Anything else leaks the universal lifetime.
        for start in graph.keys() {
            let Type::SkolemizedVariable(skolem) = &**start else {
                continue;
            };

            if !variables.skolem_lifetimes.contains(skolem) {
                continue;
            }

            let mut seen = FxHashSet::default();
            let mut stack = vec![start.clone()];

            while let Some(next) = stack.pop() {
                if !seen.insert(next.clone()) {
                    continue;
                }

                if next != *start
                    && !variables.is_internal_lifetime_inference_type(&next)
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
        variables: &HrtbVariables,
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
                    Self::push_clean_hrtb_constraint(
                        &mut cleaned,
                        source.clone(),
                        next.clone(),
                        static_lifetime.clone(),
                        variables,
                    );
                }

                if (variables.is_internal_lifetime_inference_type(&next)
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
        cleaned: &mut Constraints,
        lesser: Interned<Type>,
        greater: Interned<Type>,
        static_lifetime: Interned<Type>,
        variables: &HrtbVariables,
    ) {
        if variables.is_internal_lifetime_inference_type(&lesser)
            || variables.is_internal_lifetime_inference_type(&greater)
        {
            return;
        }

        // `R: !s` is the observable remnant of proving an external lifetime
        // outlives every choice of the bound lifetime. That is equivalent to
        // requiring `R: 'static`.
        if variables.is_internal_lifetime_skolem_type(&greater)
            && !variables.contains_internal_hrtb_variable(&lesser)
        {
            cleaned.extend([Outlives::new(lesser, static_lifetime)]);
            return;
        }

        if variables.contains_internal_hrtb_variable(&lesser)
            || variables.contains_internal_hrtb_variable(&greater)
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

    fn hrtb_variables_from_instantiations<'a>(
        &self,
        instantiations: impl Iterator<Item = &'a Interned<Type>>,
    ) -> HrtbVariables {
        let mut variables = HrtbVariables::default();

        for instantiation in instantiations {
            match &**instantiation {
                Type::InferenceVariable(variable)
                    if self.get_inference_variable_kind(variable)
                        == TyKind::Lifetime =>
                {
                    variables.inference_lifetimes.insert(*variable);
                }

                Type::SkolemizedVariable(variable)
                    if self.get_skolemized_variable_kind(variable)
                        == TyKind::Lifetime =>
                {
                    variables.skolem_lifetimes.insert(*variable);
                }

                Type::GenericParameter(_)
                | Type::InferenceVariable(_)
                | Type::BoundVariable(_)
                | Type::SkolemizedVariable(_)
                | Type::Application(_) => {}
            }
        }

        variables
    }

    fn static_lifetime(&self) -> Interned<Type> {
        self.intern(Type::Application(Application::new(
            Constructor::Lifetime(Lifetime::Static),
            self.engine().intern_unsized(Vec::<Interned<Type>>::new()),
        )))
    }
}
