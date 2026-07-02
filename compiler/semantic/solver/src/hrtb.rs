use pernixc_hash::{FxHashMap, FxHashSet};
use pernixc_type::{
    predicate::Outlives,
    r#type::{
        Type2,
        constructor::{Application, Constructor, Lifetime},
        context::TyContext,
        inference::InferenceVariable,
        kind::TyKind,
        skolem::SkolemizedVariable,
    },
};
use qbice::storage::intern::Interned;

use crate::{constraints::Constraints, solver::Solver};

type ConstraintGraph = FxHashMap<Interned<Type2>, FxHashSet<Interned<Type2>>>;

#[derive(Debug, Clone, Default)]
pub(crate) struct HrtbVariables {
    inference_lifetimes: FxHashSet<InferenceVariable>,
    skolem_lifetimes: FxHashSet<SkolemizedVariable>,
}

impl HrtbVariables {
    pub(crate) fn union_into(mut self, other: Self) -> Self {
        self.inference_lifetimes.extend(other.inference_lifetimes);
        self.skolem_lifetimes.extend(other.skolem_lifetimes);
        self
    }

    fn is_empty(&self) -> bool {
        self.inference_lifetimes.is_empty() && self.skolem_lifetimes.is_empty()
    }

    fn contains_internal_variable(&self, ty: &Interned<Type2>) -> bool {
        match &**ty {
            Type2::InferenceVariable(variable) => {
                self.inference_lifetimes.contains(variable)
            }
            Type2::SkolemizedVariable(variable) => {
                self.skolem_lifetimes.contains(variable)
            }
            Type2::Application(application) => application
                .arguments()
                .iter()
                .any(|argument| self.contains_internal_variable(argument)),
            Type2::GenericParameter(_) | Type2::BoundVariable(_) => false,
        }
    }

    fn is_internal_inference(&self, ty: &Interned<Type2>) -> bool {
        matches!(&**ty, Type2::InferenceVariable(variable) if self.inference_lifetimes.contains(variable))
    }

    fn is_internal_skolem(&self, ty: &Interned<Type2>) -> bool {
        matches!(&**ty, Type2::SkolemizedVariable(variable) if self.skolem_lifetimes.contains(variable))
    }
}

impl Solver<'_> {
    pub(crate) fn hrtb_variables_from_instantiations<'a>(
        &self,
        instantiations: impl Iterator<Item = &'a Interned<Type2>>,
    ) -> HrtbVariables {
        let mut variables = HrtbVariables::default();

        for instantiation in instantiations {
            match &**instantiation {
                Type2::InferenceVariable(variable)
                    if self.get_inference_variable_kind(variable)
                        == TyKind::Lifetime =>
                {
                    variables.inference_lifetimes.insert(*variable);
                }
                Type2::SkolemizedVariable(variable)
                    if self.get_skolemized_variable_kind(variable)
                        == TyKind::Lifetime =>
                {
                    variables.skolem_lifetimes.insert(*variable);
                }
                Type2::GenericParameter(_)
                | Type2::InferenceVariable(_)
                | Type2::BoundVariable(_)
                | Type2::SkolemizedVariable(_)
                | Type2::Application(_) => {}
            }
        }

        variables
    }

    pub(crate) fn check_and_clean_hrtb_constraints(
        &self,
        constraints: Constraints,
        variables: &HrtbVariables,
    ) -> Option<Constraints> {
        // don't do any work if there are no HRTB variables
        if variables.is_empty() {
            return Some(constraints);
        }

        let graph = constraint_graph(&constraints);
        leak_check(&graph, variables)
            .then(|| self.clean_hrtb_constraints(&graph, variables))
    }

    fn clean_hrtb_constraints(
        &self,
        graph: &ConstraintGraph,
        variables: &HrtbVariables,
    ) -> Constraints {
        let mut cleaned = Constraints::new();
        let static_lifetime =
            self.intern(Type2::Application(Application::new(
                Constructor::Lifetime(Lifetime::Static),
                self.engine().intern_unsized(Vec::<Interned<Type2>>::new()),
            )));

        for source in graph.keys() {
            let mut seen = FxHashSet::default();
            let mut stack = vec![source.clone()];

            while let Some(next) = stack.pop() {
                if !seen.insert(next.clone()) {
                    continue;
                }

                if next != *source {
                    push_clean_constraint(
                        &mut cleaned,
                        source.clone(),
                        next.clone(),
                        static_lifetime.clone(),
                        variables,
                    );
                }

                if (variables.is_internal_inference(&next) || next == *source)
                    && let Some(edges) = graph.get(&next)
                {
                    stack.extend(edges.iter().cloned());
                }
            }
        }

        cleaned
    }
}

fn leak_check(graph: &ConstraintGraph, variables: &HrtbVariables) -> bool {
    for start in graph.keys() {
        let Type2::SkolemizedVariable(skolem) = &**start else {
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
            if next != *start && !variables.is_internal_inference(&next) {
                return false;
            }
            if let Some(edges) = graph.get(&next) {
                stack.extend(edges.iter().cloned());
            }
        }
    }

    true
}

fn push_clean_constraint(
    cleaned: &mut Constraints,
    lesser: Interned<Type2>,
    greater: Interned<Type2>,
    static_lifetime: Interned<Type2>,
    variables: &HrtbVariables,
) {
    if variables.is_internal_inference(&lesser)
        || variables.is_internal_inference(&greater)
    {
        return;
    }
    if variables.is_internal_skolem(&greater)
        && !variables.contains_internal_variable(&lesser)
    {
        cleaned.extend([Outlives::new(lesser, static_lifetime)]);
        return;
    }
    if variables.contains_internal_variable(&lesser)
        || variables.contains_internal_variable(&greater)
    {
        return;
    }

    cleaned.extend([Outlives::new(lesser, greater)]);
}

fn constraint_graph(constraints: &Constraints) -> ConstraintGraph {
    let mut graph = FxHashMap::<Interned<Type2>, FxHashSet<_>>::default();
    for constraint in constraints.clone() {
        graph
            .entry(constraint.lesser().clone())
            .or_default()
            .insert(constraint.greater().clone());
    }
    graph
}
