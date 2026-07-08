//! Handling leak-checking and cleaning up constraints from temporary
//! higher-ranked variables.
//!
//! To be honest, at the time of writing this, I don't fully understand the
//! concept of higher-ranked lifetimes and how they are considered "leaked".
//! I summarize my understanding of the problem as follows:
//!
//! Leak check mainly deals with the problem of whether:
//!
//! 1. Skolemized lifetimes can reach (directly or transitively) to another
//!    skolemized lifetimes (e.g. S1: S2)
//! 2. Skolemized lifetimes can reach (directly or transitively) to the lifetime
//!    that cannot name it (e.g. S1@U1: R@U0). This means that the skolemized
//!    lifetimes cannot reach to the "outside lifetimes". In this case, the
//!    "outside lifetimes" could be, `'a` lifetime parameter, or a `'static`
//!    lifetime.
//!
//! Furthermore, it erases all the created temporary instantiations of higher-
//! ranked lifetimes. For instance, `'a: '?0, '?0: 'b`, where `'?0` is a
//! temporary instantiation of a higher-ranked lifetime, will be erased to
//! `'a: 'b` if it passes the leak check.

use pernixc_hash::{FxHashMap, FxHashSet};
use pernixc_type::{
    predicate::Outlives,
    r#type::{
        Type2, constructor::Lifetime, kind::TyKind, universe::UniverseIndex,
    },
};
use qbice::storage::intern::Interned;

use crate::{constraints::Constraints, solver::Solver};

type ConstraintGraph = FxHashMap<Interned<Type2>, FxHashSet<Interned<Type2>>>;

impl Solver<'_> {
    pub(crate) fn check_and_clean_hrtb_constraints(
        &self,
        constraints: Constraints,
        closing_universe: UniverseIndex,
    ) -> Option<Constraints> {
        let graph = constraint_graph(&constraints);

        // don't do any work if there are no HRTB variables
        if !contains_internal_variable_in_graph(&graph, closing_universe) {
            return Some(constraints);
        }

        leak_check(&graph, closing_universe)
            .then(|| self.clean_hrtb_constraints(&graph, closing_universe))
    }

    fn clean_hrtb_constraints(
        &self,
        graph: &ConstraintGraph,
        closing_universe: UniverseIndex,
    ) -> Constraints {
        let mut cleaned = Constraints::new();
        let static_lifetime =
            Type2::new_lifetime(Lifetime::Static, self.engine());

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
                        closing_universe,
                    );
                }

                if (is_internal_inference(&next, closing_universe)
                    || next == *source)
                    && let Some(edges) = graph.get(&next)
                {
                    stack.extend(edges.iter().cloned());
                }
            }
        }

        cleaned
    }
}

fn leak_check(
    graph: &ConstraintGraph,
    closing_universe: UniverseIndex,
) -> bool {
    for start in graph.keys() {
        if !is_internal_skolem(start, closing_universe) {
            continue;
        }

        let mut seen = FxHashSet::default();
        let mut stack = vec![start.clone()];
        while let Some(next) = stack.pop() {
            if !seen.insert(next.clone()) {
                continue;
            }
            if next != *start && !is_internal_inference(&next, closing_universe)
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

fn push_clean_constraint(
    cleaned: &mut Constraints,
    lesser: Interned<Type2>,
    greater: Interned<Type2>,
    static_lifetime: Interned<Type2>,
    closing_universe: UniverseIndex,
) {
    if is_internal_inference(&lesser, closing_universe)
        || is_internal_inference(&greater, closing_universe)
    {
        return;
    }
    if is_internal_skolem(&greater, closing_universe)
        && !contains_internal_variable(&lesser, closing_universe)
    {
        cleaned.extend([Outlives::new(lesser, static_lifetime)]);
        return;
    }
    if contains_internal_variable(&lesser, closing_universe)
        || contains_internal_variable(&greater, closing_universe)
    {
        return;
    }

    cleaned.extend([Outlives::new(lesser, greater)]);
}

fn contains_internal_variable_in_graph(
    graph: &ConstraintGraph,
    closing_universe: UniverseIndex,
) -> bool {
    graph.iter().any(|(lesser, greaters)| {
        contains_internal_variable(lesser, closing_universe)
            || greaters.iter().any(|greater| {
                contains_internal_variable(greater, closing_universe)
            })
    })
}

fn contains_internal_variable(
    ty: &Interned<Type2>,
    closing_universe: UniverseIndex,
) -> bool {
    match &**ty {
        Type2::InferenceVariable(_) => {
            is_internal_inference(ty, closing_universe)
        }
        Type2::SkolemizedVariable(_) => {
            is_internal_skolem(ty, closing_universe)
        }
        Type2::Application(application) => {
            application.arguments().iter().any(|argument| {
                contains_internal_variable(argument, closing_universe)
            })
        }
        Type2::GenericParameter(_) | Type2::BoundVariable(_) => false,
    }
}

fn is_internal_inference(
    ty: &Interned<Type2>,
    closing_universe: UniverseIndex,
) -> bool {
    matches!(
        &**ty,
        Type2::InferenceVariable(variable)
            if variable.kind() == TyKind::Lifetime
                && variable.universe_index() == closing_universe
    )
}

fn is_internal_skolem(
    ty: &Interned<Type2>,
    closing_universe: UniverseIndex,
) -> bool {
    matches!(
        &**ty,
        Type2::SkolemizedVariable(variable)
            if variable.kind() == TyKind::Lifetime
                && variable.universe_index() == closing_universe
    )
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

#[cfg(test)]
mod test {
    use pernixc_qbice::create_minimal_engine as create_engine;
    use pernixc_type::r#type::{
        constructor::Lifetime, inference::InferenceVariable, kind::TyKind,
        skolem::SkolemizedVariable,
    };

    use super::*;
    use crate::premise::Premise;

    // input: 'static: ?a@U1, ?a@U1: 'erased
    // premise: U1 is the closing universe
    // output: 'static: 'erased
    #[tokio::test]
    async fn hrtb_cleanup_erases_inference_lifetimes_in_closing_universe() {
        let engine = create_engine().await;
        let closing_universe = UniverseIndex::root().next();
        let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
        let erased_lifetime = Type2::new_lifetime(Lifetime::Erased, &engine);
        let inference_lifetime = Type2::new_inference_variable(
            InferenceVariable::new(0, TyKind::Lifetime, closing_universe),
            &engine,
        );
        let constraints = Constraints::lifetimes_outlives(
            static_lifetime.clone(),
            inference_lifetime.clone(),
        )
        .union_into(Constraints::lifetimes_outlives(
            inference_lifetime,
            erased_lifetime.clone(),
        ));

        let cleaned = Solver::new(&Premise::default(), &engine)
            .check_and_clean_hrtb_constraints(constraints, closing_universe)
            .unwrap();

        assert_eq!(
            cleaned,
            Constraints::lifetimes_outlives(static_lifetime, erased_lifetime)
        );
    }

    // input: !s@U1: ?a@U1, ?a@U1: 'static
    // premise: U1 is the closing universe
    // output: leak check failure
    #[tokio::test]
    async fn hrtb_leak_check_checks_skolems_in_closing_universe() {
        let engine = create_engine().await;
        let closing_universe = UniverseIndex::root().next();
        let skolem_lifetime = Type2::new_skolemized_variable(
            SkolemizedVariable::new(0, TyKind::Lifetime, closing_universe),
            &engine,
        );
        let inference_lifetime = Type2::new_inference_variable(
            InferenceVariable::new(1, TyKind::Lifetime, closing_universe),
            &engine,
        );
        let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
        let constraints = Constraints::lifetimes_outlives(
            skolem_lifetime,
            inference_lifetime.clone(),
        )
        .union_into(Constraints::lifetimes_outlives(
            inference_lifetime,
            static_lifetime,
        ));

        let cleaned = Solver::new(&Premise::default(), &engine)
            .check_and_clean_hrtb_constraints(constraints, closing_universe);

        assert_eq!(cleaned, None);
    }
}
