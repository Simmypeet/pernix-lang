use pernixc_arena::ID;
use pernixc_qbice::{TrackedEngine, create_minimal_engine as create_engine};
use pernixc_symbol::{GlobalSymbolID, SymbolID};
use pernixc_target::TargetID;
use pernixc_type::{
    generic_parameters::{GenericParameter, GenericParameterID},
    r#type::{
        Type2, constructor::Lifetime, inference::InferenceVariable,
        kind::TyKind, skolem::SkolemizedVariable, universe::UniverseIndex,
    },
};
use qbice::storage::intern::Interned;

use crate::{constraints::Constraints, premise::Premise, solver::Solver};

const LIFETIME_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(1));

fn lifetime_parameter(index: u64, engine: &TrackedEngine) -> Interned<Type2> {
    Type2::new_generic_parameter(
        GenericParameterID::new(
            LIFETIME_SYMBOL_ID,
            ID::<GenericParameter>::new(index),
        ),
        engine,
    )
}

// input: 'static: ?a@U1, ?a@U1: 'erased
// premise: U1 is the closing universe
// output: 'static: 'erased
#[tokio::test]
async fn higher_ranked_cleanup_erases_inference_lifetimes_in_closing_universe()
{
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
        .check_and_clean_higher_ranked_constraints(
            constraints,
            closing_universe,
        )
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
async fn higher_ranked_leak_check_checks_skolems_in_closing_universe() {
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
        .check_and_clean_higher_ranked_constraints(
            constraints,
            closing_universe,
        );

    assert_eq!(cleaned, None);
}

// input: 'a: ?x@U1, ?x@U1: !y@U1
// premise: U1 is the closing universe
// output: 'a: 'static
#[tokio::test]
async fn higher_ranked_cleanup_replaces_reachable_skolem_with_static() {
    let engine = create_engine().await;
    let closing_universe = UniverseIndex::root().next();
    let lifetime_parameter = lifetime_parameter(0, &engine);
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
    let inference_lifetime = Type2::new_inference_variable(
        InferenceVariable::new(0, TyKind::Lifetime, closing_universe),
        &engine,
    );
    let skolem_lifetime = Type2::new_skolemized_variable(
        SkolemizedVariable::new(1, TyKind::Lifetime, closing_universe),
        &engine,
    );
    let constraints = Constraints::lifetimes_outlives(
        lifetime_parameter.clone(),
        inference_lifetime.clone(),
    )
    .union_into(Constraints::lifetimes_outlives(
        inference_lifetime,
        skolem_lifetime,
    ));

    let cleaned = Solver::new(&Premise::default(), &engine)
        .check_and_clean_higher_ranked_constraints(
            constraints,
            closing_universe,
        )
        .unwrap();

    assert_eq!(
        cleaned,
        Constraints::lifetimes_outlives(lifetime_parameter, static_lifetime)
    );
}

// input: 'a: ?x@U1, ?x@U1: ?y@U1
// premise: U1 is the closing universe
// output: {}
#[tokio::test]
async fn higher_ranked_cleanup_drops_internal_inference_chain() {
    let engine = create_engine().await;
    let closing_universe = UniverseIndex::root().next();
    let lifetime_parameter = lifetime_parameter(0, &engine);
    let inference_x = Type2::new_inference_variable(
        InferenceVariable::new(0, TyKind::Lifetime, closing_universe),
        &engine,
    );
    let inference_y = Type2::new_inference_variable(
        InferenceVariable::new(1, TyKind::Lifetime, closing_universe),
        &engine,
    );
    let constraints = Constraints::lifetimes_outlives(
        lifetime_parameter,
        inference_x.clone(),
    )
    .union_into(Constraints::lifetimes_outlives(inference_x, inference_y));

    let cleaned = Solver::new(&Premise::default(), &engine)
        .check_and_clean_higher_ranked_constraints(
            constraints,
            closing_universe,
        )
        .unwrap();

    assert_eq!(cleaned, Constraints::new());
}

// input: !x@U1: ?y@U1, ?y@U1: ?z@U1
// premise: U1 is the closing universe
// output: {}
#[tokio::test]
async fn higher_ranked_cleanup_drops_internal_skolem_to_inference_chain() {
    let engine = create_engine().await;
    let closing_universe = UniverseIndex::root().next();
    let skolem_x = Type2::new_skolemized_variable(
        SkolemizedVariable::new(0, TyKind::Lifetime, closing_universe),
        &engine,
    );
    let inference_y = Type2::new_inference_variable(
        InferenceVariable::new(1, TyKind::Lifetime, closing_universe),
        &engine,
    );
    let inference_z = Type2::new_inference_variable(
        InferenceVariable::new(2, TyKind::Lifetime, closing_universe),
        &engine,
    );
    let constraints =
        Constraints::lifetimes_outlives(skolem_x, inference_y.clone())
            .union_into(Constraints::lifetimes_outlives(
                inference_y,
                inference_z,
            ));

    let cleaned = Solver::new(&Premise::default(), &engine)
        .check_and_clean_higher_ranked_constraints(
            constraints,
            closing_universe,
        )
        .unwrap();

    assert_eq!(cleaned, Constraints::new());
}

// input: !x@U2: ?y@U2, ?y@U2: ?z@U1
// premise: U2 is the closing universe
// output: leak check failure
#[tokio::test]
async fn higher_ranked_leak_check_rejects_reachable_lower_universe_inference() {
    let engine = create_engine().await;
    let closing_universe = UniverseIndex::root().next().next();
    let lower_universe = UniverseIndex::root().next();
    let skolem_x = Type2::new_skolemized_variable(
        SkolemizedVariable::new(0, TyKind::Lifetime, closing_universe),
        &engine,
    );
    let inference_y = Type2::new_inference_variable(
        InferenceVariable::new(1, TyKind::Lifetime, closing_universe),
        &engine,
    );
    let inference_z = Type2::new_inference_variable(
        InferenceVariable::new(2, TyKind::Lifetime, lower_universe),
        &engine,
    );
    let constraints =
        Constraints::lifetimes_outlives(skolem_x, inference_y.clone())
            .union_into(Constraints::lifetimes_outlives(
                inference_y,
                inference_z,
            ));

    let cleaned = Solver::new(&Premise::default(), &engine)
        .check_and_clean_higher_ranked_constraints(
            constraints,
            closing_universe,
        );

    assert_eq!(cleaned, None);
}
