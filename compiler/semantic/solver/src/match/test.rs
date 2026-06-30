use pernixc_qbice::{
    TrackedEngine, create_minimal_engine as create_test_engine,
};
use pernixc_type::r#type::{Type2, constructor::Primitive, kind::TyKind};
use qbice::storage::intern::Interned;

use crate::{premise::Premise, solver::Solver};

// input: match for<'a> fn() -> bool against fn() -> bool
// premise: {}
// output: no match
#[tokio::test]
async fn function_pointer_binders_must_match_exactly() {
    let engine = create_test_engine().await;
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let head = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [],
        bool_type.clone(),
        &engine,
    );
    let subject = Type2::new_function_pointer([], bool_type, &engine);
    let premise = Premise::default();

    let result =
        Solver::new(&premise, &engine).match_types(&head, &subject).await;

    assert!(result.is_none());
}

fn repeated_inference_tuple(
    solver: &mut Solver,
    engine: &TrackedEngine,
) -> Interned<Type2> {
    let variable = Type2::new_inference_variable(
        solver.fresh_inference_variable(TyKind::Type),
        engine,
    );

    Type2::new_tuple([variable.clone(), variable], engine)
}

// Input: match (?T, ?T) against (Int32, Int32).
// Premise: empty; ?T is a fresh type inference variable.
// Output: match succeeds with ?T = Int32.
#[tokio::test]
async fn repeated_inference_variable_matches_consistent_arguments() {
    let engine = pernixc_qbice::create_minimal_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let head = repeated_inference_tuple(&mut solver, &engine);
    let subject = Type2::new_tuple(
        [
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Int32, &engine),
        ],
        &engine,
    );

    assert!(solver.match_types(&head, &subject).await.is_some());
}

// Input: match (?T, ?T) against (Int32, Bool).
// Premise: empty; ?T is a fresh type inference variable.
// Output: match fails after the first substitution is applied.
#[tokio::test]
async fn repeated_inference_variable_rejects_inconsistent_arguments() {
    let engine = pernixc_qbice::create_minimal_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let head = repeated_inference_tuple(&mut solver, &engine);
    let subject = Type2::new_tuple(
        [
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Bool, &engine),
        ],
        &engine,
    );

    assert!(solver.match_types(&head, &subject).await.is_none());
}
