use pernixc_qbice::create_minimal_engine as create_test_engine;
use pernixc_type::r#type::{
    Type2, inference::InferenceVariable, skolem::SkolemizedVariable,
};

use crate::{constraints::Constraints, premise::Premise, solver::Solver};

// input: !S: 'R
// premise: !S is a skolemized higher-ranked lifetime
// output: None
#[tokio::test]
async fn rejects_skolem_outliving_another_lifetime() {
    let engine = create_test_engine().await;
    let skolem = SkolemizedVariable::new(0);
    let constraints = Constraints::lifetimes_outlives(
        Type2::new_skolemized_variable(skolem, &engine),
        Type2::new_inference_variable(InferenceVariable::new(0), &engine),
    );

    let checked = Solver::new(&Premise::default(), &engine)
        .lite_leak_check(constraints, [skolem]);

    assert_eq!(checked, None);
}

// input: 'R: !S
// premise: !S is a skolemized higher-ranked lifetime
// output: {'R: 'static}
#[tokio::test]
async fn rewrites_lifetime_outliving_skolem_to_static() {
    let engine = create_test_engine().await;
    let skolem = SkolemizedVariable::new(0);
    let lifetime =
        Type2::new_inference_variable(InferenceVariable::new(0), &engine);
    let constraints = Constraints::lifetimes_outlives(
        lifetime.clone(),
        Type2::new_skolemized_variable(skolem, &engine),
    );

    let checked = Solver::new(&Premise::default(), &engine)
        .lite_leak_check(constraints, [skolem]);

    assert_eq!(
        checked,
        Some(Constraints::lifetimes_outlives(
            lifetime,
            Type2::new_static_lifetime(&engine),
        ))
    );
}

// input: !S: !S
// premise: !S is a skolemized higher-ranked lifetime
// output: {}
#[tokio::test]
async fn discards_skolem_outliving_itself() {
    let engine = create_test_engine().await;
    let skolem = SkolemizedVariable::new(0);
    let lifetime = Type2::new_skolemized_variable(skolem, &engine);
    let constraints =
        Constraints::lifetimes_outlives(lifetime.clone(), lifetime);

    let checked = Solver::new(&Premise::default(), &engine)
        .lite_leak_check(constraints, [skolem]);

    assert_eq!(checked, Some(Constraints::new()));
}

// input: !T: 'R
// premise: only !S is a skolemized higher-ranked lifetime
// output: {!T: 'R}
#[tokio::test]
async fn preserves_constraints_for_unlisted_skolems() {
    let engine = create_test_engine().await;
    let listed_skolem = SkolemizedVariable::new(0);
    let other_skolem = SkolemizedVariable::new(1);
    let constraints = Constraints::lifetimes_outlives(
        Type2::new_skolemized_variable(other_skolem, &engine),
        Type2::new_inference_variable(InferenceVariable::new(0), &engine),
    );

    let checked = Solver::new(&Premise::default(), &engine)
        .lite_leak_check(constraints.clone(), [listed_skolem]);

    assert_eq!(checked, Some(constraints));
}
