use pernixc_qbice::create_minimal_engine as create_test_engine;
use pernixc_type::r#type::{Type, constructor::Primitive};

use crate::{premise::Premise, solver::Solver};

// input: match for<'a> fn() -> bool against fn() -> bool
// premise: {}
// output: no match
#[tokio::test]
async fn function_pointer_binders_must_match_exactly() {
    let engine = create_test_engine().await;
    let bool_type = Type::new_primitive(Primitive::Bool, &engine);
    let head = Type::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [],
        bool_type.clone(),
        &engine,
    );
    let subject = Type::new_function_pointer([], bool_type, &engine);
    let premise = Premise::default();

    let result =
        Solver::new(&premise, &engine).match_types(&head, &subject).await;

    assert!(result.is_none());
}
