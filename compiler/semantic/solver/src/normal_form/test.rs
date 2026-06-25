use pernixc_qbice::{
    TrackedEngine, create_minimal_engine as create_test_engine,
};
use pernixc_type::{
    predicate::{Equality, Predicate},
    r#type::{
        Type,
        bound::{Binder, BoundVariable},
        constructor::Primitive,
        kind::TyKind,
        skolem::SkolemizedVariable,
    },
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    premise::Premise,
    solver::{OverflowError, Solver},
};

fn equality(
    left: Interned<Type>,
    right: Interned<Type>,
    engine: &TrackedEngine,
) -> Predicate {
    higher_ranked_equality(&[], left, right, engine)
}

fn higher_ranked_equality(
    bound_vars: &[TyKind],
    left: Interned<Type>,
    right: Interned<Type>,
    engine: &TrackedEngine,
) -> Predicate {
    Predicate::Equality(Equality::new(
        Binder::new(engine.intern_unsized(bound_vars.to_vec())),
        left,
        right,
    ))
}

async fn normal_form(
    ty: Interned<Type>,
    premise: &Premise,
    engine: &TrackedEngine,
) -> Result<Option<(Interned<Type>, Constraints)>, OverflowError> {
    Solver::new(premise, engine).normal_form(ty).await
}

async fn normal_form_with_lifetime_skolems(
    ty: Interned<Type>,
    premise: &Premise,
    engine: &TrackedEngine,
    skolem_count: usize,
) -> Result<Option<(Interned<Type>, Constraints)>, OverflowError> {
    let mut solver = Solver::new(premise, engine);

    for _ in 0..skolem_count {
        solver.fresh_skolem_variable(TyKind::Lifetime);
    }

    solver.normal_form(ty).await
}

// input: bool
// premise: {}
// output: Some((bool, {}))
#[tokio::test]
async fn returns_irreducible_type() {
    let engine = create_test_engine().await;
    let subject = Type::new_primitive(Primitive::Bool, &engine);

    let (normalized, constraints) =
        normal_form(subject.clone(), &Premise::default(), &engine)
            .await
            .unwrap()
            .unwrap();

    assert_eq!(normalized, subject);
    assert_eq!(constraints, Constraints::default());
}

// input: ...(...(int32, bool))
// premise: {}
// output: Some(((int32, bool), {}))
#[tokio::test]
async fn recursively_reduces_type() {
    let engine = create_test_engine().await;
    let subject = Type::new_tuple_with_unpack(
        [Type::new_tuple_with_unpack(
            [Type::new_tuple(
                [
                    Type::new_primitive(Primitive::Int32, &engine),
                    Type::new_primitive(Primitive::Bool, &engine),
                ],
                &engine,
            )],
            [0],
            &engine,
        )],
        [0],
        &engine,
    );

    let (normalized, constraints) =
        normal_form(subject, &Premise::default(), &engine)
            .await
            .unwrap()
            .unwrap();

    assert_eq!(
        normalized,
        Type::new_tuple(
            [
                Type::new_primitive(Primitive::Int32, &engine),
                Type::new_primitive(Primitive::Bool, &engine),
            ],
            &engine,
        )
    );
    assert_eq!(constraints, Constraints::default());
}

// input: &skolem(1) bool
// premise: &skolem(0) bool = bool
// output: Some((bool, {skolem(0) = skolem(1)}))
#[tokio::test]
async fn preserves_lifetime_constraints() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(equality(
        Type::new_immutable_reference(
            Type::new_skolemized_variable(SkolemizedVariable::new(0), &engine),
            Type::new_primitive(Primitive::Bool, &engine),
            &engine,
        ),
        Type::new_primitive(Primitive::Bool, &engine),
        &engine,
    ));

    let (normalized, constraints) = normal_form_with_lifetime_skolems(
        Type::new_immutable_reference(
            Type::new_skolemized_variable(SkolemizedVariable::new(1), &engine),
            Type::new_primitive(Primitive::Bool, &engine),
            &engine,
        ),
        &premise,
        &engine,
        2,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(normalized, Type::new_primitive(Primitive::Bool, &engine));
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(
            Type::new_skolemized_variable(SkolemizedVariable::new(0), &engine),
            Type::new_skolemized_variable(SkolemizedVariable::new(1), &engine),
        )
    );
}

// input: infer_type(0)
// premise: {}
// output: None
#[tokio::test]
async fn rejects_direct_type_inference_variable() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let inference = Type::new_inference_variable(
        solver.fresh_inference_variable(TyKind::Type),
        &engine,
    );

    assert_eq!(solver.normal_form(inference).await.unwrap(), None);
}

// input: infer_instance(0)
// premise: {}
// output: None
#[tokio::test]
async fn rejects_direct_instance_inference_variable() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let inference = Type::new_inference_variable(
        solver.fresh_inference_variable(TyKind::Instance),
        &engine,
    );

    assert_eq!(solver.normal_form(inference).await.unwrap(), None);
}

// input: (infer_type(0))
// premise: {}
// output: None
#[tokio::test]
async fn rejects_nested_type_inference_variable() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let inference = Type::new_inference_variable(
        solver.fresh_inference_variable(TyKind::Type),
        &engine,
    );

    assert_eq!(
        solver
            .normal_form(Type::new_tuple([inference], &engine))
            .await
            .unwrap(),
        None
    );
}

// input: &infer_lifetime(0) bool
// premise: {}
// output: Some((&infer_lifetime(0) bool, {}))
#[tokio::test]
async fn accepts_lifetime_inference_variable_argument() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let lifetime = Type::new_inference_variable(
        solver.fresh_inference_variable(TyKind::Lifetime),
        &engine,
    );
    let subject = Type::new_immutable_reference(
        lifetime,
        Type::new_primitive(Primitive::Bool, &engine),
        &engine,
    );

    let (normalized, constraints) =
        solver.normal_form(subject.clone()).await.unwrap().unwrap();

    assert_eq!(normalized, subject);
    assert_eq!(constraints, Constraints::default());
}

// input: (infer_type(0))
// premise: forall T. (T) = bool
// output: Some((bool, {}))
#[tokio::test]
async fn accepts_inference_eliminated_by_reduction() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(higher_ranked_equality(
        &[TyKind::Type],
        Type::new_tuple(
            [Type::new_bound_variable(BoundVariable::new(0, 0), &engine)],
            &engine,
        ),
        Type::new_primitive(Primitive::Bool, &engine),
        &engine,
    ));

    let mut solver = Solver::new(&premise, &engine);
    let inference = Type::new_inference_variable(
        solver.fresh_inference_variable(TyKind::Type),
        &engine,
    );
    let subject = Type::new_tuple([inference], &engine);

    let (normalized, constraints) =
        solver.normal_form(subject).await.unwrap().unwrap();

    assert_eq!(normalized, Type::new_primitive(Primitive::Bool, &engine));
    assert_eq!(constraints, Constraints::default());
}

// input: skolem(1)
// premise: skolem(1) = (bool, skolem(1))
// output: Overflow
#[tokio::test]
async fn propagates_reduction_overflow() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(equality(
        Type::new_skolemized_variable(SkolemizedVariable::new(1), &engine),
        Type::new_tuple(
            [
                Type::new_primitive(Primitive::Bool, &engine),
                Type::new_skolemized_variable(
                    SkolemizedVariable::new(1),
                    &engine,
                ),
            ],
            &engine,
        ),
        &engine,
    ));

    assert!(
        normal_form_with_lifetime_skolems(
            Type::new_skolemized_variable(SkolemizedVariable::new(1), &engine),
            &premise,
            &engine,
            2,
        )
        .await
        .is_err()
    );
}
