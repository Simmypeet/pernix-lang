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

async fn reduce_type(
    ty: Interned<Type>,
    premise: &Premise,
    engine: &TrackedEngine,
) -> Result<Option<(Interned<Type>, Constraints)>, OverflowError> {
    Solver::new(premise, engine).reduce_type(ty).await
}

async fn reduce_type_with_lifetime_skolems(
    ty: Interned<Type>,
    premise: &Premise,
    engine: &TrackedEngine,
    skolem_count: usize,
) -> Result<Option<(Interned<Type>, Constraints)>, OverflowError> {
    let mut solver = Solver::new(premise, engine);

    for _ in 0..skolem_count {
        solver.fresh_skolem_variable(TyKind::Lifetime);
    }

    solver.reduce_type(ty).await
}

// input: (int32, ...(bool, float32),  uint64)
// premise: {}
// output: (int32, bool, float32, uint64), {}
#[tokio::test]
async fn reduce_type_recursively_reduces_nested_applications() {
    let engine = create_test_engine().await;

    let inner = Type::new_tuple_with_unpack(
        [
            Type::new_primitive(Primitive::Int32, &engine),
            Type::new_tuple(
                [
                    Type::new_primitive(Primitive::Bool, &engine),
                    Type::new_primitive(Primitive::Float32, &engine),
                ],
                &engine,
            ),
            Type::new_primitive(Primitive::Uint64, &engine),
        ],
        [1],
        &engine,
    );

    let subject = Type::new_tuple_with_unpack([inner], [0], &engine);

    let (reduced, constraints) =
        reduce_type(subject, &Premise::default(), &engine)
            .await
            .unwrap()
            .unwrap();

    assert_eq!(
        reduced,
        Type::new_tuple(
            [
                Type::new_primitive(Primitive::Int32, &engine),
                Type::new_primitive(Primitive::Bool, &engine),
                Type::new_primitive(Primitive::Float32, &engine),
                Type::new_primitive(Primitive::Uint64, &engine),
            ],
            &engine
        )
    );
    assert_eq!(constraints, Constraints::default());
}

// input: &skolem(1) bool
// premise: &skolem(0) bool = bool
// output: bool, skolem(0) = skolem(1)
#[tokio::test]
async fn reduce_type_emits_lifetime_constraints_from_equality_match() {
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

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
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

    assert_eq!(reduced, Type::new_primitive(Primitive::Bool, &engine));
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(
            Type::new_skolemized_variable(SkolemizedVariable::new(0), &engine),
            Type::new_skolemized_variable(SkolemizedVariable::new(1), &engine),
        )
    );
}

// input: &skolem(1) int32
// premise: forall T. &skolem(0) T = T
// output: int32, skolem(0) = skolem(1)
#[tokio::test]
async fn reduce_type_emits_lifetime_constraints_with_higher_ranked_equality() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(higher_ranked_equality(
        &[TyKind::Type],
        Type::new_immutable_reference(
            Type::new_skolemized_variable(SkolemizedVariable::new(0), &engine),
            Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
            &engine,
        ),
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
        &engine,
    ));

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
        Type::new_immutable_reference(
            Type::new_skolemized_variable(SkolemizedVariable::new(1), &engine),
            Type::new_primitive(Primitive::Int32, &engine),
            &engine,
        ),
        &premise,
        &engine,
        2,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(reduced, Type::new_primitive(Primitive::Int32, &engine));
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(
            Type::new_skolemized_variable(SkolemizedVariable::new(0), &engine),
            Type::new_skolemized_variable(SkolemizedVariable::new(1), &engine),
        )
    );
}

// input: &skolem(0) bool
// premise: forall 'a. &'a bool = bool
// output: bool, {}
#[tokio::test]
async fn reduce_type_instantiates_higher_ranked_lifetime() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(higher_ranked_equality(
        &[TyKind::Lifetime],
        Type::new_immutable_reference(
            Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
            Type::new_primitive(Primitive::Bool, &engine),
            &engine,
        ),
        Type::new_primitive(Primitive::Bool, &engine),
        &engine,
    ));

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
        Type::new_immutable_reference(
            Type::new_skolemized_variable(SkolemizedVariable::new(0), &engine),
            Type::new_primitive(Primitive::Bool, &engine),
            &engine,
        ),
        &premise,
        &engine,
        1,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(reduced, Type::new_primitive(Primitive::Bool, &engine));
    assert_eq!(constraints, Constraints::default());
}

// input: &skolem(0) bool
// premise: forall 'a. &'a bool = &'a int32
// output: &skolem(0) int32, {}
#[tokio::test]
async fn reduce_type_substitutes_higher_ranked_lifetime_in_output() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(higher_ranked_equality(
        &[TyKind::Lifetime],
        Type::new_immutable_reference(
            Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
            Type::new_primitive(Primitive::Bool, &engine),
            &engine,
        ),
        Type::new_immutable_reference(
            Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
            Type::new_primitive(Primitive::Int32, &engine),
            &engine,
        ),
        &engine,
    ));

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
        Type::new_immutable_reference(
            Type::new_skolemized_variable(SkolemizedVariable::new(0), &engine),
            Type::new_primitive(Primitive::Bool, &engine),
            &engine,
        ),
        &premise,
        &engine,
        1,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(
        reduced,
        Type::new_immutable_reference(
            Type::new_skolemized_variable(SkolemizedVariable::new(0), &engine),
            Type::new_primitive(Primitive::Int32, &engine),
            &engine
        )
    );
    assert_eq!(constraints, Constraints::default());
}

// input: (&skolem(0) bool, &skolem(1) int32)
// premise: forall 'a, 'b. (&'a bool, &'b int32) =
//          (&'b uint64, &'a int32)
// output: (&skolem(1) uint64, &skolem(0) int32), {}
#[tokio::test]
async fn reduce_type_substitutes_multiple_higher_ranked_lifetimes() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(higher_ranked_equality(
        &[TyKind::Lifetime, TyKind::Lifetime],
        Type::new_tuple(
            [
                Type::new_immutable_reference(
                    Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
                    Type::new_primitive(Primitive::Bool, &engine),
                    &engine,
                ),
                Type::new_immutable_reference(
                    Type::new_bound_variable(BoundVariable::new(0, 1), &engine),
                    Type::new_primitive(Primitive::Int32, &engine),
                    &engine,
                ),
            ],
            &engine,
        ),
        Type::new_tuple(
            [
                Type::new_immutable_reference(
                    Type::new_bound_variable(BoundVariable::new(0, 1), &engine),
                    Type::new_primitive(Primitive::Uint64, &engine),
                    &engine,
                ),
                Type::new_immutable_reference(
                    Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
                    Type::new_primitive(Primitive::Int32, &engine),
                    &engine,
                ),
            ],
            &engine,
        ),
        &engine,
    ));

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
        Type::new_tuple(
            [
                Type::new_immutable_reference(
                    Type::new_skolemized_variable(
                        SkolemizedVariable::new(0),
                        &engine,
                    ),
                    Type::new_primitive(Primitive::Bool, &engine),
                    &engine,
                ),
                Type::new_immutable_reference(
                    Type::new_skolemized_variable(
                        SkolemizedVariable::new(1),
                        &engine,
                    ),
                    Type::new_primitive(Primitive::Int32, &engine),
                    &engine,
                ),
            ],
            &engine,
        ),
        &premise,
        &engine,
        2,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(
        reduced,
        Type::new_tuple(
            [
                Type::new_immutable_reference(
                    Type::new_skolemized_variable(
                        SkolemizedVariable::new(1),
                        &engine
                    ),
                    Type::new_primitive(Primitive::Uint64, &engine),
                    &engine
                ),
                Type::new_immutable_reference(
                    Type::new_skolemized_variable(
                        SkolemizedVariable::new(0),
                        &engine
                    ),
                    Type::new_primitive(Primitive::Int32, &engine),
                    &engine
                ),
            ],
            &engine
        )
    );
    assert_eq!(constraints, Constraints::default());
}

// input: (&skolem(1) int32, &skolem(2) int32)
// premise: forall 'a. (&'a int32, &'a int32) = bool
// output: bool, skolem(1) = skolem(2)
#[tokio::test]
async fn reduce_type_emits_constraints_for_repeated_higher_ranked_lifetime() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(higher_ranked_equality(
        &[TyKind::Lifetime],
        Type::new_tuple(
            [
                Type::new_immutable_reference(
                    Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
                    Type::new_primitive(Primitive::Int32, &engine),
                    &engine,
                ),
                Type::new_immutable_reference(
                    Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
                    Type::new_primitive(Primitive::Int32, &engine),
                    &engine,
                ),
            ],
            &engine,
        ),
        Type::new_primitive(Primitive::Bool, &engine),
        &engine,
    ));

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
        Type::new_tuple(
            [
                Type::new_immutable_reference(
                    Type::new_skolemized_variable(
                        SkolemizedVariable::new(1),
                        &engine,
                    ),
                    Type::new_primitive(Primitive::Int32, &engine),
                    &engine,
                ),
                Type::new_immutable_reference(
                    Type::new_skolemized_variable(
                        SkolemizedVariable::new(2),
                        &engine,
                    ),
                    Type::new_primitive(Primitive::Int32, &engine),
                    &engine,
                ),
            ],
            &engine,
        ),
        &premise,
        &engine,
        3,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(reduced, Type::new_primitive(Primitive::Bool, &engine));
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(
            Type::new_skolemized_variable(SkolemizedVariable::new(1), &engine),
            Type::new_skolemized_variable(SkolemizedVariable::new(2), &engine),
        )
    );
}

// input: (&skolem(1) uint64, &skolem(2) uint64)
// premise: forall 'a, T. (&'a T, &'a T) = T
// output: uint64, skolem(1) = skolem(2)
#[tokio::test]
async fn reduce_type_emits_constraints_for_repeated_lifetime_with_type_output()
{
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(higher_ranked_equality(
        &[TyKind::Lifetime, TyKind::Type],
        Type::new_tuple(
            [
                Type::new_immutable_reference(
                    Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
                    Type::new_bound_variable(BoundVariable::new(0, 1), &engine),
                    &engine,
                ),
                Type::new_immutable_reference(
                    Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
                    Type::new_bound_variable(BoundVariable::new(0, 1), &engine),
                    &engine,
                ),
            ],
            &engine,
        ),
        Type::new_bound_variable(BoundVariable::new(0, 1), &engine),
        &engine,
    ));

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
        Type::new_tuple(
            [
                Type::new_immutable_reference(
                    Type::new_skolemized_variable(
                        SkolemizedVariable::new(1),
                        &engine,
                    ),
                    Type::new_primitive(Primitive::Uint64, &engine),
                    &engine,
                ),
                Type::new_immutable_reference(
                    Type::new_skolemized_variable(
                        SkolemizedVariable::new(2),
                        &engine,
                    ),
                    Type::new_primitive(Primitive::Uint64, &engine),
                    &engine,
                ),
            ],
            &engine,
        ),
        &premise,
        &engine,
        3,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(reduced, Type::new_primitive(Primitive::Uint64, &engine));
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(
            Type::new_skolemized_variable(SkolemizedVariable::new(1), &engine),
            Type::new_skolemized_variable(SkolemizedVariable::new(2), &engine),
        )
    );
}

// input: (bool)
// premise: forall T. (T) = T
// output: bool, {}
#[tokio::test]
async fn reduce_type_instantiates_higher_ranked_equality() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(higher_ranked_equality(
        &[TyKind::Type],
        Type::new_tuple(
            [Type::new_bound_variable(BoundVariable::new(0, 0), &engine)],
            &engine,
        ),
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
        &engine,
    ));

    let (reduced, constraints) = reduce_type(
        Type::new_tuple(
            [Type::new_primitive(Primitive::Bool, &engine)],
            &engine,
        ),
        &premise,
        &engine,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(reduced, Type::new_primitive(Primitive::Bool, &engine));
    assert_eq!(constraints, Constraints::default());
}

// input: (int8, float32, bool)
// premise: forall T, U. (T, U, bool) = (U, T)
// output: (float32, int8), {}
#[tokio::test]
async fn reduce_type_substitutes_multiple_higher_ranked_variables() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(higher_ranked_equality(
        &[TyKind::Type, TyKind::Type],
        Type::new_tuple(
            [
                Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
                Type::new_bound_variable(BoundVariable::new(0, 1), &engine),
                Type::new_primitive(Primitive::Bool, &engine),
            ],
            &engine,
        ),
        Type::new_tuple(
            [
                Type::new_bound_variable(BoundVariable::new(0, 1), &engine),
                Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
            ],
            &engine,
        ),
        &engine,
    ));

    let (reduced, constraints) = reduce_type(
        Type::new_tuple(
            [
                Type::new_primitive(Primitive::Int8, &engine),
                Type::new_primitive(Primitive::Float32, &engine),
                Type::new_primitive(Primitive::Bool, &engine),
            ],
            &engine,
        ),
        &premise,
        &engine,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(
        reduced,
        Type::new_tuple(
            [
                Type::new_primitive(Primitive::Float32, &engine),
                Type::new_primitive(Primitive::Int8, &engine),
            ],
            &engine
        )
    );
    assert_eq!(constraints, Constraints::default());
}

// input: (((int32, bool)...))
// premise: forall T. (T) = T
// output: (int32, bool), {}
#[tokio::test]
async fn reduce_type_continues_after_higher_ranked_equality() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(higher_ranked_equality(
        &[TyKind::Type],
        Type::new_tuple(
            [Type::new_bound_variable(BoundVariable::new(0, 0), &engine)],
            &engine,
        ),
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
        &engine,
    ));

    let reducible = Type::new_tuple_with_unpack(
        [Type::new_tuple(
            [
                Type::new_primitive(Primitive::Int32, &engine),
                Type::new_primitive(Primitive::Bool, &engine),
            ],
            &engine,
        )],
        [0],
        &engine,
    );

    let (reduced, constraints) =
        reduce_type(Type::new_tuple([reducible], &engine), &premise, &engine)
            .await
            .unwrap()
            .unwrap();

    assert_eq!(
        reduced,
        Type::new_tuple(
            [
                Type::new_primitive(Primitive::Int32, &engine),
                Type::new_primitive(Primitive::Bool, &engine),
            ],
            &engine
        )
    );
    assert_eq!(constraints, Constraints::default());
}

// input: int8
// premise: int8 = int16, int16 = bool
// output: bool, {}
#[tokio::test]
async fn reduce_type_transitively_reduces_equality_chain() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(equality(
        Type::new_primitive(Primitive::Int8, &engine),
        Type::new_primitive(Primitive::Int16, &engine),
        &engine,
    ));
    premise.insert(equality(
        Type::new_primitive(Primitive::Int16, &engine),
        Type::new_primitive(Primitive::Bool, &engine),
        &engine,
    ));

    let (reduced, constraints) = reduce_type(
        Type::new_primitive(Primitive::Int8, &engine),
        &premise,
        &engine,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(reduced, Type::new_primitive(Primitive::Bool, &engine));
    assert_eq!(constraints, Constraints::default());
}

// input: int8
// premise: int8 = ...((int32, bool))
// output: (int32, bool), {}
#[tokio::test]
async fn reduce_type_reduces_type_reached_by_equality() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();

    premise.insert(equality(
        Type::new_primitive(Primitive::Int8, &engine),
        Type::new_tuple_with_unpack(
            [Type::new_tuple(
                [
                    Type::new_primitive(Primitive::Int32, &engine),
                    Type::new_primitive(Primitive::Bool, &engine),
                ],
                &engine,
            )],
            [0],
            &engine,
        ),
        &engine,
    ));

    let (reduced, constraints) = reduce_type(
        Type::new_primitive(Primitive::Int8, &engine),
        &premise,
        &engine,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(
        reduced,
        Type::new_tuple(
            [
                Type::new_primitive(Primitive::Int32, &engine),
                Type::new_primitive(Primitive::Bool, &engine),
            ],
            &engine
        )
    );
    assert_eq!(constraints, Constraints::default());
}

// input: skolem(1)
// premise: skolem(1) = (bool, skolem(1))
// output: Overflow
#[tokio::test]
async fn reduce_type_overflows_on_self_expanding_equality() {
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
        reduce_type_with_lifetime_skolems(
            Type::new_skolemized_variable(SkolemizedVariable::new(1), &engine),
            &premise,
            &engine,
            2,
        )
        .await
        .is_err()
    );
}
