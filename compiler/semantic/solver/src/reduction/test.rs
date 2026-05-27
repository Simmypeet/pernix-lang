use std::sync::Arc;

use pernixc_qbice::{Engine, InMemoryFactory, TrackedEngine};
use pernixc_type::{
    predicate::{Equality, Predicate},
    r#type::{
        Type,
        bound::{Binder, BoundVariable},
        constructor::{
            Application, Constructor, Mutability, Primitive, Reference, Tuple,
        },
        kind::TyKind,
        skolem::SkolemizedVariable,
    },
};
use qbice::{
    serialize::Plugin, stable_hash::SeededStableHasherBuilder,
    storage::intern::Interned,
};

use crate::{
    constraints::Constraints,
    premise::Premise,
    solver::{OverflowError, Solver},
};

async fn create_test_engine() -> TrackedEngine {
    let engine = Engine::new_with(
        Plugin::default(),
        InMemoryFactory,
        SeededStableHasherBuilder::new(0),
    )
    .await
    .unwrap();

    Arc::new(engine).tracked().await
}

fn primitive(primitive: Primitive, engine: &TrackedEngine) -> Interned<Type> {
    engine.intern(Type::Application(Application::new(
        Constructor::Primitive(primitive),
        engine.intern_unsized(Vec::<Interned<Type>>::new()),
    )))
}

fn reference(
    lifetime: Interned<Type>,
    pointee: Interned<Type>,
    engine: &TrackedEngine,
) -> Interned<Type> {
    engine.intern(Type::Application(Application::new(
        Constructor::Reference(Reference::new(Mutability::Immutable)),
        engine.intern_unsized(vec![lifetime, pointee]),
    )))
}

fn skolem_lifetime(id: u64, engine: &TrackedEngine) -> Interned<Type> {
    engine.intern(Type::SkolemizedVariable(SkolemizedVariable::new(id)))
}

fn tuple(
    arguments: &[Interned<Type>],
    unpacked_positions: &[usize],
    engine: &TrackedEngine,
) -> Interned<Type> {
    engine.intern(Type::Application(Application::new(
        Constructor::Tuple(Tuple::new(
            engine.intern_unsized(unpacked_positions.to_vec()),
        )),
        engine.intern_unsized(arguments.to_vec()),
    )))
}

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

fn bound_type(index: usize, engine: &TrackedEngine) -> Interned<Type> {
    engine.intern(Type::BoundVariable(BoundVariable::new(0, index)))
}

fn bound_lifetime(index: usize, engine: &TrackedEngine) -> Interned<Type> {
    bound_type(index, engine)
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

    let inner = tuple(
        &[
            primitive(Primitive::Int32, &engine),
            tuple(
                &[
                    primitive(Primitive::Bool, &engine),
                    primitive(Primitive::Float32, &engine),
                ],
                &[],
                &engine,
            ),
            primitive(Primitive::Uint64, &engine),
        ],
        &[1],
        &engine,
    );

    let subject = tuple(&[inner], &[0], &engine);

    let (reduced, constraints) =
        reduce_type(subject, &Premise::default(), &engine)
            .await
            .unwrap()
            .unwrap();

    assert_eq!(
        reduced,
        tuple(
            &[
                primitive(Primitive::Int32, &engine),
                primitive(Primitive::Bool, &engine),
                primitive(Primitive::Float32, &engine),
                primitive(Primitive::Uint64, &engine),
            ],
            &[],
            &engine,
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
        reference(
            skolem_lifetime(0, &engine),
            primitive(Primitive::Bool, &engine),
            &engine,
        ),
        primitive(Primitive::Bool, &engine),
        &engine,
    ));

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
        reference(
            skolem_lifetime(1, &engine),
            primitive(Primitive::Bool, &engine),
            &engine,
        ),
        &premise,
        &engine,
        2,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(reduced, primitive(Primitive::Bool, &engine));
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(
            skolem_lifetime(0, &engine),
            skolem_lifetime(1, &engine),
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
        reference(skolem_lifetime(0, &engine), bound_type(0, &engine), &engine),
        bound_type(0, &engine),
        &engine,
    ));

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
        reference(
            skolem_lifetime(1, &engine),
            primitive(Primitive::Int32, &engine),
            &engine,
        ),
        &premise,
        &engine,
        2,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(reduced, primitive(Primitive::Int32, &engine));
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(
            skolem_lifetime(0, &engine),
            skolem_lifetime(1, &engine),
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
        reference(
            bound_lifetime(0, &engine),
            primitive(Primitive::Bool, &engine),
            &engine,
        ),
        primitive(Primitive::Bool, &engine),
        &engine,
    ));

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
        reference(
            skolem_lifetime(0, &engine),
            primitive(Primitive::Bool, &engine),
            &engine,
        ),
        &premise,
        &engine,
        1,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(reduced, primitive(Primitive::Bool, &engine));
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
        reference(
            bound_lifetime(0, &engine),
            primitive(Primitive::Bool, &engine),
            &engine,
        ),
        reference(
            bound_lifetime(0, &engine),
            primitive(Primitive::Int32, &engine),
            &engine,
        ),
        &engine,
    ));

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
        reference(
            skolem_lifetime(0, &engine),
            primitive(Primitive::Bool, &engine),
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
        reference(
            skolem_lifetime(0, &engine),
            primitive(Primitive::Int32, &engine),
            &engine,
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
        tuple(
            &[
                reference(
                    bound_lifetime(0, &engine),
                    primitive(Primitive::Bool, &engine),
                    &engine,
                ),
                reference(
                    bound_lifetime(1, &engine),
                    primitive(Primitive::Int32, &engine),
                    &engine,
                ),
            ],
            &[],
            &engine,
        ),
        tuple(
            &[
                reference(
                    bound_lifetime(1, &engine),
                    primitive(Primitive::Uint64, &engine),
                    &engine,
                ),
                reference(
                    bound_lifetime(0, &engine),
                    primitive(Primitive::Int32, &engine),
                    &engine,
                ),
            ],
            &[],
            &engine,
        ),
        &engine,
    ));

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
        tuple(
            &[
                reference(
                    skolem_lifetime(0, &engine),
                    primitive(Primitive::Bool, &engine),
                    &engine,
                ),
                reference(
                    skolem_lifetime(1, &engine),
                    primitive(Primitive::Int32, &engine),
                    &engine,
                ),
            ],
            &[],
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
        tuple(
            &[
                reference(
                    skolem_lifetime(1, &engine),
                    primitive(Primitive::Uint64, &engine),
                    &engine,
                ),
                reference(
                    skolem_lifetime(0, &engine),
                    primitive(Primitive::Int32, &engine),
                    &engine,
                ),
            ],
            &[],
            &engine,
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
        tuple(
            &[
                reference(
                    bound_lifetime(0, &engine),
                    primitive(Primitive::Int32, &engine),
                    &engine,
                ),
                reference(
                    bound_lifetime(0, &engine),
                    primitive(Primitive::Int32, &engine),
                    &engine,
                ),
            ],
            &[],
            &engine,
        ),
        primitive(Primitive::Bool, &engine),
        &engine,
    ));

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
        tuple(
            &[
                reference(
                    skolem_lifetime(1, &engine),
                    primitive(Primitive::Int32, &engine),
                    &engine,
                ),
                reference(
                    skolem_lifetime(2, &engine),
                    primitive(Primitive::Int32, &engine),
                    &engine,
                ),
            ],
            &[],
            &engine,
        ),
        &premise,
        &engine,
        3,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(reduced, primitive(Primitive::Bool, &engine));
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(
            skolem_lifetime(1, &engine),
            skolem_lifetime(2, &engine),
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
        tuple(
            &[
                reference(
                    bound_lifetime(0, &engine),
                    bound_type(1, &engine),
                    &engine,
                ),
                reference(
                    bound_lifetime(0, &engine),
                    bound_type(1, &engine),
                    &engine,
                ),
            ],
            &[],
            &engine,
        ),
        bound_type(1, &engine),
        &engine,
    ));

    let (reduced, constraints) = reduce_type_with_lifetime_skolems(
        tuple(
            &[
                reference(
                    skolem_lifetime(1, &engine),
                    primitive(Primitive::Uint64, &engine),
                    &engine,
                ),
                reference(
                    skolem_lifetime(2, &engine),
                    primitive(Primitive::Uint64, &engine),
                    &engine,
                ),
            ],
            &[],
            &engine,
        ),
        &premise,
        &engine,
        3,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(reduced, primitive(Primitive::Uint64, &engine));
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(
            skolem_lifetime(1, &engine),
            skolem_lifetime(2, &engine),
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
        tuple(&[bound_type(0, &engine)], &[], &engine),
        bound_type(0, &engine),
        &engine,
    ));

    let (reduced, constraints) = reduce_type(
        tuple(&[primitive(Primitive::Bool, &engine)], &[], &engine),
        &premise,
        &engine,
    )
    .await
    .unwrap()
    .unwrap();

    assert_eq!(reduced, primitive(Primitive::Bool, &engine));
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
        tuple(
            &[
                bound_type(0, &engine),
                bound_type(1, &engine),
                primitive(Primitive::Bool, &engine),
            ],
            &[],
            &engine,
        ),
        tuple(&[bound_type(1, &engine), bound_type(0, &engine)], &[], &engine),
        &engine,
    ));

    let (reduced, constraints) = reduce_type(
        tuple(
            &[
                primitive(Primitive::Int8, &engine),
                primitive(Primitive::Float32, &engine),
                primitive(Primitive::Bool, &engine),
            ],
            &[],
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
        tuple(
            &[
                primitive(Primitive::Float32, &engine),
                primitive(Primitive::Int8, &engine),
            ],
            &[],
            &engine,
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
        tuple(&[bound_type(0, &engine)], &[], &engine),
        bound_type(0, &engine),
        &engine,
    ));

    let reducible = tuple(
        &[tuple(
            &[
                primitive(Primitive::Int32, &engine),
                primitive(Primitive::Bool, &engine),
            ],
            &[],
            &engine,
        )],
        &[0],
        &engine,
    );

    let (reduced, constraints) =
        reduce_type(tuple(&[reducible], &[], &engine), &premise, &engine)
            .await
            .unwrap()
            .unwrap();

    assert_eq!(
        reduced,
        tuple(
            &[
                primitive(Primitive::Int32, &engine),
                primitive(Primitive::Bool, &engine),
            ],
            &[],
            &engine,
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
        primitive(Primitive::Int8, &engine),
        primitive(Primitive::Int16, &engine),
        &engine,
    ));
    premise.insert(equality(
        primitive(Primitive::Int16, &engine),
        primitive(Primitive::Bool, &engine),
        &engine,
    ));

    let (reduced, constraints) =
        reduce_type(primitive(Primitive::Int8, &engine), &premise, &engine)
            .await
            .unwrap()
            .unwrap();

    assert_eq!(reduced, primitive(Primitive::Bool, &engine));
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
        primitive(Primitive::Int8, &engine),
        tuple(
            &[tuple(
                &[
                    primitive(Primitive::Int32, &engine),
                    primitive(Primitive::Bool, &engine),
                ],
                &[],
                &engine,
            )],
            &[0],
            &engine,
        ),
        &engine,
    ));

    let (reduced, constraints) =
        reduce_type(primitive(Primitive::Int8, &engine), &premise, &engine)
            .await
            .unwrap()
            .unwrap();

    assert_eq!(
        reduced,
        tuple(
            &[
                primitive(Primitive::Int32, &engine),
                primitive(Primitive::Bool, &engine),
            ],
            &[],
            &engine,
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
        skolem_lifetime(1, &engine),
        tuple(
            &[primitive(Primitive::Bool, &engine), skolem_lifetime(1, &engine)],
            &[],
            &engine,
        ),
        &engine,
    ));

    assert!(
        reduce_type_with_lifetime_skolems(
            skolem_lifetime(1, &engine),
            &premise,
            &engine,
            2,
        )
        .await
        .is_err()
    );
}
