use std::sync::Arc;

use pernixc_qbice::{Engine, InMemoryFactory, TrackedEngine};
use pernixc_type::{
    predicate::Subtype,
    substitution::Substitution,
    r#type::{
        Type,
        constructor::{
            Application, Constructor, Lifetime, Mutability, Primitive,
            Reference, Tuple,
        },
    },
    variance::Variance,
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

async fn create_engine() -> TrackedEngine {
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

fn lifetime(lifetime: Lifetime, engine: &TrackedEngine) -> Interned<Type> {
    engine.intern(Type::Application(Application::new(
        Constructor::Lifetime(lifetime),
        engine.intern_unsized(Vec::<Interned<Type>>::new()),
    )))
}

fn tuple(
    arguments: Vec<Interned<Type>>,
    engine: &TrackedEngine,
) -> Interned<Type> {
    engine.intern(Type::Application(Application::new(
        Constructor::Tuple(Tuple::new(engine.intern_unsized(Vec::new()))),
        engine.intern_unsized(arguments),
    )))
}

fn reference(
    lifetime: Interned<Type>,
    pointee: Interned<Type>,
    mutability: Mutability,
    engine: &TrackedEngine,
) -> Interned<Type> {
    engine.intern(Type::Application(Application::new(
        Constructor::Reference(Reference::new(mutability)),
        engine.intern_unsized(vec![lifetime, pointee]),
    )))
}

async fn resolve_one(
    lesser: Interned<Type>,
    greater: Interned<Type>,
    variance: Variance,
    engine: &TrackedEngine,
) -> Result<Constraints, OverflowError> {
    let (substitution, residual_subtypes, constraints) =
        Solver::new(&Premise::default(), engine)
            .resolve_subtypes(vec![Subtype::new(lesser, greater, variance)])
            .await?;

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_subtypes, Vec::new());

    Ok(constraints)
}

// input: ('static) <: ('erased) @ Covariant
// premise: {}
// output: 'static: 'erased
#[tokio::test]
async fn tuple_arguments_follow_parent_covariance() {
    let engine = create_engine().await;
    let static_lifetime = lifetime(Lifetime::Static, &engine);
    let erased_lifetime = lifetime(Lifetime::Erased, &engine);

    let constraints = resolve_one(
        tuple(vec![static_lifetime.clone()], &engine),
        tuple(vec![erased_lifetime.clone()], &engine),
        Variance::Covariant,
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(
        constraints,
        Constraints::lifetimes_outlives(static_lifetime, erased_lifetime)
    );
}

// input: ('static) <: ('erased) @ Contravariant
// premise: {}
// output: 'erased: 'static
#[tokio::test]
async fn tuple_arguments_flip_under_parent_contravariance() {
    let engine = create_engine().await;
    let static_lifetime = lifetime(Lifetime::Static, &engine);
    let erased_lifetime = lifetime(Lifetime::Erased, &engine);

    let constraints = resolve_one(
        tuple(vec![static_lifetime.clone()], &engine),
        tuple(vec![erased_lifetime.clone()], &engine),
        Variance::Contravariant,
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(
        constraints,
        Constraints::lifetimes_outlives(erased_lifetime, static_lifetime)
    );
}

// input: ('static) <: ('erased) @ Invariant
// premise: {}
// output: 'static = 'erased
#[tokio::test]
async fn tuple_arguments_become_equal_under_parent_invariance() {
    let engine = create_engine().await;
    let static_lifetime = lifetime(Lifetime::Static, &engine);
    let erased_lifetime = lifetime(Lifetime::Erased, &engine);

    let constraints = resolve_one(
        tuple(vec![static_lifetime.clone()], &engine),
        tuple(vec![erased_lifetime.clone()], &engine),
        Variance::Invariant,
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(static_lifetime, erased_lifetime)
    );
}

// input: ('static) <: ('erased) @ Bivariant
// premise: {}
// output: {}
#[tokio::test]
async fn tuple_arguments_are_ignored_under_parent_bivariance() {
    let engine = create_engine().await;

    let constraints = resolve_one(
        tuple(vec![lifetime(Lifetime::Static, &engine)], &engine),
        tuple(vec![lifetime(Lifetime::Erased, &engine)], &engine),
        Variance::Bivariant,
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(constraints, Constraints::default());
}

// input: &mut &'static bool <: &mut &'erased bool @ Covariant
// premise: {}
// output: 'static = 'erased
#[tokio::test]
async fn mutable_reference_pointees_are_invariant() {
    let engine = create_engine().await;
    let static_lifetime = lifetime(Lifetime::Static, &engine);
    let erased_lifetime = lifetime(Lifetime::Erased, &engine);
    let common_reference_lifetime = lifetime(Lifetime::Static, &engine);
    let bool_type = primitive(Primitive::Bool, &engine);

    let constraints = resolve_one(
        reference(
            common_reference_lifetime.clone(),
            reference(
                static_lifetime.clone(),
                bool_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            Mutability::Mutable,
            &engine,
        ),
        reference(
            common_reference_lifetime,
            reference(
                erased_lifetime.clone(),
                bool_type,
                Mutability::Immutable,
                &engine,
            ),
            Mutability::Mutable,
            &engine,
        ),
        Variance::Covariant,
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(static_lifetime, erased_lifetime)
    );
}
