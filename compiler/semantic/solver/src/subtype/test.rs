use std::sync::Arc;

use pernixc_qbice::{Engine, InMemoryFactory, TrackedEngine};
use pernixc_symbol::GlobalSymbolID;
use pernixc_type::{
    predicate::Subtype,
    substitution::Substitution,
    r#type::{
        Type,
        bound::{Binder, BoundVariable},
        constructor::{
            Application, Constructor, FunctionPointer, InstanceAssociated,
            Lifetime, Mutability, Primitive, Reference, Tuple,
        },
        kind::TyKind,
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
    subtype::Step,
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

fn bound_lifetime(index: usize, engine: &TrackedEngine) -> Interned<Type> {
    engine.intern(Type::BoundVariable(BoundVariable::new(0, index)))
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

fn unit(engine: &TrackedEngine) -> Interned<Type> { tuple(Vec::new(), engine) }

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

fn function_pointer(
    lifetimes: usize,
    arguments: Vec<Interned<Type>>,
    engine: &TrackedEngine,
) -> Interned<Type> {
    engine.intern(Type::Application(Application::new(
        Constructor::FunctionPointer(FunctionPointer::new(Binder::new(
            engine.intern_unsized(vec![TyKind::Lifetime; lifetimes]),
        ))),
        engine.intern_unsized(arguments),
    )))
}

fn instance_associated(
    arguments: Vec<Interned<Type>>,
    engine: &TrackedEngine,
) -> Interned<Type> {
    engine.intern(Type::Application(Application::new(
        Constructor::InstanceAssociated(InstanceAssociated::new(
            GlobalSymbolID::default(),
        )),
        engine.intern_unsized(arguments),
    )))
}

async fn destructure_application(
    lesser: &Interned<Type>,
    greater: &Interned<Type>,
    engine: &TrackedEngine,
) -> Result<Option<Step>, OverflowError> {
    let Type::Application(lesser_application) = &**lesser else {
        panic!("expected application");
    };
    let Type::Application(greater_application) = &**greater else {
        panic!("expected application");
    };

    Solver::new(&Premise::default(), engine)
        .handle_application(
            lesser,
            greater,
            lesser_application,
            greater_application,
            Variance::Covariant,
        )
        .await
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

async fn resolve_step(
    lesser: Interned<Type>,
    greater: Interned<Type>,
    variance: Variance,
    engine: &TrackedEngine,
) -> Result<Step, OverflowError> {
    Solver::new(&Premise::default(), engine)
        .resolve_subtypes(vec![Subtype::new(lesser, greater, variance)])
        .await
}

fn contains_variable(ty: &Interned<Type>) -> bool {
    match &**ty {
        Type::InferenceVariable(_) | Type::SkolemizedVariable(_) => true,
        Type::Application(application) => {
            application.arguments().iter().any(contains_variable)
        }
        Type::GenericParameter(_) | Type::BoundVariable(_) => false,
    }
}

fn assert_no_variables_in_step(
    substitution: &Substitution,
    residual_subtypes: &[Subtype],
    constraints: &Constraints,
) {
    assert!(substitution.iter().all(|(_, ty)| !contains_variable(ty)));
    assert!(residual_subtypes.iter().all(|subtype| {
        !contains_variable(subtype.lesser())
            && !contains_variable(subtype.greater())
    }));
    assert!(constraints.clone().into_iter().all(|constraint| {
        !contains_variable(constraint.lesser())
            && !contains_variable(constraint.greater())
    }));
}

#[tokio::test]
async fn instance_associated_arguments_must_be_solved_immediately() {
    let engine = create_engine().await;
    let common_instance = primitive(Primitive::Bool, &engine);
    let lesser = instance_associated(
        vec![common_instance.clone(), primitive(Primitive::Int32, &engine)],
        &engine,
    );
    let greater = instance_associated(
        vec![common_instance, primitive(Primitive::Float32, &engine)],
        &engine,
    );

    assert_eq!(
        destructure_application(&lesser, &greater, &engine).await.unwrap(),
        None
    );
}

#[tokio::test]
async fn solved_instance_associated_arguments_are_not_deferred() {
    let engine = create_engine().await;
    let common_instance = primitive(Primitive::Bool, &engine);
    let static_lifetime = lifetime(Lifetime::Static, &engine);
    let erased_lifetime = lifetime(Lifetime::Erased, &engine);
    let lesser = instance_associated(
        vec![common_instance.clone(), static_lifetime.clone()],
        &engine,
    );
    let greater = instance_associated(
        vec![common_instance, erased_lifetime.clone()],
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        destructure_application(&lesser, &greater, &engine)
            .await
            .unwrap()
            .expect("arguments should solve immediately");

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_subtypes, Vec::new());
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(static_lifetime, erased_lifetime)
    );
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

// input:
// for<'a> fn(&'a u32, &'a u32) -> () <:
// for<'b, 'c> fn(&'b u32, &'c u32) -> () @ Covariant
// premise: {}
// output: {}
#[tokio::test]
async fn higher_ranked_lifetime_arguments_can_split() {
    let engine = create_engine().await;
    let u32_type = primitive(Primitive::Uint32, &engine);
    let lhs_lifetime = bound_lifetime(0, &engine);
    let rhs_first_lifetime = bound_lifetime(0, &engine);
    let rhs_second_lifetime = bound_lifetime(1, &engine);

    let lesser = function_pointer(
        1,
        vec![
            reference(
                lhs_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            reference(
                lhs_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            unit(&engine),
        ],
        &engine,
    );
    let greater = function_pointer(
        2,
        vec![
            reference(
                rhs_first_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            reference(
                rhs_second_lifetime,
                u32_type,
                Mutability::Immutable,
                &engine,
            ),
            unit(&engine),
        ],
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance::Covariant, &engine)
            .await
            .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_subtypes, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input:
// for<'a> fn(&'a u32, &'a u32) -> &'a u32 <:
// for<'b, 'c> fn(&'b u32, &'c u32) -> &'b u32 @ Covariant
// premise: {}
// output: stuck subtype problem
#[tokio::test]
async fn higher_ranked_lifetime_return_cannot_split_argument_identity() {
    let engine = create_engine().await;
    let u32_type = primitive(Primitive::Uint32, &engine);
    let lhs_lifetime = bound_lifetime(0, &engine);
    let rhs_first_lifetime = bound_lifetime(0, &engine);
    let rhs_second_lifetime = bound_lifetime(1, &engine);

    let lesser = function_pointer(
        1,
        vec![
            reference(
                lhs_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            reference(
                lhs_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            reference(
                lhs_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
        ],
        &engine,
    );
    let greater = function_pointer(
        2,
        vec![
            reference(
                rhs_first_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            reference(
                rhs_second_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            reference(
                rhs_first_lifetime,
                u32_type,
                Mutability::Immutable,
                &engine,
            ),
        ],
        &engine,
    );

    let (_, residual_subtypes, _) =
        resolve_step(lesser, greater, Variance::Covariant, &engine)
            .await
            .unwrap();

    assert!(!residual_subtypes.is_empty());
}

// input: for<'a> fn(&'a u32) -> () <: fn(&'static u32) -> () @ Covariant
// premise: {}
// output: {}
#[tokio::test]
async fn mixed_ranked_and_unranked_function_pointers_destructure() {
    let engine = create_engine().await;
    let u32_type = primitive(Primitive::Uint32, &engine);
    let ranked_lifetime = bound_lifetime(0, &engine);
    let static_lifetime = lifetime(Lifetime::Static, &engine);

    let lesser = function_pointer(
        1,
        vec![
            reference(
                ranked_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            unit(&engine),
        ],
        &engine,
    );
    let greater = function_pointer(
        0,
        vec![
            reference(
                static_lifetime,
                u32_type,
                Mutability::Immutable,
                &engine,
            ),
            unit(&engine),
        ],
        &engine,
    );

    let (_, residual_subtypes, _) =
        resolve_step(lesser, greater, Variance::Covariant, &engine)
            .await
            .unwrap();

    assert_eq!(residual_subtypes, Vec::new());
}

// input: fn(&'static u32) -> () <: for<'a> fn(&'a u32) -> () @ Covariant
// premise: {}
// output: stuck subtype problem
#[tokio::test]
async fn covariant_hrtb_rejects_skolem_to_external_leak() {
    let engine = create_engine().await;
    let u32_type = primitive(Primitive::Uint32, &engine);
    let ranked_lifetime = bound_lifetime(0, &engine);
    let static_lifetime = lifetime(Lifetime::Static, &engine);

    let lesser = function_pointer(
        0,
        vec![
            reference(
                static_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            unit(&engine),
        ],
        &engine,
    );
    let greater = function_pointer(
        1,
        vec![
            reference(
                ranked_lifetime,
                u32_type,
                Mutability::Immutable,
                &engine,
            ),
            unit(&engine),
        ],
        &engine,
    );

    let (_, residual_subtypes, _) =
        resolve_step(lesser, greater, Variance::Covariant, &engine)
            .await
            .unwrap();

    assert!(!residual_subtypes.is_empty());
}

// input:
// fn(&'static u32) -> () <: for<'a> fn(&'a u32) -> () @ Contravariant
// premise: {}
// output: {}
#[tokio::test]
async fn contravariant_top_level_variance_flips_hrtb_sides() {
    let engine = create_engine().await;
    let u32_type = primitive(Primitive::Uint32, &engine);
    let ranked_lifetime = bound_lifetime(0, &engine);
    let static_lifetime = lifetime(Lifetime::Static, &engine);

    let lesser = function_pointer(
        0,
        vec![
            reference(
                static_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            unit(&engine),
        ],
        &engine,
    );
    let greater = function_pointer(
        1,
        vec![
            reference(
                ranked_lifetime,
                u32_type,
                Mutability::Immutable,
                &engine,
            ),
            unit(&engine),
        ],
        &engine,
    );

    let (_, residual_subtypes, _) =
        resolve_step(lesser, greater, Variance::Contravariant, &engine)
            .await
            .unwrap();

    assert_eq!(residual_subtypes, Vec::new());
}

// input:
// for<'a> fn(&'a u32) -> &'a u32 <:
// for<'b> fn(&'b u32) -> &'b u32 @ Invariant
// premise: {}
// output: {}
#[tokio::test]
async fn invariant_hrtb_uses_independent_directional_runs() {
    let engine = create_engine().await;
    let u32_type = primitive(Primitive::Uint32, &engine);
    let lhs_lifetime = bound_lifetime(0, &engine);
    let rhs_lifetime = bound_lifetime(0, &engine);

    let lesser = function_pointer(
        1,
        vec![
            reference(
                lhs_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            reference(
                lhs_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
        ],
        &engine,
    );
    let greater = function_pointer(
        1,
        vec![
            reference(
                rhs_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            reference(rhs_lifetime, u32_type, Mutability::Immutable, &engine),
        ],
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance::Invariant, &engine)
            .await
            .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_subtypes, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: fn() -> &'static u32 <: for<'a> fn() -> &'a u32 @ Covariant
// premise: {}
// output: no inference or skolem variables in the returned step
#[tokio::test]
async fn hrtb_step_does_not_expose_internal_variables() {
    let engine = create_engine().await;
    let u32_type = primitive(Primitive::Uint32, &engine);
    let ranked_lifetime = bound_lifetime(0, &engine);
    let static_lifetime = lifetime(Lifetime::Static, &engine);

    let lesser = function_pointer(
        0,
        vec![
            unit(&engine),
            reference(
                static_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
        ],
        &engine,
    );
    let greater = function_pointer(
        1,
        vec![
            unit(&engine),
            reference(
                ranked_lifetime,
                u32_type,
                Mutability::Immutable,
                &engine,
            ),
        ],
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance::Covariant, &engine)
            .await
            .unwrap();

    assert_no_variables_in_step(
        &substitution,
        &residual_subtypes,
        &constraints,
    );
}

// input: fn() -> &'static u32 <: for<'a> fn() -> &'a u32 @ Covariant
// premise: {}
// output: 'static: 'static
#[tokio::test]
async fn external_to_skolem_return_obligation_rewrites_to_static() {
    let engine = create_engine().await;
    let u32_type = primitive(Primitive::Uint32, &engine);
    let ranked_lifetime = bound_lifetime(0, &engine);
    let static_lifetime = lifetime(Lifetime::Static, &engine);

    let lesser = function_pointer(
        0,
        vec![
            unit(&engine),
            reference(
                static_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
        ],
        &engine,
    );
    let greater = function_pointer(
        1,
        vec![
            unit(&engine),
            reference(
                ranked_lifetime,
                u32_type,
                Mutability::Immutable,
                &engine,
            ),
        ],
        &engine,
    );

    let (_, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance::Covariant, &engine)
            .await
            .unwrap();

    assert_eq!(residual_subtypes, Vec::new());
    assert_eq!(
        constraints,
        Constraints::lifetimes_outlives(
            static_lifetime.clone(),
            static_lifetime
        )
    );
}

// input:
// for<'a> fn(&'a u32) -> &'a u32 <:
// for<'b> fn(&'b u32) -> &'b u32 @ Bivariant
// premise: {}
// output: {}
#[tokio::test]
async fn bivariant_hrtb_function_pointers_do_not_emit_work() {
    let engine = create_engine().await;
    let u32_type = primitive(Primitive::Uint32, &engine);
    let lhs_lifetime = bound_lifetime(0, &engine);
    let rhs_lifetime = bound_lifetime(0, &engine);

    let lesser = function_pointer(
        1,
        vec![
            reference(
                lhs_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            reference(
                lhs_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
        ],
        &engine,
    );
    let greater = function_pointer(
        1,
        vec![
            reference(
                rhs_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            reference(rhs_lifetime, u32_type, Mutability::Immutable, &engine),
        ],
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance::Bivariant, &engine)
            .await
            .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_subtypes, Vec::new());
    assert_eq!(constraints, Constraints::default());
}
