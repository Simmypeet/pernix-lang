use std::sync::Arc;

use pernixc_arena::ID;
use pernixc_qbice::{Config, Engine, InMemoryFactory, TrackedEngine};
use pernixc_symbol::{GlobalSymbolID, SymbolID, kind::Kind};
use pernixc_target::TargetID;
use pernixc_type::{
    generic_parameters::{
        self, GenericParameter, GenericParameterID, GenericParameterKind,
        GenericParameters, InstanceParameterKind,
    },
    predicate::{Equality, Outlives, Predicate},
    r#type::{
        Type,
        bound::{Binder, BoundVariable},
        constructor::{Lifetime, Primitive},
        kind::TyKind,
    },
};
use qbice::{
    executor, serialize::Plugin, stable_hash::SeededStableHasherBuilder,
    storage::intern::Interned,
};

use crate::{premise::Premise, solver::Solver};

const LIFETIME_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(1));
const TYPE_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(2));
const INSTANCE_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(3));
const TRAIT_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(4));
const ASSOCIATED_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(5));

struct GenericParametersExecutor;

impl executor::Executor<generic_parameters::Key, Config>
    for GenericParametersExecutor
{
    async fn execute(
        &self,
        key: &generic_parameters::Key,
        engine: &TrackedEngine,
    ) -> Interned<GenericParameters> {
        let kind = match key.symbol_id {
            LIFETIME_SYMBOL_ID => GenericParameterKind::Lifetime,
            TYPE_SYMBOL_ID => GenericParameterKind::Type,
            INSTANCE_SYMBOL_ID => {
                GenericParameterKind::Instance(InstanceParameterKind::new(None))
            }
            _ => panic!("unexpected generic parameter owner"),
        };

        engine.intern(GenericParameters::new((0..16).map(|index| {
            GenericParameter::new(
                engine.intern_unsized(format!("T{index}")),
                None,
                kind.clone(),
            )
        })))
    }
}

struct KindExecutor;

impl executor::Executor<pernixc_symbol::kind::Key, Config> for KindExecutor {
    async fn execute(
        &self,
        key: &pernixc_symbol::kind::Key,
        _: &TrackedEngine,
    ) -> Kind {
        match key.symbol_id {
            TYPE_SYMBOL_ID => Kind::Struct,
            INSTANCE_SYMBOL_ID => Kind::Instance,
            TRAIT_ID => Kind::Trait,
            ASSOCIATED_ID => Kind::InstanceAssociatedType,
            LIFETIME_SYMBOL_ID => {
                panic!("a generic parameter owner has no type constructor")
            }
            _ => panic!("unexpected kind lookup"),
        }
    }
}

async fn create_engine() -> TrackedEngine {
    let mut engine = Engine::new_with(
        Plugin::default(),
        InMemoryFactory,
        SeededStableHasherBuilder::new(0),
    )
    .await
    .unwrap();

    engine.register_executor(Arc::new(GenericParametersExecutor));
    engine.register_executor(Arc::new(KindExecutor));

    Arc::new(engine).tracked().await
}

fn generic(
    owner: GlobalSymbolID,
    index: u64,
    engine: &TrackedEngine,
) -> Interned<Type> {
    Type::new_generic_parameter(
        GenericParameterID::new(owner, ID::<GenericParameter>::new(index)),
        engine,
    )
}

fn lifetime_parameter(index: u64, engine: &TrackedEngine) -> Interned<Type> {
    generic(LIFETIME_SYMBOL_ID, index, engine)
}

fn type_parameter(index: u64, engine: &TrackedEngine) -> Interned<Type> {
    generic(TYPE_SYMBOL_ID, index, engine)
}

fn instance_parameter(index: u64, engine: &TrackedEngine) -> Interned<Type> {
    generic(INSTANCE_SYMBOL_ID, index, engine)
}

fn outlives(lesser: Interned<Type>, greater: Interned<Type>) -> Predicate {
    Predicate::Outlives(Outlives::new(lesser, greater))
}

fn equality(
    left: Interned<Type>,
    right: Interned<Type>,
    engine: &TrackedEngine,
) -> Predicate {
    Predicate::Equality(Equality::new(
        Binder::new(engine.intern_unsized(Vec::new())),
        left,
        right,
    ))
}

async fn prove(
    premise: &Premise,
    lesser: Interned<Type>,
    greater: Interned<Type>,
    engine: &TrackedEngine,
) -> bool {
    Solver::new(premise, engine)
        .outlives(&Outlives::new(lesser, greater))
        .await
        .unwrap()
}

// input: 'erased: 'erased, 'static: 'erased, 'erased: 'static
// premise: {}
// output: true, true, false
#[tokio::test]
async fn lifetime_reflexivity_static_and_erased() {
    let engine = create_engine().await;
    let erased = Type::new_lifetime(Lifetime::Erased, &engine);
    let static_lifetime = Type::new_lifetime(Lifetime::Static, &engine);

    assert!(
        prove(&Premise::default(), erased.clone(), erased.clone(), &engine,)
            .await
    );
    assert!(
        prove(&Premise::default(), static_lifetime, erased.clone(), &engine,)
            .await
    );
    assert!(
        !prove(
            &Premise::default(),
            erased,
            Type::new_lifetime(Lifetime::Static, &engine),
            &engine,
        )
        .await
    );
}

// input: 'a: 'c, 'a: 'd
// premise: 'a: 'b, 'b: 'c, 'c: 'b
// output: true, false
#[tokio::test]
async fn lifetime_premises_are_transitive_and_cycles_terminate() {
    let engine = create_engine().await;
    let a = lifetime_parameter(0, &engine);
    let b = lifetime_parameter(1, &engine);
    let c = lifetime_parameter(2, &engine);
    let d = lifetime_parameter(3, &engine);
    let mut premise = Premise::default();
    premise.insert(outlives(a.clone(), b.clone()));
    premise.insert(outlives(b.clone(), c.clone()));
    premise.insert(outlives(c.clone(), b));

    assert!(prove(&premise, a.clone(), c, &engine).await);
    assert!(!prove(&premise, a, d, &engine).await);
}

// input: 'b: 'bound
// premise: 'a: 'bound
// output: false
#[tokio::test]
async fn lifetime_premise_matching_requires_same_lesser() {
    let engine = create_engine().await;
    let a = lifetime_parameter(0, &engine);
    let b = lifetime_parameter(1, &engine);
    let bound = lifetime_parameter(2, &engine);
    let mut premise = Premise::default();
    premise.insert(outlives(a, bound.clone()));

    assert!(!prove(&premise, b, bound, &engine).await);
}

// input: (&'a T,): 'bound, bool: 'bound
// premise: 'a: 'bound, then T: 'bound
// output: false before T: 'bound, then true; bool is always true
#[tokio::test]
async fn ordinary_terms_require_every_component() {
    let engine = create_engine().await;
    let a = lifetime_parameter(0, &engine);
    let ty = type_parameter(0, &engine);
    let bound = lifetime_parameter(1, &engine);
    let subject = Type::new_tuple(
        [Type::new_immutable_reference(a.clone(), ty.clone(), &engine)],
        &engine,
    );
    let mut premise = Premise::default();
    premise.insert(outlives(a, bound.clone()));

    assert!(!prove(&premise, subject.clone(), bound.clone(), &engine).await);

    premise.insert(outlives(ty, bound.clone()));
    assert!(prove(&premise, subject, bound, &engine).await);
    assert!(
        prove(
            &Premise::default(),
            Type::new_primitive(Primitive::Bool, &engine),
            lifetime_parameter(2, &engine),
            &engine,
        )
        .await
    );
}

// input: Instance[T]: 'bound
// premise: {}, then T: 'bound
// output: false, then true
#[tokio::test]
async fn symbolic_instance_arguments_are_components() {
    let engine = create_engine().await;
    let argument = type_parameter(0, &engine);
    let bound = lifetime_parameter(0, &engine);
    let subject =
        Type::new_symbolic(INSTANCE_SYMBOL_ID, [argument.clone()], &engine);
    let mut premise = Premise::default();

    assert!(!prove(&premise, subject.clone(), bound.clone(), &engine).await);
    premise.insert(outlives(argument, bound.clone()));
    assert!(prove(&premise, subject, bound, &engine).await);
}

// input: for<'a> fn(&'a bool, T): 'bound
// premise: {}, then T: 'bound
// output: false, then true
#[tokio::test]
async fn function_pointer_ignores_bound_lifetimes_but_keeps_free_components() {
    let engine = create_engine().await;
    let free = type_parameter(0, &engine);
    let bound = lifetime_parameter(0, &engine);
    let subject = Type::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type::new_immutable_reference(
            Type::new_bound_variable(BoundVariable::new(0, 0), &engine),
            Type::new_primitive(Primitive::Bool, &engine),
            &engine,
        )],
        free.clone(),
        &engine,
    );
    let mut premise = Premise::default();

    assert!(!prove(&premise, subject.clone(), bound.clone(), &engine).await);
    premise.insert(outlives(free, bound.clone()));
    assert!(prove(&premise, subject, bound, &engine).await);
}

// input: ?T: 'static, !T: 'static
// premise: {}
// output: false, false
#[tokio::test]
async fn inference_and_skolem_variables_are_rigid_components() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let inference = solver.fresh_inference_variable(TyKind::Type);
    let skolem = solver.fresh_skolem_variable(TyKind::Type);
    let bound = Type::new_lifetime(Lifetime::Static, &engine);

    assert!(
        !solver
            .outlives(&Outlives::new(
                Type::new_inference_variable(inference, &engine),
                bound.clone(),
            ))
            .await
            .unwrap()
    );
    assert!(
        !solver
            .outlives(&Outlives::new(
                Type::new_skolemized_variable(skolem, &engine),
                bound,
            ))
            .await
            .unwrap()
    );
}

// input: 'a: 'bound, T: 'bound
// premise: (&'a T,): 'bound
// output: true, true
#[tokio::test]
async fn composite_premise_entails_each_component() {
    let engine = create_engine().await;
    let a = lifetime_parameter(0, &engine);
    let ty = type_parameter(0, &engine);
    let bound = lifetime_parameter(1, &engine);
    let mut premise = Premise::default();
    premise.insert(outlives(
        Type::new_tuple(
            [Type::new_immutable_reference(a.clone(), ty.clone(), &engine)],
            &engine,
        ),
        bound.clone(),
    ));

    assert!(prove(&premise, a, bound.clone(), &engine).await);
    assert!(prove(&premise, ty, bound, &engine).await);
}

// input: I::Assoc['a]: 'bound, I: 'bound, 'a: 'bound
// premise: I::Assoc['a]: 'bound
// output: true, false, false
#[tokio::test]
async fn associated_premise_is_atomic() {
    let engine = create_engine().await;
    let instance = instance_parameter(0, &engine);
    let argument = lifetime_parameter(0, &engine);
    let bound = lifetime_parameter(1, &engine);
    let associated = Type::new_instance_associated(
        ASSOCIATED_ID,
        instance.clone(),
        [argument.clone()],
        &engine,
    );
    let mut premise = Premise::default();
    premise.insert(outlives(associated.clone(), bound.clone()));

    assert!(prove(&premise, associated, bound.clone(), &engine).await);
    assert!(!prove(&premise, instance, bound.clone(), &engine).await);
    assert!(!prove(&premise, argument, bound, &engine).await);
}

// input: I::Assoc['a]: 'bound, I: 'bound, 'a: 'bound
// premise: (I::Assoc['a],): 'bound
// output: true, false, false
#[tokio::test]
async fn nested_associated_component_remains_atomic() {
    let engine = create_engine().await;
    let instance = instance_parameter(0, &engine);
    let argument = lifetime_parameter(0, &engine);
    let bound = lifetime_parameter(1, &engine);
    let associated = Type::new_instance_associated(
        ASSOCIATED_ID,
        instance.clone(),
        [argument.clone()],
        &engine,
    );
    let mut premise = Premise::default();
    premise.insert(outlives(
        Type::new_tuple([associated.clone()], &engine),
        bound.clone(),
    ));

    assert!(prove(&premise, associated, bound.clone(), &engine).await);
    assert!(!prove(&premise, instance, bound.clone(), &engine).await);
    assert!(!prove(&premise, argument, bound, &engine).await);
}

// input: I::Assoc['a]: 'bound
// premise: I: 'bound, then 'a: 'bound
// output: false, then true
#[tokio::test]
async fn associated_goal_falls_back_to_argument_components() {
    let engine = create_engine().await;
    let instance = instance_parameter(0, &engine);
    let argument = lifetime_parameter(0, &engine);
    let bound = lifetime_parameter(1, &engine);
    let associated = Type::new_instance_associated(
        ASSOCIATED_ID,
        instance.clone(),
        [argument.clone()],
        &engine,
    );
    let mut premise = Premise::default();
    premise.insert(outlives(instance, bound.clone()));

    assert!(!prove(&premise, associated.clone(), bound.clone(), &engine).await);
    premise.insert(outlives(argument, bound.clone()));
    assert!(prove(&premise, associated, bound, &engine).await);
}

// input: I::Assoc: 'bound
// premise: I::Assoc = bool
// output: true
#[tokio::test]
async fn associated_goal_can_be_reduced_by_equality() {
    let engine = create_engine().await;
    let associated = Type::new_instance_associated(
        ASSOCIATED_ID,
        Type::new_anonymous_trait_instance(TRAIT_ID, &engine),
        [],
        &engine,
    );
    let bound = lifetime_parameter(0, &engine);
    let mut premise = Premise::default();
    premise.insert(equality(
        associated.clone(),
        Type::new_primitive(Primitive::Bool, &engine),
        &engine,
    ));

    assert!(prove(&premise, associated, bound, &engine).await);
}

// input: I::Assoc['b]: 'bound
// premise: I::Assoc['a]: 'bound, then 'a: 'b and 'b: 'a
// output: false, then true
#[tokio::test]
async fn premise_matching_checks_lifetime_equality_constraints() {
    let engine = create_engine().await;
    let a = lifetime_parameter(0, &engine);
    let b = lifetime_parameter(1, &engine);
    let bound = lifetime_parameter(2, &engine);
    let instance = Type::new_anonymous_trait_instance(TRAIT_ID, &engine);
    let premise_associated = Type::new_instance_associated(
        ASSOCIATED_ID,
        instance.clone(),
        [a.clone()],
        &engine,
    );
    let goal_associated = Type::new_instance_associated(
        ASSOCIATED_ID,
        instance,
        [b.clone()],
        &engine,
    );
    let mut premise = Premise::default();
    premise.insert(outlives(premise_associated, bound.clone()));

    assert!(
        !prove(&premise, goal_associated.clone(), bound.clone(), &engine).await
    );

    premise.insert(outlives(a.clone(), b.clone()));
    premise.insert(outlives(b, a));
    assert!(prove(&premise, goal_associated, bound, &engine).await);
}

// input: bool: 'bound
// premise: ?T: 'bound
// output: panic because outlives premises may not contain inference variables
#[tokio::test]
#[should_panic(
    expected = "outlives premises must not contain inference variables"
)]
async fn premise_matching_rejects_inference_variable_substitution() {
    let engine = create_engine().await;
    let mut premise = Premise::default();
    let empty_premise = Premise::default();
    let mut variable_solver = Solver::new(&empty_premise, &engine);
    let inference = variable_solver.fresh_inference_variable(TyKind::Type);
    let inference = Type::new_inference_variable(inference, &engine);
    let bound = lifetime_parameter(0, &engine);
    premise.insert(outlives(inference, bound.clone()));

    let _ = prove(
        &premise,
        Type::new_primitive(Primitive::Bool, &engine),
        bound,
        &engine,
    )
    .await;
}
