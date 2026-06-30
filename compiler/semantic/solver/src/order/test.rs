use std::sync::Arc;

use pernixc_arena::ID;
use pernixc_qbice::{Config, Engine, TrackedEngine};
use pernixc_symbol::{GlobalSymbolID, SymbolID};
use pernixc_target::TargetID;
use pernixc_type::{
    generic_parameters::{
        self, GenericParameter, GenericParameterID, GenericParameterKind,
        GenericParameters,
    },
    symbol::TraitRef,
    r#type::{Type, constructor::Primitive},
};
use qbice::{
    executor, serialize::Plugin, stable_hash::SeededStableHasherBuilder,
    storage::intern::Interned,
};

use crate::{
    order::Order,
    premise::Premise,
    solver::{OverflowError, Solver},
};

const TRAIT_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(1));
const LEFT_GENERIC_OWNER_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(2));
const RIGHT_GENERIC_OWNER_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(3));

struct GenericParametersExecutor;

impl executor::Executor<generic_parameters::Key, Config>
    for GenericParametersExecutor
{
    async fn execute(
        &self,
        key: &generic_parameters::Key,
        engine: &TrackedEngine,
    ) -> Interned<GenericParameters> {
        match key.symbol_id {
            LEFT_GENERIC_OWNER_ID | RIGHT_GENERIC_OWNER_ID => {
                engine.intern(GenericParameters::new((0..4).map(|index| {
                    GenericParameter::new(
                        engine.intern_unsized(format!("T{index}")),
                        None,
                        GenericParameterKind::Type,
                    )
                })))
            }

            _ => engine.intern(GenericParameters::new([])),
        }
    }
}

async fn create_engine() -> TrackedEngine {
    let mut engine = Engine::new_with(
        Plugin::default(),
        pernixc_qbice::InMemoryFactory,
        SeededStableHasherBuilder::new(0),
    )
    .await
    .unwrap();

    engine.register_executor(Arc::new(GenericParametersExecutor));

    Arc::new(engine).tracked().await
}

async fn order(
    left: TraitRef,
    right: TraitRef,
    engine: &TrackedEngine,
) -> Result<Order, OverflowError> {
    Solver::new(&Premise::default(), engine)
        .order_trait_refs(&left, &right)
        .await
}

fn trait_ref(
    arguments: impl IntoIterator<Item = Interned<Type>>,
    engine: &TrackedEngine,
) -> TraitRef {
    TraitRef::new(
        TRAIT_ID,
        engine.intern_unsized(arguments.into_iter().collect::<Vec<_>>()),
    )
}

fn left_generic(index: u64, engine: &TrackedEngine) -> Interned<Type> {
    Type::new_generic_parameter(
        GenericParameterID::new(
            LEFT_GENERIC_OWNER_ID,
            ID::<GenericParameter>::new(index),
        ),
        engine,
    )
}

fn right_generic(index: u64, engine: &TrackedEngine) -> Interned<Type> {
    Type::new_generic_parameter(
        GenericParameterID::new(
            RIGHT_GENERIC_OWNER_ID,
            ID::<GenericParameter>::new(index),
        ),
        engine,
    )
}

fn int(engine: &TrackedEngine) -> Interned<Type> {
    Type::new_primitive(Primitive::Int32, engine)
}

fn bool(engine: &TrackedEngine) -> Interned<Type> {
    Type::new_primitive(Primitive::Bool, engine)
}

// Input: left = Trait[T], right = Trait[Int32].
// Premise: empty; T is a left-side type generic parameter.
// Output: left is more general than right.
#[tokio::test]
async fn generic_trait_ref_is_more_general_than_concrete() {
    let engine = create_engine().await;
    let generic = trait_ref([left_generic(0, &engine)], &engine);
    let concrete = trait_ref([int(&engine)], &engine);

    assert_eq!(
        order(generic, concrete, &engine).await.unwrap(),
        Order::MoreGeneral
    );
}

// Input: left = Trait[Int32], right = Trait[T].
// Premise: empty; T is a right-side type generic parameter.
// Output: left is more specific than right.
#[tokio::test]
async fn concrete_trait_ref_is_more_specific_than_generic() {
    let engine = create_engine().await;
    let concrete = trait_ref([int(&engine)], &engine);
    let generic = trait_ref([right_generic(0, &engine)], &engine);

    assert_eq!(
        order(concrete, generic, &engine).await.unwrap(),
        Order::MoreSpecific
    );
}

// Input: left = Trait[Int32], right = Trait[Bool].
// Premise: empty.
// Output: left and right are incompatible.
#[tokio::test]
async fn different_concrete_arguments_are_incompatible() {
    let engine = create_engine().await;
    let int_ref = trait_ref([int(&engine)], &engine);
    let bool_ref = trait_ref([bool(&engine)], &engine);

    assert_eq!(
        order(int_ref, bool_ref, &engine).await.unwrap(),
        Order::Incompatible
    );
}

// Input: left = Trait[T], right = Trait[U].
// Premise: empty; T and U are distinct type generic parameters.
// Output: left and right are ambiguous.
#[tokio::test]
async fn alpha_equivalent_generics_are_ambiguous() {
    let engine = create_engine().await;
    let left = trait_ref([left_generic(0, &engine)], &engine);
    let right = trait_ref([right_generic(0, &engine)], &engine);

    assert_eq!(order(left, right, &engine).await.unwrap(), Order::Ambiguous);
}

// Input: left = Trait[T, T], right = Trait[Int32, Bool].
// Premise: empty; T is a left-side type generic parameter.
// Output: left and right are incompatible.
#[tokio::test]
async fn repeated_parameter_must_match_consistently() {
    let engine = create_engine().await;
    let repeated = trait_ref(
        [left_generic(0, &engine), left_generic(0, &engine)],
        &engine,
    );
    let mixed = trait_ref([int(&engine), bool(&engine)], &engine);

    assert_eq!(
        order(repeated, mixed, &engine).await.unwrap(),
        Order::Incompatible
    );
}

// Input: left = Trait[Int32, T], right = Trait[U, Int32].
// Premise: empty; T and U are distinct type generic parameters.
// Output: left and right overlap at Trait[Int32, Int32], so they are ambiguous.
#[tokio::test]
async fn cross_shape_overlap_is_ambiguous() {
    let engine = create_engine().await;
    let left = trait_ref([int(&engine), left_generic(0, &engine)], &engine);
    let right = trait_ref([right_generic(0, &engine), int(&engine)], &engine);

    assert_eq!(order(left, right, &engine).await.unwrap(), Order::Ambiguous);
}
