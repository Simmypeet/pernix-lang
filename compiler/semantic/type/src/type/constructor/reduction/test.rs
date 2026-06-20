use std::sync::Arc;

use pernixc_arena::ID;
use pernixc_qbice::{Config, Engine, InMemoryFactory, TrackedEngine};
use pernixc_symbol::{GlobalSymbolID, SymbolID, kind::Kind, member::Member};
use pernixc_target::TargetID;
use qbice::{
    executor, serialize::Plugin, stable_hash::SeededStableHasherBuilder,
    storage::intern::Interned,
};

use super::*;
use crate::{
    generic_parameters::{
        self, GenericParameter, GenericParameterID, GenericParameterKind,
        GenericParameters,
    },
    instance_associated,
    r#type::{
        Type,
        constructor::{Primitive, Tuple},
    },
};

const INSTANCE_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(1));
const TRAIT_ASSOC_INSTANCE_PARAM_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(2));
const TRAIT_ASSOC_MIXED_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(3));
const TRAIT_ASSOC_NON_TYPE_KIND_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(4));
const TRAIT_ASSOC_RECURSIVE_INNER_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(5));
const INSTANCE_ASSOC_INSTANCE_PARAM_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(12));
const INSTANCE_ASSOC_MIXED_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(13));
const INSTANCE_ASSOC_NON_TYPE_KIND_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(14));

struct KindExecutor;

impl executor::Executor<pernixc_symbol::kind::Key, Config> for KindExecutor {
    async fn execute(
        &self,
        key: &pernixc_symbol::kind::Key,
        _: &TrackedEngine,
    ) -> Kind {
        match key.symbol_id {
            INSTANCE_SYMBOL_ID => Kind::Instance,
            TRAIT_ASSOC_INSTANCE_PARAM_SYMBOL_ID
            | TRAIT_ASSOC_MIXED_SYMBOL_ID
            | TRAIT_ASSOC_RECURSIVE_INNER_SYMBOL_ID => {
                Kind::TraitAssociatedType
            }
            TRAIT_ASSOC_NON_TYPE_KIND_SYMBOL_ID => {
                Kind::TraitAssociatedInstance
            }
            INSTANCE_ASSOC_INSTANCE_PARAM_SYMBOL_ID
            | INSTANCE_ASSOC_MIXED_SYMBOL_ID => Kind::InstanceAssociatedType,
            INSTANCE_ASSOC_NON_TYPE_KIND_SYMBOL_ID => {
                Kind::InstanceAssociatedInstance
            }
            _ => panic!("unexpected symbol id: {:?}", key.symbol_id),
        }
    }
}

struct NameExecutor;

impl executor::Executor<pernixc_symbol::name::Key, Config> for NameExecutor {
    async fn execute(
        &self,
        key: &pernixc_symbol::name::Key,
        engine: &TrackedEngine,
    ) -> Interned<str> {
        match key.symbol_id {
            TRAIT_ASSOC_INSTANCE_PARAM_SYMBOL_ID
            | INSTANCE_ASSOC_INSTANCE_PARAM_SYMBOL_ID => {
                engine.intern_unsized("assoc_param".to_owned())
            }
            TRAIT_ASSOC_MIXED_SYMBOL_ID | INSTANCE_ASSOC_MIXED_SYMBOL_ID => {
                engine.intern_unsized("assoc_mixed".to_owned())
            }
            TRAIT_ASSOC_NON_TYPE_KIND_SYMBOL_ID
            | INSTANCE_ASSOC_NON_TYPE_KIND_SYMBOL_ID => {
                engine.intern_unsized("assoc_non_type".to_owned())
            }
            TRAIT_ASSOC_RECURSIVE_INNER_SYMBOL_ID => {
                engine.intern_unsized("assoc_recursive".to_owned())
            }
            _ => panic!("unexpected symbol id: {:?}", key.symbol_id),
        }
    }
}

struct MemberExecutor;

impl executor::Executor<pernixc_symbol::member::Key, Config>
    for MemberExecutor
{
    async fn execute(
        &self,
        key: &pernixc_symbol::member::Key,
        engine: &TrackedEngine,
    ) -> Interned<Member> {
        match key.symbol_id {
            INSTANCE_SYMBOL_ID => {
                let mut member = Member::default();
                member.member_ids_by_name.insert(
                    engine.intern_unsized("assoc_param".to_owned()),
                    INSTANCE_ASSOC_INSTANCE_PARAM_SYMBOL_ID.id,
                );
                member.member_ids_by_name.insert(
                    engine.intern_unsized("assoc_mixed".to_owned()),
                    INSTANCE_ASSOC_MIXED_SYMBOL_ID.id,
                );
                member.member_ids_by_name.insert(
                    engine.intern_unsized("assoc_non_type".to_owned()),
                    INSTANCE_ASSOC_NON_TYPE_KIND_SYMBOL_ID.id,
                );

                engine.intern(member)
            }

            _ => panic!("unexpected member lookup: {:?}", key.symbol_id),
        }
    }
}

struct GenericParametersExecutor;

impl executor::Executor<generic_parameters::Key, Config>
    for GenericParametersExecutor
{
    async fn execute(
        &self,
        key: &generic_parameters::Key,
        engine: &TrackedEngine,
    ) -> Interned<GenericParameters> {
        let generic_parameters = match key.symbol_id {
            INSTANCE_SYMBOL_ID | INSTANCE_ASSOC_MIXED_SYMBOL_ID => {
                GenericParameters::from_kinds(
                    [GenericParameterKind::Type],
                    engine,
                )
            }
            INSTANCE_ASSOC_INSTANCE_PARAM_SYMBOL_ID
            | INSTANCE_ASSOC_NON_TYPE_KIND_SYMBOL_ID => {
                GenericParameters::from_kinds([], engine)
            }
            _ => panic!(
                "unexpected generic parameters lookup: {:?}",
                key.symbol_id
            ),
        };

        engine.intern(generic_parameters)
    }
}

struct InstanceAssociatedTypeExecutor;

impl executor::Executor<instance_associated::Key, Config>
    for InstanceAssociatedTypeExecutor
{
    async fn execute(
        &self,
        key: &instance_associated::Key,
        engine: &TrackedEngine,
    ) -> Interned<Type> {
        match key.symbol_id {
            INSTANCE_ASSOC_MIXED_SYMBOL_ID => tuple_type(
                &[
                    generic_parameter_type(
                        GenericParameterID::new(
                            INSTANCE_SYMBOL_ID,
                            ID::<GenericParameter>::new(0),
                        ),
                        engine,
                    ),
                    generic_parameter_type(
                        GenericParameterID::new(
                            INSTANCE_ASSOC_MIXED_SYMBOL_ID,
                            ID::<GenericParameter>::new(0),
                        ),
                        engine,
                    ),
                ],
                &[],
                engine,
            ),

            INSTANCE_ASSOC_INSTANCE_PARAM_SYMBOL_ID
            | INSTANCE_ASSOC_NON_TYPE_KIND_SYMBOL_ID => generic_parameter_type(
                GenericParameterID::new(
                    INSTANCE_SYMBOL_ID,
                    ID::<GenericParameter>::new(0),
                ),
                engine,
            ),

            _ => panic!(
                "unexpected instance associated symbol: {:?}",
                key.symbol_id
            ),
        }
    }
}

async fn create_test_engine() -> TrackedEngine {
    let mut engine = Engine::new_with(
        Plugin::default(),
        InMemoryFactory,
        SeededStableHasherBuilder::new(0),
    )
    .await
    .unwrap();

    engine.register_executor(Arc::new(KindExecutor));
    engine.register_executor(Arc::new(NameExecutor));
    engine.register_executor(Arc::new(MemberExecutor));
    engine.register_executor(Arc::new(GenericParametersExecutor));
    engine.register_executor(Arc::new(InstanceAssociatedTypeExecutor));

    Arc::new(engine).tracked().await
}

fn intern_primitive(
    primitive: Primitive,
    engine: &TrackedEngine,
) -> Interned<Type> {
    engine.intern(Type::Application(Application {
        constructor: Constructor::Primitive(primitive),
        arguments: engine.intern_unsized(Vec::<Interned<Type>>::new()),
    }))
}

fn generic_parameter_type(
    parameter_id: GenericParameterID,
    engine: &TrackedEngine,
) -> Interned<Type> {
    engine.intern(Type::GenericParameter(parameter_id))
}

fn symbolic_type(
    symbolic_id: GlobalSymbolID,
    arguments: &[Interned<Type>],
    engine: &TrackedEngine,
) -> Interned<Type> {
    engine.intern(Type::Application(Application {
        constructor: Constructor::Symbolic(Symbolic { symbolic_id }),
        arguments: engine.intern_unsized(arguments.to_vec()),
    }))
}

fn tuple_type(
    arguments: &[Interned<Type>],
    unpacked_positions: &[usize],
    engine: &TrackedEngine,
) -> Interned<Type> {
    engine.intern(Type::Application(Application {
        constructor: Constructor::Tuple(Tuple {
            unpacked_positions: engine
                .intern_unsized(unpacked_positions.to_vec()),
        }),
        arguments: engine.intern_unsized(arguments.to_vec()),
    }))
}

fn tuple_application(
    arguments: &[Interned<Type>],
    unpacked_positions: &[usize],
    engine: &TrackedEngine,
) -> Application {
    let Type::Application(application) =
        &*tuple_type(arguments, unpacked_positions, engine)
    else {
        panic!("expected tuple application");
    };

    application.clone()
}

fn instance_associated_application(
    instance: Interned<Type>,
    trait_associated_id: GlobalSymbolID,
    associated_arguments: &[Interned<Type>],
    engine: &TrackedEngine,
) -> Application {
    let mut arguments = Vec::with_capacity(1 + associated_arguments.len());
    arguments.push(instance);
    arguments.extend_from_slice(associated_arguments);

    Application {
        constructor: Constructor::InstanceAssociated(
            crate::r#type::constructor::InstanceAssociated {
                trait_associated_id,
            },
        ),
        arguments: engine.intern_unsized(arguments),
    }
}

#[tokio::test]
async fn reduce_tuple_flattens_unpacked_tuple_argument() {
    let engine = create_test_engine().await;

    let reduced = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &engine),
            tuple_type(
                &[
                    intern_primitive(Primitive::Bool, &engine),
                    intern_primitive(Primitive::Float32, &engine),
                    intern_primitive(Primitive::Usize, &engine),
                ],
                &[],
                &engine,
            ),
            intern_primitive(Primitive::Uint64, &engine),
        ],
        &[1],
        &engine,
    )
    .reduce(&engine)
    .await
    .unwrap();

    assert_eq!(
        reduced,
        tuple_type(
            &[
                intern_primitive(Primitive::Int32, &engine),
                intern_primitive(Primitive::Bool, &engine),
                intern_primitive(Primitive::Float32, &engine),
                intern_primitive(Primitive::Usize, &engine),
                intern_primitive(Primitive::Uint64, &engine),
            ],
            &[],
            &engine,
        )
    );
}

#[tokio::test]
async fn reduce_tuple_returns_none_for_non_tuple_unpacked_argument() {
    let engine = create_test_engine().await;

    let original = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &engine),
            intern_primitive(Primitive::Bool, &engine),
            intern_primitive(Primitive::Uint64, &engine),
        ],
        &[1],
        &engine,
    );

    assert_eq!(original.reduce(&engine).await, None);
}

#[tokio::test]
async fn reduce_tuple_preserves_inner_unpacked_positions() {
    let engine = create_test_engine().await;

    let reduced = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &engine),
            tuple_type(
                &[
                    intern_primitive(Primitive::Bool, &engine),
                    intern_primitive(Primitive::Float32, &engine),
                    intern_primitive(Primitive::Usize, &engine),
                ],
                &[1],
                &engine,
            ),
            intern_primitive(Primitive::Uint64, &engine),
        ],
        &[1],
        &engine,
    )
    .reduce(&engine)
    .await
    .unwrap();

    assert_eq!(
        reduced,
        tuple_type(
            &[
                intern_primitive(Primitive::Int32, &engine),
                intern_primitive(Primitive::Bool, &engine),
                intern_primitive(Primitive::Float32, &engine),
                intern_primitive(Primitive::Usize, &engine),
                intern_primitive(Primitive::Uint64, &engine),
            ],
            &[2],
            &engine,
        )
    );
}

#[tokio::test]
async fn reduce_tuple_shifts_later_unpacked_positions() {
    let engine = create_test_engine().await;

    let reduced = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &engine),
            tuple_type(
                &[
                    intern_primitive(Primitive::Bool, &engine),
                    intern_primitive(Primitive::Float32, &engine),
                ],
                &[],
                &engine,
            ),
            intern_primitive(Primitive::Uint64, &engine),
        ],
        &[1, 2],
        &engine,
    )
    .reduce(&engine)
    .await
    .unwrap();

    assert_eq!(
        reduced,
        tuple_type(
            &[
                intern_primitive(Primitive::Int32, &engine),
                intern_primitive(Primitive::Bool, &engine),
                intern_primitive(Primitive::Float32, &engine),
                intern_primitive(Primitive::Uint64, &engine),
            ],
            &[3],
            &engine,
        )
    );
}

#[tokio::test]
async fn reduce_instance_associated_substitutes_instance_generic_argument() {
    let engine = create_test_engine().await;

    let reduced = instance_associated_application(
        symbolic_type(
            INSTANCE_SYMBOL_ID,
            &[intern_primitive(Primitive::Bool, &engine)],
            &engine,
        ),
        TRAIT_ASSOC_INSTANCE_PARAM_SYMBOL_ID,
        &[],
        &engine,
    )
    .reduce(&engine)
    .await
    .unwrap();

    assert_eq!(reduced, intern_primitive(Primitive::Bool, &engine));
}

#[tokio::test]
async fn reduce_instance_associated_substitutes_associated_generic_arguments() {
    let engine = create_test_engine().await;

    let reduced = instance_associated_application(
        symbolic_type(
            INSTANCE_SYMBOL_ID,
            &[intern_primitive(Primitive::Bool, &engine)],
            &engine,
        ),
        TRAIT_ASSOC_MIXED_SYMBOL_ID,
        &[intern_primitive(Primitive::Uint64, &engine)],
        &engine,
    )
    .reduce(&engine)
    .await
    .unwrap();

    assert_eq!(
        reduced,
        tuple_type(
            &[
                intern_primitive(Primitive::Bool, &engine),
                intern_primitive(Primitive::Uint64, &engine),
            ],
            &[],
            &engine,
        )
    );
}

#[tokio::test]
async fn reduce_instance_associated_does_not_require_type_kind() {
    let engine = create_test_engine().await;

    let reduced = instance_associated_application(
        symbolic_type(
            INSTANCE_SYMBOL_ID,
            &[intern_primitive(Primitive::Bool, &engine)],
            &engine,
        ),
        TRAIT_ASSOC_NON_TYPE_KIND_SYMBOL_ID,
        &[],
        &engine,
    )
    .reduce(&engine)
    .await
    .unwrap();

    assert_eq!(reduced, intern_primitive(Primitive::Bool, &engine));
}

#[tokio::test]
async fn reduce_instance_associated_does_not_recurse_into_inner_instance() {
    let engine = create_test_engine().await;

    let inner_instance =
        engine.intern(Type::Application(instance_associated_application(
            symbolic_type(
                INSTANCE_SYMBOL_ID,
                &[intern_primitive(Primitive::Bool, &engine)],
                &engine,
            ),
            TRAIT_ASSOC_RECURSIVE_INNER_SYMBOL_ID,
            &[],
            &engine,
        )));

    let outer = instance_associated_application(
        inner_instance,
        TRAIT_ASSOC_NON_TYPE_KIND_SYMBOL_ID,
        &[],
        &engine,
    );

    assert_eq!(outer.reduce(&engine,).await, None);
}
