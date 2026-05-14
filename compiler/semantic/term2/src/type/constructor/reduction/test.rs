use std::{
    future::{Future, ready},
    sync::Arc,
};

use pernixc_qbice::{Engine, InMemoryFactory, TrackedEngine};
use qbice::{
    serialize::Plugin, stable_hash::SeededStableHasherBuilder,
    storage::intern::Interned,
};

use super::*;
use crate::r#type::{
    Type,
    constructor::{Primitive, Tuple},
    kind::TyKind,
};

struct TestTyContext;

impl crate::r#type::context::InferenceVariableContext for TestTyContext {
    fn get_inference_variable_kind(
        &self,
        _: crate::r#type::inference::InferenceVariable,
    ) -> impl Future<Output = TyKind> + Send {
        ready(TyKind::Type)
    }
}

async fn create_test_engine() -> TrackedEngine {
    Arc::new(
        Engine::new_with(
            Plugin::default(),
            InMemoryFactory,
            SeededStableHasherBuilder::new(0),
        )
        .await
        .unwrap(),
    )
    .tracked()
    .await
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

#[tokio::test]
async fn reduce_tuple_flattens_unpacked_tuple_argument() {
    let engine = create_test_engine().await;
    let tyctx = TestTyContext;

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
    .reduce(&engine, &tyctx)
    .unwrap();

    assert_eq!(
        reduced,
        tuple_application(
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
    let tyctx = TestTyContext;

    let original = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &engine),
            intern_primitive(Primitive::Bool, &engine),
            intern_primitive(Primitive::Uint64, &engine),
        ],
        &[1],
        &engine,
    );

    assert_eq!(original.reduce(&engine, &tyctx), None);
}

#[tokio::test]
async fn reduce_tuple_preserves_inner_unpacked_positions() {
    let engine = create_test_engine().await;
    let tyctx = TestTyContext;

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
    .reduce(&engine, &tyctx)
    .unwrap();

    assert_eq!(
        reduced,
        tuple_application(
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
    let tyctx = TestTyContext;

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
    .reduce(&engine, &tyctx)
    .unwrap();

    assert_eq!(
        reduced,
        tuple_application(
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
