#![allow(clippy::trivially_copy_pass_by_ref)]
use pernixc_qbice::{
    TrackedEngine, create_minimal_engine as create_test_engine,
};

use super::*;
use crate::r#type::{constructor::Primitive, kind::TyKind};

fn primitive_application(
    primitive: Primitive,
    arguments: &[Interned<Type2>],
    engine: &TrackedEngine,
) -> Application {
    Application {
        constructor: Constructor::Primitive(primitive),
        arguments: engine.intern_unsized(arguments.to_vec()),
    }
}

fn tuple_type(
    arguments: &[Interned<Type2>],
    unpacked_positions: &[usize],
    engine: &TrackedEngine,
) -> Interned<Type2> {
    Type2::new_tuple_with_unpack(
        arguments.iter().cloned(),
        unpacked_positions.iter().copied(),
        engine,
    )
}

fn tuple_application(
    arguments: &[Interned<Type2>],
    unpacked_positions: &[usize],
    engine: &TrackedEngine,
) -> Application {
    let Type2::Application(application) =
        &*tuple_type(arguments, unpacked_positions, engine)
    else {
        panic!("expected application");
    };

    application.clone()
}

fn application(ty: &Interned<Type2>) -> Application {
    let Type2::Application(application) = &**ty else {
        panic!("expected application");
    };

    application.clone()
}

#[tokio::test]
async fn destructure_same_non_tuple_constructor() {
    // lhs: int32<(bool, float32)>, rhs: int32<(usize, uint64)>
    // result: (bool, usize), (float32, uint64)
    let engine = create_test_engine().await;
    let lhs = primitive_application(
        Primitive::Int32,
        &[
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Float32, &engine),
        ],
        &engine,
    );
    let rhs = primitive_application(
        Primitive::Int32,
        &[
            Type2::new_primitive(Primitive::Usize, &engine),
            Type2::new_primitive(Primitive::Uint64, &engine),
        ],
        &engine,
    );

    let destructured = lhs
        .destructure(&rhs, DestructureOptions::ignore_binders(), &engine)
        .unwrap()
        .collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Usize, &engine),
        ),
        (
            Type2::new_primitive(Primitive::Float32, &engine),
            Type2::new_primitive(Primitive::Uint64, &engine),
        ),
    ]);
}

// input: for<'a> fn() -> bool vs fn() -> bool, ignoring binders
// premise: {}
// output: destructures return types
#[tokio::test]
async fn destructure_function_pointers_can_ignore_binders() {
    let engine = create_test_engine().await;
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let lhs =
        application(&Type2::new_function_pointer_with_higher_ranked_lifetimes(
            1,
            [],
            bool_type.clone(),
            &engine,
        ));
    let rhs = application(&Type2::new_function_pointer(
        [],
        bool_type.clone(),
        &engine,
    ));

    let destructured = lhs
        .destructure(&rhs, DestructureOptions::ignore_binders(), &engine)
        .unwrap()
        .collect::<Vec<_>>();

    assert_eq!(destructured, vec![(bool_type.clone(), bool_type)]);
}

// input: function pointers with equal and unequal binders
// premise: {}
// output: equal binders destructure; unequal binders fail
#[tokio::test]
async fn destructure_function_pointers_can_require_equal_binders() {
    let engine = create_test_engine().await;
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let lhs =
        application(&Type2::new_function_pointer_with_higher_ranked_lifetimes(
            1,
            [],
            bool_type.clone(),
            &engine,
        ));
    let same =
        application(&Type2::new_function_pointer_with_higher_ranked_lifetimes(
            1,
            [],
            bool_type.clone(),
            &engine,
        ));
    let different_kind = application(&Type2::new_function_pointer_with_binder(
        crate::r#type::bound::Binder::new(
            engine.intern_unsized(vec![TyKind::Type]),
        ),
        [],
        bool_type,
        &engine,
    ));

    assert!(
        lhs.destructure(
            &same,
            DestructureOptions::require_equal_binders(),
            &engine
        )
        .is_some()
    );
    assert!(
        lhs.destructure(
            &different_kind,
            DestructureOptions::require_equal_binders(),
            &engine
        )
        .is_none()
    );
}

#[tokio::test]
async fn destructure_different_non_tuple_constructors_fails() {
    // lhs: int32<(int32)>, rhs: bool<(int32)>
    // result: None
    let engine = create_test_engine().await;
    let lhs = primitive_application(
        Primitive::Int32,
        &[Type2::new_primitive(Primitive::Int32, &engine)],
        &engine,
    );
    let rhs = primitive_application(
        Primitive::Bool,
        &[Type2::new_primitive(Primitive::Int32, &engine)],
        &engine,
    );

    assert!(
        lhs.destructure(&rhs, DestructureOptions::ignore_binders(), &engine)
            .is_none()
    );
}

#[tokio::test]
async fn destructure_same_non_tuple_constructor_with_different_arity_fails() {
    // lhs: int32<(bool, float32)>, rhs: int32<(usize)>
    // result: None
    let engine = create_test_engine().await;
    let lhs = primitive_application(
        Primitive::Int32,
        &[
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Float32, &engine),
        ],
        &engine,
    );
    let rhs = primitive_application(
        Primitive::Int32,
        &[Type2::new_primitive(Primitive::Usize, &engine)],
        &engine,
    );

    assert!(
        lhs.destructure(&rhs, DestructureOptions::ignore_binders(), &engine)
            .is_none()
    );
}

#[tokio::test]
async fn destructure_plain_tuples() {
    // lhs: (int32, bool), rhs: (float32, usize)
    // result: (int32, float32), (bool, usize)
    let engine = create_test_engine().await;
    let lhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Bool, &engine),
        ],
        &[],
        &engine,
    );
    let rhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Float32, &engine),
            Type2::new_primitive(Primitive::Usize, &engine),
        ],
        &[],
        &engine,
    );

    let destructured = lhs
        .destructure(&rhs, DestructureOptions::ignore_binders(), &engine)
        .unwrap()
        .collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Float32, &engine),
        ),
        (
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Usize, &engine),
        ),
    ]);
}

#[tokio::test]
async fn destructure_tuple_with_unpacked_right_hand_side() {
    // lhs: (int32, bool, float32, usize), rhs: (uint32, ...int64, uint64)
    // result: (int32, uint32), ((bool, float32), int64), (usize, uint64)
    let engine = create_test_engine().await;
    let lhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Float32, &engine),
            Type2::new_primitive(Primitive::Usize, &engine),
        ],
        &[],
        &engine,
    );
    let rhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Uint32, &engine),
            Type2::new_primitive(Primitive::Int64, &engine),
            Type2::new_primitive(Primitive::Uint64, &engine),
        ],
        &[1],
        &engine,
    );

    let destructured = lhs
        .destructure(&rhs, DestructureOptions::ignore_binders(), &engine)
        .unwrap()
        .collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Uint32, &engine),
        ),
        (
            tuple_type(
                &[
                    Type2::new_primitive(Primitive::Bool, &engine),
                    Type2::new_primitive(Primitive::Float32, &engine),
                ],
                &[],
                &engine,
            ),
            Type2::new_primitive(Primitive::Int64, &engine),
        ),
        (
            Type2::new_primitive(Primitive::Usize, &engine),
            Type2::new_primitive(Primitive::Uint64, &engine),
        ),
    ]);
}

#[tokio::test]
async fn destructure_tuple_with_unpacked_left_hand_side() {
    // lhs: (uint32, ...int64, uint64), rhs: (int32, bool, float32, usize)
    // result: (uint32, int32), (int64, (bool, float32)), (uint64, usize)
    let engine = create_test_engine().await;
    let lhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Uint32, &engine),
            Type2::new_primitive(Primitive::Int64, &engine),
            Type2::new_primitive(Primitive::Uint64, &engine),
        ],
        &[1],
        &engine,
    );
    let rhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Float32, &engine),
            Type2::new_primitive(Primitive::Usize, &engine),
        ],
        &[],
        &engine,
    );

    let destructured = lhs
        .destructure(&rhs, DestructureOptions::ignore_binders(), &engine)
        .unwrap()
        .collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            Type2::new_primitive(Primitive::Uint32, &engine),
            Type2::new_primitive(Primitive::Int32, &engine),
        ),
        (
            Type2::new_primitive(Primitive::Int64, &engine),
            tuple_type(
                &[
                    Type2::new_primitive(Primitive::Bool, &engine),
                    Type2::new_primitive(Primitive::Float32, &engine),
                ],
                &[],
                &engine,
            ),
        ),
        (
            Type2::new_primitive(Primitive::Uint64, &engine),
            Type2::new_primitive(Primitive::Usize, &engine),
        ),
    ]);
}

#[tokio::test]
async fn destructure_tuple_with_unpacked_left_hand_side_to_empty_tuple() {
    // lhs: (...uint32, int64, uint64), rhs: (int32, bool)
    // result: (uint32, ()), (int64, int32), (uint64, bool)
    let engine = create_test_engine().await;
    let lhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Uint32, &engine),
            Type2::new_primitive(Primitive::Int64, &engine),
            Type2::new_primitive(Primitive::Uint64, &engine),
        ],
        &[0],
        &engine,
    );
    let rhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Bool, &engine),
        ],
        &[],
        &engine,
    );

    let destructured = lhs
        .destructure(&rhs, DestructureOptions::ignore_binders(), &engine)
        .unwrap()
        .collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            Type2::new_primitive(Primitive::Uint32, &engine),
            tuple_type(&[], &[], &engine),
        ),
        (
            Type2::new_primitive(Primitive::Int64, &engine),
            Type2::new_primitive(Primitive::Int32, &engine),
        ),
        (
            Type2::new_primitive(Primitive::Uint64, &engine),
            Type2::new_primitive(Primitive::Bool, &engine),
        ),
    ]);
}

#[tokio::test]
async fn destructure_same_tuple_shape_pairs_element_wise() {
    // lhs: (int32, ...bool, float32), rhs: (uint32, ...int64, uint64)
    // result: (int32, uint32), (bool, int64), (float32, uint64)
    let engine = create_test_engine().await;
    let lhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Float32, &engine),
        ],
        &[1],
        &engine,
    );
    let rhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Uint32, &engine),
            Type2::new_primitive(Primitive::Int64, &engine),
            Type2::new_primitive(Primitive::Uint64, &engine),
        ],
        &[1],
        &engine,
    );

    let destructured = lhs
        .destructure(&rhs, DestructureOptions::ignore_binders(), &engine)
        .unwrap()
        .collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Uint32, &engine),
        ),
        (
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Int64, &engine),
        ),
        (
            Type2::new_primitive(Primitive::Float32, &engine),
            Type2::new_primitive(Primitive::Uint64, &engine),
        ),
    ]);
}

#[tokio::test]
async fn destructure_grouped_tuple_preserves_other_unpacked_position() {
    // lhs: (int32, ...bool, float32), rhs: (uint32, int64, ...uint64,
    // isize) result: (int32, uint32), (bool, (int64, ...uint64)),
    // (float32, isize)
    let engine = create_test_engine().await;
    let lhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Float32, &engine),
        ],
        &[1],
        &engine,
    );
    let rhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Uint32, &engine),
            Type2::new_primitive(Primitive::Int64, &engine),
            Type2::new_primitive(Primitive::Uint64, &engine),
            Type2::new_primitive(Primitive::Isize, &engine),
        ],
        &[2],
        &engine,
    );

    let destructured = lhs
        .destructure(&rhs, DestructureOptions::ignore_binders(), &engine)
        .unwrap()
        .collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Uint32, &engine),
        ),
        (
            Type2::new_primitive(Primitive::Bool, &engine),
            tuple_type(
                &[
                    Type2::new_primitive(Primitive::Int64, &engine),
                    Type2::new_primitive(Primitive::Uint64, &engine),
                ],
                &[1],
                &engine,
            ),
        ),
        (
            Type2::new_primitive(Primitive::Float32, &engine),
            Type2::new_primitive(Primitive::Isize, &engine),
        ),
    ]);
}

#[tokio::test]
async fn destructure_tuple_mismatch_fails_when_other_unpacked_is_outside_range()
{
    // lhs: (int32, ...bool, float32), rhs: (...uint32, int64, uint64,
    // isize) result: None
    let engine = create_test_engine().await;
    let lhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Float32, &engine),
        ],
        &[1],
        &engine,
    );
    let rhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Uint32, &engine),
            Type2::new_primitive(Primitive::Int64, &engine),
            Type2::new_primitive(Primitive::Uint64, &engine),
            Type2::new_primitive(Primitive::Isize, &engine),
        ],
        &[0],
        &engine,
    );

    assert!(
        lhs.destructure(&rhs, DestructureOptions::ignore_binders(), &engine)
            .is_none()
    );
}

#[tokio::test]
async fn destructure_tuple_with_invalid_multiple_unpacked_positions_fails() {
    // lhs: (...int32, bool, ...float32), rhs: (uint32, int64, uint64)
    // result: None
    let engine = create_test_engine().await;
    let lhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Float32, &engine),
        ],
        &[0, 2],
        &engine,
    );
    let rhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Uint32, &engine),
            Type2::new_primitive(Primitive::Int64, &engine),
            Type2::new_primitive(Primitive::Uint64, &engine),
        ],
        &[],
        &engine,
    );

    assert!(
        lhs.destructure(&rhs, DestructureOptions::ignore_binders(), &engine)
            .is_none()
    );
}

#[tokio::test]
async fn destructure_tuple_with_ambiguous_unpacked_match_fails() {
    // lhs: (...int32, bool, float32, usize), rhs: (uint32, isize,
    // ...uint64) result: None, because there are multiple valid
    // destructuring solutions.
    let engine = create_test_engine().await;
    let lhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Float32, &engine),
            Type2::new_primitive(Primitive::Usize, &engine),
        ],
        &[0],
        &engine,
    );
    let rhs = tuple_application(
        &[
            Type2::new_primitive(Primitive::Uint32, &engine),
            Type2::new_primitive(Primitive::Isize, &engine),
            Type2::new_primitive(Primitive::Uint64, &engine),
        ],
        &[2],
        &engine,
    );

    assert!(
        lhs.destructure(&rhs, DestructureOptions::ignore_binders(), &engine)
            .is_none()
    );
}
