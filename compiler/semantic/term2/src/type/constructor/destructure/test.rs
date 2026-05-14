#![allow(clippy::trivially_copy_pass_by_ref)]
use pernixc_qbice::DuplicatingInterner;

use super::*;
use crate::r#type::constructor::Primitive;

fn intern_primitive(
    primitive: Primitive,
    interner: &DuplicatingInterner,
) -> Interned<Type> {
    interner.intern(Type::Application(Application {
        constructor: Constructor::Primitive(primitive),
        arguments: interner.intern_unsized(Vec::<Interned<Type>>::new()),
    }))
}

fn primitive_application(
    primitive: Primitive,
    arguments: &[Interned<Type>],
    interner: &DuplicatingInterner,
) -> Application {
    Application {
        constructor: Constructor::Primitive(primitive),
        arguments: interner.intern_unsized(arguments.to_vec()),
    }
}

fn tuple_type(
    arguments: &[Interned<Type>],
    unpacked_positions: &[usize],
    interner: &DuplicatingInterner,
) -> Interned<Type> {
    interner.intern(Type::Application(Application {
        constructor: Constructor::Tuple(Tuple {
            unpacked_positions: interner
                .intern_unsized(unpacked_positions.to_vec()),
        }),
        arguments: interner.intern_unsized(arguments.to_vec()),
    }))
}

fn tuple_application(
    arguments: &[Interned<Type>],
    unpacked_positions: &[usize],
    interner: &DuplicatingInterner,
) -> Application {
    let Type::Application(application) =
        &*tuple_type(arguments, unpacked_positions, interner)
    else {
        panic!("expected application");
    };

    application.clone()
}

#[test]
fn destructure_same_non_tuple_constructor() {
    // lhs: int32<(bool, float32)>, rhs: int32<(usize, uint64)>
    // result: (bool, usize), (float32, uint64)
    let interner = DuplicatingInterner;
    let lhs = primitive_application(
        Primitive::Int32,
        &[
            intern_primitive(Primitive::Bool, &interner),
            intern_primitive(Primitive::Float32, &interner),
        ],
        &interner,
    );
    let rhs = primitive_application(
        Primitive::Int32,
        &[
            intern_primitive(Primitive::Usize, &interner),
            intern_primitive(Primitive::Uint64, &interner),
        ],
        &interner,
    );

    let destructured =
        lhs.destructure(&rhs, &interner).unwrap().collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            intern_primitive(Primitive::Bool, &interner),
            intern_primitive(Primitive::Usize, &interner),
        ),
        (
            intern_primitive(Primitive::Float32, &interner),
            intern_primitive(Primitive::Uint64, &interner),
        ),
    ]);
}

#[test]
fn destructure_different_non_tuple_constructors_fails() {
    // lhs: int32<(int32)>, rhs: bool<(int32)>
    // result: None
    let interner = DuplicatingInterner;
    let lhs = primitive_application(
        Primitive::Int32,
        &[intern_primitive(Primitive::Int32, &interner)],
        &interner,
    );
    let rhs = primitive_application(
        Primitive::Bool,
        &[intern_primitive(Primitive::Int32, &interner)],
        &interner,
    );

    assert!(lhs.destructure(&rhs, &interner).is_none());
}

#[test]
fn destructure_same_non_tuple_constructor_with_different_arity_fails() {
    // lhs: int32<(bool, float32)>, rhs: int32<(usize)>
    // result: None
    let interner = DuplicatingInterner;
    let lhs = primitive_application(
        Primitive::Int32,
        &[
            intern_primitive(Primitive::Bool, &interner),
            intern_primitive(Primitive::Float32, &interner),
        ],
        &interner,
    );
    let rhs = primitive_application(
        Primitive::Int32,
        &[intern_primitive(Primitive::Usize, &interner)],
        &interner,
    );

    assert!(lhs.destructure(&rhs, &interner).is_none());
}

#[test]
fn destructure_plain_tuples() {
    // lhs: (int32, bool), rhs: (float32, usize)
    // result: (int32, float32), (bool, usize)
    let interner = DuplicatingInterner;
    let lhs = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &interner),
            intern_primitive(Primitive::Bool, &interner),
        ],
        &[],
        &interner,
    );
    let rhs = tuple_application(
        &[
            intern_primitive(Primitive::Float32, &interner),
            intern_primitive(Primitive::Usize, &interner),
        ],
        &[],
        &interner,
    );

    let destructured =
        lhs.destructure(&rhs, &interner).unwrap().collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            intern_primitive(Primitive::Int32, &interner),
            intern_primitive(Primitive::Float32, &interner),
        ),
        (
            intern_primitive(Primitive::Bool, &interner),
            intern_primitive(Primitive::Usize, &interner),
        ),
    ]);
}

#[test]
fn destructure_tuple_with_unpacked_right_hand_side() {
    // lhs: (int32, bool, float32, usize), rhs: (uint32, ...int64, uint64)
    // result: (int32, uint32), ((bool, float32), int64), (usize, uint64)
    let interner = DuplicatingInterner;
    let lhs = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &interner),
            intern_primitive(Primitive::Bool, &interner),
            intern_primitive(Primitive::Float32, &interner),
            intern_primitive(Primitive::Usize, &interner),
        ],
        &[],
        &interner,
    );
    let rhs = tuple_application(
        &[
            intern_primitive(Primitive::Uint32, &interner),
            intern_primitive(Primitive::Int64, &interner),
            intern_primitive(Primitive::Uint64, &interner),
        ],
        &[1],
        &interner,
    );

    let destructured =
        lhs.destructure(&rhs, &interner).unwrap().collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            intern_primitive(Primitive::Int32, &interner),
            intern_primitive(Primitive::Uint32, &interner),
        ),
        (
            tuple_type(
                &[
                    intern_primitive(Primitive::Bool, &interner),
                    intern_primitive(Primitive::Float32, &interner),
                ],
                &[],
                &interner,
            ),
            intern_primitive(Primitive::Int64, &interner),
        ),
        (
            intern_primitive(Primitive::Usize, &interner),
            intern_primitive(Primitive::Uint64, &interner),
        ),
    ]);
}

#[test]
fn destructure_tuple_with_unpacked_left_hand_side() {
    // lhs: (uint32, ...int64, uint64), rhs: (int32, bool, float32, usize)
    // result: (uint32, int32), (int64, (bool, float32)), (uint64, usize)
    let interner = DuplicatingInterner;
    let lhs = tuple_application(
        &[
            intern_primitive(Primitive::Uint32, &interner),
            intern_primitive(Primitive::Int64, &interner),
            intern_primitive(Primitive::Uint64, &interner),
        ],
        &[1],
        &interner,
    );
    let rhs = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &interner),
            intern_primitive(Primitive::Bool, &interner),
            intern_primitive(Primitive::Float32, &interner),
            intern_primitive(Primitive::Usize, &interner),
        ],
        &[],
        &interner,
    );

    let destructured =
        lhs.destructure(&rhs, &interner).unwrap().collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            intern_primitive(Primitive::Uint32, &interner),
            intern_primitive(Primitive::Int32, &interner),
        ),
        (
            intern_primitive(Primitive::Int64, &interner),
            tuple_type(
                &[
                    intern_primitive(Primitive::Bool, &interner),
                    intern_primitive(Primitive::Float32, &interner),
                ],
                &[],
                &interner,
            ),
        ),
        (
            intern_primitive(Primitive::Uint64, &interner),
            intern_primitive(Primitive::Usize, &interner),
        ),
    ]);
}

#[test]
fn destructure_tuple_with_unpacked_left_hand_side_to_empty_tuple() {
    // lhs: (...uint32, int64, uint64), rhs: (int32, bool)
    // result: (uint32, ()), (int64, int32), (uint64, bool)
    let interner = DuplicatingInterner;
    let lhs = tuple_application(
        &[
            intern_primitive(Primitive::Uint32, &interner),
            intern_primitive(Primitive::Int64, &interner),
            intern_primitive(Primitive::Uint64, &interner),
        ],
        &[0],
        &interner,
    );
    let rhs = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &interner),
            intern_primitive(Primitive::Bool, &interner),
        ],
        &[],
        &interner,
    );

    let destructured =
        lhs.destructure(&rhs, &interner).unwrap().collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            intern_primitive(Primitive::Uint32, &interner),
            tuple_type(&[], &[], &interner),
        ),
        (
            intern_primitive(Primitive::Int64, &interner),
            intern_primitive(Primitive::Int32, &interner),
        ),
        (
            intern_primitive(Primitive::Uint64, &interner),
            intern_primitive(Primitive::Bool, &interner),
        ),
    ]);
}

#[test]
fn destructure_same_tuple_shape_pairs_element_wise() {
    // lhs: (int32, ...bool, float32), rhs: (uint32, ...int64, uint64)
    // result: (int32, uint32), (bool, int64), (float32, uint64)
    let interner = DuplicatingInterner;
    let lhs = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &interner),
            intern_primitive(Primitive::Bool, &interner),
            intern_primitive(Primitive::Float32, &interner),
        ],
        &[1],
        &interner,
    );
    let rhs = tuple_application(
        &[
            intern_primitive(Primitive::Uint32, &interner),
            intern_primitive(Primitive::Int64, &interner),
            intern_primitive(Primitive::Uint64, &interner),
        ],
        &[1],
        &interner,
    );

    let destructured =
        lhs.destructure(&rhs, &interner).unwrap().collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            intern_primitive(Primitive::Int32, &interner),
            intern_primitive(Primitive::Uint32, &interner),
        ),
        (
            intern_primitive(Primitive::Bool, &interner),
            intern_primitive(Primitive::Int64, &interner),
        ),
        (
            intern_primitive(Primitive::Float32, &interner),
            intern_primitive(Primitive::Uint64, &interner),
        ),
    ]);
}

#[test]
fn destructure_grouped_tuple_preserves_other_unpacked_position() {
    // lhs: (int32, ...bool, float32), rhs: (uint32, int64, ...uint64,
    // isize) result: (int32, uint32), (bool, (int64, ...uint64)),
    // (float32, isize)
    let interner = DuplicatingInterner;
    let lhs = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &interner),
            intern_primitive(Primitive::Bool, &interner),
            intern_primitive(Primitive::Float32, &interner),
        ],
        &[1],
        &interner,
    );
    let rhs = tuple_application(
        &[
            intern_primitive(Primitive::Uint32, &interner),
            intern_primitive(Primitive::Int64, &interner),
            intern_primitive(Primitive::Uint64, &interner),
            intern_primitive(Primitive::Isize, &interner),
        ],
        &[2],
        &interner,
    );

    let destructured =
        lhs.destructure(&rhs, &interner).unwrap().collect::<Vec<_>>();

    assert_eq!(destructured, vec![
        (
            intern_primitive(Primitive::Int32, &interner),
            intern_primitive(Primitive::Uint32, &interner),
        ),
        (
            intern_primitive(Primitive::Bool, &interner),
            tuple_type(
                &[
                    intern_primitive(Primitive::Int64, &interner),
                    intern_primitive(Primitive::Uint64, &interner),
                ],
                &[1],
                &interner,
            ),
        ),
        (
            intern_primitive(Primitive::Float32, &interner),
            intern_primitive(Primitive::Isize, &interner),
        ),
    ]);
}

#[test]
fn destructure_tuple_mismatch_fails_when_other_unpacked_is_outside_range() {
    // lhs: (int32, ...bool, float32), rhs: (...uint32, int64, uint64,
    // isize) result: None
    let interner = DuplicatingInterner;
    let lhs = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &interner),
            intern_primitive(Primitive::Bool, &interner),
            intern_primitive(Primitive::Float32, &interner),
        ],
        &[1],
        &interner,
    );
    let rhs = tuple_application(
        &[
            intern_primitive(Primitive::Uint32, &interner),
            intern_primitive(Primitive::Int64, &interner),
            intern_primitive(Primitive::Uint64, &interner),
            intern_primitive(Primitive::Isize, &interner),
        ],
        &[0],
        &interner,
    );

    assert!(lhs.destructure(&rhs, &interner).is_none());
}

#[test]
fn destructure_tuple_with_invalid_multiple_unpacked_positions_fails() {
    // lhs: (...int32, bool, ...float32), rhs: (uint32, int64, uint64)
    // result: None
    let interner = DuplicatingInterner;
    let lhs = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &interner),
            intern_primitive(Primitive::Bool, &interner),
            intern_primitive(Primitive::Float32, &interner),
        ],
        &[0, 2],
        &interner,
    );
    let rhs = tuple_application(
        &[
            intern_primitive(Primitive::Uint32, &interner),
            intern_primitive(Primitive::Int64, &interner),
            intern_primitive(Primitive::Uint64, &interner),
        ],
        &[],
        &interner,
    );

    assert!(lhs.destructure(&rhs, &interner).is_none());
}

#[test]
fn destructure_tuple_with_ambiguous_unpacked_match_fails() {
    // lhs: (...int32, bool, float32, usize), rhs: (uint32, isize,
    // ...uint64) result: None, because there are multiple valid
    // destructuring solutions.
    let interner = DuplicatingInterner;
    let lhs = tuple_application(
        &[
            intern_primitive(Primitive::Int32, &interner),
            intern_primitive(Primitive::Bool, &interner),
            intern_primitive(Primitive::Float32, &interner),
            intern_primitive(Primitive::Usize, &interner),
        ],
        &[0],
        &interner,
    );
    let rhs = tuple_application(
        &[
            intern_primitive(Primitive::Uint32, &interner),
            intern_primitive(Primitive::Isize, &interner),
            intern_primitive(Primitive::Uint64, &interner),
        ],
        &[2],
        &interner,
    );

    assert!(lhs.destructure(&rhs, &interner).is_none());
}
