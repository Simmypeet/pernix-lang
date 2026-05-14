use enum_as_inner::EnumAsInner;
use pernixc_qbice::Interner;
use pernixc_symbol::GlobalSymbolID;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::r#type::{Type, context::TyContext, kind::TyKind};

/// Simple primitive types
///
/// Kind: Type
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    StableHash,
    Encode,
    Decode,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum Primitive {
    #[display("int8")]
    Int8,
    #[display("int16")]
    Int16,
    #[display("int32")]
    Int32,
    #[display("int64")]
    Int64,
    #[display("uint8")]
    Uint8,
    #[display("uint16")]
    Uint16,
    #[display("uint32")]
    Uint32,
    #[display("uint64")]
    Uint64,
    #[display("float32")]
    Float32,
    #[display("float64")]
    Float64,
    #[display("bool")]
    Bool,
    #[display("usize")]
    Usize,
    #[display("isize")]
    Isize,
}

/// Represents a simple lifetime.
///
/// Kind: Lifetime
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub enum Lifetime {
    Static,
    Erased,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub enum Mutability {
    Mutable,
    Immutable,
}

/// Represents a reference type constructor, such as `&T` or `&mut T`.
///
/// Kind: (Lifetime, Type) -> Type
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Reference {
    mutability: Mutability,
}

/// Represents an algebraic data type (ADT) constructor, such as `Option` or
/// `Result`.
///
/// Kind: ( <Adt's Generic Parameter Kinds> ) -> Type
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Adt {
    adt_symbol_id: GlobalSymbolID,
}

/// Represents an unpacked element in the tuple, such as (int32, ...T, float64).
/// Where `...` represents the unpacking constructor, and `T` is the
/// application of the unpacking constructor
///
/// Kind: Type -> Unpacked
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Unpacked(());

/// Represents a tuple type constructor, such as `(T1, T2, T3)`. Which can
/// include `Unpacked` elements.
///
/// Kinds: ( Type* ) -> Type
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Tuple {
    unpacked_positions: Interned<[usize]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TupleShape {
    Regular,
    Unpacked(usize),
}

impl Tuple {
    fn shape(&self) -> Option<TupleShape> {
        match self.unpacked_positions.len() {
            0 => Some(TupleShape::Regular),
            1 => Some(TupleShape::Unpacked(self.unpacked_positions[0])),
            _ => None,
        }
    }
}

/// Represents an associated member of an instance, such as an associated type
/// or an associated instance.
///
/// Kind: ( Instance, <Associated's Generic Parameter Kinds> ) -> (Type |
/// Instance)
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct InstanceAssociated {
    instance_associated_symbol_id: GlobalSymbolID,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    StableHash,
    Encode,
    Decode,
)]
pub enum Constructor {
    Primitive(Primitive),
    Lifetime(Lifetime),
    Reference(Reference),
    Adt(Adt),
    Tuple(Tuple),
    InstanceAssociated(InstanceAssociated),
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Application {
    constructor: Constructor,
    arguments: Interned<[Interned<Type>]>,
}

type RegularDestructure<'a> = std::iter::Zip<
    std::iter::Cloned<std::slice::Iter<'a, Interned<Type>>>,
    std::iter::Cloned<std::slice::Iter<'a, Interned<Type>>>,
>;

#[derive(Debug)]
enum Destructure<'a, I: Interner + ?Sized> {
    Regular(RegularDestructure<'a>),
    OneSidedTuple(OneSidedTupleDestructure<'a, I>),
}

impl<I: Interner + ?Sized> Iterator for Destructure<'_, I> {
    type Item = (Interned<Type>, Interned<Type>);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Regular(iterator) => iterator.next(),
            Self::OneSidedTuple(iterator) => iterator.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Regular(iterator) => iterator.size_hint(),
            Self::OneSidedTuple(iterator) => iterator.size_hint(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OneSidedTupleState {
    Head,
    Middle,
    Tail,
    Done,
}

#[derive(Debug)]
struct OneSidedTupleDestructure<'a, I: Interner + ?Sized> {
    from_arguments: &'a [Interned<Type>],
    to_arguments: &'a [Interned<Type>],
    unpacked_position: usize,
    to_unpack_range: std::ops::Range<usize>,
    to_unpacked_position: Option<usize>,
    interner: &'a I,
    swap: bool,
    next_head_index: usize,
    next_tail_offset: usize,
    state: OneSidedTupleState,
}

impl<I: Interner + ?Sized> OneSidedTupleDestructure<'_, I> {
    const fn tail_len(&self) -> usize {
        self.from_arguments.len() - self.unpacked_position - 1
    }

    const fn remaining_len(&self) -> usize {
        match self.state {
            OneSidedTupleState::Head => {
                self.unpacked_position - self.next_head_index
                    + 1
                    + self.tail_len()
                    - self.next_tail_offset
            }
            OneSidedTupleState::Middle => {
                1 + self.tail_len() - self.next_tail_offset
            }
            OneSidedTupleState::Tail => self.tail_len() - self.next_tail_offset,
            OneSidedTupleState::Done => 0,
        }
    }
}

impl<I: Interner + ?Sized> Iterator for OneSidedTupleDestructure<'_, I> {
    type Item = (Interned<Type>, Interned<Type>);

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            OneSidedTupleState::Head => {
                if self.next_head_index < self.unpacked_position {
                    let index = self.next_head_index;
                    self.next_head_index += 1;

                    return Some(Application::destructured_pair(
                        self.from_arguments[index].clone(),
                        self.to_arguments[index].clone(),
                        self.swap,
                    ));
                }

                self.state = OneSidedTupleState::Middle;
                self.next()
            }
            OneSidedTupleState::Middle => {
                self.state = OneSidedTupleState::Tail;

                Some(Application::destructured_pair(
                    self.from_arguments[self.unpacked_position].clone(),
                    Application::intern_tuple_range(
                        self.to_arguments,
                        self.to_unpack_range.clone(),
                        self.to_unpacked_position,
                        self.interner,
                    ),
                    self.swap,
                ))
            }
            OneSidedTupleState::Tail => {
                if self.next_tail_offset < self.tail_len() {
                    let from_index =
                        self.unpacked_position + 1 + self.next_tail_offset;
                    let to_index = self.to_arguments.len() - self.tail_len()
                        + self.next_tail_offset;
                    self.next_tail_offset += 1;

                    return Some(Application::destructured_pair(
                        self.from_arguments[from_index].clone(),
                        self.to_arguments[to_index].clone(),
                        self.swap,
                    ));
                }

                self.state = OneSidedTupleState::Done;
                None
            }
            OneSidedTupleState::Done => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.remaining_len();
        (remaining, Some(remaining))
    }
}

impl Application {
    pub async fn kind(&self, ctx: &impl TyContext) -> TyKind {
        match self.constructor {
            Constructor::Tuple(_)
            | Constructor::Adt(_)
            | Constructor::Primitive(_)
            | Constructor::Reference(_) => TyKind::Type,

            Constructor::Lifetime(_) => TyKind::Lifetime,

            Constructor::InstanceAssociated(inst) => {
                ctx.get_instance_associated_type_kind(
                    inst.instance_associated_symbol_id,
                )
                .await
            }
        }
    }

    pub fn destructure<'a, I: Interner + ?Sized>(
        &'a self,
        other: &'a Self,
        interner: &'a I,
    ) -> Option<impl Iterator<Item = (Interned<Type>, Interned<Type>)> + 'a>
    {
        if let (Constructor::Tuple(lhs), Constructor::Tuple(rhs)) =
            (&self.constructor, &other.constructor)
        {
            Self::destructure_tuples(
                lhs,
                &self.arguments,
                rhs,
                &other.arguments,
                interner,
            )
        } else if self.constructor == other.constructor {
            Self::destructure_regular(&self.arguments, &other.arguments)
                .map(Destructure::Regular)
        } else {
            None
        }
    }

    fn destructure_regular<'a>(
        lhs: &'a [Interned<Type>],
        rhs: &'a [Interned<Type>],
    ) -> Option<RegularDestructure<'a>> {
        (lhs.len() == rhs.len())
            .then(|| lhs.iter().cloned().zip(rhs.iter().cloned()))
    }

    fn destructure_tuples<'a, I: Interner + ?Sized>(
        lhs_tuple: &Tuple,
        lhs_arguments: &'a [Interned<Type>],
        rhs_tuple: &Tuple,
        rhs_arguments: &'a [Interned<Type>],
        interner: &'a I,
    ) -> Option<Destructure<'a, I>> {
        let lhs_shape = lhs_tuple.shape()?;
        let rhs_shape = rhs_tuple.shape()?;

        if lhs_arguments.len() == rhs_arguments.len() && lhs_shape == rhs_shape
        {
            return Some(Destructure::Regular(
                lhs_arguments
                    .iter()
                    .cloned()
                    .zip(rhs_arguments.iter().cloned()),
            ));
        }

        match (lhs_shape, rhs_shape) {
            (TupleShape::Regular, TupleShape::Regular) => {
                Self::destructure_regular(lhs_arguments, rhs_arguments)
                    .map(Destructure::Regular)
            }
            (TupleShape::Unpacked(position), rhs_shape) => {
                Self::destructure_one_sided_tuple(
                    lhs_arguments,
                    position,
                    rhs_arguments,
                    rhs_shape,
                    interner,
                    false,
                )
                .map(Destructure::OneSidedTuple)
            }
            (lhs_shape, TupleShape::Unpacked(position)) => {
                Self::destructure_one_sided_tuple(
                    rhs_arguments,
                    position,
                    lhs_arguments,
                    lhs_shape,
                    interner,
                    true,
                )
                .map(Destructure::OneSidedTuple)
            }
        }
    }

    fn destructure_one_sided_tuple<'a, I: Interner + ?Sized>(
        from_arguments: &'a [Interned<Type>],
        unpacked_position: usize,
        to_arguments: &'a [Interned<Type>],
        to_shape: TupleShape,
        interner: &'a I,
        swap: bool,
    ) -> Option<OneSidedTupleDestructure<'a, I>> {
        if from_arguments.len() > to_arguments.len() + 1 {
            return None;
        }

        let head_range = 0..unpacked_position;
        let from_tail_range = unpacked_position + 1..from_arguments.len();
        let to_tail_range =
            (to_arguments.len() - from_tail_range.len())..to_arguments.len();
        let to_unpack_range = unpacked_position..to_tail_range.start;

        let to_unpacked_position = match to_shape {
            TupleShape::Regular => None,
            TupleShape::Unpacked(position) => Some(position),
        };

        if to_unpacked_position.is_some_and(|position| {
            head_range.contains(&position) || to_tail_range.contains(&position)
        }) {
            return None;
        }

        Some(OneSidedTupleDestructure {
            from_arguments,
            to_arguments,
            unpacked_position,
            to_unpack_range,
            to_unpacked_position,
            interner,
            swap,
            next_head_index: 0,
            next_tail_offset: 0,
            state: OneSidedTupleState::Head,
        })
    }

    fn intern_tuple_range(
        arguments: &[Interned<Type>],
        range: std::ops::Range<usize>,
        unpacked_position: Option<usize>,
        interner: &(impl Interner + ?Sized),
    ) -> Interned<Type> {
        let unpacked_positions = match unpacked_position {
            Some(position) if range.contains(&position) => {
                interner.intern_unsized(vec![position - range.start])
            }
            Some(_) | None => interner.intern_unsized(Vec::<usize>::new()),
        };

        interner.intern(Type::Application(Self {
            constructor: Constructor::Tuple(Tuple { unpacked_positions }),
            arguments: interner.intern_unsized(arguments[range].to_vec()),
        }))
    }

    const fn destructured_pair(
        lhs: Interned<Type>,
        rhs: Interned<Type>,
        swap: bool,
    ) -> (Interned<Type>, Interned<Type>) {
        if swap { (rhs, lhs) } else { (lhs, rhs) }
    }
}

#[cfg(test)]
#[allow(clippy::trivially_copy_pass_by_ref)]
mod test {
    use pernixc_qbice::DuplicatingInterner;

    use super::*;

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
}
