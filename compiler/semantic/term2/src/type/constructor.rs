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

    pub fn destructure(
        &self,
        other: &Self,
        interner: &impl Interner,
    ) -> Option<impl Iterator<Item = (Interned<Type>, Interned<Type>)>> {
        let pairs = if let (Constructor::Tuple(lhs), Constructor::Tuple(rhs)) =
            (&self.constructor, &other.constructor)
        {
            Self::destructure_tuples(
                lhs,
                &self.arguments,
                rhs,
                &other.arguments,
                interner,
            )?
        } else {
            if self.constructor != other.constructor {
                return None;
            }

            Self::destructure_regular(&self.arguments, &other.arguments)?
        };

        Some(pairs.into_iter())
    }

    fn destructure_regular(
        lhs: &[Interned<Type>],
        rhs: &[Interned<Type>],
    ) -> Option<Vec<(Interned<Type>, Interned<Type>)>> {
        (lhs.len() == rhs.len())
            .then(|| lhs.iter().cloned().zip(rhs.iter().cloned()).collect())
    }

    fn destructure_tuples(
        lhs_tuple: &Tuple,
        lhs_arguments: &[Interned<Type>],
        rhs_tuple: &Tuple,
        rhs_arguments: &[Interned<Type>],
        interner: &impl Interner,
    ) -> Option<Vec<(Interned<Type>, Interned<Type>)>> {
        let lhs_shape = lhs_tuple.shape()?;
        let rhs_shape = rhs_tuple.shape()?;

        if lhs_arguments.len() == rhs_arguments.len() && lhs_shape == rhs_shape
        {
            return Some(
                lhs_arguments
                    .iter()
                    .cloned()
                    .zip(rhs_arguments.iter().cloned())
                    .collect(),
            );
        }

        match (lhs_shape, rhs_shape) {
            (TupleShape::Regular, TupleShape::Regular) => {
                Self::destructure_regular(lhs_arguments, rhs_arguments)
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
            }
        }
    }

    fn destructure_one_sided_tuple(
        from_arguments: &[Interned<Type>],
        unpacked_position: usize,
        to_arguments: &[Interned<Type>],
        to_shape: TupleShape,
        interner: &impl Interner,
        swap: bool,
    ) -> Option<Vec<(Interned<Type>, Interned<Type>)>> {
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

        let mut result = Vec::with_capacity(from_arguments.len());

        for (from_argument, to_argument) in from_arguments[head_range.clone()]
            .iter()
            .cloned()
            .zip(to_arguments[head_range].iter().cloned())
        {
            Self::push_destructured_pair(
                &mut result,
                from_argument,
                to_argument,
                swap,
            );
        }

        let grouped_to_argument = Self::intern_tuple_range(
            to_arguments,
            to_unpack_range,
            to_unpacked_position,
            interner,
        );
        Self::push_destructured_pair(
            &mut result,
            from_arguments[unpacked_position].clone(),
            grouped_to_argument,
            swap,
        );

        for (from_argument, to_argument) in from_arguments[from_tail_range]
            .iter()
            .cloned()
            .zip(to_arguments[to_tail_range].iter().cloned())
        {
            Self::push_destructured_pair(
                &mut result,
                from_argument,
                to_argument,
                swap,
            );
        }

        Some(result)
    }

    fn intern_tuple_range(
        arguments: &[Interned<Type>],
        range: std::ops::Range<usize>,
        unpacked_position: Option<usize>,
        interner: &impl Interner,
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

    fn push_destructured_pair(
        result: &mut Vec<(Interned<Type>, Interned<Type>)>,
        lhs: Interned<Type>,
        rhs: Interned<Type>,
        swap: bool,
    ) {
        if swap {
            result.push((rhs, lhs));
        } else {
            result.push((lhs, rhs));
        }
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
    fn destructure_same_tuple_shape_pairs_element_wise() {
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
