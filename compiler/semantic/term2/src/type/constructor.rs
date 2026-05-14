use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::{
    GlobalSymbolID,
    kind::{Kind, get_kind},
};
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::r#type::{Type, kind::TyKind};

mod destructure;
mod instantiate;

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
    pub async fn kind(&self, engine: &TrackedEngine) -> TyKind {
        match self.constructor {
            Constructor::Tuple(_)
            | Constructor::Adt(_)
            | Constructor::Primitive(_)
            | Constructor::Reference(_) => TyKind::Type,

            Constructor::Lifetime(_) => TyKind::Lifetime,

            Constructor::InstanceAssociated(inst) => {
                let kind =
                    engine.get_kind(inst.instance_associated_symbol_id).await;

                match kind {
                    Kind::InstanceAssociatedType => TyKind::Type,
                    Kind::InstanceAssociatedInstance => TyKind::Instance,

                    _ => panic!(
                        "Expected an instance associated type or instance, \
                         but got a different kind"
                    ),
                }
            }
        }
    }
}
