use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::{
    GlobalSymbolID,
    kind::{Kind, get_kind},
};
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::r#type::{Type, bound::Binder, kind::TyKind};

mod destructure;
mod reduction;
pub mod rewrite;

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

impl Reference {
    #[must_use]
    pub const fn new(mutability: Mutability) -> Self { Self { mutability } }

    #[must_use]
    pub const fn mutability(&self) -> Mutability { self.mutability }
}

/// Represents a symbolic type constructor, supplying generic arguments to a
/// symbol, such as `Option<T>` or `SomeInstance<X, Y, Z>`.
///
/// Kind: ( <Symbol's Generic Parameter Kinds> ) -> (Type | Instance)
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
pub struct Symbolic {
    symbolic_id: GlobalSymbolID,
}

impl Symbolic {
    #[must_use]
    pub const fn new(symbolic_id: GlobalSymbolID) -> Self {
        Self { symbolic_id }
    }

    #[must_use]
    pub const fn symbol_id(&self) -> GlobalSymbolID { self.symbolic_id }
}

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
    #[must_use]
    pub const fn new(unpacked_positions: Interned<[usize]>) -> Self {
        Self { unpacked_positions }
    }

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
/// The first argument is always the instance, and the remaining arguments are
/// the generic arguments supplied to the associated member.
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
    trait_associated_id: GlobalSymbolID,
}

impl InstanceAssociated {
    #[must_use]
    pub const fn new(trait_associated_id: GlobalSymbolID) -> Self {
        Self { trait_associated_id }
    }

    #[must_use]
    pub const fn trait_associated_id(&self) -> GlobalSymbolID {
        self.trait_associated_id
    }
}

/// Refers to an instance that is coupled with a trait when user writes
/// `this.Associated` syntax.
///
/// For example, consider the following program:
///
/// ```pnx
/// public trait MyTrait:
///   public type Assoc
///
///   public function a(a: this.Assoc)
/// ```
///
/// The type `this.Assoc` declared under `MyTrait` would be represented as an
/// instance associated type where the instance is `AnonymousTraitInstance
/// {MyTrait}`.
///
/// The `AnonymousTraitInstance` can only appear under the trait declaration.
///
/// Kind: Instance
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
pub struct AnonymousTraitInstance {
    trait_id: GlobalSymbolID,
}

impl AnonymousTraitInstance {
    #[must_use]
    pub const fn new(trait_id: GlobalSymbolID) -> Self { Self { trait_id } }

    #[must_use]
    pub const fn trait_id(&self) -> GlobalSymbolID { self.trait_id }
}

/// Represents a function pointer, such as `fn(T1, T2) -> T3`.
///
/// The last type argument is assumed to be return type, and the preceding type
/// arguments are assumed to be parameter types. The binder contains the late
/// bound lifetimes of the function pointer, if any.
///
/// Kind: (Type*) -> Type
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
pub struct FunctionPointer {
    binder: Binder,
}

impl FunctionPointer {
    #[must_use]
    pub const fn new(binder: Binder) -> Self { Self { binder } }
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
    Symbolic(Symbolic),
    Tuple(Tuple),
    FunctionPointer(FunctionPointer),
    AnonymousTraitInstance(AnonymousTraitInstance),
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
    #[must_use]
    pub const fn new(
        constructor: Constructor,
        arguments: Interned<[Interned<Type>]>,
    ) -> Self {
        Self { constructor, arguments }
    }

    #[must_use]
    pub const fn arguments(&self) -> &Interned<[Interned<Type>]> {
        &self.arguments
    }

    #[must_use]
    pub const fn binder(&self) -> Option<&Binder> {
        match &self.constructor {
            Constructor::FunctionPointer(fp) => Some(&fp.binder),
            _ => None,
        }
    }

    #[must_use]
    pub const fn constructor(&self) -> &Constructor { &self.constructor }

    pub async fn kind(&self, engine: &TrackedEngine) -> TyKind {
        match self.constructor {
            Constructor::Tuple(_)
            | Constructor::Primitive(_)
            | Constructor::FunctionPointer(_)
            | Constructor::Reference(_) => TyKind::Type,

            Constructor::Symbolic(symbol) => {
                let kind = engine.get_kind(symbol.symbolic_id).await;

                match kind {
                    Kind::Struct | Kind::Enum => TyKind::Type,
                    Kind::Instance => TyKind::Instance,

                    _ => panic!(
                        "Expected an ADT, primitive, or trait, but got a \
                         different kind"
                    ),
                }
            }

            Constructor::Lifetime(_) => TyKind::Lifetime,

            Constructor::AnonymousTraitInstance(_) => TyKind::Instance,

            Constructor::InstanceAssociated(inst) => {
                let kind = engine.get_kind(inst.trait_associated_id).await;

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
