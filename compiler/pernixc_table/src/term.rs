use std::{fmt::Debug, hash::Hash};

use constant::Constant;
use lifetime::Lifetime;
use r#type::Type;
use serde::{Deserialize, Serialize};

use crate::GlobalID;

pub mod constant;
pub mod generic_parameters;
pub mod lifetime;
pub mod r#type;

/// A type that can't never be instantiated.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum Never {}

/// The model that the terms will be based on.
///
/// The model is used for defining the inferences that can be made in the terms.
pub trait Model:
    Debug
    + Clone
    + Copy
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Hash
    + std::default::Default
    + 'static
    + Send
    + Sync
{
    /// The type to use for lifetime inference.
    type LifetimeInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + From<Never>
        + Serialize
        + for<'x> Deserialize<'x>;

    /// The type to use for type inference.
    type TypeInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + From<Never>
        + Serialize
        + for<'x> Deserialize<'x>;

    /// The type to use for constant inference.
    type ConstantInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + From<Never>
        + Serialize
        + for<'x> Deserialize<'x>;

    /// Converts a type from the default model to the current model.
    fn from_default_type(ty: Type<Default>) -> Type<Self>;

    /// Converts a lifetime from the default model to the current model.
    fn from_default_lifetime(lifetime: Lifetime<Default>) -> Lifetime<Self>;

    /// Converts a constant from the default model to the current model.
    fn from_default_constant(constant: Constant<Default>) -> Constant<Self>;
}

/// The default model where all inferences are [`Never`].
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct Default;

impl Model for Default {
    type LifetimeInference = Never;
    type TypeInference = Never;
    type ConstantInference = Never;

    fn from_default_type(ty: Type<Default>) -> Type<Self> { ty }
    fn from_default_lifetime(lifetime: Lifetime<Default>) -> Lifetime<Self> {
        lifetime
    }
    fn from_default_constant(constant: Constant<Default>) -> Constant<Self> {
        constant
    }
}

/// Represents an errornuos term. Used for representing errors in the type
/// system.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub struct Error;

/// Represents a term where its value is stored as a symbol (i.e., `type` or
/// `const` declaration).
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Symbol<M: Model> {
    /// The ID of the symbol that contains the value of the term.
    pub id: GlobalID,

    /// The generic arguments supplied to the symbol.
    pub generic_arguments: GenericArguments<M>,
}

/// Represents a term where its value is stored as a member of a particular
/// symbol
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct MemberSymbol<M: Model> {
    /// The ID of the symbol that contains the value of the term.
    pub id: GlobalID,

    /// The generic arguments supplied to the member.
    pub member_generic_arguments: GenericArguments<M>,

    /// The generic arguments supplied to the parent scope.
    pub parent_generic_arguments: GenericArguments<M>,
}

/// Represents a generic arguments supplied to a term (i.e., `type[ARGS]`).
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct GenericArguments<M: Model> {
    /// The lifetimes supplied to the term.
    pub lifetimes: Vec<Lifetime<M>>,

    /// The types supplied to the term.
    pub types: Vec<Type<M>>,

    /// The constants supplied to the term.
    pub constants: Vec<Constant<M>>,
}

/// Represents a single element of a tuple.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct TupleElement<Term> {
    /// The term stored in this element.
    pub term: Term,

    /// Whether the term is unpacked.
    pub is_unpacked: bool,
}

/// Represents a tuple of terms.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct Tuple<Term> {
    /// The elements of the tuple.
    pub elements: Vec<TupleElement<Term>>,
}
