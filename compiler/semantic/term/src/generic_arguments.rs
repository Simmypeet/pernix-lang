//! Contains the definition of [`GenericArguments`] and related types.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::{constant::Constant, lifetime::Lifetime, r#type::Type};

/// Represents a list of generic arguments supplied to a particular symbol that
/// has generic parameters (e.g., `symbol[ARGS]`).
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
    StableHash,
)]
pub struct GenericArguments {
    /// The lifetimes supplied to the term.
    pub lifetimes: Vec<Lifetime>,

    /// The types supplied to the term.
    pub types: Vec<Type>,

    /// The constants supplied to the term.
    pub constants: Vec<Constant>,
}

/// A trait for retrieving the arguments array from a generic arguments.
#[allow(missing_docs)]
pub trait Element {
    fn get(generic_arguments: &GenericArguments) -> &[Self]
    where
        Self: Sized;

    fn get_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self>
    where
        Self: Sized;
}

impl Element for Lifetime {
    fn get(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.lifetimes
    }

    fn get_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self> {
        &mut generic_arguments.lifetimes
    }
}

impl Element for Type {
    fn get(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.types
    }

    fn get_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self> {
        &mut generic_arguments.types
    }
}

impl Element for Constant {
    fn get(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.constants
    }

    fn get_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self> {
        &mut generic_arguments.constants
    }
}

/// Represents a term where the a symbol is supplied with generic arguments
/// (e.g., `symbol[ARGS]`).
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Symbol {
    /// The ID of the symbol that is supplied with generic arguments.
    pub symbol_id: Global<pernixc_symbol::ID>,

    /// The generic arguments supplied to the symbol.
    pub generic_arguments: GenericArguments,
}

/// Represents a term where the associated symbol is supplied with generic
/// arguments as well as their parent (e.g., `symbol[ARGS]::associated[ARGS]`).
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct MemberSymbol {
    /// The ID of the associated symbol.
    ///
    /// By associated symbol, we mean a symbol that is defined in the context
    /// of another symbol, such as a method or an associated type.
    pub associated_id: Global<pernixc_symbol::ID>,

    /// The generic arguments supplied to the parent of the associated symbol.
    pub parent_generic_arguments: GenericArguments,

    /// The generic arguments supplied to the associated symbol.
    pub associated_generic_arguments: GenericArguments,
}
