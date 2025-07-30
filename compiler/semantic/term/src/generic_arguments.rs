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
    pub member_generic_arguments: GenericArguments,
}

/// Represents a sub-term location where the sub-term is stored as a generic
/// arguments.
///
/// The `usize` represents the index of the sub-term in the generic arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SubSymbolLocation(pub usize);

/// Represents a sub-term location where the sub-term is stored as a generic
/// arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SubMemberSymbolLocation {
    /// The index of the sub-term in the generic arguments.
    pub index: usize,

    /// True if the sub-term is in the parent's generic arguments part,
    /// otherwise false.
    pub from_parent: bool,
}

/// A new type wrapper for [`SubMemberSymbolLocation`] to represent a sub-term
/// in trait member symbols.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SubTraitMemberLocation(pub SubMemberSymbolLocation);

impl MemberSymbol {
    /// Returns a mutable reference to a particular sub-term of this generic
    /// arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term_mut<T: Element>(
        &mut self,
        location: SubMemberSymbolLocation,
    ) -> Option<&mut T> {
        let generic_arguments = if location.from_parent {
            T::get_mut(&mut self.parent_generic_arguments)
        } else {
            T::get_mut(&mut self.member_generic_arguments)
        };

        generic_arguments.get_mut(location.index)
    }

    /// Returns a reference to a particular sub-term of this generic arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term<T: Element>(
        &self,
        location: SubMemberSymbolLocation,
    ) -> Option<&T> {
        let generic_arguments = if location.from_parent {
            T::get(&self.parent_generic_arguments)
        } else {
            T::get(&self.member_generic_arguments)
        };

        generic_arguments.get(location.index)
    }
}

impl Symbol {
    /// Returns a mutable reference to a particular sub-term of this generic
    /// arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term_mut<T: Element>(
        &mut self,
        location: SubSymbolLocation,
    ) -> Option<&mut T> {
        let generic_arguments = T::get_mut(&mut self.generic_arguments);

        generic_arguments.get_mut(location.0)
    }

    /// Returns a reference to a particular sub-term of this generic arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term<T: Element>(
        &self,
        location: SubSymbolLocation,
    ) -> Option<&T> {
        let generic_arguments = T::get(&self.generic_arguments);

        generic_arguments.get(location.0)
    }
}
