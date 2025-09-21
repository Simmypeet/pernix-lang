//! Contains the definition of [`GenericArguments`] and related types.

use std::{fmt::Write, ops::Not};

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    name::{get_name, get_qualified_name},
    parent::get_parent,
};
use pernixc_target::Global;

use crate::{
    constant::Constant,
    generic_parameters::GenericKind,
    lifetime::Lifetime,
    matching::{Matching, Substructural},
    r#type::Type,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

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

impl GenericArguments {
    /// Checks if the generic arguments are empty.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.lifetimes.is_empty()
            && self.types.is_empty()
            && self.constants.is_empty()
    }
}

impl crate::display::Display for GenericArguments {
    async fn fmt(
        &self,
        engine: &pernixc_query::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        if !formatter.configuration().generic_arguments_will_be_displayed(self)
        {
            return Ok(());
        }

        let last_is =
            if self.constants.is_empty().not() {
                GenericKind::Constant
            } else if self.types.is_empty().not() {
                GenericKind::Type
            } else if self.lifetimes.iter().any(|x| {
                formatter.configuration().lifetime_will_be_displayed(x)
            }) {
                GenericKind::Lifetime
            } else {
                unreachable!()
            };

        write!(formatter, "[")?;

        let lts = self
            .lifetimes
            .iter()
            .filter(|lt| {
                formatter.configuration().lifetime_will_be_displayed(lt)
            })
            .collect::<Vec<_>>();
        let lts_len = lts.len();

        for (i, lt) in lts.into_iter().enumerate() {
            crate::display::Display::fmt(lt, engine, formatter).await?;

            if i + 1 != lts_len || last_is != GenericKind::Lifetime {
                write!(formatter, ", ")?;
            }
        }

        let tys_len = self.types.len();

        for (i, ty) in self.types.iter().enumerate() {
            crate::display::Display::fmt(ty, engine, formatter).await?;

            if i + 1 != tys_len || last_is != GenericKind::Type {
                write!(formatter, ", ")?;
            }
        }

        let consts_len = self.constants.len();

        for (i, constant) in self.constants.iter().enumerate() {
            crate::display::Display::fmt(constant, engine, formatter).await?;

            if i + 1 != consts_len || last_is != GenericKind::Constant {
                write!(formatter, ", ")?;
            }
        }

        write!(formatter, "]")
    }
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
    pub id: Global<pernixc_symbol::ID>,

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
    pub id: Global<pernixc_symbol::ID>,

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

impl GenericArguments {
    /// Substructurally matches `self` to `to`.
    pub fn substructural_match<L, T, C, Y>(
        &self,
        other: &Self,
        mut existing: Substructural<L, T, C>,
        to_location: impl Fn(usize) -> Y,
    ) -> Option<Substructural<L, T, C>>
    where
        Y: Into<L> + Into<T> + Into<C> + Copy,
    {
        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
        {
            return None;
        }

        for (idx, (lhs, rhs)) in self
            .lifetimes
            .iter()
            .copied()
            .zip(other.lifetimes.iter().copied())
            .enumerate()
        {
            let location = to_location(idx);
            existing.lifetimes.push(Matching {
                lhs,
                rhs,
                lhs_location: location.into(),
                rhs_location: location.into(),
            });
        }

        for (idx, (lhs, rhs)) in self
            .types
            .iter()
            .cloned()
            .zip(other.types.iter().cloned())
            .enumerate()
        {
            let location = to_location(idx);
            existing.types.push(Matching {
                lhs,
                rhs,
                lhs_location: location.into(),
                rhs_location: location.into(),
            });
        }

        for (idx, (lhs, rhs)) in self
            .constants
            .iter()
            .cloned()
            .zip(other.constants.iter().cloned())
            .enumerate()
        {
            let location = to_location(idx);
            existing.constants.push(Matching {
                lhs,
                rhs,
                lhs_location: location.into(),
                rhs_location: location.into(),
            });
        }

        Some(existing)
    }
}

/// A new type wrapper representing a trait associated type.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_more::Deref,
    derive_more::DerefMut,
)]
pub struct TraitMember(pub MemberSymbol);

impl crate::display::Display for TraitMember {
    async fn fmt(
        &self,
        engine: &pernixc_query::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        self.0.fmt(engine, formatter).await
    }
}

impl GenericArguments {
    /// Checks if there's any errornous term in the generic arguments.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.lifetimes.iter().any(Lifetime::is_error)
            || self.types.iter().any(Type::is_error)
            || self.constants.iter().any(Constant::is_error)
    }
}

impl crate::display::Display for Symbol {
    async fn fmt(
        &self,
        engine: &pernixc_query::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        let qualified_name = engine.get_qualified_name(self.id).await;

        write!(formatter, "{qualified_name}")?;
        self.generic_arguments.fmt(engine, formatter).await
    }
}

impl crate::display::Display for MemberSymbol {
    async fn fmt(
        &self,
        engine: &pernixc_query::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        let parent_id = self
            .id
            .target_id
            .make_global(engine.get_parent(self.id).await.unwrap());

        let parent_qualified_name = engine.get_qualified_name(parent_id).await;
        write!(formatter, "{parent_qualified_name}")?;
        self.parent_generic_arguments.fmt(engine, formatter).await?;

        let name = engine.get_name(self.id).await;
        write!(formatter, "::{name}")?;

        self.member_generic_arguments.fmt(engine, formatter).await?;

        Ok(())
    }
}
