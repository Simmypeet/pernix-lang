//! Contains logic related to matching the term structurally.

use derive_new::new;

use super::sub_term::SubTerm;
use crate::{constant::Constant, lifetime::Lifetime, r#type::Type};

/// Represents a match between two terms.
pub trait Match: Sized + SubTerm {
    /// Returns the matching substructural matches between `self` and `other`.
    ///
    /// # Example
    ///
    /// If `self` is `List[T, Map[U, V]]` and `other` is `List[A, B]`, then the
    /// substructural matches are:
    ///
    /// - `T` and `A`
    /// - `Map[U, V]` and `B`
    ///
    /// # Returns
    ///
    /// Returns `None` if the terms cannot be substructurally matched.
    #[allow(clippy::type_complexity)]
    fn substructural_match(
        &self,
        other: &Self,
    ) -> Option<
        Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    >;

    #[doc(hidden)]
    fn get_substructural(
        substructural: &Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Matching<Self, Self::ThisSubTermLocation>>;

    #[doc(hidden)]
    fn get_substructural_mut(
        substructural: &mut Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Matching<Self, Self::ThisSubTermLocation>>;
}

/// Represents a match between two terms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, new)]
pub struct Matching<T, Location> {
    /// The left-hand side of the match.
    lhs: T,

    /// The right-hand side of the match.
    rhs: T,

    /// The location in lhs where the match occurred
    lhs_location: Location,

    /// The location in rhs where the match occurred
    rhs_location: Location,
}

impl<T, Location> Matching<T, Location> {
    /// Returns a reference to the left-hand side of the match.
    #[must_use]
    pub const fn lhs(&self) -> &T { &self.lhs }

    /// Returns a reference to the right-hand side of the match.
    #[must_use]
    pub const fn rhs(&self) -> &T { &self.rhs }

    /// Returns a reference to the location in lhs where the match occurred.
    #[must_use]
    pub const fn lhs_location(&self) -> &Location { &self.lhs_location }

    /// Returns a reference to the location in rhs where the match occurred.
    #[must_use]
    pub const fn rhs_location(&self) -> &Location { &self.rhs_location }
}

/// Represents a substructural matching between two terms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
#[allow(missing_docs)]
pub struct Substructural<
    SubLifetimeLocation,
    SubTypeLocation,
    SubConstantLocation,
> {
    lifetimes: Vec<Matching<Lifetime, SubLifetimeLocation>>,
    types: Vec<Matching<Type, SubTypeLocation>>,
    constants: Vec<Matching<Constant, SubConstantLocation>>,
}

impl<L, T, C> Substructural<L, T, C> {
    /// Returns a reference to the lifetime matchings.
    #[must_use]
    pub const fn lifetimes(&self) -> &Vec<Matching<Lifetime, L>> {
        &self.lifetimes
    }

    /// Returns a mutable reference to the lifetime matchings.
    #[must_use]
    pub const fn lifetimes_mut(&mut self) -> &mut Vec<Matching<Lifetime, L>> {
        &mut self.lifetimes
    }

    /// Returns a reference to the type matchings.
    #[must_use]
    pub const fn types(&self) -> &Vec<Matching<Type, T>> { &self.types }

    /// Returns a mutable reference to the type matchings.
    #[must_use]
    pub const fn types_mut(&mut self) -> &mut Vec<Matching<Type, T>> {
        &mut self.types
    }

    /// Returns a reference to the constant matchings.
    #[must_use]
    pub const fn constants(&self) -> &Vec<Matching<Constant, C>> {
        &self.constants
    }

    /// Returns a mutable reference to the constant matchings.
    #[must_use]
    pub const fn constants_mut(&mut self) -> &mut Vec<Matching<Constant, C>> {
        &mut self.constants
    }
}

impl<L, T, C> Default for Substructural<L, T, C> {
    fn default() -> Self { Self::new(Vec::new(), Vec::new(), Vec::new()) }
}
