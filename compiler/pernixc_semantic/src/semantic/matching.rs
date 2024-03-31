//!

use super::{
    sub_term::SubTerm,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type},
};

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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Matching<T, Location> {
    /// The left-hand side of the match.
    pub lhs: T,

    /// The right-hand side of the match.
    pub rhs: T,

    /// The location in lhs where the match occurred
    pub lhs_location: Location,

    /// The location in rhs where the match occurred
    pub rhs_location: Location,
}

/// Represents a substructural matching between two terms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Substructural<
    SubLifetimeLocation,
    SubTypeLocation,
    SubConstantLocation,
> {
    pub lifetimes: Vec<Matching<Lifetime, SubLifetimeLocation>>,
    pub types: Vec<Matching<Type, SubTypeLocation>>,
    pub constants: Vec<Matching<Constant, SubConstantLocation>>,
}

impl<L, T, C> Default for Substructural<L, T, C> {
    fn default() -> Self {
        Self { lifetimes: Vec::new(), types: Vec::new(), constants: Vec::new() }
    }
}
