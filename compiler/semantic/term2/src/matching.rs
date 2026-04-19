//! Contains logic related to matching terms structurally.

use derive_new::new;
use enum_as_inner::EnumAsInner;
use qbice::{Identifiable, StableHash, storage::intern::Interned};

use super::sub_term::SubTerm;
use crate::{
    constant::Constant, instance::Instance, lifetime::Lifetime, r#type::Type,
};

/// Represents a match between two terms.
pub trait Match:
    Sized + SubTerm + Identifiable + StableHash + Send + Sync + 'static
{
    /// Returns the matching substructural matches between `self` and `other`.
    ///
    /// Returns `None` if the terms cannot be substructurally matched.
    #[allow(clippy::type_complexity)]
    fn substructural_match<'a>(
        &'a self,
        other: &'a Self,
    ) -> Option<
        impl Iterator<
            Item = Substructural<
                Self::SubLifetimeLocation,
                Self::SubTypeLocation,
                Self::SubConstantLocation,
                Self::SubInstanceLocation,
            >,
        > + 'a,
    >;

    #[doc(hidden)]
    fn from_self_matching(
        matching: Matching<Interned<Self>, Self::ThisSubTermLocation>,
    ) -> Substructural<
        Self::SubLifetimeLocation,
        Self::SubTypeLocation,
        Self::SubConstantLocation,
        Self::SubInstanceLocation,
    >;
}

/// Represents a match between two terms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, new)]
pub struct Matching<T, Location> {
    lhs: T,
    rhs: T,
    lhs_location: Location,
    rhs_location: Location,
}

impl<T, Location> Matching<T, Location> {
    /// Returns a reference to the left-hand side of the match.
    #[must_use]
    pub const fn lhs(&self) -> &T { &self.lhs }

    /// Returns a reference to the right-hand side of the match.
    #[must_use]
    pub const fn rhs(&self) -> &T { &self.rhs }

    /// Returns a reference to the left-hand-side location.
    #[must_use]
    pub const fn lhs_location(&self) -> &Location { &self.lhs_location }

    /// Returns a reference to the right-hand-side location.
    #[must_use]
    pub const fn rhs_location(&self) -> &Location { &self.rhs_location }

    /// Destructures the matching into its components.
    #[must_use]
    pub fn destructure(self) -> (T, T, Location, Location) {
        (self.lhs, self.rhs, self.lhs_location, self.rhs_location)
    }
}

/// Represents a single streamed substructural match.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum Substructural<
    SubLifetimeLocation,
    SubTypeLocation,
    SubConstantLocation,
    SubInstanceLocation,
> {
    /// A matched lifetime component.
    #[from]
    Lifetime(Matching<Interned<Lifetime>, SubLifetimeLocation>),

    /// A matched type component.
    #[from]
    Type(Matching<Interned<Type>, SubTypeLocation>),

    /// A matched constant component.
    #[from]
    Constant(Matching<Interned<Constant>, SubConstantLocation>),

    /// A matched instance component.
    #[from]
    Instance(Matching<Interned<Instance>, SubInstanceLocation>),
}
