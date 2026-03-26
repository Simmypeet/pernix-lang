//! Contains the definition of [`Constraints`].

use pernixc_hash::HashSet;
use qbice::{Decode, Encode, StableHash};

use crate::lifetime_constraint::LifetimeConstraint;

/// A collection of lifetime constraints.
///
/// This is a wrapper around [`HashSet<LifetimeConstraint>`] that provides
/// a convenient interface for working with lifetime constraints.
#[derive(Debug, Clone, PartialEq, Eq, Default, StableHash, Encode, Decode)]
pub struct Constraints(HashSet<LifetimeConstraint>);

impl Constraints {
    /// Creates a new empty [`Constraints`].
    #[must_use]
    pub fn new() -> Self { Self(HashSet::default()) }

    /// Returns the number of constraints in the collection.
    #[must_use]
    pub fn len(&self) -> usize { self.0.len() }

    /// Returns `true` if the collection contains no constraints.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.0.is_empty() }

    /// Inserts a constraint into the collection.
    ///
    /// Returns `true` if the constraint was not already present.
    pub fn insert(&mut self, constraint: LifetimeConstraint) -> bool {
        self.0.insert(constraint)
    }

    /// Returns `true` if the collection contains the given constraint.
    #[must_use]
    pub fn contains(&self, constraint: &LifetimeConstraint) -> bool {
        self.0.contains(constraint)
    }

    /// Returns an iterator over the constraints.
    #[must_use]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &LifetimeConstraint> {
        self.0.iter()
    }

    /// Extends the collection with the constraints from the given iterator.
    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = LifetimeConstraint>,
    {
        self.0.extend(iter);
    }

    /// Clears the collection, removing all constraints.
    pub fn clear(&mut self) { self.0.clear(); }

    /// Removes a constraint from the collection.
    ///
    /// Returns `true` if the constraint was present.
    pub fn remove(&mut self, constraint: &LifetimeConstraint) -> bool {
        self.0.remove(constraint)
    }

    /// Retains only the constraints specified by the predicate.
    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&LifetimeConstraint) -> bool,
    {
        self.0.retain(f);
    }
}

impl Extend<LifetimeConstraint> for Constraints {
    fn extend<T: IntoIterator<Item = LifetimeConstraint>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}

impl IntoIterator for Constraints {
    type Item = LifetimeConstraint;
    type IntoIter = std::collections::hash_set::IntoIter<LifetimeConstraint>;

    fn into_iter(self) -> Self::IntoIter { self.0.into_iter() }
}

impl<'a> IntoIterator for &'a Constraints {
    type Item = &'a LifetimeConstraint;
    type IntoIter = std::collections::hash_set::Iter<'a, LifetimeConstraint>;

    fn into_iter(self) -> Self::IntoIter { self.0.iter() }
}

impl FromIterator<LifetimeConstraint> for Constraints {
    fn from_iter<T: IntoIterator<Item = LifetimeConstraint>>(iter: T) -> Self {
        Self(HashSet::from_iter(iter))
    }
}
