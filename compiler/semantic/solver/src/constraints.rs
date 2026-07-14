use std::hash::BuildHasherDefault;

use im::HashSet;
use pernixc_hash::FxHasher;
use pernixc_qbice::Interner;
use pernixc_type::{
    predicate::Outlives,
    substitution::{Substitutable, Substitution},
    r#type::Type2,
};
use qbice::storage::intern::Interned;

type ConstraintSet = HashSet<Outlives, BuildHasherDefault<FxHasher>>;

/// A collection of [`Outlives`] constraints.
///
/// Internally, it uses immutable data structures to allow for efficient sharing
/// and O(1) cheap-to-clone operations.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Constraints(ConstraintSet);

impl Constraints {
    #[must_use]
    pub fn union_into(self, other: Self) -> Self { Self(self.0.union(other.0)) }
}

impl Substitutable for Constraints {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self>
    where
        Self: Sized,
    {
        let iter = self.0.iter();
        let mut new_constraints = None;

        for constraint in iter {
            if let Some(new_constraint) = constraint.apply(subst, interner) {
                let new_constraints =
                    new_constraints.get_or_insert(self.0.clone());

                new_constraints.remove(constraint);
                new_constraints.insert(new_constraint);
            }
        }

        new_constraints.map(Self)
    }
}

impl Constraints {
    #[must_use]
    pub fn new() -> Self { Self::default() }

    #[must_use]
    pub fn lifetimes_eq(a: Interned<Type2>, b: Interned<Type2>) -> Self {
        let mut lifetime_eq = Self::new();

        lifetime_eq.0.insert(Outlives::new(a.clone(), b.clone()));
        lifetime_eq.0.insert(Outlives::new(b, a));

        lifetime_eq
    }

    #[must_use]
    pub fn lifetimes_outlives(
        lesser: Interned<Type2>,
        greater: Interned<Type2>,
    ) -> Self {
        let mut lifetime_eq = Self::new();

        lifetime_eq.0.insert(Outlives::new(lesser, greater));

        lifetime_eq
    }

    pub fn insert(&mut self, constraint: Outlives) {
        self.0.insert(constraint);
    }
}

impl Extend<Outlives> for Constraints {
    fn extend<T: IntoIterator<Item = Outlives>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}

impl IntoIterator for Constraints {
    type Item = Outlives;
    type IntoIter = <ConstraintSet as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter { self.0.into_iter() }
}
