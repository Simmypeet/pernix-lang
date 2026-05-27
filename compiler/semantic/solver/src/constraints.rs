use std::hash::BuildHasherDefault;

use pernixc_hash::FxHashSet;
use pernixc_qbice::Interner;
use pernixc_type::{
    predicate::Outlives,
    substitution::{Substitutable, Substitution},
    r#type::Type,
};
use qbice::storage::intern::Interned;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Constraints(FxHashSet<Outlives>);

impl Substitutable for Constraints {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self>
    where
        Self: Sized,
    {
        let mut iter = self.0.iter().enumerate();

        while let Some((index, next)) = iter.next() {
            if let Some(changed) = next.apply(subst, interner) {
                let mut new_constraints = FxHashSet::with_capacity_and_hasher(
                    self.0.len(),
                    BuildHasherDefault::default(),
                );

                new_constraints.extend(self.0.iter().take(index).cloned());
                new_constraints.insert(changed);
                new_constraints.extend(
                    iter.map(|(_, c)| c.apply_or_clone(subst, interner)),
                );

                return Some(Self(new_constraints));
            }
        }

        None
    }
}

impl Constraints {
    #[must_use]
    pub fn new() -> Self { Self::default() }

    #[must_use]
    pub fn lifetimes_eq(a: Interned<Type>, b: Interned<Type>) -> Self {
        let mut lifetime_eq = Self::new();

        lifetime_eq.0.insert(Outlives::new(a.clone(), b.clone()));
        lifetime_eq.0.insert(Outlives::new(b, a));

        lifetime_eq
    }
}

impl Extend<Outlives> for Constraints {
    fn extend<T: IntoIterator<Item = Outlives>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}

impl IntoIterator for Constraints {
    type Item = Outlives;
    type IntoIter = <FxHashSet<Outlives> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter { self.0.into_iter() }
}
