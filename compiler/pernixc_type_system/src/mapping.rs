//! Contains the definition of [`Mapping`]

use std::collections::{BTreeMap, BTreeSet};

use getset::Getters;
use pernixc_term::{
    constant::Constant, lifetime::Lifetime, r#type::Type, sub_term::SubTerm,
    Model, ModelOf,
};

use super::unification::{self, Unifier};

/// Represents an equality mapping between two terms.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
#[allow(missing_docs)]
pub struct Mapping<M: Model> {
    pub lifetimes: BTreeMap<Lifetime<M>, BTreeSet<Lifetime<M>>>,
    pub types: BTreeMap<Type<M>, BTreeSet<Type<M>>>,
    pub constants: BTreeMap<Constant<M>, BTreeSet<Constant<M>>>,
}

impl<M: Model> Mapping<M> {
    /// Creates a new mapping from the given equality pairs.
    pub fn from_pairs(
        lifetimes: impl IntoIterator<Item = (Lifetime<M>, Lifetime<M>)>,
        types: impl IntoIterator<Item = (Type<M>, Type<M>)>,
        constants: impl IntoIterator<Item = (Constant<M>, Constant<M>)>,
    ) -> Self {
        let mut mappings = Self::default();

        for (first, second) in lifetimes {
            mappings.insert(first, second);
        }
        for (first, second) in types {
            mappings.insert(first, second);
        }
        for (first, second) in constants {
            mappings.insert(first, second);
        }

        mappings
    }

    /// Inserts a new pair of equalities into the mapping.
    pub fn insert<T: Element<Model = M> + Ord>(&mut self, first: T, second: T) {
        T::get_mut(self).entry(first).or_default().insert(second);
    }

    fn remove_recursive_internal<T: Element<Model = M> + Ord + Clone>(
        &mut self,
        term: &T,
    ) -> BTreeSet<T> {
        let map = T::get_mut(self);

        let mut terms = map.remove(term).unwrap_or_default();
        let mut results = terms.clone();

        let mut to_be_removed = Vec::<T>::new();

        for (key, values) in map.iter_mut() {
            if values.remove(term) {
                terms.insert(key.clone());
            }

            if values.is_empty() {
                to_be_removed.push(key.clone());
            }
        }

        for key in to_be_removed {
            map.remove(&key);
        }

        for term in terms {
            results.extend(self.remove_recursive_internal(&term));
        }

        results
    }

    /// Removes all the equalities that are associated with the given term.
    pub fn remove_recursive<T: Element<Model = M> + Ord + Clone>(
        &mut self,
        term: &T,
    ) {
        self.remove_recursive_internal(term);
    }

    /// Gets the number of equalities in this mapping.
    #[must_use]
    pub fn mapping_count(&self) -> usize {
        self.lifetimes
            .values()
            .map(BTreeSet::len)
            .chain(
                self.types
                    .values()
                    .map(BTreeSet::len)
                    .chain(self.constants.values().map(BTreeSet::len)),
            )
            .sum()
    }

    /// Appends all the [`unification::Matching::Unifiable`]s from the given
    /// unification into this mapping recursively.
    pub fn append_from_unifier<
        T: Element<Model = M> + SubTerm<Model = M> + Ord,
    >(
        &mut self,
        unifier: Unifier<T>,
    ) {
        match unifier.matching {
            unification::Matching::Unifiable(lhs, rhs) => {
                T::get_mut(self).entry(lhs).or_default().insert(rhs);
            }
            unification::Matching::Substructural(substructural) => {
                for (_, unification) in substructural.lifetimes {
                    self.append_from_unifier(unification);
                }

                for (_, unification) in substructural.types {
                    self.append_from_unifier(unification);
                }

                for (_, unification) in substructural.constants {
                    self.append_from_unifier(unification);
                }
            }
            unification::Matching::Equality => {}
        }
    }

    /// Creates a new mapping from all the
    /// [`unification::Matching::Unifiable`]s in the given unification.
    pub fn from_unifier<T: Element<Model = M> + SubTerm<Model = M> + Ord>(
        unifier: Unifier<T>,
    ) -> Self {
        let mut mapping = Self::default();

        mapping.append_from_unifier(unifier);

        mapping
    }
}

/// A trait for retrieving the mapping map from the [`Mapping`]
/// struct.
#[allow(missing_docs)]
pub trait Element: ModelOf {
    fn get(mapping: &Mapping<Self::Model>) -> &BTreeMap<Self, BTreeSet<Self>>
    where
        Self: Sized;

    fn get_mut(
        mapping: &mut Mapping<Self::Model>,
    ) -> &mut BTreeMap<Self, BTreeSet<Self>>
    where
        Self: Sized;
}

impl<M: Model> Element for Lifetime<M> {
    fn get(mapping: &Mapping<M>) -> &BTreeMap<Self, BTreeSet<Self>> {
        &mapping.lifetimes
    }

    fn get_mut(
        mapping: &mut Mapping<M>,
    ) -> &mut BTreeMap<Self, BTreeSet<Self>> {
        &mut mapping.lifetimes
    }
}

impl<M: Model> Element for Type<M> {
    fn get(mapping: &Mapping<M>) -> &BTreeMap<Self, BTreeSet<Self>> {
        &mapping.types
    }

    fn get_mut(
        mapping: &mut Mapping<M>,
    ) -> &mut BTreeMap<Self, BTreeSet<Self>> {
        &mut mapping.types
    }
}

impl<M: Model> Element for Constant<M> {
    fn get(mapping: &Mapping<M>) -> &BTreeMap<Self, BTreeSet<Self>> {
        &mapping.constants
    }

    fn get_mut(
        mapping: &mut Mapping<M>,
    ) -> &mut BTreeMap<Self, BTreeSet<Self>> {
        &mut mapping.constants
    }
}
