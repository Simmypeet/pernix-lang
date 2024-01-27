//! Contains the definition of [`Mapping`]

use std::collections::{HashMap, HashSet};

use getset::Getters;

use super::term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term};

/// Represents an equality mapping between two terms.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
#[allow(missing_docs)]
pub struct Mapping {
    pub lifetimes: HashMap<Lifetime, HashSet<Lifetime>>,
    pub types: HashMap<Type, HashSet<Type>>,
    pub constants: HashMap<Constant, HashSet<Constant>>,
}

impl Mapping {
    /// Creates a new mapping from the given equality pairs.
    pub fn from_pairs(
        lifetimes: impl IntoIterator<Item = (Lifetime, Lifetime)>,
        types: impl IntoIterator<Item = (Type, Type)>,
        constants: impl IntoIterator<Item = (Constant, Constant)>,
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
    pub fn insert<T: Term>(&mut self, first: T, second: T) {
        T::get_mapping_mut(self).entry(first).or_default().insert(second);
    }

    fn remove_recursive_internal<T: Term>(&mut self, term: &T) -> HashSet<T> {
        let map = T::get_mapping_mut(self);

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
    pub fn remove_recursive<T: Term>(&mut self, term: &T) {
        self.remove_recursive_internal(term);
    }

    /// Gets the number of equalities in this mapping.
    #[must_use]
    pub fn mapping_count(&self) -> usize {
        self.lifetimes
            .values()
            .map(HashSet::len)
            .chain(
                self.types
                    .values()
                    .map(HashSet::len)
                    .chain(self.constants.values().map(HashSet::len)),
            )
            .sum()
    }
}
