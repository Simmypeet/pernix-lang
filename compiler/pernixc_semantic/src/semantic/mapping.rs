//! Contains the definition of [`Mapping`]

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use getset::Getters;

use super::term::{constant::Constant, lifetime::Lifetime, r#type::Type};

/// Represents an equality mapping between two terms.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
#[allow(missing_docs)]
pub struct Mapping {
    /// The list of lifetimes that are equal to each other.
    #[get = "pub"]
    lifetimes: HashMap<Lifetime, HashSet<Lifetime>>,

    /// The list of types that are equal to each other.
    #[get = "pub"]
    types: HashMap<Type, HashSet<Type>>,

    /// The list of constants that are equal to each other.
    #[get = "pub"]
    constants: HashMap<Constant, HashSet<Constant>>,
}

macro_rules! insert_item {
    ($map:expr, $expr:expr) => {{
        for (lhs, rhs) in $expr {
            $map.entry(lhs.clone()).or_default().insert(rhs.clone());
            $map.entry(rhs).or_default().insert(lhs);
        }
    }};
}

impl Mapping {
    /// Creates a new mapping from the given equality pairs.
    pub fn from_pairs(
        lifetimes: impl IntoIterator<Item = (Lifetime, Lifetime)>,
        types: impl IntoIterator<Item = (Type, Type)>,
        constants: impl IntoIterator<Item = (Constant, Constant)>,
    ) -> Self {
        let mut mappings = Self::default();

        insert_item!(mappings.lifetimes, lifetimes);
        insert_item!(mappings.types, types);
        insert_item!(mappings.constants, constants);

        mappings
    }

    /// Inserts a new pair of equalities into the mapping.
    pub fn insert<T: Map + Clone + Ord + Hash + Eq>(
        &mut self,
        first: T,
        second: T,
    ) {
        T::get_mut(self)
            .entry(first.clone())
            .or_default()
            .insert(second.clone());
        T::get_mut(self).entry(second).or_default().insert(first);
    }

    /// Removes all the equalities that are associated with the given term.
    pub fn remove_equality<T: Map + Ord + Hash + Eq>(
        &mut self,
        term: &T,
    ) -> Option<HashSet<T>> {
        let map = T::get_mut(self);

        let Some(terms) = map.remove(term) else {
            return None;
        };

        for t in &terms {
            if let Some(x) = map.get_mut(t) {
                x.remove(term);

                if x.is_empty() {
                    map.remove(t);
                }
            }
        }

        Some(terms)
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

/// Used to map a value to a set of equivalent values.
pub trait Map: Sized {
    /// Gets a reference to the mapping of this kind of term.
    fn get(mapping: &Mapping) -> &HashMap<Self, HashSet<Self>>;

    /// Gets a mutable reference to the mapping of this kind of term.
    fn get_mut(mapping: &mut Mapping) -> &mut HashMap<Self, HashSet<Self>>;
}

impl Map for Lifetime {
    fn get(mapping: &Mapping) -> &HashMap<Self, HashSet<Self>> {
        &mapping.lifetimes
    }

    fn get_mut(mapping: &mut Mapping) -> &mut HashMap<Self, HashSet<Self>> {
        &mut mapping.lifetimes
    }
}

impl Map for Constant {
    fn get(mapping: &Mapping) -> &HashMap<Self, HashSet<Self>> {
        &mapping.constants
    }

    fn get_mut(mapping: &mut Mapping) -> &mut HashMap<Self, HashSet<Self>> {
        &mut mapping.constants
    }
}

impl Map for Type {
    fn get(mapping: &Mapping) -> &HashMap<Self, HashSet<Self>> {
        &mapping.types
    }

    fn get_mut(mapping: &mut Mapping) -> &mut HashMap<Self, HashSet<Self>> {
        &mut mapping.types
    }
}
