//! Contains the definition of [`Mapping`]

use std::collections::{BTreeMap, BTreeSet};

use getset::Getters;

use super::term::{constant::Constant, lifetime::Lifetime, r#type::Type};

/// Represents an equality mapping between two terms.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
#[allow(missing_docs)]
pub struct Mapping {
    /// The list of lifetimes that are equal to each other.
    #[get = "pub"]
    lifetimes: BTreeMap<Lifetime, BTreeSet<Lifetime>>,

    /// The list of types that are equal to each other.
    #[get = "pub"]
    types: BTreeMap<Type, BTreeSet<Type>>,

    /// The list of constants that are equal to each other.
    #[get = "pub"]
    constants: BTreeMap<Constant, BTreeSet<Constant>>,
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
    pub fn insert<T: Map + Clone + Ord>(&mut self, first: T, second: T) {
        T::get_mut(self)
            .entry(first.clone())
            .or_default()
            .insert(second.clone());
        T::get_mut(self).entry(second).or_default().insert(first);
    }

    /// Removes all the equalities that are associated with the given term.
    pub fn remove_equality<T: Map + Eq + Ord>(&mut self, term: &T) -> Option<BTreeSet<T>> {
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
}

/// Used to map a value to a set of equivalent values.
pub trait Map: Sized {
    /// Gets a reference to the mapping of this kind of term.
    fn get(mapping: &Mapping) -> &BTreeMap<Self, BTreeSet<Self>>;

    /// Gets a mutable reference to the mapping of this kind of term.
    fn get_mut(mapping: &mut Mapping) -> &mut BTreeMap<Self, BTreeSet<Self>>;
}

impl Map for Lifetime {
    fn get(mapping: &Mapping) -> &BTreeMap<Self, BTreeSet<Self>> { &mapping.lifetimes }

    fn get_mut(mapping: &mut Mapping) -> &mut BTreeMap<Self, BTreeSet<Self>> {
        &mut mapping.lifetimes
    }
}

impl Map for Constant {
    fn get(mapping: &Mapping) -> &BTreeMap<Self, BTreeSet<Self>> { &mapping.constants }

    fn get_mut(mapping: &mut Mapping) -> &mut BTreeMap<Self, BTreeSet<Self>> {
        &mut mapping.constants
    }
}

impl Map for Type {
    fn get(mapping: &Mapping) -> &BTreeMap<Self, BTreeSet<Self>> { &mapping.types }

    fn get_mut(mapping: &mut Mapping) -> &mut BTreeMap<Self, BTreeSet<Self>> { &mut mapping.types }
}
