//! Contains the definition of [`Mapping`]

use std::collections::{HashMap, HashSet};

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

    /// Inserts a new equality pair into the mapping.
    pub fn insert_lifetime(&mut self, lhs: Lifetime, rhs: Lifetime) {
        self.lifetimes.entry(lhs).or_default().insert(rhs);
        self.lifetimes.entry(rhs).or_default().insert(lhs);
    }

    /// Inserts a new equality pair into the mapping.
    pub fn insert_type(&mut self, lhs: Type, rhs: Type) {
        self.types
            .entry(lhs.clone())
            .or_default()
            .insert(rhs.clone());
        self.types.entry(rhs).or_default().insert(lhs);
    }

    /// Inserts a new equality pair into the mapping.
    pub fn insert_constant(&mut self, lhs: Constant, rhs: Constant) {
        self.constants
            .entry(lhs.clone())
            .or_default()
            .insert(rhs.clone());
        self.constants.entry(rhs).or_default().insert(lhs);
    }
}

/// Used to map a value to a set of equivalent values.
pub trait Map: Sized {
    /// Returns the set of equivalent values for the given value.
    fn map<'a>(&'a self, mapping: &'a Mapping) -> Option<&'a HashSet<Self>>;

    /// Gets all the available mappings of this term.
    fn get(mapping: &Mapping) -> &HashMap<Self, HashSet<Self>>;

    /// Inserts a new equality pair into the mapping.
    fn insert(mapping: &mut Mapping, first: Self, second: Self);
}

impl Map for Lifetime {
    fn map<'a>(&'a self, mapping: &'a Mapping) -> Option<&'a HashSet<Self>> {
        mapping.lifetimes.get(self)
    }

    fn get(mapping: &Mapping) -> &HashMap<Self, HashSet<Self>> { &mapping.lifetimes }

    fn insert(mapping: &mut Mapping, first: Self, second: Self) {
        mapping.insert_lifetime(first, second);
    }
}

impl Map for Constant {
    fn map<'a>(&'a self, mapping: &'a Mapping) -> Option<&'a HashSet<Self>> {
        mapping.constants.get(self)
    }

    fn get(mapping: &Mapping) -> &HashMap<Self, HashSet<Self>> { &mapping.constants }

    fn insert(mapping: &mut Mapping, first: Self, second: Self) {
        mapping.insert_constant(first, second);
    }
}

impl Map for Type {
    fn map<'a>(&'a self, mapping: &'a Mapping) -> Option<&'a HashSet<Self>> {
        mapping.types.get(self)
    }

    fn get(mapping: &Mapping) -> &HashMap<Self, HashSet<Self>> { &mapping.types }

    fn insert(mapping: &mut Mapping, first: Self, second: Self) {
        mapping.insert_type(first, second);
    }
}
