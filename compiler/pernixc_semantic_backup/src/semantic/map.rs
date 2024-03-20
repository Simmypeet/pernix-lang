//! Contains the definition of [`Mapping`]

use std::collections::{BTreeMap, HashSet};

use getset::Getters;

use super::{
    model::Model,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type},
};

/// Represents an equality mapping between two terms.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
#[allow(missing_docs)]
pub struct Mapping<S: Model> {
    /// The list of lifetimes that are equal to each other.
    #[get = "pub"]
    lifetimes: BTreeMap<Lifetime<S>, HashSet<Lifetime<S>>>,

    /// The list of types that are equal to each other.
    #[get = "pub"]
    types: BTreeMap<Type<S>, HashSet<Type<S>>>,

    /// The list of constants that are equal to each other.
    #[get = "pub"]
    constants: BTreeMap<Constant<S>, HashSet<Constant<S>>>,
}

macro_rules! insert_item {
    ($map:expr, $expr:expr) => {{
        for (lhs, rhs) in $expr {
            $map.entry(lhs.clone()).or_default().insert(rhs.clone());
            $map.entry(rhs).or_default().insert(lhs);
        }
    }};
}

impl<S: Model> Mapping<S> {
    /// Creates a new mapping from the given equality pairs.
    pub fn from_pairs(
        lifetimes: impl IntoIterator<Item = (Lifetime<S>, Lifetime<S>)>,
        types: impl IntoIterator<Item = (Type<S>, Type<S>)>,
        constants: impl IntoIterator<Item = (Constant<S>, Constant<S>)>,
    ) -> Self {
        let mut mappings = Self::default();

        insert_item!(mappings.lifetimes, lifetimes);
        insert_item!(mappings.types, types);
        insert_item!(mappings.constants, constants);

        mappings
    }

    /// Inserts a new equality pair into the mapping.
    pub fn insert_lifetime(&mut self, lhs: Lifetime<S>, rhs: Lifetime<S>) {
        self.lifetimes.entry(lhs.clone()).or_default().insert(rhs.clone());
        self.lifetimes.entry(rhs).or_default().insert(lhs);
    }

    /// Inserts a new equality pair into the mapping.
    pub fn insert_type(&mut self, lhs: Type<S>, rhs: Type<S>) {
        self.types.entry(lhs.clone()).or_default().insert(rhs.clone());
        self.types.entry(rhs).or_default().insert(lhs);
    }

    /// Inserts a new equality pair into the mapping.
    pub fn insert_constant(&mut self, lhs: Constant<S>, rhs: Constant<S>) {
        self.constants.entry(lhs.clone()).or_default().insert(rhs.clone());
        self.constants.entry(rhs).or_default().insert(lhs);
    }
}

/// Used to map a value to a set of equivalent values.
pub trait Map: Sized {
    /// The model of the term.
    type Model: Model;

    /// Returns the set of equivalent values for the given value.
    fn map<'a>(
        &'a self,
        mapping: &'a Mapping<Self::Model>,
    ) -> Option<&'a HashSet<Self>>;

    /// Gets all the available mappings of this term.
    fn get(mapping: &Mapping<Self::Model>) -> &BTreeMap<Self, HashSet<Self>>;

    /// Inserts a new equality pair into the mapping.
    fn insert(mapping: &mut Mapping<Self::Model>, first: Self, second: Self);
}

impl<S: Model> Map for Lifetime<S> {
    type Model = S;

    fn map<'a>(&'a self, mapping: &'a Mapping<S>) -> Option<&'a HashSet<Self>> {
        mapping.lifetimes.get(self)
    }

    fn get(mapping: &Mapping<S>) -> &BTreeMap<Self, HashSet<Self>> {
        &mapping.lifetimes
    }

    fn insert(mapping: &mut Mapping<Self::Model>, first: Self, second: Self) {
        mapping.insert_lifetime(first, second);
    }
}

impl<S: Model> Map for Constant<S> {
    type Model = S;

    fn map<'a>(&'a self, mapping: &'a Mapping<S>) -> Option<&'a HashSet<Self>> {
        mapping.constants.get(self)
    }

    fn get(mapping: &Mapping<S>) -> &BTreeMap<Self, HashSet<Self>> {
        &mapping.constants
    }

    fn insert(mapping: &mut Mapping<Self::Model>, first: Self, second: Self) {
        mapping.insert_constant(first, second);
    }
}

impl<S: Model> Map for Type<S> {
    type Model = S;

    fn map<'a>(&'a self, mapping: &'a Mapping<S>) -> Option<&'a HashSet<Self>> {
        mapping.types.get(self)
    }

    fn get(mapping: &Mapping<S>) -> &BTreeMap<Self, HashSet<Self>> {
        &mapping.types
    }

    fn insert(mapping: &mut Mapping<Self::Model>, first: Self, second: Self) {
        mapping.insert_type(first, second);
    }
}
