//! Contains code related to logic applied to the entities.

use std::collections::{HashMap, HashSet};

use crate::entity::{constant::Constant, r#type::Type, region::Region, Model};

pub mod equality;
pub mod substitution;
pub mod r#trait;
pub mod unification;

/// Is used to store the requested queries to prevent infinite recursion.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct QueryRecords<S: Model> {
    type_equals: HashSet<(Type<S>, Type<S>)>,
    constant_equals: HashSet<(Constant<S>, Constant<S>)>,
    region_equals: HashSet<(Region<S>, Region<S>)>,

    type_unifies: HashSet<(Type<S>, Type<S>)>,
    constant_unifies: HashSet<(Constant<S>, Constant<S>)>,
}

/// Represents a substitution of terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Substitution<S: Model> {
    pub types: HashMap<Type<S>, Type<S>>,
    pub constants: HashMap<Constant<S>, Constant<S>>,
    pub regions: HashMap<Region<S>, Region<S>>,
}

/// Represents a mapping of terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Mapping<S: Model> {
    regions: HashMap<Region<S>, HashSet<Region<S>>>,
    types: HashMap<Type<S>, HashSet<Type<S>>>,
    constants: HashMap<Constant<S>, HashSet<Constant<S>>>,
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
        regions: impl IntoIterator<Item = (Region<S>, Region<S>)>,
        types: impl IntoIterator<Item = (Type<S>, Type<S>)>,
        constants: impl IntoIterator<Item = (Constant<S>, Constant<S>)>,
    ) -> Self {
        let mut mappings = Self::default();

        insert_item!(mappings.regions, regions);
        insert_item!(mappings.types, types);
        insert_item!(mappings.constants, constants);

        mappings
    }
}
