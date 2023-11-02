//! Contains code related to logic applied to the entities.

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

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
}

/// Represents a substitution of terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Substitution<'a, S: Model> {
    pub types: HashMap<Cow<'a, Type<S>>, Cow<'a, Type<S>>>,
    pub constants: HashMap<Cow<'a, Constant<S>>, Cow<'a, Constant<S>>>,
    pub regions: HashMap<Cow<'a, Region<S>>, Cow<'a, Region<S>>>,
}

/// Represents a mapping of terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Mapping<'a, S: Model> {
    regions: HashMap<Cow<'a, Region<S>>, HashSet<Cow<'a, Region<S>>>>,
    types: HashMap<Cow<'a, Type<S>>, HashSet<Cow<'a, Type<S>>>>,
    constants: HashMap<Cow<'a, Constant<S>>, HashSet<Cow<'a, Constant<S>>>>,
}

macro_rules! insert_item {
    ($map:expr, $expr:expr) => {{
        for (lhs, rhs) in $expr {
            $map.entry(Cow::Borrowed(lhs))
                .or_default()
                .insert(Cow::Borrowed(rhs));
            $map.entry(Cow::Borrowed(rhs))
                .or_default()
                .insert(Cow::Borrowed(lhs));
        }
    }};
}

impl<'a, S: Model> Mapping<'a, S> {
    /// Creates a new mapping from the given equality pairs.
    pub fn from_pairs(
        regions: impl IntoIterator<Item = (&'a Region<S>, &'a Region<S>)>,
        types: impl IntoIterator<Item = (&'a Type<S>, &'a Type<S>)>,
        constants: impl IntoIterator<Item = (&'a Constant<S>, &'a Constant<S>)>,
    ) -> Self {
        let mut mappings = Self::default();

        insert_item!(mappings.regions, regions);
        insert_item!(mappings.types, types);
        insert_item!(mappings.constants, constants);

        mappings
    }
}

#[cfg(test)]
pub(super) mod tests;
