//! Contains the definition of the [`Parent`] component.

use std::sync::Arc;

use derive_more::{Deref, DerefMut};
use pernixc_extend::extend;
use pernixc_hash::HashMap;
use pernixc_query::{TrackedEngine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::{Global, TargetID};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::{
    get_table_of_symbol, get_target_root_module_id,
    kind::{get_kind, Kind},
    name::get_name,
    ID,
};

/// Describes the relationship between two symbols in the hierarchy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HierarchyRelationship {
    /// The first symbol is the parent of the second symbol.
    Parent,

    /// The first symbol is the child of the second symbol.
    Child,

    /// Both symbols are two equivalent symbols.
    Equivalent,

    /// Both symbols are defined in different hierarchy scope.
    Unrelated,
}

/// Query for retrieving the parent symbol of the given symbol ID in the
/// qualified-identifier hierarchy.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deref,
    DerefMut,
    Default,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Option<ID>)]
#[extend(method(get_parent), no_cyclic)]
pub struct Key(pub Global<ID>);

/// Represents an iterator that walks through the scope of the given symbol. It
/// goes through all the parent symbols until it reaches the root.
///
/// The iterator iterates through the scope in id-to-parent order including the
/// initial symbol.
#[derive(Debug, Clone)]
pub struct ScopeWalker<'a> {
    engine: &'a TrackedEngine<'a>,
    current_id: Option<ID>,
    target_id: TargetID,
}

impl Iterator for ScopeWalker<'_> {
    type Item = ID;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current_id {
            Some(current_id) => {
                let next = self
                    .engine
                    .get_parent(self.target_id.make_global(current_id));

                self.current_id = next;
                Some(current_id)
            }
            None => None,
        }
    }
}
/// Gets the [`ScopeWalker`] that walks through the scope hierarchy of the
/// given [`GlobalID`].
///
/// See [`ScopeWalker`] for more information.
#[extend]
pub fn scope_walker(self: &TrackedEngine<'_>, id: Global<ID>) -> ScopeWalker {
    ScopeWalker {
        engine: self,
        current_id: Some(id.id),
        target_id: id.target_id,
    }
}

/// Computes the [`HierarchyRelationship`] between the two given item IDs.
///
/// The returned [`HierarchyRelationship`] is based on the `first` symbol.
#[extend]
pub fn symbol_hierarchy_relationship(
    self: &TrackedEngine<'_>,
    target_id: TargetID,
    first: ID,
    second: ID,
) -> HierarchyRelationship {
    // the two symbols are the same.
    if first == second {
        return HierarchyRelationship::Equivalent;
    }

    for first_parent in self.scope_walker(Global::new(target_id, first)) {
        if first_parent == second {
            return HierarchyRelationship::Child;
        }
    }

    for second_parent in self.scope_walker(Global::new(target_id, second)) {
        if second_parent == first {
            return HierarchyRelationship::Parent;
        }
    }

    HierarchyRelationship::Unrelated
}

/// Returns the [`symbol::ID`] that is the module and closest to the given
/// [`Global<symbol::ID>`] (including itself).
#[extend]
pub fn get_closest_module_id(
    self: &TrackedEngine<'_>,
    mut id: Global<ID>,
) -> ID {
    loop {
        if self.get_kind(id) == Kind::Module {
            return id.id;
        }

        id = Global::new(
            id.target_id,
            self.get_parent(id).expect("should always have a parent "),
        );
    }
}

/// The executor for the [`Parent`] component.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &TrackedEngine,
        key: &Key,
    ) -> Result<Option<ID>, pernixc_query::runtime::executor::CyclicError> {
        if key.0.id == engine.get_target_root_module_id(key.0.target_id) {
            return Ok(None);
        }

        let intermediate = engine
            .query(&IntermediateKey(key.0.target_id))
            .expect("should have no cyclic dependencies");

        let parent_id =
            intermediate.get(&key.0.id).copied().unwrap_or_else(|| {
                panic!(
                    "symbol `{}` doesn't have a parent",
                    engine.get_name(key.0)
                );
            });

        Ok(Some(parent_id))
    }
}

/// Intermediate query to compute the [`Parent`] component for all symbols
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Deref,
    DerefMut,
    Default,
    Serialize,
    Deserialize,
    Value,
    StableHash,
)]
#[id(TargetID)]
#[key(IntermediateKey)]
#[value(Arc<Intermediate>)]
#[doc(hidden)]
pub struct Intermediate(pub HashMap<ID, ID>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[doc(hidden)]
pub struct IntermediateExecutor;

impl pernixc_query::runtime::executor::Executor<IntermediateKey>
    for IntermediateExecutor
{
    fn execute(
        &self,
        engine: &TrackedEngine,
        &IntermediateKey(target_id): &IntermediateKey,
    ) -> Result<Arc<Intermediate>, pernixc_query::runtime::executor::CyclicError>
    {
        let map = engine.query(&crate::MapKey(target_id))?;

        Ok(Arc::new(Intermediate(
            map.keys_by_symbol_id
                .par_iter()
                .filter_map(|x| {
                    let table = engine
                        .get_table_of_symbol(target_id.make_global(*x.key()));

                    table
                        .members
                        .get(x.key())
                        .cloned()
                        .map(|members| (*x.key(), members))
                })
                .flat_map_iter(|(symbol, member)| {
                    member
                        .member_ids_by_name
                        .values()
                        .copied()
                        .chain(member.redefinitions.iter().copied())
                        .map(move |member_id| (member_id, symbol))
                        .collect::<Vec<_>>()
                })
                .collect(),
        )))
    }
}
