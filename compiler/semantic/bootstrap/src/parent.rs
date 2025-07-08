//! Contains the definition of the [`Parent`] component.

use std::sync::Arc;

use derive_more::{Deref, DerefMut};
use pernixc_extend::extend;
use pernixc_hash::HashMap;
use pernixc_query::{TrackedEngine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::{Global, TargetID};

use crate::{
    kind::{get_kind, Kind},
    member::try_get_members,
    name::get_name,
    symbol,
    target::get_target,
    HierarchyRelationship,
};

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
#[value(Option<symbol::ID>)]
#[extend(method(get_parent), no_cyclic)]
pub struct Key(pub Global<symbol::ID>);

/// The executor for the [`Parent`] component.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &TrackedEngine,
        key: &Key,
    ) -> Result<Option<symbol::ID>, pernixc_query::runtime::executor::CyclicError>
    {
        if key.0.id == symbol::ID::ROOT_MODULE {
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

// Intermediate query to compute the [`Parent`] component for all symbols
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
pub struct Intermediate(pub HashMap<symbol::ID, symbol::ID>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[doc(hidden)]
pub struct IntermediateExecutor;

impl pernixc_query::runtime::executor::Executor<IntermediateKey>
    for IntermediateExecutor
{
    fn execute(
        &self,
        engine: &TrackedEngine,
        key: &IntermediateKey,
    ) -> Result<Arc<Intermediate>, pernixc_query::runtime::executor::CyclicError>
    {
        let mut result = HashMap::default();
        let target = engine.get_target(key.0);

        for (symbol, member) in target
            .all_symbol_ids
            .iter()
            .copied()
            .map(|x| key.0.make_global(x))
            .filter_map(|x| engine.try_get_members(x).map(|y| (x, y)))
        {
            for member_id in member
                .member_ids_by_name
                .values()
                .copied()
                .chain(member.redefinitions.iter().copied())
            {
                result.insert(member_id, symbol.id);
            }
        }

        Ok(Arc::new(Intermediate(result)))
    }
}

/// Represents an iterator that walks through the scope of the given symbol. It
/// goes through all the parent symbols until it reaches the root.
///
/// The iterator iterates through the scope in id-to-parent order including the
/// initial symbol.
#[derive(Debug, Clone)]
pub struct ScopeWalker<'a> {
    engine: &'a TrackedEngine<'a>,
    current_id: Option<symbol::ID>,
    target_id: TargetID,
}

impl Iterator for ScopeWalker<'_> {
    type Item = symbol::ID;

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
pub fn scope_walker(
    self: &TrackedEngine<'_>,
    id: Global<symbol::ID>,
) -> ScopeWalker {
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
    first: symbol::ID,
    second: symbol::ID,
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
    mut id: Global<symbol::ID>,
) -> symbol::ID {
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
