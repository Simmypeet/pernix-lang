//! Contains the definition of the [`Parent`] component.

use std::sync::Arc;

use derive_more::{Deref, DerefMut};
use extend::ext;
use pernixc_hash::HashMap;
use pernixc_query::{Engine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::{Global, TargetID};

use crate::{
    kind::{Ext as _, Kind},
    member::Ext as _,
    symbol,
    target::Ext as _,
    HierarchyRelationship,
};

/// A component specifying the parent of a symbol in the hierarchy.
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
    Value,
    StableHash,
)]
#[id(Global<symbol::ID>)]
pub struct Parent(pub Option<symbol::ID>);

/// The executor for the [`Parent`] component.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &Engine,
        key: Key,
    ) -> Result<
        <Key as pernixc_query::Key>::Value,
        pernixc_query::runtime::executor::CyclicError,
    > {
        if key.0.id == symbol::ID::ROOT_MODULE {
            return Ok(Parent(None));
        }

        let intermediate = engine
            .query(&IntermediateKey(key.0.target_id))
            .expect("should have no cyclic dependencies");

        let parent_id = intermediate
            .get(&key.0.id)
            .copied()
            .expect("this symbol doesn't have a parent");

        Ok(Parent(Some(parent_id)))
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
        engine: &Engine,
        key: IntermediateKey,
    ) -> Result<
        <IntermediateKey as pernixc_query::Key>::Value,
        pernixc_query::runtime::executor::CyclicError,
    > {
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
    engine: &'a Engine,
    current_id: Option<symbol::ID>,
    target_id: TargetID,
}

impl Iterator for ScopeWalker<'_> {
    type Item = symbol::ID;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current_id {
            Some(current_id) => {
                let next = *self
                    .engine
                    .get_parent(self.target_id.make_global(current_id));

                self.current_id = next;
                Some(current_id)
            }
            None => None,
        }
    }
}

/// An extension trait for [`Engine`] related to symbol hierarchy and
/// relationships.
#[ext(name = Ext)]
pub impl Engine {
    /// Retrieves the [`Parent`] of this symbol.
    fn get_parent(&self, id: Global<symbol::ID>) -> Parent {
        self.query(&Key(id)).expect("should have no cyclic dependencies")
    }

    /// Gets the [`ScopeWalker`] that walks through the scope hierarchy of the
    /// given [`GlobalID`].
    ///
    /// See [`ScopeWalker`] for more information.
    fn scope_walker(&self, id: Global<symbol::ID>) -> ScopeWalker {
        ScopeWalker {
            engine: self,
            current_id: Some(id.id),
            target_id: id.target_id,
        }
    }

    /// Computes the [`HierarchyRelationship`] between the two given item IDs.
    ///
    /// The returned [`HierarchyRelationship`] is based on the `first` symbol.
    fn symbol_hierarchy_relationship(
        &self,
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
    fn get_closest_module_id(&self, mut id: Global<symbol::ID>) -> symbol::ID {
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
}
