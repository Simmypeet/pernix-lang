//! Contains the definition of the [`Parent`] component.

use std::sync::Arc;

use derive_more::{Deref, DerefMut};
use pernixc_extend::extend;
use pernixc_hash::HashMap;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::{Global, TargetID};

use crate::{
    get_table_of_symbol, get_target_root_module_id,
    kind::{get_kind, Kind},
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
    engine: &'a TrackedEngine,
    current_id: Option<ID>,
    target_id: TargetID,
}

impl ScopeWalker<'_> {
    /// Iterates through the scope of the given symbol.
    pub async fn next(&mut self) -> Option<ID> {
        match self.current_id {
            Some(current_id) => {
                let next = self
                    .engine
                    .get_parent(self.target_id.make_global(current_id))
                    .await;

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
pub fn scope_walker(self: &TrackedEngine, id: Global<ID>) -> ScopeWalker {
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
pub async fn symbol_hierarchy_relationship(
    self: &TrackedEngine,
    target_id: TargetID,
    first: ID,
    second: ID,
) -> HierarchyRelationship {
    // the two symbols are the same.
    if first == second {
        return HierarchyRelationship::Equivalent;
    }

    let mut scope_walker = self.scope_walker(Global::new(target_id, first));
    while let Some(first_parent) = scope_walker.next().await {
        if first_parent == second {
            return HierarchyRelationship::Child;
        }
    }

    let mut second_scope_walker =
        self.scope_walker(Global::new(target_id, second));
    while let Some(second_parent) = second_scope_walker.next().await {
        if second_parent == first {
            return HierarchyRelationship::Parent;
        }
    }

    HierarchyRelationship::Unrelated
}

/// Returns the [`symbol::ID`] that is the module and closest to the given
/// [`Global<symbol::ID>`] (including itself).
#[extend]
pub async fn get_closest_module_id(
    self: &TrackedEngine,
    mut id: Global<ID>,
) -> ID {
    loop {
        if self.get_kind(id).await == Kind::Module {
            return id.id;
        }

        id = Global::new(
            id.target_id,
            self.get_parent(id).await.expect("should always have a parent "),
        );
    }
}

/// The executor for the [`Parent`] component.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    async fn execute(
        &self,
        engine: &TrackedEngine,
        key: &Key,
    ) -> Result<Option<ID>, pernixc_query::runtime::executor::CyclicError> {
        if key.0.id == engine.get_target_root_module_id(key.0.target_id).await {
            return Ok(None);
        }

        let intermediate = engine
            .query(&IntermediateKey(key.0.target_id))
            .await
            .expect("should have no cyclic dependencies");

        let parent_id = intermediate.get(&key.0.id).copied().unwrap();

        Ok(Some(parent_id))
    }
}

#[pernixc_query::query(
    key(IntermediateKey),
    id(TargetID),
    value(Arc<HashMap<ID, ID>>),
    executor(IntermediateExecutor),
)]
pub async fn intermediate_executor(
    target_id: TargetID,
    engine: &TrackedEngine,
) -> Result<Arc<HashMap<ID, ID>>, pernixc_query::runtime::executor::CyclicError>
{
    let map = engine.query(&crate::MapKey(target_id)).await?;

    let mut key_and_members = Vec::new();

    for (symbol, _) in map.keys_by_symbol_id.iter() {
        let engine = engine.clone();
        let symbol = *symbol;

        key_and_members.push(tokio::spawn(async move {
            let table =
                engine.get_table_of_symbol(target_id.make_global(symbol)).await;

            table.members.get(&symbol).map(|members| {
                (
                    symbol,
                    members
                        .member_ids_by_name
                        .values()
                        .copied()
                        .chain(members.redefinitions.iter().copied())
                        .collect::<Vec<_>>(),
                )
            })
        }));
    }

    let key_and_members: Vec<(ID, Vec<ID>)> =
        futures::future::join_all(key_and_members)
            .await
            .into_iter()
            .filter_map(|x| x.unwrap())
            .collect();

    let mut parent_map = HashMap::default();

    for (symbol, members) in key_and_members {
        for member in members {
            assert!(parent_map.insert(member, symbol).is_none());
        }
    }

    Ok(Arc::new(parent_map))
}
