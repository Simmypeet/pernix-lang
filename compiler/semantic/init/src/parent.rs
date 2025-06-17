//! Contains the definition of the [`Parent`] component.

use derive_more::{Deref, DerefMut};
use extend::ext;
use pernixc_query::{Engine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_target::{Global, TargetID};

use crate::{
    kind::{Ext as _, Kind},
    symbol, HierarchyRelationship,
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
)]
#[id(Global<symbol::ID>)]
pub struct Parent(pub Option<symbol::ID>);

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
    #[must_use]
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
    #[must_use]
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
