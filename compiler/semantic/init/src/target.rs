//! Contains the definition of the [`Target`] component.

use std::sync::Arc;

use derive_more::{Deref, DerefMut};
use extend::ext;
use fnv::{FnvHashMap, FnvHashSet};
use pernixc_query::{Engine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_target::TargetID;

use crate::symbol;

/// Stores the information about a target
#[derive(
    Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize, Value,
)]
#[id(TargetID)]
#[value(Arc<Target>)]
pub struct Target {
    /// All the symbol IDs defined in this target.
    pub all_symbol_ids: FnvHashSet<symbol::ID>,

    /// The target IDs linked to this target.
    pub linked_targets: FnvHashSet<TargetID>,
}

/// Contains all the targets and their names currently known to the engine.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
    Value,
)]
#[id(())]
#[value(Arc<Map>)]
#[key(TargetMap)]
pub struct Map(pub FnvHashMap<String, TargetID>);

/// The extension trait for the [`Engine`] to provide access to targets.
#[ext(name = Ext)]
pub impl Engine {
    /// Returns the target associated with the given ID.
    fn get_target(&self, id: TargetID) -> Arc<Target> {
        self.query(&Key(id)).expect("should have no cyclic dependencies")
    }

    /// Returns the global target map
    fn get_target_map(&self) -> Arc<Map> {
        self.query(&TargetMap(())).expect("should have no cyclic dependencies")
    }
}
