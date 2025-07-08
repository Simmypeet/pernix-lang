//! Contains the definition of the [`Target`] component.

use std::sync::Arc;

use derive_more::{Deref, DerefMut};
use flexstr::SharedStr;
use pernixc_extend::extend;
use pernixc_hash::{HashMap, HashSet};
use pernixc_query::{TrackedEngine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::TargetID;

use crate::symbol;

/// Stores the information about a target
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    Value,
    StableHash,
)]
#[id(TargetID)]
#[value(Arc<Target>)]
#[extend(method(get_target), no_cyclic)]
pub struct Target {
    /// All the symbol IDs defined in this target.
    pub all_symbol_ids: HashSet<symbol::ID>,

    /// The target IDs linked to this target.
    pub linked_targets: HashSet<TargetID>,
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
    StableHash,
)]
#[id(())]
#[key(MapKey)]
#[value(Arc<Map>)]
pub struct Map(pub HashMap<SharedStr, TargetID>);

/// Gets the map from the name of the target to its ID.
#[extend]
pub fn get_target_map(self: &TrackedEngine<'_>) -> Arc<Map> {
    self.query(&MapKey(())).expect("should have no cyclic dependencies")
}
