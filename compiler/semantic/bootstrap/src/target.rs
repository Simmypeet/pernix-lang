//! Contains the definition of the [`Target`] component.

use std::sync::Arc;

use derive_more::{Deref, DerefMut};
use extend::ext;
use flexstr::SharedStr;
use pernixc_hash::{HashMap, HashSet};
use pernixc_query::Value;
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
#[ext(
    method(get_target),
    unwrap("should have no cyclic dependencies"),
    trait(Ext)
)]
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
#[value(Arc<Map>)]
#[key(TargetMapKey)]
pub struct Map(pub HashMap<SharedStr, TargetID>);

/// Extension trait related to retrieving the target information.
#[ext(name = MapExt)]
pub impl pernixc_query::Engine {
    /// Gets the map from the name of the target to its ID.
    fn get_target_map(&self) -> Arc<Map> {
        self.query(&TargetMapKey(()))
            .expect("should have no cyclic dependencies")
    }
}
