//! Contains the implementation of building the symbol table from the file tree.

use std::{path::Path, sync::Arc};

use flexstr::SharedStr;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::TargetID;

use crate::ID;

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub enum NodeKey {
    Root(TargetID),
    Submodule { external_submodule: ExternalSubmodule, target_id: TargetID },
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct ExternalSubmodule {
    pub parent_module_id: ID,
    pub submodule_id: ID,
    pub submodule_qualified_name: Arc<[SharedStr]>,
    pub path: Arc<Path>,
}
