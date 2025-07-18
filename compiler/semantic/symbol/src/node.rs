//! Contains the implementation of building the symbol table from the file tree.

use std::{path::Path, sync::Arc};

use flexstr::SharedStr;
use pernixc_query::TrackedEngine;
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
pub enum Key {
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
    pernixc_query::Key,
)]
#[value(Result<Arc<Table>, pernixc_file_tree::load::Error>)]
pub struct TableKey(pub Key);

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
pub struct Table {}

#[pernixc_query::executor(key(TableKey), name(TableExecutor))]
pub fn table_executor(
    key: &TableKey,
    engine: &TrackedEngine,
) -> Result<
    Result<Arc<Table>, pernixc_file_tree::load::Error>,
    pernixc_query::runtime::executor::CyclicError,
> {
    todo!()
}
