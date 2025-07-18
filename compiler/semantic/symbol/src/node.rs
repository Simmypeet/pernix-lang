//! Contains the implementation of building the symbol table from the file tree.

use std::{path::Path, sync::Arc};

use flexstr::SharedStr;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::{get_invocation_arguments, TargetID};

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

enum ModuleKind {}

#[pernixc_query::executor(key(TableKey), name(TableExecutor))]
pub fn table_executor(
    TableKey(key): &TableKey,
    engine: &TrackedEngine,
) -> Result<
    Result<Arc<Table>, pernixc_file_tree::load::Error>,
    pernixc_query::runtime::executor::CyclicError,
> {
    match key {
        Key::Root(target_id) => {
            let invocation_arguments =
                engine.get_invocation_arguments(*target_id);

            engine.query(&pernixc_file_tree::syntax_tree::Key {
                path: invocation_arguments.command.input().file.clone().into(),
                target_id: *target_id,
            });

            todo!()
        }

        Key::Submodule { external_submodule, target_id } => todo!(),
    };
}
