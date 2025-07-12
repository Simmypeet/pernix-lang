//! Contains the definition of all syntax-related queries

use std::sync::Arc;

use pernixc_query::{
    executor, runtime::executor::CyclicError, Key, TrackedEngine,
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::ID;

/// The query for retrieving a list of import syntax items for a module.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    Key,
)]
#[value(Arc<[pernixc_syntax::item::module::Import]>)]
#[extend(method(get_module_imports_syntax), no_cyclic)]
pub struct ImportKey(pub Global<ID>);

/// Implementation of the `get_module_imports_syntax` method
#[executor(key(ImportKey), name(ImportExecutor))]
pub fn import_syntax_executor(
    key: &ImportKey,
    engine: &TrackedEngine,
) -> Result<Arc<[pernixc_syntax::item::module::Import]>, CyclicError> {
    let symbol_table = engine.query(&crate::Key(key.0.target_id))?;
    let entry =
        symbol_table.entries_by_id.get(&key.0.id).unwrap_or_else(|| {
            panic!("No symbol table entry found for ID: {:?}", key.0.id)
        });

    Ok(entry.imports.clone().unwrap_or_else(|| {
        panic!("No imports found for ID: {:?} in symbol table", key.0.id)
    }))
}
