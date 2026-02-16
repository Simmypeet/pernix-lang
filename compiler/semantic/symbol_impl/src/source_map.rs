//! Contains the definition of the [`SourceMap`] type, which is a collection
//! of source files that can be used when reporting diagnostics.
use std::{collections::HashMap, path::Path};

use linkme::distributed_slice;
use pernixc_extend::extend;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_source_file::{FilePathKey, GlobalSourceID, SourceFile};
use pernixc_target::{Global, TargetID};
use qbice::{executor, program::Registration, storage::intern::Interned};

use crate::table::MapKey;

#[executor(config = Config)]
async fn file_path_executor(
    key: &FilePathKey,
    engine: &TrackedEngine,
) -> Interned<Path> {
    let table = engine.query(&MapKey(key.id.target_id)).await;

    table.paths_by_source_id.get(&key.id.id).map_or_else(
        || panic!("Source file path not found for ID: {:?}", key.id),
        |x| x.0.clone(),
    )
}

#[distributed_slice(PERNIX_PROGRAM)]
static FILE_PATH_EXECUTOR: Registration<Config> =
    Registration::new::<FilePathKey, FilePathExecutor>();

/// A collection of source files that can be accessed by their global IDs.
///
/// This is used when reporting diagnostics to retrieve source file content.
#[derive(Debug, Clone)]
pub struct SourceMap(pub HashMap<GlobalSourceID, SourceFile>);

/// Creates a new [`SourceMap`] that will allow diagnostics to
/// retrieve source files by their IDs.
#[extend]
pub async fn create_source_map(
    self: &TrackedEngine,
    target_id: TargetID,
) -> SourceMap {
    let map = self.query(&MapKey(target_id)).await;
    let mut source_files = HashMap::default();

    for (id, source_file) in map.paths_by_source_id.iter() {
        let Ok(source_file) = self
            .query(&pernixc_source_file::Key {
                path: source_file.0.clone(),
                target_id,
            })
            .await
        else {
            continue;
        };

        source_files.insert(Global::new(target_id, *id), source_file);
    }

    SourceMap(source_files)
}
