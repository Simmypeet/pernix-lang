//! Contains the implementation of the source file loading executor.

use std::sync::Arc;

use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_source_file::{Error, SourceFile};
use qbice::{ExecutionStyle, executor, program::Registration};

/// An implementation of an executor that loads a source file from the file
/// system.
#[executor(config = Config, style = ExecutionStyle::ExternalInput)]
#[allow(clippy::unused_async)]
pub async fn load_source_file_executor(
    key: &pernixc_source_file::Key,
    engine: &TrackedEngine,
) -> Result<Arc<SourceFile>, Error> {
    std::fs::File::open(key.path.as_ref())
        .and_then(|file| {
            Ok(Arc::new(SourceFile::load(file, key.path.to_path_buf())?))
        })
        .map_err(|x| Error(engine.intern_unsized(x.to_string())))
}

#[distributed_slice(PERNIX_PROGRAM)]
static LOAD_SOURCE_FILE_EXECUTOR: Registration<Config> =
    Registration::new::<pernixc_source_file::Key, LoadSourceFileExecutor>();
