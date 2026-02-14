//! Contains the implementation of the source file loading executor.

use std::{hash::Hash, path::Path, sync::Arc};

use linkme::distributed_slice;
use pernixc_qbice::{Config, InputSession, PERNIX_PROGRAM, TrackedEngine};
use pernixc_source_file::{Error, LocalSourceID, SourceFile};
use pernixc_target::{get_invocation_arguments, get_target_seed};
use qbice::{
    Decode, Encode, ExecutionStyle, Query, StableHash, executor,
    program::Registration, storage::intern::Interned,
};
use siphasher::sip128::Hasher128;

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

/// A query for canonicalizing a path.
///
/// This query is considered to be impure, so its executor implementation will
/// be marked with [`ExecutionStyle::ExternalInput`].
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Query,
)]
#[value(Result<Interned<Path>, Interned<str>>)]
struct CanonicalizePathKey {
    /// The path to calculate the stable ID for.
    ///
    /// This can be either a relative or absolute path. The path will be
    /// canonicalized before computing the stable ID.
    pub path: Interned<Path>,
}

#[executor(config = Config, style = ExecutionStyle::ExternalInput)]
#[allow(clippy::unused_async)]
async fn canonicalize_path_executor(
    key: &CanonicalizePathKey,
    engine: &TrackedEngine,
) -> Result<Interned<Path>, Interned<str>> {
    match std::fs::canonicalize(key.path.as_ref()) {
        Ok(canonical_path) => Ok(engine.intern_unsized(canonical_path)),

        Err(err) => Err(engine.intern_unsized(format!(
            "failed to canonicalize path '{}': {}",
            key.path.as_ref().display(),
            err
        ))),
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static CANONICALIZE_PATH_EXECUTOR: Registration<Config> =
    Registration::new::<CanonicalizePathKey, CanonicalizePathExecutor>();

/// An executor that computes a stable 128-bit file ID from a path.
///
/// The path is first canonicalized to resolve relative components and symbolic
/// links, ensuring that the same physical file always produces the same ID
/// regardless of how the path is specified.
#[executor(config = Config)]
#[allow(clippy::unused_async)]
pub async fn stable_path_id_executor(
    key: &pernixc_source_file::StablePathIDKey,
    engine: &TrackedEngine,
) -> Result<LocalSourceID, qbice::storage::intern::Interned<str>> {
    let mut hasher = siphasher::sip128::SipHasher24::default();

    let this_canonical_path =
        engine.query(&CanonicalizePathKey { path: key.path.clone() }).await?;

    let target_args = engine.get_invocation_arguments(key.target_id).await;

    let target_root_file = engine
        .query(&CanonicalizePathKey {
            path: engine
                .intern_unsized(target_args.command.input().file.clone()),
        })
        .await?;

    let target_directory =
        target_root_file.parent().unwrap_or_else(|| std::path::Path::new(""));

    // skip the prefix components to avoid absolute path issues (e.g. \\?\ on
    // Windows)
    let this_components =
        this_canonical_path.components().skip(1).collect::<Vec<_>>();
    let target_components =
        target_directory.components().skip(1).collect::<Vec<_>>();

    if target_components.len() > this_components.len() {
        return Err(engine
            .intern_unsized("the given path is not in the target directory"));
    }

    for (a, b) in target_components.iter().zip(this_components.iter()) {
        if a != b {
            return Err(engine.intern_unsized(
                "the given path is not in the target directory",
            ));
        }
    }

    // hash only the relative path from the target directory
    for component in this_components.iter().skip(target_components.len()) {
        component.as_os_str().hash(&mut hasher);
    }

    let target_seed = engine.get_target_seed(key.target_id).await;
    target_seed.hash(&mut hasher);

    let finish = hasher.finish128();

    Ok(LocalSourceID::new(finish.h1, finish.h2))
}

#[distributed_slice(PERNIX_PROGRAM)]
static STABLE_PATH_ID_EXECUTOR: Registration<Config> = Registration::new::<
    pernixc_source_file::StablePathIDKey,
    StablePathIdExecutor,
>();

/// A dummy function to make sure this crate is linked by the compiler.
pub const fn black_box() {}

/// Refreshes the source file executors in the given input session.
pub async fn refresh_source_file_executors(input_session: &mut InputSession) {
    input_session.refresh::<pernixc_source_file::Key>().await;
    input_session.refresh::<CanonicalizePathKey>().await;
}
