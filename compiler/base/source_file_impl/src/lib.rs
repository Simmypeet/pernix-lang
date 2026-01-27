//! Contains the implementation of the source file loading executor.

use std::{hash::Hash, sync::Arc};

use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_source_file::{Error, LocalSourceID, SourceFile};
use qbice::{ExecutionStyle, executor, program::Registration};
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

/// The fixed seed used for computing stable path IDs.
///
/// This seed is chosen to be a random 128-bit value that is unlikely to collide
/// with other uses of `SipHash` in the codebase. It ensures that path hashing
/// is deterministic across runs and platforms (given the same canonicalized
/// path).
const STABLE_PATH_ID_SEED: u128 = 0x7a3b_8f2c_d1e4_5609_b2a7_c3d8_e9f0_1234;

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
    // Canonicalize the path to ensure the same file always produces the same
    // ID, regardless of whether it was accessed via relative or absolute path.
    let canonical_path = match std::fs::canonicalize(key.path.as_ref()) {
        Ok(path) => path,
        Err(err) => {
            return Err(engine.intern_unsized(format!(
                "failed to canonicalize path '{}': {}",
                key.path.display(),
                err
            )));
        }
    };

    // Use `SipHash-2-4` with a fixed seed to produce a stable 128-bit hash.
    // The seed ensures deterministic hashing across runs.
    let mut hasher = siphasher::sip128::SipHasher24::new_with_key(
        &STABLE_PATH_ID_SEED.to_le_bytes(),
    );

    // Hash the canonicalized path bytes (using the platform-specific
    // representation). On Unix, this is the raw bytes. On Windows, this
    // is the UTF-16 encoded string.
    //
    // We use `as_os_str().as_encoded_bytes()` to get a consistent
    // byte representation that works across platforms.
    canonical_path.as_os_str().as_encoded_bytes().hash(&mut hasher);

    let hash = hasher.finish128();

    Ok(LocalSourceID::new(hash.h1, hash.h2))
}

#[distributed_slice(PERNIX_PROGRAM)]
static STABLE_PATH_ID_EXECUTOR: Registration<Config> = Registration::new::<
    pernixc_source_file::StablePathIDKey,
    StablePathIdExecutor,
>();

/// A dummy function to make sure this crate is linked by the compiler.
pub const fn black_box() {}
