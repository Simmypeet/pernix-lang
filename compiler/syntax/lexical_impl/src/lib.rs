//! Contains the implementation of the lexical analysis executor.

use linkme::distributed_slice;
use pernixc_handler::Storage;
use pernixc_lexical::{DiagnosticKey, Key, error, tree::Tree};
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_source_file::get_stable_path_id;
use qbice::{executor, program::Registration, storage::intern::Interned};

#[executor(config = Config)]
#[allow(clippy::type_complexity)]
async fn parse_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Result<
    (Interned<Tree>, Interned<[error::Error]>),
    pernixc_source_file::Error,
> {
    // load the source file
    let source_file = engine
        .query(&pernixc_source_file::Key {
            path: key.path.clone(),
            target_id: key.target_id,
        })
        .await?;

    let storage = Storage::<error::Error>::default();
    let tree = Tree::from_source(
        source_file.content(),
        key.target_id.make_global(
            engine.get_stable_path_id(key.path.clone()).await.expect(
                "should be sucessful since the source file is loaded \
                 successfully",
            ),
        ),
        engine,
        &storage,
    );

    Ok((engine.intern(tree), engine.intern_unsized(storage.into_vec())))
}

#[distributed_slice(PERNIX_PROGRAM)]
static PARSE_EXECUTOR: Registration<Config> =
    Registration::new::<Key, ParseExecutor>();

#[executor(config = Config)]
async fn diagnostic_executor(
    DiagnosticKey(key): &DiagnosticKey,
    engine: &TrackedEngine,
) -> Result<Interned<[error::Error]>, pernixc_source_file::Error> {
    let token_tree = engine.query(key).await?;

    Ok(token_tree.1)
}

#[distributed_slice(PERNIX_PROGRAM)]
static DIAGNOSTIC_EXECUTOR: Registration<Config> =
    Registration::new::<DiagnosticKey, DiagnosticExecutor>();

/// A dummy function to make sure this crate is linked by the compiler.
pub const fn black_box() {}
