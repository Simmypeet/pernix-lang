//! Contains the implementation of the lexical analysis executor.

use pernixc_handler::Storage;
use pernixc_lexical::{DiagnosticKey, Key, error, tree::Tree};
use pernixc_qbice::{Config, TrackedEngine};
use pernixc_source_file::calculate_path_id;
use qbice::{executor, storage::intern::Interned};

/// An executor that parses a token tree from the given source file path.
#[executor(config = Config)]
#[allow(clippy::type_complexity)]
pub async fn parse_executor(
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
            engine.calculate_path_id(&key.path, key.target_id).await.expect(
                "should be sucessful since the source file is loaded \
                 successfully",
            ),
        ),
        engine,
        &storage,
    );

    Ok((engine.intern(tree), engine.intern_unsized(storage.into_vec())))
}

/// An executor that retrieves the diagnostics that occurred while parsing the
/// token tree from the source file.
#[executor(config = Config)]
pub async fn diagnostic_executor(
    DiagnosticKey(key): &DiagnosticKey,
    engine: &TrackedEngine,
) -> Result<Interned<[error::Error]>, pernixc_source_file::Error> {
    let token_tree = engine.query(key).await?;

    Ok(token_tree.1)
}
