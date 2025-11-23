//! This crate implements the lexical analysis phase of the compiler.

use std::{path::Path, sync::Arc};

use pernixc_handler::Storage;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::calculate_path_id;
use pernixc_stable_hash::StableHash;
use pernixc_target::TargetID;

pub mod error;
pub mod kind;
pub mod token;
pub mod tree;

/// Query for parsing a token tree from the given source file path.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(Result<(Arc<tree::Tree>, Arc<[error::Error]>), pernixc_source_file::Error>)]
pub struct Key {
    /// The path to load the source file.
    pub path: Arc<Path>,

    /// The target ID that requested the source file parsing.
    pub target_id: TargetID,
}

pernixc_register::register!(Key, Executor);

#[pernixc_query::executor(key(Key), name(Executor))]
#[allow(clippy::type_complexity)]
pub async fn parse_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Result<
    Result<(Arc<tree::Tree>, Arc<[error::Error]>), pernixc_source_file::Error>,
    CyclicError,
> {
    // load the source file
    let source_file = match engine
        .query(&pernixc_source_file::Key {
            path: key.path.clone(),
            target_id: key.target_id,
        })
        .await?
    {
        Ok(source_code) => source_code,
        Err(error) => return Ok(Err(error)),
    };

    let storage = Storage::<error::Error>::default();
    let tree = tree::Tree::from_source(
        source_file.content(),
        key.target_id.make_global(
            engine.calculate_path_id(&key.path, key.target_id).await.expect(
                "should be sucessful since the source file is loaded \
                 successfully",
            ),
        ),
        &storage,
    );

    Ok(Ok((Arc::new(tree), Arc::from(storage.into_vec()))))
}

/// A key for retrieving the diagnostics that occurred while parsing the token
/// tree from the source file.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(Result<Arc<[error::Error]>, pernixc_source_file::Error>)]
pub struct DiagnosticKey(pub Key);

pernixc_register::register!(DiagnosticKey, DiagnosticExecutor);

#[pernixc_query::executor(key(DiagnosticKey), name(DiagnosticExecutor))]
#[allow(clippy::type_complexity)]
pub async fn diagnostic_executor(
    DiagnosticKey(key): &DiagnosticKey,
    engine: &TrackedEngine,
) -> Result<Result<Arc<[error::Error]>, pernixc_source_file::Error>, CyclicError>
{
    let token_tree = match engine.query(key).await? {
        Ok(token_tree) => token_tree,
        Err(error) => return Ok(Err(error)),
    };

    Ok(Ok(token_tree.1))
}
