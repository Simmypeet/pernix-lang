//! This crate implements the lexical analysis phase of the compiler.

use std::{path::Path, sync::Arc};

use pernixc_handler::Storage;
use pernixc_query::{
    runtime::{
        executor::CyclicError,
        persistence::{serde::DynamicRegistry, Persistence},
    },
    TrackedEngine,
};
use pernixc_serialize::{
    de::Deserializer, ser::Serializer, Deserialize, Serialize,
};
use pernixc_source_file::calculate_path_id;
use pernixc_stable_hash::StableHash;
use pernixc_target::TargetID;

pub mod error;
pub mod kind;
pub mod token;
pub mod tree;

/// Registers all the required executors to run the queries.
pub fn register_executors(
    executor: &mut pernixc_query::runtime::executor::Registry,
) {
    executor.register(Arc::new(Executor));
    executor.register(Arc::new(DiagnosticExecutor));
}

/// Registers all the necessary runtime information for the query engine.
pub fn register_serde<
    S: Serializer<Registry>,
    D: Deserializer<Registry>,
    Registry: DynamicRegistry<S, D> + Send + Sync,
>(
    serde_registry: &mut Registry,
) where
    S::Error: Send + Sync,
{
    serde_registry.register::<Key>();
    serde_registry.register::<DiagnosticKey>();
}

/// Registers the keys that should be skipped during serialization and
/// deserialization in the query engine's persistence layer
pub const fn skip_persistence(_persistence: &mut Persistence) {}

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
            engine.calculate_path_id(&key.path, key.target_id).await,
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
