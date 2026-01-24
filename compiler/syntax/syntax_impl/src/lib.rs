//! Implements the syntax parsing executor.

use linkme::distributed_slice;
use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_syntax::{DiagnosticKey, Key, item::module};
use qbice::{executor, program::Registration, storage::intern::Interned};

#[executor(config = Config)]
#[allow(clippy::type_complexity)]
async fn parse_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Result<
    (Option<module::Content>, Interned<[pernixc_parser::error::Error]>),
    pernixc_source_file::Error,
> {
    // load the token tree
    let token_tree = engine
        .query(&pernixc_lexical::Key {
            path: key.path.clone(),
            target_id: key.target_id,
        })
        .await?;

    let (module, errors) = module::Content::parse(&token_tree.0, engine);

    Ok((module, engine.intern_unsized(errors)))
}

#[distributed_slice(PERNIX_PROGRAM)]
static PARSE_EXECUTOR: Registration<Config> =
    Registration::new::<Key, ParseExecutor>();

#[executor(config = Config)]
#[allow(clippy::type_complexity)]
async fn diagnostic_executor(
    DiagnosticKey(key): &DiagnosticKey,
    engine: &TrackedEngine,
) -> Result<Interned<[pernixc_parser::error::Error]>, pernixc_source_file::Error>
{
    let token_tree = engine.query(key).await?;

    Ok(token_tree.1)
}

#[distributed_slice(PERNIX_PROGRAM)]
static DIAGNOSTIC_EXECUTOR: Registration<Config> =
    Registration::new::<DiagnosticKey, DiagnosticExecutor>();
