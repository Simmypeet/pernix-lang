//! Crate responsible for starting and setting up the query engine for the
//! compilation process.
use std::sync::Arc;

use pernixc_query::{
    runtime::{
        executor,
        persistence::{
            serde::{DynamicDeserialize, DynamicRegistry, DynamicSerialize},
            Persistence, Reader, Writer,
        },
    },
    Engine,
};
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    de::Deserializer,
    ser::Serializer,
};

use crate::arguments::Arguments;

pub mod accessibility;
pub mod arguments;
pub mod diagnostic;
pub mod implemented;
pub mod implements;
pub mod import;
pub mod kind;
pub mod member;
pub mod module_tree;
pub mod name;
pub mod parent;
pub mod source_file;
pub mod span;
pub mod symbol;
pub mod syntax;
pub mod syntax_tree;
pub mod target;
pub mod token_tree;

// mod build;

/// A fatal error that aborts the compilation process.
#[derive(Debug, thiserror::Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    SetupPersistence(std::io::Error),

    #[error(transparent)]
    LoadIncrementalDatabase(std::io::Error),
}

/// Describes the relationship between two symbols in the hierarchy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HierarchyRelationship {
    /// The first symbol is the parent of the second symbol.
    Parent,

    /// The first symbol is the child of the second symbol.
    Child,

    /// Both symbols are two equivalent symbols.
    Equivalent,

    /// Both symbols are defined in different hierarchy scope.
    Unrelated,
}

/// Setup the query database for the compilation process.
#[allow(clippy::type_complexity)]
pub fn bootstrap<
    Registry: DynamicRegistry<BinarySerializer<Writer>, BinaryDeserializer<Reader>>
        + DynamicSerialize<BinarySerializer<Writer>>
        + DynamicDeserialize<BinaryDeserializer<Reader>>
        + Send
        + Sync
        + 'static,
>(
    arguments: Arguments,
    serder_registry: Arc<Registry>,
) -> Result<Engine, Error> {
    let mut engine = Engine::default();

    if let Some(incremental_path) = &arguments.command.input().incremental_path
    {
        let mut persistence =
            Persistence::new(incremental_path.clone(), serder_registry)
                .map_err(Error::SetupPersistence)?;

        engine.database = persistence
            .load_database()
            .map_err(Error::LoadIncrementalDatabase)?;

        skip_persistence(&mut persistence);

        engine.runtime.persistence = Some(persistence);
    }

    // register all the required executor
    bootstrap_executor(&mut engine.runtime.executor);

    engine.input_session(|x| {
        // always re-verify the inputs for incremental database
        x.always_reverify();

        // set the arguments as the initial input
        x.set_input(
            arguments::Key(pernixc_target::TargetID::Local),
            Arc::new(arguments),
        );
    });

    Ok(engine)
}

fn bootstrap_executor(executor: &mut executor::Registry) {
    executor.register(Arc::new(source_file::Executor));
    executor.register(Arc::new(token_tree::Executor));
    executor.register(Arc::new(syntax_tree::Executor));
    executor.register(Arc::new(module_tree::Executor));
    // executor.register(Arc::new(parent::IntermediateExecutor));
    // executor.register(Arc::new(parent::Executor));
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
    serde_registry.register::<arguments::Key>();
    serde_registry.register::<source_file::Key>();
    serde_registry.register::<token_tree::Key>();
    serde_registry.register::<syntax_tree::Key>();

    // serde_registry.register::<accessibility::Key>();
    // serde_registry.register::<implemented::Key>();
    // serde_registry.register::<implements::Key>();
    // serde_registry.register::<kind::Key>();
    // serde_registry.register::<member::Key>();
    // serde_registry.register::<name::Key>();
    // serde_registry.register::<parent::Key>();
    // serde_registry.register::<parent::IntermediateKey>();
    // serde_registry.register::<target::Key>();
    // serde_registry.register::<target::MapKey>();
    // serde_registry.register::<span::Key>();
    // serde_registry.register::<import::Key>();

    // serde_registry.register::<syntax::FunctionSignatureKey>();
    // serde_registry.register::<syntax::GenericParametersKey>();
    // serde_registry.register::<syntax::WhereClauseKey>();
    // serde_registry.register::<syntax::TypeAliasKey>();
    // serde_registry.
    // register::<syntax::ImplementationQualifiedIdentifierKey>();
    // serde_registry.register::<syntax::StatementsKey>();
    // serde_registry.register::<syntax::FieldsKey>();
}

/// Registers the keys that should be skipped during serialization and
/// deserialization in the query engine's persistence layer
pub fn skip_persistence(persistence: &mut Persistence) {
    persistence.skip_cache_value::<arguments::Key>();
    persistence.skip_cache_value::<source_file::Key>();
    persistence.skip_cache_value::<token_tree::Key>();
    persistence.skip_cache_value::<module_tree::Key>();

    // persistence.skip_cache_value::<syntax::FunctionSignatureKey>();
    // persistence.skip_cache_value::<syntax::GenericParametersKey>();
    // persistence.skip_cache_value::<syntax::WhereClauseKey>();
    // persistence.skip_cache_value::<syntax::TypeAliasKey>();
    // persistence
    //     .skip_cache_value::<syntax::ImplementationQualifiedIdentifierKey>();
    // persistence.skip_cache_value::<syntax::StatementsKey>();
    // persistence.skip_cache_value::<syntax::FieldsKey>();
}
