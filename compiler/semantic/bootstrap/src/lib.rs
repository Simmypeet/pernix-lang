//! Crate responsible for starting and setting up the query engine for the
//! compilation process.
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use flexstr::SharedStr;
use parking_lot::lock_api::RwLock;
use pernixc_handler::Storage;
use pernixc_hash::{DashMap, HashSet};
use pernixc_query::{
    database::Database,
    runtime::{
        executor,
        persistence::{Persistence, ReadAny, WriteAny},
        serde::{DynamicDeserialize, DynamicRegistry, DynamicSerialize},
        Runtime,
    },
    Engine,
};
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    de::Deserializer,
    ser::Serializer,
};
use pernixc_source_file::{GlobalSourceID, SourceMap};
use pernixc_target::TargetID;

use crate::diagnostic::Diagnostic;

pub mod accessibility;
pub mod diagnostic;
pub mod implemented;
pub mod implements;
pub mod import;
pub mod kind;
pub mod member;
pub mod name;
pub mod parent;
pub mod span;
pub mod symbol;
pub mod syntax;
pub mod target;
pub mod tree;

mod build;

/// A fatal error that aborts the compilation process.
#[derive(Debug, thiserror::Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    OpenIncrementalFileIO(std::io::Error),
    #[error(transparent)]
    ReadIncrementalFileIO(std::io::Error),
    #[error(transparent)]
    IncrementalFileDeserialize(std::io::Error),
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
pub fn bootstrap<
    'l,
    Ext: DynamicDeserialize<BinaryDeserializer<Box<dyn ReadAny>>>
        + DynamicSerialize<BinarySerializer<Box<dyn WriteAny>>>
        + Send
        + Sync
        + 'static,
>(
    source_map: &mut SourceMap,
    root_source_id: GlobalSourceID,
    target_name: SharedStr,
    _library_paths: impl IntoIterator<Item = &'l Path>,
    token_trees_by_source_id: &DashMap<
        GlobalSourceID,
        pernixc_lexical::tree::Tree,
    >,
    incremental_path: Option<(PathBuf, Arc<Ext>)>,
) -> Result<(Engine, Vec<tree::Error>, Vec<Diagnostic>), Error> {
    let (tree, errors) =
        tree::parse(root_source_id, source_map, token_trees_by_source_id);

    let generated_ids_rw = RwLock::new(HashSet::default());
    let mut runtime = Runtime {
        executor: executor::Registry::default(),
        persistence: incremental_path.map(|(path, ext)| {
            let mut persistence = Persistence::new(path, ext);
            skip_persistence(&mut persistence);
            persistence
        }),
    };

    bootstrap_executor(&mut runtime.executor);

    let engine = RwLock::new(Engine { database: Database::default(), runtime });
    let handler = Storage::<Diagnostic>::new();

    build::create_module(
        &engine,
        &generated_ids_rw,
        target_name,
        tree,
        None,
        &handler,
    );

    let mut engine = engine.into_inner();

    engine.database.set_input(
        &target::Key(TargetID::Local),
        Arc::new(target::Target {
            all_symbol_ids: generated_ids_rw.into_inner(),
            linked_targets: HashSet::default(),
        }),
    );

    Ok((engine, errors, handler.into_vec()))
}

fn bootstrap_executor(executor: &mut executor::Registry) {
    executor.register(Arc::new(parent::IntermediateExecutor));
    executor.register(Arc::new(parent::Executor));
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
    serde_registry.register::<accessibility::Key>();
    serde_registry.register::<implemented::Key>();
    serde_registry.register::<implements::Key>();
    serde_registry.register::<kind::Key>();
    serde_registry.register::<member::Key>();
    serde_registry.register::<name::Key>();
    serde_registry.register::<parent::Key>();
    serde_registry.register::<parent::IntermediateKey>();
    serde_registry.register::<target::Key>();
    serde_registry.register::<span::Key>();
    serde_registry.register::<import::Key>();

    serde_registry.register::<syntax::FunctionSignatureKey>();
    serde_registry.register::<syntax::GenericParametersKey>();
    serde_registry.register::<syntax::WhereClauseKey>();
    serde_registry.register::<syntax::TypeAliasKey>();
    serde_registry.register::<syntax::ImplementationQualifiedIdentifierKey>();
    serde_registry.register::<syntax::StatementsKey>();
    serde_registry.register::<syntax::FieldsKey>();
}

/// Registers the keys that should be skipped during serialization and
/// deserialization in the query engine's persistence layer
pub fn skip_persistence(serde_registry: &mut Persistence) {
    serde_registry.register_skip_key::<syntax::FunctionSignatureKey>();
    serde_registry.register_skip_key::<syntax::GenericParametersKey>();
    serde_registry.register_skip_key::<syntax::WhereClauseKey>();
    serde_registry.register_skip_key::<syntax::TypeAliasKey>();
    serde_registry
        .register_skip_key::<syntax::ImplementationQualifiedIdentifierKey>();
    serde_registry.register_skip_key::<syntax::StatementsKey>();
    serde_registry.register_skip_key::<syntax::FieldsKey>();
}
