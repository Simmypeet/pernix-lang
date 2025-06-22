//! Crate responsible for starting and setting up the query engine for the
//! compilation process.
use std::{fs::File, io::BufReader, path::Path, sync::Arc};

use flexstr::SharedStr;
use parking_lot::lock_api::RwLock;
use pernixc_handler::Storage;
use pernixc_hash::{DashMap, HashSet};
use pernixc_query::{
    database::Database,
    runtime::{
        serde::{DynamicDeserialize, DynamicRegistry},
        Runtime,
    },
    Engine,
};
use pernixc_serialize::{
    binary::de::BinaryDeserializer, de::Deserializer, ser::Serializer,
    Deserialize,
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
    Ext: DynamicDeserialize<BinaryDeserializer<BufReader<File>>>,
>(
    source_map: &mut SourceMap,
    root_source_id: GlobalSourceID,
    target_name: SharedStr,
    _library_paths: impl IntoIterator<Item = &'l Path>,
    token_trees_by_source_id: &DashMap<
        GlobalSourceID,
        pernixc_lexical::tree::Tree,
    >,
    incremental_path: Option<(&Path, &mut Ext)>,
) -> Result<(Engine, Vec<tree::Error>, Vec<Diagnostic>), Error> {
    // load the incremental file (if any)
    let incremental_file_de =
        if let Some((incremental_path, extension)) = incremental_path {
            match std::fs::File::open(incremental_path) {
                Ok(file) => Some((file, extension)),
                Err(err) => {
                    if err.kind() == std::io::ErrorKind::NotFound {
                        None
                    } else {
                        return Err(Error::OpenIncrementalFileIO(err));
                    }
                }
            }
        } else {
            None
        };

    // create a fresh database or load the incremental file
    let database = incremental_file_de.map_or_else(
        || Ok(Database::default()),
        |(file, ext)| {
            let buf_reader = BufReader::new(file);
            let mut binary_deserializer = BinaryDeserializer::new(buf_reader);

            Database::deserialize(&mut binary_deserializer, ext)
                .map_err(Error::ReadIncrementalFileIO)
        },
    )?;

    let (tree, errors) =
        tree::parse(root_source_id, source_map, token_trees_by_source_id);

    let generated_ids_rw = RwLock::new(HashSet::default());
    let engine = RwLock::new(Engine { database, runtime: bootstrap_runtime() });
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

fn bootstrap_runtime() -> Runtime {
    let mut runtime = Runtime::default();

    runtime.executor.register(Arc::new(parent::IntermediateExecutor));
    runtime.executor.register(Arc::new(parent::Executor));

    runtime
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
}
