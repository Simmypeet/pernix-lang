//! Crate responsible for starting and setting up the query engine for the
//! compilation process.
use std::{path::Path, sync::Arc};

use flexstr::SharedStr;
use parking_lot::lock_api::RwLock;
use pernixc_handler::Storage;
use pernixc_hash::{DashMap, HashSet};
use pernixc_query::{
    database::Database,
    runtime::{
        executor,
        persistence::{serde::DynamicRegistry, Persistence},
        Runtime,
    },
    Engine,
};
use pernixc_serialize::{de::Deserializer, ser::Serializer};
use pernixc_source_file::{GlobalSourceID, SourceMap};
use pernixc_target::TargetID;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

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

/// The result of the bootstrap process, containing the engine, syntax errors,
/// and bootstrapping semantic diagnostics.
#[derive(Debug)]
pub struct Bootstrap {
    /// The query engine where all the initial inputs are bootstrapped.
    pub engine: Engine,

    /// The syntax errors that were encountered during the parsing of the
    /// source code.
    pub syntax_errors: Vec<tree::Error>,

    /// The semantic diagnostics that were generated during the bootstrapping
    pub semantic_diagnostics: Vec<Box<dyn Diagnostic>>,
}

/// Setup the query database for the compilation process.
#[allow(clippy::type_complexity)]
pub fn bootstrap<'l>(
    source_map: &mut SourceMap,
    root_source_id: GlobalSourceID,
    target_name: SharedStr,
    _library_paths: impl IntoIterator<Item = &'l Path>,
    token_trees_by_source_id: &DashMap<
        GlobalSourceID,
        pernixc_lexical::tree::Tree,
    >,
    persistence: Option<Persistence>,
) -> Result<Bootstrap, Error> {
    let ((tree, syntax_errors), mut engine) = rayon::join(
        || tree::parse(root_source_id, source_map, token_trees_by_source_id),
        || {
            let mut runtime = Runtime {
                executor: executor::Registry::default(),
                persistence,
            };

            bootstrap_executor(&mut runtime.executor);

            let database = runtime
                .persistence
                .as_mut()
                .map_or_else(Database::default, |persistence| {
                    persistence.load_database().unwrap_or_default()
                });

            Engine { database, runtime }
        },
    );

    engine.database.set_input(
        &target::TargetMapKey(()),
        Arc::new(target::Map(
            std::iter::once((target_name.clone(), TargetID::Local)).collect(),
        )),
    );

    let generated_ids_rw = RwLock::new(HashSet::default());
    let engine = RwLock::new(engine);
    let handler = Storage::<Box<dyn Diagnostic>>::new();
    let imports = DashMap::default();

    let context =
        build::Context::new(&engine, &generated_ids_rw, &imports, &handler);

    context.create_module(target_name, tree, None, &[]);

    engine.write().database.set_input(
        &target::Key(TargetID::Local),
        Arc::new(target::Target {
            all_symbol_ids: generated_ids_rw.into_inner(),
            linked_targets: HashSet::default(),
        }),
    );

    imports.into_par_iter().for_each(|import| {
        build::insert_imports(
            &engine,
            TargetID::Local.make_global(import.0),
            import.1,
            &handler,
        );
    });

    Ok(Bootstrap {
        engine: engine.into_inner(),
        syntax_errors,
        semantic_diagnostics: handler.into_vec(),
    })
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
pub fn skip_persistence(persistence: &mut Persistence) {
    persistence.register_skip_key::<syntax::FunctionSignatureKey>();
    persistence.register_skip_key::<syntax::GenericParametersKey>();
    persistence.register_skip_key::<syntax::WhereClauseKey>();
    persistence.register_skip_key::<syntax::TypeAliasKey>();
    persistence
        .register_skip_key::<syntax::ImplementationQualifiedIdentifierKey>();
    persistence.register_skip_key::<syntax::StatementsKey>();
    persistence.register_skip_key::<syntax::FieldsKey>();
}
