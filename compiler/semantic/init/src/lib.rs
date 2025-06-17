//! Crate responsible for starting and setting up the query engine for the
//! compilation process.
use std::{
    fs::File,
    io::{BufReader, BufWriter, Read},
    path::Path,
};

use dashmap::DashMap;
use fnv::FnvBuildHasher;
use pernixc_query::{
    database::Database,
    serde::{DynamicDeserialize, DynamicRegistry},
};
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    extension::{SharedPointerDeserialize, SharedPointerSerialize},
    Deserialize,
};
use pernixc_source_file::{GlobalSourceID, SourceMap};

pub mod accessibility;
pub mod implemented;
pub mod implements;
pub mod import;
pub mod kind;
pub mod member;
pub mod module;
pub mod name;
pub mod parent;
pub mod symbol;
pub mod target;

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

/// The result of calling [`start_query_database`].
#[derive(Debug)]
pub struct Start {
    /// The database where all the query inputs are set-up and is ready for
    /// querying.
    pub database: Database,

    /// A map of token trees by their source ID, which is used to later
    /// retrieve the absolute location
    pub token_trees_by_source_id:
        DashMap<GlobalSourceID, pernixc_lexical::tree::Tree, FnvBuildHasher>,

    /// The list of errors encountered while parsing the module tree.
    pub module_parsing_errors: Vec<module::Error>,
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

fn read_file_buffer(
    file: &mut std::fs::File,
) -> Result<Vec<u8>, std::io::Error> {
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

/// Setup the query database for the compilation process.
///
/// This will initialize the query database with the initial syntax tree and
/// module structure
pub fn start_query_database<
    'l,
    Ext: DynamicDeserialize<BinaryDeserializer<BufReader<File>>>,
>(
    source_map: &mut SourceMap,
    root_source_id: GlobalSourceID,
    _library_paths: impl IntoIterator<Item = &'l Path>,
    target_name: &str,
    incremental_path: Option<(&Path, &mut Ext)>,
) -> Result<Start, Error> {
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

    let parse = module::parse(root_source_id, source_map);

    Ok(Start {
        database,
        token_trees_by_source_id: parse.token_trees_by_source_id,
        module_parsing_errors: parse.errors,
    })
}

/// Registers all the necessary runtime information for the query engine.
pub fn register_runtime<
    Registry: DynamicRegistry<
            BinarySerializer<BufWriter<File>>,
            BinaryDeserializer<BufReader<File>>,
        > + SharedPointerSerialize
        + SharedPointerDeserialize,
>(
    _query_runtime: &mut pernixc_query::runtime::Runtime,
    serder_registry: &mut Registry,
) {
    serder_registry.register::<accessibility::Key>();
    serder_registry.register::<implemented::Key>();
    serder_registry.register::<implements::Key>();
    serder_registry.register::<kind::Key>();
    serder_registry.register::<member::Key>();
    serder_registry.register::<name::Key>();
    serder_registry.register::<parent::Key>();
}
