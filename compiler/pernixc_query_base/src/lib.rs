//! Crate responsible for starting and setting up the query engine for the
//! compilation process.

use std::{io::Read, path::Path};

use dashmap::DashMap;
use fnv::FnvBuildHasher;
use pernixc_query::database::Database;
use pernixc_source_file::{GlobalSourceID, SourceMap};
use serde::de::DeserializeSeed;

pub mod accessibility;
pub mod implemented;
pub mod implements;
pub mod kind;
pub mod member;
pub mod module;
pub mod name;
pub mod parent;
pub mod symbol;
pub mod target;
pub mod import;

/// A fatal error that aborts the compilation process.
#[derive(Debug, thiserror::Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    OpenIncrementalFileIO(std::io::Error),
    #[error(transparent)]
    ReadIncrementalFileIO(std::io::Error),
    #[error(transparent)]
    IncrementalFileDeserialize(postcard::Error),
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
pub fn start_query_database<'l>(
    source_map: &mut SourceMap,
    root_source_id: GlobalSourceID,
    _library_paths: impl IntoIterator<Item = &'l Path>,
    target_name: &str,
    incremental_path: Option<(&Path, &pernixc_query::serde::Serde)>,
) -> Result<Start, Error> {
    // load the incremental file (if any)
    let incremental_file = if let Some((incremental_path, _)) = incremental_path
    {
        match std::fs::File::open(incremental_path) {
            Ok(file) => Some(file),
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
    let database = incremental_file.map_or_else(
        || Ok(Database::default()),
        |mut file| {
            let buffer = read_file_buffer(&mut file)
                .map_err(Error::ReadIncrementalFileIO)?;

            let mut deserializer = postcard::Deserializer::from_flavor(
                postcard::de_flavors::Slice::new(&buffer),
            );

            incremental_path
                .unwrap()
                .1
                .database_deserializer()
                .deserialize(&mut deserializer)
                .map_err(Error::IncrementalFileDeserialize)
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
pub fn register_runtime(
    query_runtime: &mut pernixc_query::runtime::Runtime,
    serde: &mut pernixc_query::serde::Serde,
) {
    todo!()
}
