//! Crate responsible for starting and setting up the query engine for the
//! compilation proccess.

use std::path::Path;

use pernixc_query::database::Database;
use pernixc_source_file::{GlobalSourceID, SourceMap};

pub fn start_query_database<'l>(
    source_map: &mut SourceMap,
    root_source_id: GlobalSourceID,
    library_paths: impl IntoIterator<Item = &'l Path>,
    incremental_path: Option<(&Path, &pernixc_query::serde::Serde)>,
) -> Database {
    let root_file = &source_map[root_source_id];
    todo!()
}
