use std::{fmt::Display, sync::Arc};

use pernixc_base::{handler, source_file::SourceFile};
use pernixc_lexical::token_stream::{TokenStream, Tree};
use pernixc_syntax::{
    state_machine::parse::Parse,
    syntax_tree::{target::Target, SyntaxTree},
};

use crate::{
    error,
    symbol::table::{self, Suboptimal, Success, Table},
};

/// Parses the source code into a syntax tree.
pub fn parse<T: SyntaxTree>(source: impl Display) -> T {
    let source_file =
        Arc::new(SourceFile::new(source.to_string(), "test".into()));

    let token_stream = TokenStream::tokenize(source_file, &handler::Panic);
    let tree = Tree::new(&token_stream);

    let pattern = T::parse.parse_syntax(&tree, &handler::Panic).unwrap();

    pattern
}

/// Builds the table for testing purposes.
pub fn build_table(
    source: impl Display,
) -> Result<Table<Success>, (Table<Suboptimal>, Vec<Box<dyn error::Error>>)> {
    let source_file =
        Arc::new(SourceFile::new(source.to_string(), "test".into()));

    // we'll panic on syntax errors
    let target =
        Target::parse(&source_file, "test".to_string(), &handler::Panic);

    let storage = handler::Storage::new();
    let result = table::build(std::iter::once(target), &storage);

    match result {
        Ok(table) => Ok(table),
        Err(table::BuildTableError::Suboptimal(table)) => {
            Err((table, storage.into_vec()))
        }
        Err(table::BuildTableError::DuplicateTargetName(_)) => unreachable!(),
    }
}
