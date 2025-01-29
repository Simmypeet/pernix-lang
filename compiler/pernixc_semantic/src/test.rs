//! Contains test utiolities for the semantic analysis phase.

use std::{fmt::Display, sync::Arc};

use pernixc_base::{handler, source_file::SourceFile};
use pernixc_lexical::token_stream::{TokenStream, Tree};
use pernixc_syntax::{
    state_machine::parse::Parse,
    syntax_tree::{target::Target, SyntaxTree},
};

/// Parses the source code into a syntax tree.
pub fn parse<T: SyntaxTree>(source: impl Display) -> T {
    let source_file =
        Arc::new(SourceFile::new(source.to_string(), "test".into()));

    let token_stream = TokenStream::tokenize(source_file, &pernixc_handler::Panic);
    let tree = Tree::new(&token_stream);

    let pattern = T::parse.parse_syntax(&tree, &pernixc_handler::Panic).unwrap();

    pattern
}

/// Builds the target for testing purposes.
pub fn build_target(source: impl Display) -> Target {
    let source_file =
        Arc::new(SourceFile::new(source.to_string(), "test".into()));

    // we'll panic on syntax errors
    Target::parse(&source_file, "test".to_string(), &pernixc_handler::Panic)
}
