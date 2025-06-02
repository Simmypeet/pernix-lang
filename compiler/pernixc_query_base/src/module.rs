//! Module syntax query system for the Pernix compiler.

use std::sync::Arc;

use pernixc_diagnostic::{Diagnostic, Report};
use pernixc_handler::Storage;
use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_query::{runtime::executor::Executor, Key};
use pernixc_source_file::{ByteIndex, GlobalSourceID};
use serde::{Deserialize, Serialize};

/// Query key for retrieving module syntax.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
)]
#[value(Parse)]
pub struct ParseKey(pub GlobalSourceID);

/// The results of parsing a module's syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    Default,
)]
pub struct Parse {
    /// The parsed module content containing all syntax elements.
    pub syntax: pernixc_syntax::item::module::Content,

    /// All the diagnostics generated during parsing.
    pub diagnostics: Arc<[Diagnostic<ByteIndex>]>,
}

/// A key for retrieving the [`Parse::syntax`] of a module.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
)]
#[value(pernixc_syntax::item::module::Content)]
pub struct SyntaxKey(pub GlobalSourceID);

/// An executor for the [`SyntaxKey`] that retrieves the parsed syntax of a
/// module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SyntaxExecutor;

impl Executor<SyntaxKey> for SyntaxExecutor {
    fn execute(
        &self,
        engine: &pernixc_query::Engine,
        key: SyntaxKey,
    ) -> Result<
        pernixc_syntax::item::module::Content,
        pernixc_query::runtime::executor::CyclicError,
    > {
        Ok(engine.module_parsing(key.0).syntax)
    }
}

/// Extension trait for convenient module syntax queries.
pub trait Ext {
    /// Retrieves the parsed syntax for a module identified by its source ID.
    fn module_parsing(&self, source_id: GlobalSourceID) -> Parse;
}

impl Ext for pernixc_query::Engine {
    fn module_parsing(&self, source_id: GlobalSourceID) -> Parse {
        self.query(&ParseKey(source_id)).expect(
            "should have no cyclic dependencies since this is a base query",
        )
    }
}

impl Parse {
    /// Parses the syntax of a module from its source ID and source map,
    /// returning the parsed syntax and the associated token tree.
    #[must_use]
    pub fn from_source_id(
        source_id: GlobalSourceID,
        source_map: &pernixc_source_file::SourceMap,
    ) -> (Self, pernixc_lexical::tree::Tree) {
        // create a token tree for the source file
        let lexical_errors = Storage::<pernixc_lexical::error::Error>::new();
        let token_tree = pernixc_lexical::tree::Tree::from_source(
            source_map[source_id].content(),
            source_id,
            &lexical_errors,
        );

        let (syntax_tree, syntax_errors) =
            pernixc_syntax::item::module::Content::parse(&token_tree);

        let syntax_tree =
            syntax_tree.expect("no step into parsing module syntax");

        let lexical_errors = lexical_errors.into_vec();
        let mut diagnostics =
            Vec::with_capacity(lexical_errors.len() + syntax_errors.len());

        for error in lexical_errors {
            diagnostics.push(error.report(source_map));
        }

        for error in syntax_errors {
            diagnostics.push(error.report((&token_tree, source_id)));
        }

        (
            Self { syntax: syntax_tree, diagnostics: diagnostics.into() },
            token_tree,
        )
    }
}
