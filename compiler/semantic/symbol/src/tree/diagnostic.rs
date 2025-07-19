use std::path::PathBuf;

use pernixc_diagnostic::Report;
use pernixc_file_tree::source_map::to_absolute_span;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::{
    name::{get_name, get_qualified_name},
    span::get_span,
    ID,
};

/// Enumeration of all diagnostics that can be reported while building table
/// tree.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub enum Diagnostic {
    ItemRedefinition(ItemRedefinition),
    RecursiveFileRequest(RecursiveFileRequest),
}

/// The item symbol with the same name already exists in the given scope.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct ItemRedefinition {
    /// The ID of the existing symbol.
    pub existing_id: Global<ID>,

    /// The span containing the redefinition.
    pub redefinition_span: RelativeSpan,

    /// The scope in which the duplication occurred.
    pub in_id: Global<ID>,
}

impl Report<&TrackedEngine<'_>> for ItemRedefinition {
    type Location = ByteIndex;

    fn report(
        &self,
        engine: &TrackedEngine<'_>,
    ) -> pernixc_diagnostic::Diagnostic<ByteIndex> {
        let existing_symbol_span = engine.get_span(self.existing_id);
        let existing_symbol_name = engine.get_name(self.existing_id);
        let in_name = engine.get_qualified_name(self.in_id);

        pernixc_diagnostic::Diagnostic {
            span: Some((
                engine.to_absolute_span(&self.redefinition_span),
                Some("redefinition here".to_string()),
            )),

            message: format!(
                "symbol `{existing_symbol_name}` is already defined in the \
                 scope `{in_name}`"
            ),
            severity: pernixc_diagnostic::Severity::Error,
            help_message: None,
            related: existing_symbol_span
                .as_ref()
                .map(|span| pernixc_diagnostic::Related {
                    span: engine.to_absolute_span(span),
                    message: format!(
                        "symbol `{existing_symbol_name}` is already defined \
                         here"
                    ),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// The `public module someFile` ended up requesting the current file itself.
/// This only happens at the root of the file tree, where the file name is the
/// same as the submodule name.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct RecursiveFileRequest {
    /// The span of the submodule declaration in the root file.
    pub submodule_span: RelativeSpan,

    /// The path to the source file that was requested.
    pub path: PathBuf,
}

impl Report<&TrackedEngine<'_>> for RecursiveFileRequest {
    type Location = ByteIndex;

    fn report(
        &self,
        engine: &TrackedEngine<'_>,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic {
            span: Some((
                engine.to_absolute_span(&self.submodule_span),
                Some(
                    "this module declaration causes recursive loading"
                        .to_string(),
                ),
            )),
            message: format!(
                "the module declaration ened up loading the current file `{}`",
                self.path.display()
            ),
            severity: pernixc_diagnostic::Severity::Error,
            help_message: Some(format!(
                "try changing the module name from `{}` to something else \
                 that is not the same as the file name",
                self.path.file_stem().unwrap_or_default().to_string_lossy()
            )),
            related: Vec::new(),
        }
    }
}
