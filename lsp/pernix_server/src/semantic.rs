//! Contains logic related to handling semantic errors.
//!
//! This module enables the server to provide code actions, hover information,
//! go to definition, and other features that require semantic analysis.

use std::collections::HashMap;

use pernixc_base::diagnostic::Diagnostic;
use tower_lsp::lsp_types::Url;

use crate::workspace;

/// Handles the semantic checking of the code.
#[derive(Debug, Default)]
pub struct Semantic {
    #[allow(dead_code)]
    latest_semantic_errors_by_uri: HashMap<Url, Vec<Diagnostic>>,
}

/// Determines how the semantic analysis should be performed.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OperatingMode<'a> {
    /// Analyze by using the given workspace as the context.
    Workspace(&'a workspace::Workspace),

    /// Analyze it as the given uri is the root file.
    SingleFile(Url),
}
