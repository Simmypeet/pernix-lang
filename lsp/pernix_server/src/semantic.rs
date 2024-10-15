//! Contains logic related to handling semantic errors.
//!
//! This module enables the server to provide code actions, hover information,
//! go to definition, and other features that require semantic analysis.

use std::{collections::HashMap, sync::Arc};

use by_address::ByAddress;
use getset::Getters;
use log::{error, info};
use parking_lot::RwLock;
use pernixc_base::{
    diagnostic::Report,
    handler::{Handler, Storage},
    source_file::SourceFile,
};
use pernixc_lexical::token::Identifier;
use pernixc_semantic::symbol::table::{self, BuildTableError};
use pernixc_syntax::syntax_tree::target::Target;
use tower_lsp::lsp_types::Url;

use crate::{extension::DaignosticExt, workspace};

/// Handles the semantic checking of the code.
#[derive(Debug, Default, Getters)]
pub struct Semantic {
    /// The semantic errors from the latest analysis.
    #[get = "pub"]
    latest_semantic_errors_by_uri:
        HashMap<Url, Vec<tower_lsp::lsp_types::Diagnostic>>,
}

/// Determines how the semantic analysis should be performed.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OperatingMode<'a> {
    /// Analyze by using the given workspace as the context.
    Workspace(&'a workspace::Workspace),

    /// Analyze it as the given uri is the root file.
    SingleFile(Url),
}

#[derive(Debug)]
struct DiagnosticCollector(RwLock<Vec<pernixc_base::diagnostic::Diagnostic>>);

impl<E: Report<()>> Handler<E> for DiagnosticCollector {
    fn receive(&self, error: E) {
        let Ok(diagnostic) = error.report(()) else {
            return;
        };

        self.0.write().push(diagnostic);
    }
}

impl Semantic {
    /// Performs semantic anaylsis on the code.
    ///
    /// The semantic diagnostics are stored in the
    /// [`Semantic::latest_semantic_errors_by_uri`] field. The previous stored
    /// diagnostics are cleared before the new diagnostics are stored.
    pub fn analyze(
        &mut self,
        mode: OperatingMode,
    ) -> Vec<(Url, Vec<tower_lsp::lsp_types::Diagnostic>)> {
        // find the root file and target name based on the workspace
        // configurationc
        let (root, target_name) = match mode {
            OperatingMode::Workspace(workspace) => (
                workspace.configuration().root_file.clone(),
                workspace.configuration().target_name.clone(),
            ),
            OperatingMode::SingleFile(uri) => {
                info!("no workspace found, use the saved file as a root");

                let Ok(file_path) = uri.to_file_path() else {
                    error!(
                        "failed to convert uri to file path: {}",
                        uri.path()
                    );
                    return Vec::new();
                };

                let Some(name) = file_path.file_stem() else {
                    error!("failed to get file name: {}", file_path.display());
                    return Vec::new();
                };

                if !Identifier::is_valid_identifier_string(
                    name.to_string_lossy().as_ref(),
                ) {
                    error!("invalid file name: {}", name.to_string_lossy());
                    return Vec::new();
                }

                (
                    file_path.parent().unwrap().to_path_buf(),
                    name.to_string_lossy().to_string(),
                )
            }
        };

        let file = match std::fs::File::open(&root) {
            Ok(file) => file,
            Err(err) => {
                error!("{}: {}", root.display(), err);
                return Vec::new();
            }
        };

        let root = match SourceFile::load(file, root.clone()) {
            Ok(root) => Arc::new(root),
            Err(err) => {
                error!("{}: {}", root.display(), err);
                return Vec::new();
            }
        };

        let semantic_error_storage =
            Storage::<Box<dyn pernixc_semantic::error::Error>>::new();
        let collector = DiagnosticCollector(RwLock::new(Vec::new()));

        let target = Target::parse(&root, target_name, &collector);
        let table =
            table::build(std::iter::once(target), &semantic_error_storage);

        info!("{:#?}", table);
        info!("{:#?}", semantic_error_storage.as_vec());

        // group the diagnostics by source file
        let mut lsp_diagnostics_by_source_file = HashMap::<_, Vec<_>>::new();

        for diagnostic in collector.0.into_inner() {
            lsp_diagnostics_by_source_file
                .entry(ByAddress(diagnostic.span.source_file().clone()))
                .or_default()
                .push(diagnostic.into_diagnostic());
        }

        // clear the latest diagnostics
        self.latest_semantic_errors_by_uri.clear();

        if let Err(BuildTableError::Suboptimal(table)) = &table {
            for error in semantic_error_storage.into_vec() {
                let Ok(diagnostic) = error.report(table) else {
                    continue;
                };

                let Some(uri) = std::fs::canonicalize(
                    diagnostic.span.source_file().full_path(),
                )
                .ok()
                .and_then(|x| Url::from_file_path(x).ok()) else {
                    continue;
                };

                let source_file =
                    ByAddress(diagnostic.span.source_file().clone());
                let lsp_diagnostic = diagnostic.into_diagnostic();

                self.latest_semantic_errors_by_uri
                    .entry(uri)
                    .or_default()
                    .push(lsp_diagnostic.clone());

                lsp_diagnostics_by_source_file
                    .entry(source_file)
                    .or_default()
                    .push(lsp_diagnostic);
            }
        }

        // report the diagnostics in batch
        let mut result = Vec::new();
        for (source_file, diagnostics) in lsp_diagnostics_by_source_file {
            let Some(uri) = std::fs::canonicalize(source_file.full_path())
                .ok()
                .and_then(|x| Url::from_file_path(x).ok())
            else {
                continue;
            };

            result.push((uri, diagnostics));
        }

        result
    }
}
