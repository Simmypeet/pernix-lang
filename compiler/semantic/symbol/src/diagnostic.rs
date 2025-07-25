//! Contains the diagnostics that can be reported while building the
//! symbol table tree.

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use pernixc_diagnostic::Report;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_target::{Global, TargetID};

use crate::{
    kind::Kind,
    name::{get_name, get_qualified_name},
    source_map::{to_absolute_span, SourceMap},
    span::get_span,
    DiagnosticKey, Key, ID,
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
#[allow(missing_docs)]
pub enum Diagnostic {
    ItemRedefinition(ItemRedefinition),
    RecursiveFileRequest(RecursiveFileRequest),
    SourceFileLoadFail(SourceFileLoadFail),
}

impl Report<&TrackedEngine> for Diagnostic {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        match self {
            Self::ItemRedefinition(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::RecursiveFileRequest(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::SourceFileLoadFail(diagnostic) => {
                diagnostic.report(engine).await
            }
        }
    }
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

impl Report<&TrackedEngine> for ItemRedefinition {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<ByteIndex> {
        let existing_symbol_span = engine.get_span(self.existing_id).await;
        let existing_symbol_name = engine.get_name(self.existing_id).await;
        let in_name = engine.get_qualified_name(self.in_id).await;

        pernixc_diagnostic::Diagnostic {
            span: Some((
                engine.to_absolute_span(&self.redefinition_span).await,
                Some("redefinition here".to_string()),
            )),

            message: format!(
                "symbol `{existing_symbol_name}` is already defined in the \
                 scope `{in_name}`"
            ),
            severity: pernixc_diagnostic::Severity::Error,
            help_message: None,
            related: match existing_symbol_span.as_ref() {
                Some(span) => vec![pernixc_diagnostic::Related {
                    span: engine.to_absolute_span(span).await,
                    message: format!(
                        "symbol `{existing_symbol_name}` is already defined \
                         here"
                    ),
                }],
                None => Vec::new(),
            },
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

impl Report<&TrackedEngine> for RecursiveFileRequest {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic {
            span: Some((
                engine.to_absolute_span(&self.submodule_span).await,
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

/// Failed to load source file when building the symbol table.
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
pub struct SourceFileLoadFail {
    /// The error message from the file loading failure.
    pub error_message: String,

    /// The path to the source file that failed to load.
    pub path: Arc<Path>,

    /// The span of the submodule identifier declaration, if this failure
    /// occurred when loading a submodule. `None` for root file load failures.
    pub submodule_span: Option<RelativeSpan>,
}

impl Report<&TrackedEngine> for SourceFileLoadFail {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let (span, context_message) = match self.submodule_span.as_ref() {
            Some(submodule_span) => (
                Some((
                    engine.to_absolute_span(submodule_span).await,
                    Some("submodule declaration here".to_string()),
                )),
                format!(
                    "failed to load submodule file `{}`",
                    self.path.display()
                ),
            ),
            None => (
                None,
                format!("failed to load root file `{}`", self.path.display()),
            ),
        };

        pernixc_diagnostic::Diagnostic {
            span,
            message: format!("{}: {}", context_message, self.error_message),
            severity: pernixc_diagnostic::Severity::Error,
            help_message: Some(
                "check if the file exists and is accessible".to_string(),
            ),
            related: Vec::new(),
        }
    }
}

/// A query for retrieving all rendered diagnostics for a target.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Arc<[pernixc_diagnostic::Diagnostic<pernixc_source_file::ByteIndex>]>)]
pub struct RenderedKey(pub TargetID);

#[pernixc_query::executor(key(RenderedKey), name(RenderedExecutor))]
#[allow(clippy::too_many_lines)]
pub async fn rendered_executor(
    &RenderedKey(target_id): &RenderedKey,
    engine: &TrackedEngine,
) -> Result<
    Arc<[pernixc_diagnostic::Diagnostic<pernixc_source_file::ByteIndex>]>,
    pernixc_query::runtime::executor::CyclicError,
> {
    // Get the map for this target to discover all table keys
    let map: crate::Map = engine.query(&crate::MapKey(target_id)).await?;

    // Collect diagnostics from all tables and render them using iterator
    // combinators
    let mut file_diagnostics = Vec::new();

    for path in map.paths_by_source_id.values() {
        let external_submodule_opt = path.1.clone();
        let path_key = path.0.clone();
        let engine = engine.clone();

        file_diagnostics.push(tokio::spawn(async move {
            // Create the table key based on whether it's root or external
            // submodule
            let table_key = external_submodule_opt.map_or_else(
                || Key::Root(target_id),
                |external_submodule| Key::Submodule {
                    external_submodule,
                    target_id,
                },
            );

            Ok::<_, CyclicError>((
                engine.query(&DiagnosticKey(table_key)).await?,
                engine
                    .query(&pernixc_syntax::DiagnosticKey(
                        pernixc_syntax::Key {
                            path: path_key.clone(),
                            target_id,
                        },
                    ))
                    .await?,
                engine
                    .query(&pernixc_lexical::DiagnosticKey(
                        pernixc_lexical::Key {
                            path: path_key.clone(),
                            target_id,
                        },
                    ))
                    .await?,
                path_key,
            ))
        }));
    }

    let all_modules: Arc<[crate::ID]> = engine
        .query(&crate::kind::AllSymbolOfKindKey {
            target_id,
            kind: Kind::Module,
        })
        .await?;

    let mut module_diagnostics = Vec::new();

    for module_id in all_modules.iter().copied() {
        let engine = engine.clone();

        module_diagnostics.push(tokio::spawn(async move {
            let diagnostic = engine
                .query(&crate::import::DiagnosticKey(
                    target_id.make_global(module_id),
                ))
                .await?;

            Ok::<_, CyclicError>((module_id, diagnostic))
        }));
    }

    let mut diagnostic_handles = Vec::new();

    for file_diagnostic in file_diagnostics {
        let (
            diagnostics_for_file,
            syntax_diagnostics,
            lexical_diagnostics,
            path_key,
        ) = file_diagnostic.await.unwrap()?;

        for diagnostic in diagnostics_for_file.iter() {
            let engine = engine.clone();
            let diagnostic = diagnostic.clone();

            diagnostic_handles.push(tokio::spawn(async move {
                Ok::<_, CyclicError>(diagnostic.report(&engine).await)
            }));
        }

        for diagnostic in syntax_diagnostics.iter().flat_map(|d| d.iter()) {
            let engine = engine.clone();
            let path_key = path_key.clone();
            let diagnostic = diagnostic.clone();

            diagnostic_handles.push(tokio::spawn(async move {
                let tree = engine
                    .query(&pernixc_lexical::Key {
                        path: path_key.clone(),
                        target_id,
                    })
                    .await?
                    .unwrap()
                    .0;

                Ok(diagnostic.report(&tree).await)
            }));
        }

        for diagnostic in lexical_diagnostics.iter().flat_map(|d| d.iter()) {
            let engine = engine.clone();
            let diagnostic = diagnostic.clone();

            diagnostic_handles.push(tokio::spawn(async move {
                let source_map = SourceMap(engine);
                Ok(diagnostic.report(&source_map).await)
            }));
        }
    }

    for module_diagnostic in module_diagnostics {
        let (_, diagnostic) = module_diagnostic.await.unwrap()?;

        for diagnostic in diagnostic.iter() {
            let engine = engine.clone();
            let diagnostic = diagnostic.clone();

            diagnostic_handles.push(tokio::spawn(async move {
                Ok(diagnostic.report(&engine).await)
            }));
        }
    }

    let mut diagnostics = Vec::new();

    for handle in diagnostic_handles {
        let rendered_diagnostic = handle.await.unwrap()?;
        diagnostics.push(rendered_diagnostic);
    }

    Ok(Arc::from(diagnostics))
}
