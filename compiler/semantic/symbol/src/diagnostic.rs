//! Contains the diagnostics that can be reported while building the
//! symbol table tree.

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use pernixc_diagnostic::{Highlight, Report};
use pernixc_hash::HashSet;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_target::{Global, TargetID};
use pernixc_tokio::{join_list::JoinList, scoped};

use crate::{
    kind::{get_all_symbols_of_kind, Kind},
    name::{get_name, get_qualified_name},
    source_map::to_absolute_span,
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
            primary_highlight: Some(Highlight::new(
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
                Some(span) => vec![pernixc_diagnostic::Highlight::new(
                    engine.to_absolute_span(span).await,
                    Some(format!(
                        "symbol `{existing_symbol_name}` is already defined \
                         here"
                    )),
                )],
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
            primary_highlight: Some(Highlight::new(
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
        let (highlight, context_message) = match self.submodule_span.as_ref() {
            Some(submodule_span) => (
                Some(Highlight::new(
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
            primary_highlight: highlight,
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

struct FileError {
    lexicals: Option<Arc<[pernixc_lexical::error::Error]>>,
    syntaxes: Option<Arc<[pernixc_parser::error::Error]>>,
    symbols: Arc<HashSet<Diagnostic>>,
    path: Arc<Path>,
}

fn populate_file_errors(
    engine: &TrackedEngine,
    target_id: TargetID,
    file_errors_list: &mut JoinList<Result<FileError, CyclicError>>,
    map: &crate::Map,
) {
    for path in map.paths_by_source_id.values() {
        let external_submodule_opt = path.1.clone();
        let path_key = path.0.clone();
        let engine = engine.clone();

        file_errors_list.spawn(async move {
            // Create the table key based on whether it's root or
            // external submodule
            let table_key = external_submodule_opt.map_or_else(
                || Key::Root(target_id),
                |external_submodule| Key::Submodule {
                    external_submodule,
                    target_id,
                },
            );

            Ok(FileError {
                symbols: engine.query(&DiagnosticKey(table_key)).await?,
                syntaxes: engine
                    .query(&pernixc_syntax::DiagnosticKey(
                        pernixc_syntax::Key {
                            path: path_key.clone(),
                            target_id,
                        },
                    ))
                    .await?
                    .ok(),
                lexicals: engine
                    .query(&pernixc_lexical::DiagnosticKey(
                        pernixc_lexical::Key {
                            path: path_key.clone(),
                            target_id,
                        },
                    ))
                    .await?
                    .ok(),
                path: path_key,
            })
        });
    }
}

#[allow(clippy::type_complexity)]
async fn populate_module_import_diagnostics(
    engine: &TrackedEngine,
    target_id: TargetID,
    module_import_diagnostics: &mut JoinList<
        Result<(ID, Arc<[crate::import::diagnostic::Diagnostic]>), CyclicError>,
    >,
) -> Result<(), CyclicError> {
    let all_modules =
        engine.get_all_symbols_of_kind(target_id, Kind::Module).await;

    for module_id in all_modules.iter().copied() {
        let engine = engine.clone();

        module_import_diagnostics.spawn(async move {
            let diagnostic = engine
                .query(&crate::import::DiagnosticKey(
                    target_id.make_global(module_id),
                ))
                .await?;

            Ok::<_, CyclicError>((module_id, diagnostic))
        });
    }

    Ok(())
}

async fn populate_member_is_more_accessible_diagnostics(
    engine: &TrackedEngine,
    target_id: TargetID,
    module_import_diagnostics: &mut JoinList<
        Result<
            Option<Arc<[crate::accessibility::diagnostic::Diagnostic]>>,
            CyclicError,
        >,
    >,
) -> Result<(), CyclicError> {
    let all_traits =
        engine.get_all_symbols_of_kind(target_id, Kind::Trait).await;

    for trait_id in all_traits.iter().copied() {
        let engine = engine.clone();

        module_import_diagnostics.spawn(async move {
            let diagnostic = engine
                .query(&crate::accessibility::MemberIsMoreAaccessibleKey(
                    target_id.make_global(trait_id),
                ))
                .await?;

            Ok::<_, CyclicError>(diagnostic)
        });
    }

    Ok(())
}

#[pernixc_query::executor(key(RenderedKey), name(RenderedExecutor))]
#[allow(clippy::too_many_lines)]
pub async fn rendered_executor(
    &RenderedKey(target_id): &RenderedKey,
    engine: &TrackedEngine,
) -> Result<
    Arc<[pernixc_diagnostic::Diagnostic<ByteIndex>]>,
    pernixc_query::runtime::executor::CyclicError,
> {
    scoped!(|file_diagnostics,
             module_diagnostics,
             member_is_moere_accessible_diagnostics,
             diagnostic_handles| async move {
        // Get the map for this target to discover all table keys
        let map = engine.query(&crate::MapKey(target_id)).await?;

        populate_file_errors(engine, target_id, file_diagnostics, &map);

        populate_module_import_diagnostics(
            engine,
            target_id,
            module_diagnostics,
        )
        .await?;

        populate_member_is_more_accessible_diagnostics(
            engine,
            target_id,
            member_is_moere_accessible_diagnostics,
        )
        .await?;

        while let Some(file_diagnostic) = file_diagnostics.next().await {
            let FileError { lexicals, syntaxes, symbols, path } =
                file_diagnostic?;

            for diagnostic in symbols.iter() {
                let engine = engine.clone();
                let diagnostic = diagnostic.clone();

                diagnostic_handles.spawn(async move {
                    Ok::<_, CyclicError>(diagnostic.report(&engine).await)
                });
            }

            for diagnostic in syntaxes.iter().flat_map(|x| x.iter()) {
                let engine = engine.clone();
                let path_key = path.clone();
                let diagnostic = diagnostic.clone();

                diagnostic_handles.spawn(async move {
                    let tree = engine
                        .query(&pernixc_lexical::Key {
                            path: path_key.clone(),
                            target_id,
                        })
                        .await?
                        .unwrap()
                        .0;

                    Ok(diagnostic.report(&tree).await)
                });
            }

            for diagnostic in lexicals.iter().flat_map(|x| x.iter()) {
                let diagnostic = diagnostic.clone();

                diagnostic_handles
                    .spawn(async move { Ok(diagnostic.report(()).await) });
            }
        }

        while let Some(module_diagnostic) = module_diagnostics.next().await {
            let (_, diagnostic) = module_diagnostic?;

            for diagnostic in diagnostic.iter() {
                let engine = engine.clone();
                let diagnostic = diagnostic.clone();

                diagnostic_handles
                    .spawn(async move { Ok(diagnostic.report(&engine).await) });
            }
        }

        while let Some(member_is_more_accessible_diagnostic) =
            member_is_moere_accessible_diagnostics.next().await
        {
            if let Some(diagnostic) = member_is_more_accessible_diagnostic? {
                for &diagnostic in diagnostic.iter() {
                    let engine = engine.clone();

                    diagnostic_handles.spawn(async move {
                        Ok(diagnostic.report(&engine).await)
                    });
                }
            }
        }

        let mut diagnostics = Vec::new();

        while let Some(handle) = diagnostic_handles.next().await {
            let rendered_diagnostic = handle?;
            diagnostics.push(rendered_diagnostic);
        }

        Ok(Arc::from(diagnostics))
    })
}
