//! Contains the analyzer implementation.

use std::{collections::HashMap, sync::Arc};

use pernixc_diagnostic::ByteIndex;
use pernixc_query::{runtime::executor, Engine, TrackedEngine};
use pernixc_source_file::{
    get_source_file_by_id, EditorLocation, GlobalSourceID, SourceFile,
};
use pernixc_target::{Arguments, Check, Input, TargetID};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::{DidChangeTextDocumentParams, Url};

use crate::workspace::{self, NewWorkspaceError, Workspace};

/// The struct that handles analysis of the workspace (e.g., checking errors).
#[derive(Debug)]
pub struct Analyzer {
    engine: RwLock<Arc<Engine>>,
    workspace: Workspace,
    current_target_id: TargetID,
    published_diagnostics: RwLock<Vec<Url>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct LoadSourceFileExecutor;

impl executor::Executor<pernixc_source_file::Key> for LoadSourceFileExecutor {
    /// The original executor always recomputes the value, which is not what we
    /// want here in the lsp server. We'll depend on the LSP server to tell us
    /// when to recompute.
    const ALWAYS_RECOMPUTE: bool = false;

    async fn execute(
        &self,
        _: &pernixc_query::TrackedEngine,
        key: &pernixc_source_file::Key,
    ) -> Result<
        Result<Arc<SourceFile>, pernixc_source_file::Error>,
        executor::CyclicError,
    > {
        Ok(std::fs::File::open(key.path.as_ref())
            .and_then(|file| {
                Ok(Arc::new(SourceFile::load(file, key.path.to_path_buf())?))
            })
            .map_err(|x| pernixc_source_file::Error(x.to_string().into())))
    }
}

impl Analyzer {
    /// Creates a new analyzer for the given workspace URL.
    pub async fn new(workspace_url: Url) -> Result<Self, NewWorkspaceError> {
        let workspace = workspace::Workspace::new(&workspace_url)?;

        let target_name = workspace.target_name().to_string();
        let local_target_id = TargetID::from_target_name(&target_name);

        let command = pernixc_target::Command::Check(Check {
            input: Input {
                file: workspace.root_source_file().to_path_buf(),
                target_name: Some(target_name),
                library_paths: Vec::new(),
                incremental_path: None,
                chrome_tracing: false,
                target_seed: None,
            },
        });

        // initialize the engine with corelib
        let mut engine = Engine::default();
        pernixc_register::Registration::register_executor(
            &mut engine.runtime.executor,
        );

        assert!(engine
            .runtime
            .executor
            .register(Arc::new(LoadSourceFileExecutor))
            .is_some());

        engine
            .input_session(async |x| {
                x.set_input(
                    pernixc_target::LinkKey(local_target_id),
                    Arc::new(std::iter::once(TargetID::CORE).collect()),
                )
                .await;

                x.set_input(
                    pernixc_target::AllTargetIDsKey,
                    Arc::new(
                        vec![local_target_id, TargetID::CORE]
                            .into_iter()
                            .collect(),
                    ),
                )
                .await;

                x.set_input(
                    pernixc_target::MapKey,
                    Arc::new(
                        [
                            (command.input().target_name(), local_target_id),
                            ("core".into(), TargetID::CORE),
                        ]
                        .into_iter()
                        .collect(),
                    ),
                )
                .await;

                x.set_input(
                    pernixc_target::Key(local_target_id),
                    Arc::new(Arguments { command }),
                )
                .await;
            })
            .await;

        let mut engine = Arc::new(engine);
        pernixc_corelib::initialize_corelib(&mut engine).await;

        Ok(Self {
            engine: RwLock::new(engine),
            workspace,
            current_target_id: local_target_id,
            published_diagnostics: RwLock::new(Vec::new()),
        })
    }

    /// Applies the given change to the source file in the engine.
    /// This does not perform a check, that must be done separately.
    pub async fn apply_change(&self, mut params: DidChangeTextDocumentParams) {
        let mut engine_lock = self.engine.write().await;

        let input_lock = Arc::get_mut(&mut *engine_lock).unwrap().input_lock();
        let key = pernixc_source_file::Key {
            path: params.text_document.uri.to_file_path().unwrap().into(),
            target_id: self.current_target_id,
        };

        let changes = match params
            .content_changes
            .iter()
            .rposition(|x| x.range.is_none())
        {
            Some(index) => {
                let change = &mut params.content_changes[index];

                let file_path =
                    params.text_document.uri.to_file_path().unwrap();

                input_lock
                    .set_input(
                        key.clone(),
                        Ok(Arc::new(SourceFile::new(
                            std::mem::take(&mut change.text),
                            file_path,
                        ))),
                    )
                    .await;

                &params.content_changes[index + 1..]
            }
            None => &params.content_changes,
        };

        input_lock.inplace_mutate(&key, |source_file| {
            let source_file = source_file.as_mut().unwrap();

            for change in changes {
                Self::update_source_file(
                    Arc::get_mut(source_file).expect("should be unique"),
                    &change.text,
                    change.range.unwrap(),
                );
            }
        });
    }

    fn update_source_file(
        source_file: &mut SourceFile,
        text: &str,
        range: tower_lsp::lsp_types::Range,
    ) {
        let pernix_location_start = EditorLocation::new(
            range.start.line as usize,
            range.start.character as usize,
        );
        let pernix_location_end = EditorLocation::new(
            range.end.line as usize,
            range.end.character as usize,
        );

        let start = source_file
            .into_byte_index_include_ending(pernix_location_start)
            .unwrap_or_else(|| {
                panic!(
                    "the given range {pernix_location_start:?} is invalid to \
                     the source file"
                )
            });

        let end = source_file
            .into_byte_index_include_ending(pernix_location_end)
            .unwrap_or_else(|| {
                panic!(
                    "the given range {pernix_location_end:?} is invalid to \
                     the source file"
                )
            });

        source_file.replace_range(start..end, text);
    }

    /// Performs a compile check and sends diagnostics to the client.
    pub async fn check(
        &self,
        client: &tower_lsp::Client,
        version: Option<i32>,
    ) {
        // the engine lock is intentionally held here
        let engine_lock = self.engine.read().await;
        let mut published_diagnostics =
            self.published_diagnostics.write().await;

        let engine = engine_lock.tracked();

        let Ok(check) =
            engine.query(&pernixc_check::Key(self.current_target_id)).await
        else {
            return;
        };

        // collect diagnostics and group them by source file URL
        let mut diagnostics = HashMap::new();

        for diag in check.all_diagnostics() {
            let url = if let Some(primary) = &diag.primary_highlight {
                engine.get_url_by_source_id(primary.span.source_id).await
            } else {
                Url::from_file_path(self.workspace.root_source_file()).unwrap()
            };

            diagnostics
                .entry(url)
                .or_insert_with(Vec::new)
                .push(engine.to_lsp_diagnostic(diag).await);
        }

        // clear diagnostics for files that no longer have diagnostics
        for url in published_diagnostics.iter() {
            if !diagnostics.contains_key(url) {
                client
                    .publish_diagnostics(url.clone(), Vec::new(), version)
                    .await;
            }
        }

        published_diagnostics.clear();
        for (url, diags) in diagnostics {
            client
                .publish_diagnostics(url.clone(), diags.clone(), version)
                .await;
            published_diagnostics.push(url);
        }
    }
}

#[pernixc_extend::extend]
#[allow(clippy::cast_possible_truncation)]
async fn to_lsp_range(
    self: &TrackedEngine,
    span: &pernixc_source_file::Span<ByteIndex>,
) -> tower_lsp::lsp_types::Range {
    let source_file = self.get_source_file_by_id(span.source_id).await;
    let start = source_file.get_location(span.start).unwrap_or_else(|| {
        let line = source_file.line_coount() - 1;
        let column = source_file.get_line(line).unwrap().len();

        EditorLocation::new(line, column)
    });

    let end = source_file.get_location(span.end).unwrap_or_else(|| {
        let line = source_file.line_coount() - 1;
        let column = source_file.get_line(line).unwrap().len();

        EditorLocation::new(line, column)
    });

    tower_lsp::lsp_types::Range {
        start: tower_lsp::lsp_types::Position {
            line: start.line as u32,
            character: start.column as u32,
        },
        end: tower_lsp::lsp_types::Position {
            line: end.line as u32,
            character: end.column as u32,
        },
    }
}

#[pernixc_extend::extend]
async fn get_url_by_source_id(
    self: &TrackedEngine,
    source_id: GlobalSourceID,
) -> Url {
    let source_file = self.get_source_file_by_id(source_id).await;

    let absolute_path = std::fs::canonicalize(source_file.path()).unwrap();

    Url::from_file_path(absolute_path).unwrap()
}

#[pernixc_extend::extend]
async fn to_lsp_diagnostic(
    self: &TrackedEngine,
    diagnostic: &pernixc_diagnostic::Rendered<ByteIndex>,
) -> tower_lsp::lsp_types::Diagnostic {
    let primary_range =
        if let Some(primary_highlight) = &diagnostic.primary_highlight {
            self.to_lsp_range(&primary_highlight.span).await
        } else {
            tower_lsp::lsp_types::Range {
                start: tower_lsp::lsp_types::Position { line: 0, character: 0 },
                end: tower_lsp::lsp_types::Position { line: 0, character: 0 },
            }
        };

    let severity = match diagnostic.severity {
        pernixc_diagnostic::Severity::Error => {
            tower_lsp::lsp_types::DiagnosticSeverity::ERROR
        }
        pernixc_diagnostic::Severity::Warning => {
            tower_lsp::lsp_types::DiagnosticSeverity::WARNING
        }
        pernixc_diagnostic::Severity::Info => {
            tower_lsp::lsp_types::DiagnosticSeverity::INFORMATION
        }
    };

    let related_information = {
        let mut result = Vec::new();
        for related in &diagnostic.related {
            result.push(tower_lsp::lsp_types::DiagnosticRelatedInformation {
                location: tower_lsp::lsp_types::Location {
                    uri: self
                        .get_url_by_source_id(related.span.source_id)
                        .await,
                    range: self.to_lsp_range(&related.span).await,
                },
                message: related.message.clone().unwrap_or_default(),
            });
        }

        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    };

    let mut message = diagnostic.message.clone();

    if let Some(primary_message) =
        diagnostic.primary_highlight.as_ref().and_then(|x| x.message.as_ref())
    {
        message.push('\n');
        message.push_str(primary_message);
    }

    tower_lsp::lsp_types::Diagnostic {
        range: primary_range,
        severity: Some(severity),
        code: None,
        code_description: None,
        source: Some("pernixc".to_string()),
        message,
        related_information,
        tags: None,
        data: None,
    }
}
