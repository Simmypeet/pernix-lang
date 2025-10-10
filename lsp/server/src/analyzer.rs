//! Contains the analyzer implementation.

use std::sync::Arc;

use pernixc_diagnostic::ByteIndex;
use pernixc_query::{runtime::executor, Engine, TrackedEngine};
use pernixc_source_file::{
    get_source_file_by_id, get_source_file_path, EditorLocation,
    GlobalSourceID, SourceFile,
};
use pernixc_target::{Arguments, Check, Global, Input, TargetID};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::Url;

use crate::workspace::{self, NewWorkspaceError, Workspace};

/// The struct that handles analysis of the workspace (e.g., checking errors).
#[derive(Debug)]
pub struct Analyzer {
    engine: RwLock<Arc<Engine>>,
    workspace: Workspace,
    current_target_id: TargetID,
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
        })
    }

    pub async fn check(
        &self,
    ) -> Result<Vec<tower_lsp::lsp_types::Diagnostic>, ()> {
        // the engine is intentionally held here
        let engine_lock = self.engine.read().await;
        let engine = engine_lock.tracked();

        let symbol_impl = {
            let engine = engine.clone();
            let current_target_id = self.current_target_id;

            tokio::spawn(async move {
                engine
                    .query(&pernixc_symbol_impl::diagnostic::RenderedKey(
                        current_target_id,
                    ))
                    .await
            })
        };
        let semantic_element_impl = {
            let engine = engine.clone();
            let current_target_id = self.current_target_id;

            tokio::spawn(async move {
                engine
                .query(
                    &pernixc_semantic_element_impl::diagnostic::AllRenderedKey(
                        current_target_id,
                    ),
                )
                .await
            })
        };

        todo!()
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
