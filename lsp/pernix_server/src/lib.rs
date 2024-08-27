//! Contains the server implementation.

use std::{collections::HashMap, sync::Arc};

use by_address::ByAddress;
use extension::DaignosticExt;
use log::{debug, error, info};
use parking_lot::RwLock;
use pernixc_base::{
    diagnostic::Report,
    handler::{Handler, Storage},
    source_file::SourceFile,
};
use pernixc_lexical::token::Identifier;
use pernixc_semantic::symbol::table::{self, BuildTableError};
use pernixc_syntax::syntax_tree::target::Target;
use tower_lsp::{
    jsonrpc,
    lsp_types::{
        DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, FileChangeType,
        InitializeParams, InitializeResult, InitializedParams, OneOf,
        Registration, ServerCapabilities, TextDocumentSyncCapability,
        TextDocumentSyncKind, Url, WorkspaceFoldersServerCapabilities,
        WorkspaceServerCapabilities,
    },
    Client, LanguageServer,
};

/// A diagnostic with its source file uri.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticsWithUrl {
    /// The uri where the diagnostics are reported.
    pub uri: Url,

    /// Diagnostic messages.
    pub diagnostics: Vec<tower_lsp::lsp_types::Diagnostic>,
}

pub mod extension;
pub mod semantic;
pub mod syntax;
pub mod workspace;

/// The language server protocal implementation for Pernix.
#[derive(Debug)]
pub struct Server {
    client: Client,

    syntax: syntax::Syntax,

    /// Present if the server started with a workspace.
    workspace: RwLock<Option<workspace::Workspace>>,
}

impl Server {
    /// Creates a new server instance.
    #[must_use]
    pub fn new(client: Client) -> Self {
        Self {
            client,
            syntax: syntax::Syntax::default(),
            workspace: RwLock::new(None),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Server {
    async fn initialize(
        &self,
        _: InitializeParams,
    ) -> jsonrpc::Result<InitializeResult> {
        info!("Server initialized");

        Ok(InitializeResult {
            server_info: None,
            offset_encoding: None,
            capabilities: ServerCapabilities {
                inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),

                completion_provider: None,
                execute_command_provider: None,

                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(
                        WorkspaceFoldersServerCapabilities {
                            supported: Some(true),
                            change_notifications: Some(OneOf::Left(true)),
                        },
                    ),
                    file_operations: None,
                }),

                semantic_tokens_provider: None,

                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        // initialize the workspace if it exists
        if let Some(workspace) = self
            .client
            .workspace_folders()
            .await
            .into_iter()
            .flatten()
            .flatten()
            .next()
        {
            match self
                .client
                .register_capability(vec![Registration {
                    id: "pernixWatcher".to_string(),
                    method: "workspace/didChangeWatchedFiles".to_string(),
                    register_options: Some(serde_json::json!({
                        "watchers": [
                            {
                                "globPattern": "**/pernix.json",
                                "kind": 7,
                            }
                        ]
                    })),
                }])
                .await
            {
                Ok(()) => {}

                Err(response) => {
                    error!("failed to register watcher: {response}");
                }
            };

            if !self.configure_workspace(workspace.uri).await {
                info!("failed to configure workspace");
            }
        }
    }

    async fn did_change_watched_files(
        &self,
        params: DidChangeWatchedFilesParams,
    ) {
        info!("Did change watched files: {:?}", params);

        let Some((workspace_uri, expected_file_uri)) =
            self.get_workspace_and_configuration_uri().await
        else {
            error!("failed to get workspace and configuration uri");
            return;
        };

        for change in params.changes {
            if change.uri != expected_file_uri {
                info!("ignoring file change: {:?}", change.uri);
                continue;
            }

            match change.typ {
                FileChangeType::CREATED | FileChangeType::CHANGED => {
                    // clear any prior diagnostics
                    self.client
                        .publish_diagnostics(
                            expected_file_uri.clone(),
                            vec![],
                            None,
                        )
                        .await;

                    let _ =
                        self.configure_workspace(workspace_uri.clone()).await;
                }

                FileChangeType::DELETED => {
                    self.workspace.write().take();
                }

                unknown => {
                    error!("unknown file change type: {:?}", unknown);
                }
            }
        }
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> { Ok(()) }

    /// Register the source file content and report syntax errors.
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        info!("Did open: {}", params.text_document.uri.path());

        self.syntax.register_source_file(
            params.text_document.uri.clone(),
            params.text_document.text,
        );

        let Some(diagnostics) =
            self.syntax.diagnose_syntax(&params.text_document.uri)
        else {
            error!(
                "failed to diagnose syntax: {}",
                params.text_document.uri.path()
            );
            return;
        };

        // check syntax errors
        self.client
            .publish_diagnostics(
                params.text_document.uri,
                diagnostics,
                Some(params.text_document.version),
            )
            .await;
    }

    /// synchornize the source file content with the server and report syntax
    /// errors
    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        info!("Did change: {}", params.text_document.uri.path(),);

        // if found a change that replaces the whole content, all the changes
        // before it are ignored
        let changes = match params
            .content_changes
            .iter()
            .rposition(|x| x.range.is_none())
        {
            Some(index) => {
                self.syntax.register_source_file(
                    params.text_document.uri.clone(),
                    std::mem::take(&mut params.content_changes[index].text),
                );

                &params.content_changes[index + 1..]
            }
            None => &params.content_changes,
        };

        for change in changes {
            if let Err(err) = self.syntax.update_source_file(
                &params.text_document.uri,
                &change.text,
                change.range.unwrap(),
            ) {
                error!(
                    "failed to update source file: {}, reason: {}",
                    params.text_document.uri.path(),
                    err
                );
            }
        }

        let Some(diagnostics) =
            self.syntax.diagnose_syntax(&params.text_document.uri)
        else {
            error!(
                "failed to diagnose syntax: {}",
                params.text_document.uri.path()
            );
            return;
        };

        self.client
            .publish_diagnostics(
                params.text_document.uri,
                diagnostics,
                Some(params.text_document.version),
            )
            .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        info!("Did save: {}", params.text_document.uri.path());

        self.semantic_check(params).await;
    }
}

impl Server {
    /// Reads the workspace configuration file and sets the workspace.
    #[must_use]
    async fn configure_workspace(&self, base_uri: Url) -> bool {
        let workspace = workspace::Workspace::new(&base_uri);

        // successfully create a new workspace
        match workspace {
            Ok(workspace) => {
                self.workspace.write().replace(workspace);
                true
            }

            // workspace file does exist but failed to parse the json
            Err(error) => match error {
                workspace::NewWorkspaceError::JsonConfiguration(
                    json_file_path,
                    errors,
                ) => {
                    let Ok(uri) = Url::from_file_path(&json_file_path) else {
                        error!(
                            "failed to convert file path to uri: {}",
                            json_file_path.display()
                        );
                        return false;
                    };

                    debug!(
                        "failed to parse workspace configuration: {errors:?} \
                         {uri:?}",
                    );

                    self.client
                        .publish_diagnostics(
                            uri,
                            errors
                                .into_iter()
                                .filter_map(|x| x.to_diagnostic(true))
                                .collect(),
                            None,
                        )
                        .await;

                    false
                }

                error => {
                    error!("failed to create workspace: {}", error);
                    false
                }
            },
        }
    }

    async fn get_workspace_and_configuration_uri(&self) -> Option<(Url, Url)> {
        if let Some(workspace) = self
            .client
            .workspace_folders()
            .await
            .into_iter()
            .flatten()
            .flatten()
            .next()
        {
            let mut root_path = workspace.uri.to_file_path().ok()?;
            root_path.push("pernix.json");

            Some((workspace.uri, Url::from_file_path(root_path).ok()?))
        } else {
            None
        }
    }
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

impl Server {
    async fn semantic_check(&self, params: DidSaveTextDocumentParams) {
        // find the root file and target name based on the workspace
        // configurationc
        let (root, target_name) = 'exit: {
            let workspace = self.workspace.read();

            let Some(workspace) = workspace.as_ref() else {
                info!("no workspace found, use the saved file as a root");

                let Ok(file_path) = params.text_document.uri.to_file_path()
                else {
                    error!(
                        "failed to convert uri to file path: {}",
                        params.text_document.uri.path()
                    );
                    return;
                };

                let Some(name) = file_path.file_stem() else {
                    error!("failed to get file name: {}", file_path.display());
                    return;
                };

                if !Identifier::is_valid_identifier_string(
                    name.to_string_lossy().as_ref(),
                ) {
                    error!("invalid file name: {}", name.to_string_lossy());
                    return;
                }

                break 'exit (
                    file_path.parent().unwrap().to_path_buf(),
                    name.to_string_lossy().to_string(),
                );
            };

            (
                workspace.configuration().root_file.clone(),
                workspace.configuration().target_name.clone(),
            )
        };

        let file = match std::fs::File::open(&root) {
            Ok(file) => file,
            Err(err) => {
                error!("{}: {}", root.display(), err);
                return;
            }
        };

        let root = match SourceFile::load(file, root.clone()) {
            Ok(root) => Arc::new(root),
            Err(err) => {
                error!("{}: {}", root.display(), err);
                return;
            }
        };

        let semantic_error_storage =
            Storage::<Box<dyn pernixc_semantic::error::Error>>::new();
        let collector = DiagnosticCollector(RwLock::new(Vec::new()));

        let target = Target::parse(&root, target_name, &collector);
        let table =
            table::build(std::iter::once(target), &semantic_error_storage);

        // group the diagnostics by source file
        let mut lsp_diagnostics_by_source_file = HashMap::<_, Vec<_>>::new();

        for diagnostic in collector.0.into_inner() {
            lsp_diagnostics_by_source_file
                .entry(ByAddress(diagnostic.span.source_file().clone()))
                .or_default()
                .push(diagnostic.into_diagnostic());
        }

        if let Err(BuildTableError::Suboptimal(table)) = &table {
            for error in semantic_error_storage.into_vec() {
                let Ok(diagnostic) = error.report(table) else {
                    continue;
                };

                lsp_diagnostics_by_source_file
                    .entry(ByAddress(diagnostic.span.source_file().clone()))
                    .or_default()
                    .push(diagnostic.into_diagnostic());
            }
        }

        // report the diagnostics in batch
        for (source_file, diagnostics) in lsp_diagnostics_by_source_file {
            let Some(uri) = std::fs::canonicalize(source_file.full_path())
                .ok()
                .and_then(|x| Url::from_file_path(x).ok())
            else {
                continue;
            };

            self.client.publish_diagnostics(uri, diagnostics, None).await;
        }
    }
}
