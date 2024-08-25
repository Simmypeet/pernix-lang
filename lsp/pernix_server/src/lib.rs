//! Contains the server implementation.

use std::{fs::File, sync::Arc};

use log::{error, info};
use pernixc_base::source_file::SourceFile;
use pernixc_syntax::syntax_tree::target::Target;
use tower_lsp::{
    jsonrpc,
    lsp_types::{
        DidChangeTextDocumentParams, DidSaveTextDocumentParams,
        InitializeParams, InitializeResult, OneOf, ServerCapabilities,
        TextDocumentSyncCapability, TextDocumentSyncKind,
        WorkspaceFoldersServerCapabilities, WorkspaceServerCapabilities,
    },
    Client, LanguageServer,
};

pub mod syntax_diagnostic;

/// The language server protocal implementation for Pernix.
#[derive(Debug)]
pub struct Server {
    client: Client,
}

impl Server {
    /// Creates a new server instance.
    pub fn new(client: Client) -> Self { Self { client } }
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
                    TextDocumentSyncKind::FULL,
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
                // definition: Some(GotoCapability::default()),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> { Ok(()) }

    async fn did_change(&self, _: DidChangeTextDocumentParams) {}

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        info!("Did save: {}", params.text_document.uri.path());
        let Ok(file) = File::open(params.text_document.uri.path()) else {
            error!("Failed to open file: {}", params.text_document.uri.path());
            return;
        };

        let Ok(file) = SourceFile::load(
            file,
            params.text_document.uri.path().to_string().into(),
        )
        .map(Arc::new) else {
            error!(
                "Failed to load source file: {}",
                params.text_document.uri.path()
            );
            return;
        };

        let diagnostic_collector =
            syntax_diagnostic::Collector::new(file.clone());

        let _target =
            Target::parse(&file, "main".to_string(), &diagnostic_collector);

        let diagnostics = diagnostic_collector.into_diagnostic();
        let count = diagnostics.len();

        self.client
            .publish_diagnostics(params.text_document.uri, diagnostics, None)
            .await;

        info!("{count} diagnostics published");
    }
}
