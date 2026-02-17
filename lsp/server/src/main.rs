//! The main entry point for the LSP server.

use pernix_server::Server;
use tower_lsp::LspService;
use tracing_subscriber::{
    EnvFilter, Layer, layer::SubscriberExt, util::SubscriberInitExt as _,
};

#[tokio::main]
async fn main() {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_writer(std::io::stderr)
                .with_thread_ids(true)
                .with_thread_names(true)
                .with_span_events(
                    tracing_subscriber::fmt::format::FmtSpan::CLOSE,
                )
                .with_filter(
                    EnvFilter::try_from_env("PERNIXC_LOG")
                        .unwrap_or_else(|_| "INFO".into()),
                ),
        )
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(Server::new).finish();

    tower_lsp::Server::new(stdin, stdout, socket).serve(service).await;
}
