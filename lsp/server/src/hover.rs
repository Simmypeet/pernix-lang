//! Handles the implementation of hover functionality for the LSP server.

use pernixc_extend::extend;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_symbol::name::get_qualified_name;
use pernixc_target::TargetID;
use tower_lsp::lsp_types::MarkupContent;

use crate::pointing::symbol_at;

/// Handles hover requests from the LSP client.
#[extend]
pub async fn handle_hover(
    self: &TrackedEngine,
    target_id: TargetID,
    params: tower_lsp::lsp_types::HoverParams,
) -> Result<Option<tower_lsp::lsp_types::Hover>, CyclicError> {
    let Some(symbol) = self
        .symbol_at(
            &params.text_document_position_params.position,
            &params.text_document_position_params.text_document.uri,
            target_id,
        )
        .await?
    else {
        return Ok(None);
    };

    Ok(Some(tower_lsp::lsp_types::Hover {
        contents: tower_lsp::lsp_types::HoverContents::Markup(MarkupContent {
            kind: tower_lsp::lsp_types::MarkupKind::Markdown,
            value: format!(
                "Pointing at `{}`",
                self.get_qualified_name(symbol).await
            ),
        }),
        range: None,
    }))
}
