//! Handles "go to definition" requests from the LSP client.

use pernixc_extend::extend;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_source_file::{get_source_file_by_id, get_source_file_path};
use pernixc_symbol::{source_map::to_absolute_span, span::get_span};
use pernixc_target::TargetID;
use tower_lsp::lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, Location, Range, Url,
};

use crate::{conversion::to_lsp_position, pointing::symbol_at};

/// Handles "go to definition" requests from the LSP client.
#[extend]
pub async fn handle_goto_definition(
    self: &TrackedEngine,
    target_id: TargetID,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>, CyclicError> {
    let Some(symbol_at) = self
        .symbol_at(
            &params.text_document_position_params.position,
            &params.text_document_position_params.text_document.uri,
            target_id,
        )
        .await?
    else {
        return Ok(None);
    };

    let Some(span) = self.get_span(symbol_at).await else {
        return Ok(None);
    };

    let absolute_span = self.to_absolute_span(&span).await;
    let source_file = self.get_source_file_by_id(absolute_span.source_id).await;

    let start = source_file.get_location(absolute_span.start).unwrap();
    let end = source_file.get_location(absolute_span.end).unwrap();

    Ok(Some(GotoDefinitionResponse::Scalar(Location {
        uri: Url::from_file_path(
            self.get_source_file_path(absolute_span.source_id).await,
        )
        .unwrap(),
        range: Range {
            start: start.to_lsp_position(),
            end: end.to_lsp_position(),
        },
    })))
}
