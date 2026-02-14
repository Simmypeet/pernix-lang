//! Handles "go to definition" requests from the LSP client.

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_source_file::{Span, get_source_file_by_id, get_source_file_path};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    module_kind::get_module_kind,
    source_map::to_absolute_span,
    span::get_span,
};
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
) -> Option<GotoDefinitionResponse> {
    let symbol_at = self
        .symbol_at(
            &params.text_document_position_params.position,
            &params.text_document_position_params.text_document.uri,
            target_id,
        )
        .await?;

    let kind = self.get_kind(symbol_at).await;

    // if it's a module, we have to handle the exteranl module file case
    let absolute_span = if kind == Kind::Module {
        let kind = self.get_module_kind(symbol_at).await;

        match kind {
            pernixc_symbol::module_kind::ModuleKind::ExteranlFile(None)
            | pernixc_symbol::module_kind::ModuleKind::Inline => {
                let span = self.get_span(symbol_at).await?;

                self.to_absolute_span(&span).await
            }

            pernixc_symbol::module_kind::ModuleKind::ExteranlFile(Some(id)) => {
                Span { start: 0, end: 0, source_id: target_id.make_global(id) }
            }
        }
    } else {
        let span = self.get_span(symbol_at).await?;

        self.to_absolute_span(&span).await
    };

    let source_file = self.get_source_file_by_id(absolute_span.source_id).await;

    let start = source_file.get_location(absolute_span.start).unwrap();
    let end = source_file.get_location(absolute_span.end).unwrap();

    Some(GotoDefinitionResponse::Scalar(Location {
        uri: Url::from_file_path(
            self.get_source_file_path(absolute_span.source_id).await,
        )
        .unwrap(),
        range: Range {
            start: start.to_lsp_position(),
            end: end.to_lsp_position(),
        },
    }))
}
