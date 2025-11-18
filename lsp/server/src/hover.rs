//! Handles the implementation of hover functionality for the LSP server.

use pernixc_extend::extend;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_source_file::{
    calculate_path_id, get_source_file_by_id, SourceFile,
};
use pernixc_symbol::{
    member::try_get_members, name::get_qualified_name,
    scope_span::get_scope_span, source_file_module::get_source_file_module,
    source_map::to_absolute_span,
};
use pernixc_target::{Global, TargetID};
use tower_lsp::lsp_types::MarkupContent;

/// Handles hover requests from the LSP client.
#[extend]
pub async fn handle_hover(
    self: &TrackedEngine,
    target_id: TargetID,
    params: tower_lsp::lsp_types::HoverParams,
) -> Result<Option<tower_lsp::lsp_types::Hover>, CyclicError> {
    let source_file_path = params
        .text_document_position_params
        .text_document
        .uri
        .to_file_path()
        .unwrap();

    let source_id = target_id.make_global(
        self.calculate_path_id(&source_file_path, target_id).await,
    );
    let source_file = self.get_source_file_by_id(source_id).await;

    // get the module under the source file
    let module_id =
        target_id.make_global(self.get_source_file_module(source_id).await);

    // determine the byte position
    let byte_index = source_file.lsp_position_to_byte_index(
        params.text_document_position_params.position,
    );

    // get the most specific symbol scope at the byte index
    let symbol_scope_id =
        self.get_symbol_scope_at_byte_index(module_id, byte_index).await;

    Ok(Some(tower_lsp::lsp_types::Hover {
        contents: tower_lsp::lsp_types::HoverContents::Markup(MarkupContent {
            kind: tower_lsp::lsp_types::MarkupKind::Markdown,
            value: format!(
                "Currently in `{}`",
                self.get_qualified_name(symbol_scope_id).await
            ),
        }),
        range: None,
    }))
}

/// Converts an LSP position to a byte index in the source file.
#[extend]
pub fn lsp_position_to_byte_index(
    self: &SourceFile,
    position: tower_lsp::lsp_types::Position,
) -> pernixc_source_file::ByteIndex {
    let line_starting_byte =
        self.get_starting_byte_index_of_line(position.line as usize).unwrap();

    let line_str = self.get_line(position.line as usize).unwrap();

    let mut byte_index = line_starting_byte;
    for (i, ch) in line_str.char_indices() {
        if i >= position.character as usize {
            break;
        }
        byte_index += ch.len_utf8();
    }

    byte_index
}

/// Retrieves the most specific symbol scope that contains the given byte
/// index.
#[extend]
pub async fn get_symbol_scope_at_byte_index(
    self: &TrackedEngine,
    current_scope_id: Global<pernixc_symbol::ID>,
    byte_index: pernixc_source_file::ByteIndex,
) -> Global<pernixc_symbol::ID> {
    let Some(members) = self.try_get_members(current_scope_id).await else {
        return current_scope_id;
    };

    for member in members
        .member_ids_by_name
        .values()
        .chain(members.unnameds.iter())
        .copied()
        .map(|x| current_scope_id.target_id.make_global(x))
    {
        // if the member's span contains the byte index, recurse into it
        let Some(span) = self.get_scope_span(member).await else {
            continue;
        };

        let abs_span = self.to_absolute_span(&span).await;
        if abs_span.range().contains(&byte_index) {
            return Box::pin(
                self.get_symbol_scope_at_byte_index(member, byte_index),
            )
            .await;
        }
    }

    current_scope_id
}
