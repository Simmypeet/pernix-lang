//! Converts between Pernix editor locations and LSP positions.

use pernixc_extend::extend;
use pernixc_source_file::EditorLocation;

/// Converts the given editor location to an LSP position.
#[extend]
#[allow(clippy::cast_possible_truncation)]
pub fn to_lsp_position(self: EditorLocation) -> tower_lsp::lsp_types::Position {
    tower_lsp::lsp_types::Position {
        line: self.line as u32,
        character: self.column as u32,
    }
}
