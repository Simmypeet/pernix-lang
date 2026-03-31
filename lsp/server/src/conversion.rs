//! Converts between Pernix editor locations and LSP positions.

use pernixc_extend::extend;
use pernixc_source_file::{EditorLocation, SourceFile};

/// Converts the given editor location to an LSP position.
#[extend]
#[allow(clippy::cast_possible_truncation)]
pub fn editor_location_to_lsp_position(
    self: &SourceFile,
    editor_location: &EditorLocation,
) -> tower_lsp::lsp_types::Position {
    let mut utf16_count = 0;

    if let Some(line_str) = self.get_line(editor_location.line) {
        for c in line_str.chars().take(editor_location.column) {
            utf16_count += c.len_utf16();
        }
    }

    tower_lsp::lsp_types::Position {
        line: editor_location.line as u32,
        character: utf16_count as u32,
    }
}

/// Converts the given LSP position to a Pernix editor location.
#[extend]
pub fn lsp_position_to_editor_location(
    self: &SourceFile,
    position: &tower_lsp::lsp_types::Position,
) -> EditorLocation {
    let mut utf16_count = 0;
    let mut char_count = 0;

    if let Some(line_str) = self.get_line(position.line as usize) {
        for c in line_str.chars() {
            if utf16_count >= position.character as usize {
                break;
            }
            utf16_count += c.len_utf16();
            char_count += 1;
        }
    }

    EditorLocation { line: position.line as usize, column: char_count }
}

/// Converts the given LSP position to a byte index.
#[extend]
pub fn lsp_position_to_byte_index(
    self: &SourceFile,
    position: &tower_lsp::lsp_types::Position,
) -> Option<pernixc_diagnostic::ByteIndex> {
    let editor_location = self.lsp_position_to_editor_location(position);
    self.get_byte_index_from_editor_location(&editor_location)
}
