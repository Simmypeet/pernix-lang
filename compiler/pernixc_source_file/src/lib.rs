//! Contains the code related to the source code input.

use core::str;
use std::{
    cmp::Ordering, fmt::Debug, fs::File, io::Read, ops::Range, path::PathBuf,
};

use getset::{CopyGetters, Getters};
use thiserror::Error;

/// Represents an error that occurs when loading/creating a source file.
#[derive(Debug, Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),

    #[error(transparent)]
    Utf8(#[from] std::str::Utf8Error),
}

/// Represents an source file input for the compiler.
#[derive(Clone, PartialEq, Eq, Hash, Getters)]
pub struct SourceFile {
    content: String,

    /// Gets the full path to the source file.
    #[get = "pub"]
    full_path: PathBuf,

    /// The byte ranges for each line in the source file (including the
    /// newline)
    lines: Vec<Range<usize>>,
}

impl PartialOrd for SourceFile {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SourceFile {
    fn cmp(&self, other: &Self) -> Ordering {
        self.full_path
            .cmp(&other.full_path)
            .then(self.content.cmp(&other.content))
    }
}

#[allow(clippy::missing_fields_in_debug)]
impl Debug for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SourceFile")
            .field("full_path", &self.full_path)
            .field("lines", &self.lines)
            .finish()
    }
}

impl SourceFile {
    /// Creates a new inline source file
    #[must_use]
    pub fn new(content: String, full_path: PathBuf) -> Self {
        let lines = get_line_byte_positions(&content);
        Self { content, full_path, lines }
    }

    /// Gets the content of the source file.
    #[must_use]
    pub fn content(&self) -> &str { &self.content }

    /// Translates a location to a byte index.
    #[must_use]
    pub fn into_byte_index(&self, location: Location) -> Option<ByteIndex> {
        let start = self.lines.get(location.line)?.start;
        let line = self.get_line(location.line)?;

        line.char_indices()
            .take(location.column + 1)
            .last()
            .map(|x| x.0 + start)
    }

    /// Translates a location to a byte index. This includes the ending byte.
    #[must_use]
    pub fn into_byte_index_include_ending(
        &self,
        location: Location,
    ) -> Option<ByteIndex> {
        let range = self.lines.get(location.line)?;
        let line = self.get_line(location.line)?;

        let mut index = 0;

        for (byte_idx, _) in line.char_indices() {
            if location.column == index {
                return Some(byte_idx + range.start);
            }

            index += 1;
        }

        if index == location.column {
            Some(range.end)
        } else {
            None
        }
    }

    /// Replaces the content of the source file in the given range with the
    /// given string.
    #[allow(
        clippy::cast_possible_wrap,
        clippy::cast_sign_loss,
        clippy::range_plus_one,
        clippy::missing_panics_doc
    )]
    pub fn replace_range(&mut self, range: Range<ByteIndex>, string: &str) {
        assert!(
            range.start <= self.content.len(),
            "out of bound start index {}, content length {}",
            range.start,
            self.content.len()
        );
        assert!(
            range.end <= self.content.len(),
            "out of bound end index {}, content length {}",
            range.end,
            self.content.len()
        );
        assert!(
            range.start <= range.end,
            "start index {} is greater than end index {}",
            range.start,
            range.end
        );

        // just append the string
        if range.start == range.end && range.start == self.content.len() {
            let prior_len = self.content.len();
            self.content.push_str(string);

            if !string.contains('\n') && !string.contains('\r') {
                self.lines.last_mut().unwrap().end += string.len();
            } else {
                let mut new_line_changes = get_line_byte_positions(string);

                for new_range in &mut new_line_changes {
                    new_range.start += prior_len;
                    new_range.end += prior_len;
                }

                let last_line = self.lines.last_mut().unwrap();

                last_line.end = new_line_changes[0].end;

                self.lines.extend(new_line_changes.into_iter().skip(1));
            }

            return;
        }

        assert!(
            self.content.is_char_boundary(range.start),
            "start index {} is not a char boundary",
            range.start
        );

        assert!(
            self.content.is_char_boundary(range.end),
            "end index {} is not a char boundary",
            range.end
        );

        self.content.replace_range(range.clone(), string);

        // update the line ranges
        let start_line = self.get_line_of_byte_index(range.start).unwrap();
        let end_line = self
            .get_line_of_byte_index(range.end)
            .unwrap_or(self.lines.len() - 1);

        let character_difference =
            string.len() as isize - (range.end - range.start) as isize;

        // no new lines, we can skip the line calculations for new string
        if !string.contains('\n') && !string.contains('\r') {
            let removing_lines = start_line + 1..end_line + 1;

            self.lines[start_line].end = (self.lines[end_line].end as isize
                + character_difference)
                as usize;

            self.lines.drain(removing_lines);

            for i in start_line + 1..self.lines.len() {
                self.lines[i].start = (self.lines[i].start as isize
                    + character_difference)
                    as usize;
                self.lines[i].end = (self.lines[i].end as isize
                    + character_difference)
                    as usize;
            }
        } else {
            let mut new_line_ranges = get_line_byte_positions(string);
            for new_range in &mut new_line_ranges {
                new_range.start += range.start;
                new_range.end += range.start;
            }
            let end_after_count = self.lines[end_line].end - range.end;
            self.lines[start_line].end = new_line_ranges[0].end;

            let removing_lines = start_line + 1..end_line + 1;
            self.lines.drain(removing_lines);

            self.lines.splice(
                (start_line + 1)..(start_line + 1),
                (1..(new_line_ranges.len() - 1))
                    .map(|x| new_line_ranges[x].clone()),
            );

            self.lines.insert(
                start_line + new_line_ranges.len() - 1,
                new_line_ranges.last().unwrap().start
                    ..new_line_ranges.last().unwrap().end + end_after_count,
            );

            for i in (start_line + new_line_ranges.len())..self.lines.len() {
                self.lines[i].start = (self.lines[i].start as isize
                    + character_difference)
                    as usize;
                self.lines[i].end = (self.lines[i].end as isize
                    + character_difference)
                    as usize;
            }
        }
    }

    /// Determines in which line number the given byte index is located.
    #[must_use]
    pub fn get_line_of_byte_index(
        &self,
        byte_index: ByteIndex,
    ) -> Option<usize> {
        // gets the line number by binary searching the line ranges
        self.lines
            .binary_search_by(|range| {
                if range.contains(&byte_index) {
                    Ordering::Equal
                } else if byte_index < range.start {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            })
            .ok()
    }

    /// Gets the line of the source file at the given line number.
    ///
    /// The line number starts at 0.
    #[must_use]
    pub fn get_line(&self, line: usize) -> Option<&str> {
        self.lines.get(line).map(|range| &self.content[range.clone()])
    }

    /// Replaces the content of the source file with the given content.
    pub fn replace(&mut self, content: String) {
        self.lines = get_line_byte_positions(&content);
        self.content = content;
    }

    /// Gets the number of lines in the source file.
    #[must_use]
    pub fn line_coount(&self) -> usize { self.lines.len() }

    /// Loads the source file from the given file path.
    ///
    /// # Errors
    /// - [`Error::Io`]: Error occurred when mapping the file to memory.
    /// - [`Error::Utf8`]: Error occurred when converting the mapped bytes to a
    ///   string.
    pub fn load(mut file: File, path: PathBuf) -> Result<Self, Error> {
        let mut string = Vec::new();
        file.read_to_end(&mut string)?;

        let string = String::from_utf8(string).map_err(|x| x.utf8_error())?;

        Ok(Self::new(string, path))
    }

    /// Gets the [`Location`] of the given byte index.
    #[must_use]
    #[allow(clippy::missing_panics_doc)]
    pub fn get_location(&self, byte_index: ByteIndex) -> Option<Location> {
        if !self.content.is_char_boundary(byte_index) {
            return None;
        }

        // gets the line number by binary searching the line ranges
        let line = self
            .lines
            .binary_search_by(|range| {
                if range.contains(&byte_index) {
                    Ordering::Equal
                } else if byte_index < range.start {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            })
            .ok()?;

        let line_starting_byte_index = self.lines[line].start;
        let line_str = self.get_line(line).unwrap();

        // gets the column number by iterating through the utf-8 characters
        let column = line_str
            .char_indices()
            .take_while(|(i, _)| *i + line_starting_byte_index < byte_index)
            .count();

        Some(Location { line, column })
    }
}

/// Is an unsigned integer that represents a byte index in the source code.
pub type ByteIndex = usize;

/// Represents a range of characters in a source file.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Getters,
    CopyGetters,
)]
pub struct Span {
    /// Gets the start byte index of the span.
    pub start: ByteIndex,

    /// Gets the end byte index of the span (exclusive).
    pub end: ByteIndex,
}

/// Is a struct pointing to a particular location in a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Location {
    /// The line number of the location (starts at 0).
    pub line: usize,

    /// The column number of the location (starts at 0).
    pub column: usize,
}

impl Location {
    /// Creates a new location with the given line and column numbers.
    #[must_use]
    pub const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl Span {
    /// Creates a span from the given start and end byte indices in the source
    /// file.
    #[must_use]
    pub fn new(start: ByteIndex, end: ByteIndex) -> Self {
        assert!(start <= end, "start index is greater than end index");

        Self { start, end }
    }

    /// Creates a span from the given start byte index to the end of the source
    /// file.
    ///
    /// # Returns
    ///
    /// Returns [`None`] if the `start` index is not a character boundary.
    #[must_use]
    pub fn to_end(source_file: &SourceFile, start: ByteIndex) -> Self {
        assert!(
            source_file.content.is_char_boundary(start),
            "start index `{start}` is out of bounds"
        );

        Self { start, end: source_file.content.len() }
    }

    /// Gets the starting [`Location`] of the span.
    ///
    /// Returns [`None`] if the start of the span is at the end of the source
    /// file.
    #[must_use]
    pub fn start_location(&self, source_file: &SourceFile) -> Option<Location> {
        source_file.get_location(self.start)
    }

    /// Gets the ending [`Location`] of the span.
    ///
    /// Returns [`None`] if the end of the span is the end of the source file.
    #[must_use]
    pub fn end_location(&self, source_file: &SourceFile) -> Option<Location> {
        source_file.get_location(self.end)
    }

    /// Joins the starting position of this span with the end position of the
    /// given span.
    #[must_use]
    pub fn join(&self, end: &Self) -> Self {
        assert!(
            self.start <= end.start,
            "start index is greater than end index"
        );

        Self { start: self.start, end: end.end }
    }
}

/// Represents an element that is located within a source file.
pub trait SourceElement {
    /// Gets the span location of the element.
    fn span(&self) -> Span;
}

impl<T: SourceElement> SourceElement for Box<T> {
    fn span(&self) -> Span { self.as_ref().span() }
}

fn get_line_byte_positions(text: &str) -> Vec<Range<usize>> {
    let mut current_position = 0;
    let mut results = Vec::new();

    let mut skip = false;

    for (byte, char) in text.char_indices() {
        if skip {
            skip = false;
            continue;
        }

        // ordinary lf
        if char == '\n' {
            #[allow(clippy::range_plus_one)]
            results.push(current_position..byte + 1);

            current_position = byte + 1;
        }

        // crlf
        if char == '\r' {
            if text.as_bytes().get(byte + 1) == Some(&b'\n') {
                #[allow(clippy::range_plus_one)]
                results.push(current_position..byte + 2);

                current_position = byte + 2;

                skip = true;
            } else {
                #[allow(clippy::range_plus_one)]
                results.push(current_position..byte + 1);

                current_position = byte + 1;
            }
        }
    }

    results.push(current_position..text.len());

    results
}

#[cfg(test)]
mod test;
