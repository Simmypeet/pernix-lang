use std::{ops::Range, path::PathBuf, str::Chars};

use getset::Getters;
use thiserror::Error;

/// Is a struct that contains the name and content of the source file.
#[derive(Debug, Getters)]
pub struct SourceFile {
    /// Gets the name of the source file.
    #[get = "pub"]
    name: String,

    /// Gets the string content of the source file.
    #[get = "pub"]
    content: String,
    lines: Vec<Range<usize>>,
}

impl SourceFile {
    /// For convenience, this is the new line character that is used in the source file.
    pub const NEW_LINE: char = '\n';

    /// For convenience, this is the new line character that is used in the source file.
    pub const NEW_LINE_STR: &'static str = "\n";

    /// Creates a new [`SourceFile`] from the given name and content.
    ///
    /// # Modifications
    ///
    /// In order to make the source file easier to work with, the following modifications are made:
    /// - All `\r\n` and `\r` are replaced with `\n`
    /// - Trailing new line characters are removed
    pub fn new(name: String, mut content: String) -> SourceFile {
        let mut lines = Vec::new();
        let mut start = 0;

        // Since the String::replace method allocates a new string, we use this function to modify
        // the string in place
        fn replace_string_inplace(s: &mut String, from: &str, to: &str) {
            let mut start = 0;
            while let Some(i) = s[start..].find(from) {
                s.replace_range(start + i..start + i + from.len(), to);
                start += i + to.len();
            }
        }

        replace_string_inplace(&mut content, "\r\n", Self::NEW_LINE_STR);
        replace_string_inplace(&mut content, "\r", Self::NEW_LINE_STR);

        while content.ends_with('\n') {
            content.pop();
        }

        for (i, c) in content.char_indices() {
            // The new line character is included in the line range
            if c == Self::NEW_LINE {
                let new_start = i + 1;
                lines.push(start..new_start);
                start = new_start;
            }

            // Ff the last line doesn't end with a new line character
            if i == content.len() - 1 {
                lines.push(start..content.len());
            }
        }

        SourceFile {
            name,
            content,
            lines,
        }
    }

    /// Loads a [`SourceFile`] from the given path.
    pub fn load(path: &PathBuf) -> std::io::Result<SourceFile> {
        let name = path.to_str().unwrap().to_string();
        let content = std::fs::read_to_string(path)?;

        Ok(SourceFile::new(name, content))
    }

    /// Gets the string content of the line at the given line number. The first line is 1.
    pub fn get_line(&self, line: usize) -> Option<&str> {
        // line - 1 could cause an underflow
        if line == 0 {
            return None;
        }

        let line = line - 1;
        self.lines
            .get(line)
            .map(|range| &self.content[range.clone()])
    }

    /// Gets the iterator over the characters of this [`SourceFile`]. The iterator is UTF-8 aware.
    pub fn iter(&self) -> SourceFileIterator {
        SourceFileIterator {
            source: self,
            chars: self.content.chars(),
            line: 1,
            column: 1,
            byte: 0,
        }
    }
}

/// Is an iterator over the characters of a [`SourceFile`]. It outputs a pair of a [`Location`] and
/// a character. The iterator is UTF-8 aware.
#[derive(Clone, Debug)]
pub struct SourceFileIterator<'a> {
    source: &'a SourceFile,
    chars: Chars<'a>,
    line: usize,
    column: usize,
    byte: usize,
}

impl<'a> Iterator for SourceFileIterator<'a> {
    type Item = (SourceLocation<'a>, char);

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars.next()?;
        let location = SourceLocation {
            source_file: self.source,
            line: self.line,
            column: self.column,
            byte: self.byte,
        };

        self.byte += c.len_utf8();

        // if the character is a new line character, we need to increment the line number and reset
        // the column number
        if c == '\r' || c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        Some((location, c))
    }
}

/// Represents a location in a [`SourceFile`].
#[derive(Debug, Clone, Copy, Getters)]
pub struct SourceLocation<'a> {
    /// Gets the reference to the source file that this location is in.
    #[get = "pub"]
    source_file: &'a SourceFile,

    /// Gets the line number of this location. The first line is 1.   
    #[get = "pub"]
    line: usize,

    /// Gets the column number of this location. The first column is 1.
    #[get = "pub"]
    column: usize,

    /// Gets the byte number of this location. The first byte is 0.
    #[get = "pub"]
    byte: usize,
}

/// Represents a location in a text file consisting of lines, columns and bytes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    /// Is the line number of this location. The first line is 1.
    pub line: usize,

    /// Is the column number of this location. The first column is 1.
    pub column: usize,

    /// Is the byte number of this location. The first byte is 0.
    pub byte: usize,
}

/// Desctibes how a [`Span`] range ends.
#[derive(Debug, Clone, Copy)]
pub enum SpanEnding {
    /// The span ends at the start of the given location (the given location is not included).
    Location(Location),

    /// The span ends to the end of the source file.
    EndOfFile,
}

/// Represents a span in a [`SourceFile`].
#[derive(Debug, Clone, Copy, Getters)]
pub struct Span<'a> {
    /// Gets the reference to the source file that this span is in.
    #[get = "pub"]
    source_file: &'a SourceFile,

    /// Gets the start location of this span.
    #[get = "pub"]
    start: Location,

    /// Gets the end location of this span.
    #[get = "pub"]
    end: SpanEnding,
}

impl<'a> Span<'a> {
    /// Creates a new [`Span`] from the given start and end locations.
    ///
    /// # Errors
    /// - If the references to the source files of the start and end locations are not the same.
    /// - If the location range is empty or invalid.
    pub fn from_source_location(
        start: SourceLocation<'a>,
        end: SourceLocation<'a>,
    ) -> Result<Self, SpanError> {
        if !std::ptr::eq(start.source_file, end.source_file) {
            return Err(SpanError::SourceFileMismatch);
        }

        if start.byte >= end.byte {
            return Err(SpanError::InvalidLocation);
        }

        Ok(Span {
            source_file: start.source_file,
            start: Location {
                line: start.line,
                column: start.column,
                byte: start.byte,
            },
            end: SpanEnding::Location(Location {
                line: end.line,
                column: end.column,
                byte: end.byte,
            }),
        })
    }

    /// Creates a new [`Span`] from the given start location to the end of the source file.
    pub fn from_source_location_to_end(start: SourceLocation<'a>) -> Self {
        Span {
            source_file: start.source_file,
            start: Location {
                line: start.line,
                column: start.column,
                byte: start.byte,
            },
            end: SpanEnding::EndOfFile,
        }
    }

    /// Gets the string that this span represents.
    pub fn string(&self) -> &'a str {
        match self.end {
            SpanEnding::Location(end) => &self.source_file.content[self.start.byte..end.byte],
            SpanEnding::EndOfFile => &self.source_file.content[self.start.byte..],
        }
    }
}

/// Is an error that can occur when creating a [`Span`].
#[derive(Debug, Clone, Copy, Error)]
pub enum SpanError {
    #[error("The reference to the source file between the two locations are different.")]
    SourceFileMismatch,

    #[error("The given location range is invalid. Either the span is empty or the start location is after the end location.")]
    InvalidLocation,
}

#[cfg(test)]
mod tests;
