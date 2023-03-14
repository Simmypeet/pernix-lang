use std::{iter::Peekable, ops::Range, path::PathBuf, str::Chars};

use derive_new::new;
use enum_as_inner::EnumAsInner;
use getset::Getters;

/// Represents an source file input for the compiler.
#[derive(Debug, Getters, Clone, PartialEq, Eq, Hash)]
pub struct SourceFile {
    /// Gets the name of the source file.
    #[get = "pub"]
    name: String,

    /// Gets teh string content that the source file contains.
    #[get = "pub"]
    content: String,
    lines:   Vec<Range<usize>>,
}

impl SourceFile {
    /// Is the preferred new line character that the compiler uses.
    ///
    /// Since many operating systems use different new line characters, this is the one that the
    /// compiler uses for the consistency and convenience.
    pub const NEW_LINE: char = '\n';
    /// Is the preferred new line string that the compiler uses.
    pub const NEW_LINE_STR: &'static str = "\n";

    /// Creates a new source file from the given name and content.
    pub fn new(name: String, mut content: String) -> SourceFile {
        let mut lines = Vec::new();
        let mut start = 0;

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

        // Constructs the line ranges
        for (i, c) in content.char_indices() {
            if c == Self::NEW_LINE {
                let new_start = i + 1;
                lines.push(start..new_start);
                start = new_start;
            }

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

    /// Loads a source file from the given path.
    pub fn load(path: &PathBuf) -> std::io::Result<SourceFile> {
        let name = path.to_str().unwrap().to_string();
        let content = std::fs::read_to_string(path)?;

        Ok(SourceFile::new(name, content))
    }

    /// Gets the line of the source file at the given line number.
    ///
    /// The line number starts at 1.
    pub fn get_line(&self, line: usize) -> Option<&str> {
        if line == 0 {
            return None;
        }

        let line = line - 1;
        self.lines
            .get(line)
            .map(|range| &self.content[range.clone()])
    }

    /// Gets the source file iterator for this source file.
    pub fn iter(&self) -> SourceFileIterator {
        SourceFileIterator {
            source_file: self,
            chars:       self.content.chars().peekable(),
            line:        1,
            column:      1,
            byte:        0,
        }
    }
}

/// Represents an iterator over the characters of a source file.
///
/// It outputs the [`Location`] along with the character when iterating. Moreover, it also provides
/// [peek()](Self::peek()) function right out of the box without the need to use the [`Peekable`]
/// iterator adapter.
#[derive(Clone, Debug)]
pub struct SourceFileIterator<'a> {
    source_file: &'a SourceFile,
    chars:       Peekable<Chars<'a>>,
    line:        usize,
    column:      usize,
    byte:        usize,
}

impl<'a> Iterator for SourceFileIterator<'a> {
    type Item = (Location, char);

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars.next()?;
        let location = Location {
            line:   self.line,
            column: self.column,
            byte:   self.byte,
        };

        self.byte += c.len_utf8();

        if c == '\r' || c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        Some((location, c))
    }
}

impl<'a> SourceFileIterator<'a> {
    /// Gets the source file that this iterator is iterating over.
    pub fn source_file(&self) -> &'a SourceFile { self.source_file }

    /// Peeks the next character in the source file without consuming it.
    pub fn peek(&mut self) -> Option<(Location, char)> {
        let c = self.chars.peek()?;
        let location = Location {
            line:   self.line,
            column: self.column,
            byte:   self.byte,
        };

        Some((location, *c))
    }
}

/// Represents a particular location in a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, new)]
pub struct Location {
    /// The additional line number data of the location.
    ///
    /// The line number starts at 1.
    pub line: usize,

    /// The additional column number data of the location.
    ///
    /// The column number starts at 1.
    pub column: usize,

    /// Is the byte position of the location in the source file.
    pub byte: usize,
}

/// Describes how the range in the [`Span`] ends.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum SpanEnding {
    /// The span ends at a particular location in the source file.
    Location(Location),

    /// The span covers to the end of the source file.
    EndOfFile,
}

/// Represents a range of characters in a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct Span {
    /// The starting location of the span.
    pub start: Location,

    /// The ending location of the span (exclusive).
    pub end: SpanEnding,
}

#[cfg(test)]
mod tests;

