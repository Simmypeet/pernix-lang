//! Contains the code related to project configuration and input of the compiler.

#![deny(
    missing_docs,
    missing_debug_implementations,
    missing_copy_implementations,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

use std::{
    cmp::Ordering,
    fmt::Debug,
    fs::File,
    io::{Read, Write},
    iter::Peekable,
    ops::Range,
    path::PathBuf,
    str::CharIndices,
    sync::Arc,
};

use getset::{CopyGetters, Getters};
use tempfile::TempDir;
use thiserror::Error;

/// Represents an source file input for the compiler.
#[derive(Getters)]
pub struct SourceFile {
    #[allow(dead_code)]
    source: Option<Box<dyn Source + Send + Sync + 'static>>,

    /// Gets the full path to the source file.
    #[get = "pub"]
    full_path: PathBuf,

    /// Gets the string source_code that the source file contains.
    #[get = "pub"]
    source_code: String,
    lines: Vec<Range<usize>>,
}

impl Debug for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SourceFile")
            .field("full_path", &self.full_path)
            .field("source_code", &self.source_code)
            .field("lines", &self.lines)
            .finish()
    }
}

/// A blanket trait for all types that can be used as a source for the compiler.
trait Source {}

impl Source for File {}

struct Temp {
    #[allow(dead_code)]
    temp_file: File,
    #[allow(dead_code)]
    temp_dir: TempDir,
}
impl Source for Temp {}

impl SourceFile {
    /// Is the preferred new line character that the compiler uses.
    ///
    /// Since many operating systems use different new line characters, this is the one that the
    /// compiler uses for the consistency and convenience.
    pub const NEW_LINE: char = '\n';
    /// Is the preferred new line string that the compiler uses.
    pub const NEW_LINE_STR: &'static str = "\n";

    fn new(
        full_path: PathBuf,
        mut source_code: String,
        source: Option<Box<dyn Source + Send + Sync + 'static>>,
    ) -> Arc<Self> {
        fn replace_string_inplace(s: &mut String, from: &str, to: &str) {
            let mut start = 0;
            while let Some(i) = s[start..].find(from) {
                s.replace_range(start + i..start + i + from.len(), to);
                start += i + to.len();
            }
        }

        let mut lines = Vec::new();
        let mut start = 0;

        replace_string_inplace(&mut source_code, "\r\n", Self::NEW_LINE_STR);
        replace_string_inplace(&mut source_code, "\r", Self::NEW_LINE_STR);

        // Constructs the line ranges
        for (i, c) in source_code.char_indices() {
            if c == Self::NEW_LINE {
                let new_start = i + 1;
                lines.push(start..new_start);
                start = new_start;
            }
        }

        lines.push(start..source_code.len());

        Arc::new(Self {
            source,
            full_path,
            source_code,
            lines,
        })
    }

    /// Loads a source file from the given path.
    ///
    /// The file is kept open so that it cannot be deleted or modified while the compiler is
    /// using
    ///
    /// # Errors
    /// - [`std::io::Error`] if the file cannot be read from the given path.
    pub fn load(path: &PathBuf) -> Result<Arc<Self>, std::io::Error> {
        // keep the file open so that it cannot be deleted or modified while the compiler is using
        // it
        let mut file = std::fs::File::open(path)?;

        let mut source_code = String::new();
        file.read_to_string(&mut source_code)?;
        let full_path = path.canonicalize()?;

        Ok(Self::new(full_path, source_code, Some(Box::new(file))))
    }

    /// Creates a temporary source file from the given source code.
    ///
    /// The function creates a temporary directory and a temporary file inside it. The source is
    /// written to the file. The temporary file is deleted when the [`SourceFile`] is dropped.
    ///
    /// # Errors
    /// - [`std::io::Error`] if the temporary file cannot be created, read, or write.
    pub fn temp(source: &str) -> Result<Arc<Self>, std::io::Error> {
        const TEMP_FILE_NAME: &str = "temp";

        let temp_dir = tempfile::tempdir()?;
        let temp_file_path = temp_dir.path().join(format!("{TEMP_FILE_NAME}.pnx"));
        let mut temp_file = std::fs::File::create(temp_file_path.clone())?;

        write!(&mut temp_file, "{source}")?;

        let resource = Temp {
            temp_file,
            temp_dir,
        };

        Ok(Self::new(
            temp_file_path,
            source.to_owned(),
            Some(Box::new(resource)),
        ))
    }

    /// Gets the line of the source file at the given line number.
    ///
    /// The line number starts at 1.
    #[must_use]
    pub fn get_line(&self, line: usize) -> Option<&str> {
        if line == 0 {
            return None;
        }

        let line = line - 1;
        self.lines
            .get(line)
            .map(|range| &self.source_code[range.clone()])
    }

    /// Gets the [`Iterator`] for the source file.
    #[must_use]
    pub fn iter<'a>(self: &'a Arc<Self>) -> Iterator<'a> {
        Iterator {
            source_file: self,
            iterator: self.source_code.char_indices().peekable(),
        }
    }

    /// Gets the number of lines in the source file.
    #[must_use]
    pub fn line_number(&self) -> usize {
        self.lines.len()
    }

    /// Gets the [`Location`] of the given byte index.
    #[must_use]
    pub fn get_location(&self, byte_index: ByteIndex) -> Option<Location> {
        if !self.source_code().is_char_boundary(byte_index) {
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
        let line_str = self.get_line(line + 1).unwrap();

        // gets the column number by iterating through the utf-8 characters (starts at 1)
        let column = line_str
            .char_indices()
            .take_while(|(i, _)| *i + line_starting_byte_index < byte_index)
            .count()
            + 1;

        Some(Location {
            line: line + 1,
            column,
        })
    }
}

/// Is an unsigned integer that represents a byte index in the source code.
pub type ByteIndex = usize;

/// Represents a range of characters in a source file.
#[derive(Clone, Getters, CopyGetters)]
pub struct Span {
    /// Gets the start byte index of the span.
    #[get_copy = "pub"]
    start: ByteIndex,

    /// Gets the end byte index of the span (exclusive).
    #[get_copy = "pub"]
    end: ByteIndex,

    /// Gets the source file that the span is located in.
    #[get = "pub"]
    source_file: Arc<SourceFile>,
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Span")
            .field("start", &self.start)
            .field("end", &self.end)
            .field("content", &self.str())
            .finish()
    }
}

/// Is a struct pointing to a particular location in a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Location {
    /// The line number of the location (starts at 1).
    pub line: usize,

    /// The column number of the location (starts at 1).
    pub column: usize,
}

/// Is an error that occurs related to [`Span`] operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("Is an error that occurs related to `Span` operations.")]
pub struct SpanError;

impl Span {
    /// Creates a span from the given start and end byte indices in the source file.
    ///
    /// # Parameters
    /// - `start`: The start byte index of the span.
    /// - `end`: The end byte index of the span (exclusive).
    #[must_use]
    pub fn new(source_file: Arc<SourceFile>, start: ByteIndex, end: ByteIndex) -> Option<Self> {
        if start > end
            || !source_file.source_code.is_char_boundary(start)
            || source_file.source_code().len() < end
            || (source_file.source_code().len() + 1 != end
                && !source_file.source_code().is_char_boundary(end))
        {
            return None;
        }

        Some(Self {
            start,
            end,
            source_file,
        })
    }

    /// Creates a span from the given start byte index to the end of the source file.
    #[must_use]
    pub fn to_end(source_file: Arc<SourceFile>, start: ByteIndex) -> Option<Self> {
        if !source_file.source_code().is_char_boundary(start) {
            return None;
        }
        Some(Self {
            start,
            end: source_file.source_code().len(),
            source_file,
        })
    }

    /// Gets the string slice of the source code that the span represents.
    #[must_use]
    pub fn str(&self) -> &str {
        &self.source_file.source_code()[self.start..self.end]
    }

    /// Gets the starting [`Location`] of the span.
    #[must_use]
    pub fn start_location(&self) -> Location {
        self.source_file.get_location(self.start).unwrap()
    }

    /// Gets the ending [`Location`] of the span.
    ///
    /// Returns [`None`] if the end of the span is the end of the source file.
    #[must_use]
    pub fn end_location(&self) -> Option<Location> {
        self.source_file.get_location(self.end)
    }

    /// Joins the starting position of this span with the end position of the given span.
    ///
    /// # Errors
    /// Returns [`Err`] if the spans are not in the same source file or if the end of this span is
    /// after the start of the given span.
    pub fn join(&self, end: &Self) -> Result<Self, SpanError> {
        if !Arc::ptr_eq(&self.source_file, &end.source_file) || self.start > end.end {
            return Err(SpanError);
        }

        Ok(Self {
            start: self.start,
            end: end.end,
            source_file: self.source_file.clone(),
        })
    }
}

/// Represents an element that is located within a source file.
pub trait SourceElement {
    /// Gets the span location of the element.
    ///
    /// # Errors
    /// Returns [`Err`] if the element's span cannot be properly determined.
    fn span(&self) -> Result<Span, SpanError>;
}

impl<T: SourceElement> SourceElement for Box<T> {
    fn span(&self) -> Result<Span, SpanError> {
        self.as_ref().span()
    }
}

/// Is an iterator iterating over the characters in a source file that can be peeked at.
#[derive(Debug, Clone, CopyGetters)]
pub struct Iterator<'a> {
    /// Gets the source file that the iterator is iterating over.
    #[get_copy = "pub"]
    source_file: &'a Arc<SourceFile>,
    iterator: Peekable<CharIndices<'a>>,
}

impl<'a> Iterator<'a> {
    /// Peeks at the next character in the source file.
    pub fn peek(&mut self) -> Option<(ByteIndex, char)> {
        self.iterator.peek().copied()
    }
}

impl<'a> std::iter::Iterator for Iterator<'a> {
    type Item = (ByteIndex, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.iterator.next()
    }
}

#[cfg(test)]
mod tests;
