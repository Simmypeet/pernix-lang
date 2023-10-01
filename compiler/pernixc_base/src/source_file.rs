#![allow(clippy::future_not_send)]

//! Contains the code related to the source code input.

use std::{
    cmp::Ordering,
    fmt::{Debug, Display},
    fs::File,
    iter::Peekable,
    ops::Range,
    path::PathBuf,
    str::CharIndices,
    sync::Arc,
};

use getset::{CopyGetters, Getters};
use memmap::MmapOptions;
use ouroboros::self_referencing;
use thiserror::Error;

/// Represents an error that occurs when loading/creating a source file.
#[derive(Debug, Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] std::io::Error),

    #[error(transparent)]
    Utf8Error(#[from] std::str::Utf8Error),
}

/// Represents an source file input for the compiler.
#[derive(Getters)]
pub struct SourceFile {
    source: MappedSource,

    /// Gets the full path to the source file.
    #[get = "pub"]
    full_path: PathBuf,

    /// Gets the string source.content that the source file contains.
    lines: Vec<Range<usize>>,
}

impl Debug for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SourceFile")
            .field("full_path", &self.full_path)
            .field("lines", &self.lines)
            .finish()
    }
}

#[self_referencing]
struct MappedSource {
    file: File,
    mapped: Option<memmap::Mmap>,

    #[borrows(mapped)]
    mapped_str: &'this str,
}

impl MappedSource {
    pub fn create(file: File) -> Result<Self, Error> {
        let mapped = if file.metadata().unwrap().len() == 0 {
            None
        } else {
            Some(unsafe { MmapOptions::new().map(&file)? })
        };
        MappedSourceTryBuilder {
            file,
            mapped,
            mapped_str_builder: |mapped| {
                #[allow(clippy::option_if_let_else)]
                if let Some(mmaped) = mapped {
                    std::str::from_utf8(mmaped).map_err(Error::from)
                } else {
                    Ok("")
                }
            },
        }
        .try_build()
    }

    /// Gets the string source.content that the source file contains.
    #[must_use]
    pub fn content(&self) -> &str { self.borrow_mapped_str() }
}

impl SourceFile {
    fn new(full_path: PathBuf, source: MappedSource) -> Arc<Self> {
        let lines = get_line_byte_positions(source.content());
        Arc::new(Self {
            source,
            full_path,
            lines,
        })
    }

    /// Gets the content of the source file.
    #[must_use]
    pub fn content(&self) -> &str { self.source.content() }

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
            .map(|range| &self.source.content()[range.clone()])
    }

    /// Gets the [`Iterator`] for the source file.
    #[must_use]
    pub fn iter<'a>(self: &'a Arc<Self>) -> Iterator<'a> {
        Iterator {
            source_file: self,
            iterator: self.source.content().char_indices().peekable(),
        }
    }

    /// Gets the number of lines in the source file.
    #[must_use]
    pub fn line_number(&self) -> usize { self.lines.len() }

    /// Loads the source file from the given file path.
    ///
    /// # Errors
    /// - [`Error::IoError`]: Error occurred when mapping the file to memory.
    /// - [`Error::Utf8Error`]: Error occurred when converting the mapped bytes to a string.
    pub fn load(file: File, path: PathBuf) -> Result<Arc<Self>, Error> {
        let source = MappedSource::create(file)?;
        Ok(Self::new(path, source))
    }

    /// Creates a temporary source file and writes the given displayable object to it.
    ///
    /// # Errors
    /// - [`Error::IoError`]: Error occurred when creating the temporary file, writing to, and
    ///   mapping it to memory.
    /// - [`Error::Utf8Error`]: Error occurred when converting the mapped bytes to a string.
    pub fn temp(display: impl Display) -> Result<Arc<Self>, Error> {
        use std::io::Write;

        let mut tempfile = tempfile::Builder::new()
            .prefix("flux")
            .suffix(".flux")
            .tempfile()?;

        write!(tempfile.as_file_mut(), "{display}")?;
        let path = tempfile.path().to_owned();

        Self::load(tempfile.into_file(), path)
    }

    /// Gets the [`Location`] of the given byte index.
    #[must_use]
    pub fn get_location(&self, byte_index: ByteIndex) -> Option<Location> {
        if !self.source.content().is_char_boundary(byte_index) {
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

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.source_file, &other.source_file)
            && self.start == other.start
            && self.end == other.end
    }
}

impl Eq for Span {}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let self_ptr_value = Arc::as_ptr(&self.source_file) as usize;
        let other_ptr_value = Arc::as_ptr(&other.source_file) as usize;

        Some(self_ptr_value.cmp(&other_ptr_value).then_with(|| {
            self.start
                .cmp(&other.start)
                .then_with(|| self.end.cmp(&other.end))
        }))
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Self) -> Ordering {
        let self_ptr_value = Arc::as_ptr(&self.source_file) as usize;
        let other_ptr_value = Arc::as_ptr(&other.source_file) as usize;

        self_ptr_value
            .cmp(&other_ptr_value)
            .then_with(|| self.start.cmp(&other.start))
            .then_with(|| self.end.cmp(&other.end))
    }
}

impl std::hash::Hash for Span {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.end.hash(state);
        Arc::as_ptr(&self.source_file).hash(state);
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

impl Span {
    /// Creates a span from the given start and end byte indices in the source file.
    ///
    /// # Parameters
    /// - `start`: The start byte index of the span.
    /// - `end`: The end byte index of the span (exclusive).
    #[must_use]
    pub fn new(source_file: Arc<SourceFile>, start: ByteIndex, end: ByteIndex) -> Option<Self> {
        if start > end
            || !source_file.source.content().is_char_boundary(start)
            || source_file.source.content().len() < end
            || (source_file.source.content().len() + 1 != end
                && !source_file.source.content().is_char_boundary(end))
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
        if !source_file.source.content().is_char_boundary(start) {
            return None;
        }
        Some(Self {
            start,
            end: source_file.source.content().len(),
            source_file,
        })
    }

    /// Gets the string slice of the source code that the span represents.
    #[must_use]
    pub fn str(&self) -> &str { &self.source_file.source.content()[self.start..self.end] }

    /// Gets the starting [`Location`] of the span.
    #[must_use]
    pub fn start_location(&self) -> Location { self.source_file.get_location(self.start).unwrap() }

    /// Gets the ending [`Location`] of the span.
    ///
    /// Returns [`None`] if the end of the span is the end of the source file.
    #[must_use]
    pub fn end_location(&self) -> Option<Location> { self.source_file.get_location(self.end) }

    /// Joins the starting position of this span with the end position of the given span.
    #[must_use]
    pub fn join(&self, end: &Self) -> Option<Self> {
        if !Arc::ptr_eq(&self.source_file, &end.source_file) || self.start > end.end {
            return None;
        }

        Some(Self {
            start: self.start,
            end: end.end,
            source_file: self.source_file.clone(),
        })
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
    pub fn peek(&mut self) -> Option<(ByteIndex, char)> { self.iterator.peek().copied() }
}

impl<'a> std::iter::Iterator for Iterator<'a> {
    type Item = (ByteIndex, char);

    fn next(&mut self) -> Option<Self::Item> { self.iterator.next() }
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
mod tests;
