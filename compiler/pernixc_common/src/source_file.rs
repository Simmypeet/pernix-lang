//! Contains the [`SourceFile`] type which represents a source file input for the compiler.

use std::{cmp::Ordering, iter::Peekable, ops::Range, path::PathBuf, str::CharIndices, sync::Arc};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use thiserror::Error;

/// Represents an source file input for the compiler.
#[derive(Debug, Getters, Clone, PartialEq, Eq, Hash)]
pub struct SourceFile {
    /// Gets the name of the source file without the extension.
    #[get = "pub"]
    file_name: String,

    /// Gets the directory where the source file is located.
    #[get = "pub"]
    root_directory: PathBuf,

    /// Gets whether the source file is the root file that the compiler is compiling.
    #[get = "pub"]
    module_heirarchy: Vec<String>,

    /// Gets the module's fully qualified name of the source file.
    #[get = "pub"]
    module_qualified_name: String,

    /// Gets the string source_code that the source file contains.
    #[get = "pub"]
    source_code: String,
    lines: Vec<Range<usize>>,
}

/// Is an error returned by the [ `SourceFile::load()` ] method.
#[derive(Debug, EnumAsInner, Error, From)]
#[allow(missing_docs)]
pub enum SourceFileLoadError {
    #[error("{0}")]
    Io(std::io::Error),

    #[error("The file extension of the source file must be `.pnx`.")]
    SourceFileCreateError(SourceFileCreateError),
}

/// Is an error returned by the [`SourceFile::new()`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumAsInner, From, Error)]
#[allow(missing_docs)]
pub enum SourceFileCreateError {
    #[error("The module heirarchy string was invalid or empty.")]
    InvalidModuleHeirarchy,

    #[error("The given module heirarchy was an empty list.")]
    EmptyModuleHeirarchy,
}

impl SourceFile {
    /// Is the preferred new line character that the compiler uses.
    ///
    /// Since many operating systems use different new line characters, this is the one that the
    /// compiler uses for the consistency and convenience.
    pub const NEW_LINE: char = '\n';
    /// Is the preferred new line string that the compiler uses.
    pub const NEW_LINE_STR: &'static str = "\n";

    /// Creates a new source file.
    ///
    /// # Parameters
    /// - `root_directory`: The directory where the source file is located.
    /// - `file_name`: The name of the source file without the extension.
    /// - `source_code`: The string source_code that the source file contains.
    /// - `module_heirarchy`: The module's fully qualified name of the source file.
    ///
    /// # Errors
    /// - [`SourceFileCreateError::InvalidModuleHeirarchy`]: One of the module heirarchy strings was
    ///   an invalid identifier or empty.
    /// - [`SourceFileCreateError::EmptyModuleHeirarchy`]: The given module heirarchy was an empty
    ///  list.
    pub fn new(
        root_directory: PathBuf,
        file_name: String,
        mut source_code: String,
        module_heirarchy: Vec<String>,
    ) -> Result<Arc<Self>, SourceFileCreateError> {
        fn replace_string_inplace(s: &mut String, from: &str, to: &str) {
            let mut start = 0;
            while let Some(i) = s[start..].find(from) {
                s.replace_range(start + i..start + i + from.len(), to);
                start += i + to.len();
            }
        }

        if module_heirarchy.is_empty() {
            return Err(SourceFileCreateError::EmptyModuleHeirarchy);
        }

        for module in &module_heirarchy {
            // the string must not contain any ascii punctuation/control characters
            // the first character cannot be a number
            if module
                .chars()
                .any(|c| c.is_ascii_punctuation() || c.is_ascii_control())
                || module
                    .chars()
                    .next()
                    .map(|c| c.is_ascii_digit())
                    .unwrap_or(false)
            {
                return Err(SourceFileCreateError::InvalidModuleHeirarchy);
            }
        }

        let mut lines = Vec::new();
        let mut start = 0;

        replace_string_inplace(&mut source_code, "\r\n", Self::NEW_LINE_STR);
        replace_string_inplace(&mut source_code, "\r", Self::NEW_LINE_STR);

        while source_code.ends_with('\n') {
            source_code.pop();
        }

        // Constructs the line ranges
        for (i, c) in source_code.char_indices() {
            if c == Self::NEW_LINE {
                let new_start = i + 1;
                lines.push(start..new_start);
                start = new_start;
            }

            if i == source_code.len() - 1 {
                lines.push(start..source_code.len());
            }
        }

        Ok(Arc::new(Self {
            file_name,
            root_directory,
            module_qualified_name: module_heirarchy.join("::"),
            module_heirarchy,
            source_code,
            lines,
        }))
    }

    /// Loads a source file from the given path.
    ///
    /// # Errors
    /// - [`SourceFileLoadError::Io`]: An I/O error occurred.
    /// - [`SourceFileLoadError::SourceFileCreateError`]: An error occurred while creating the
    ///   source file.
    pub fn load(
        path: PathBuf,
        module_heirarchy: Vec<String>,
    ) -> Result<Arc<Self>, SourceFileLoadError> {
        let source_code = std::fs::read_to_string(&path)?;
        let root_directory = path.parent().unwrap().to_path_buf();
        let file_name = path.file_stem().unwrap().to_str().unwrap().to_string();

        Ok(Self::new(
            root_directory,
            file_name,
            source_code,
            module_heirarchy,
        )?)
    }

    /// Gets whether the source file is the root file that the compiler is compiling.
    #[must_use]
    pub fn is_root(&self) -> bool { self.module_heirarchy.len() == 1 }

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
    pub fn iter<'a>(self: &'a Arc<Self>) -> Iterator<'a> {
        Iterator {
            source_file: self,
            iterator: self.source_code.char_indices().peekable(),
        }
    }

    /// Gets the number of lines in the source file.
    #[must_use]
    pub fn line_number(&self) -> usize { self.lines.len() }

    /// Gets the [`Location`] of the given byte index.
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
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
            || !source_file.source_code.is_char_boundary(start)
            || source_file.source_code().len() < end
            || (source_file.source_code().len() + 1 != end
                && !source_file.source_code().is_char_boundary(end))
        {
            return None;
        }

        Some(Span {
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
        Some(Span {
            start,
            end: source_file.source_code().len(),
            source_file,
        })
    }

    /// Gets the string slice of the source code that the span represents.
    #[must_use]
    pub fn source_code(&self) -> &str { &self.source_file.source_code()[self.start..self.end] }

    /// Gets the starting [`Location`] of the span.
    #[must_use]
    pub fn start_location(&self) -> Location { self.source_file.get_location(self.start).unwrap() }

    /// Gets the ending [`Location`] of the span.
    ///
    /// Returns [`None`] if the end of the span is the end of the source file.
    pub fn end_location(&self) -> Option<Location> { self.source_file.get_location(self.end) }

    /// Joins the starting position of this span with the end position of the given span.
    ///
    /// Returns [`None`] if the spans are not in the same source file or if the end of this span is
    /// after the start of the given span.
    pub fn join(&self, end: &Self) -> Option<Self> {
        if !Arc::ptr_eq(&self.source_file, &end.source_file) || self.start > end.end {
            return None;
        }

        Some(Span {
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

#[cfg(test)]
mod tests;
