//! Contains the [`SourceFile`] type which represents a source file input for the compiler.

use std::{
    iter::Peekable,
    ops::{Index, Range},
    path::PathBuf,
    str::Chars,
};

use derive_new::new;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use thiserror::Error;

/// Represents an source file input for the compiler.
#[derive(Debug, Getters, Clone, PartialEq, Eq, Hash)]
pub struct SourceFile {
    /// Gets the path input that the source file was created from.
    #[get = "pub"]
    path_input: PathBuf,

    /// Gets the full path of the source file.
    #[get = "pub"]
    full_path: PathBuf,

    /// Gets whether the source file is the root file that the compiler is compiling.
    #[get = "pub"]
    module_heirarchy: Vec<String>,

    /// Gets the module's fully qualified name of the source file.
    #[get = "pub"]
    module_qualified_name: String,

    /// Gets teh string content that the source file contains.
    #[get = "pub"]
    content: String,
    lines: Vec<Range<usize>>,
}

/// Is an enumeration containing all kinds of errors that can occur while loading a source file.
#[derive(Debug, EnumAsInner, Error)]
#[allow(missing_docs)]
pub enum LoadError {
    #[error("{0}")]
    Io(#[from] std::io::Error),

    #[error("The file extension of the source file must be `.pnx`")]
    InvalidFileExtension,

    #[error("The module heirarchy of the source file is invalid")]
    InvalidModuleHeirarchy,

    #[error("The module heirarchy of the source file is empty")]
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

    /// Loads a source file from the given path.
    ///
    /// # Errors
    /// - [`LoadError::InvalidFileExtension`]: The file extension of the source file must be `.pnx`.
    /// - [`LoadError::InvalidModuleHeirarchy`]: The module heirarchy of the source file is invalid.
    /// - [`LoadError::InvalidModuleHeirarchy`]: The module heirarchy of the source file is empty.
    /// - [`LoadError::Io`]: The source file could not be read.
    pub fn load(path: PathBuf, module_heirarchy: Vec<String>) -> Result<Self, LoadError> {
        fn replace_string_inplace(s: &mut String, from: &str, to: &str) {
            let mut start = 0;
            while let Some(i) = s[start..].find(from) {
                s.replace_range(start + i..start + i + from.len(), to);
                start += i + to.len();
            }
        }

        // get the full path of the file
        let full_path = if path.is_absolute() {
            path.clone()
        } else {
            std::fs::canonicalize(path.clone())?
        };

        // the file extension must be `.pnx`
        if full_path.extension().map_or(true, |ext| ext != "pnx") {
            return Err(LoadError::InvalidFileExtension);
        }

        if module_heirarchy.is_empty() {
            return Err(LoadError::EmptyModuleHeirarchy);
        }

        for module in &module_heirarchy {
            if module.is_empty() {
                return Err(LoadError::InvalidModuleHeirarchy);
            }
        }

        // read the string content of the source file
        let mut content = std::fs::read_to_string(&full_path)?;

        let mut lines = Vec::new();
        let mut start = 0;

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

        Ok(Self {
            path_input: path,
            full_path,
            content,
            lines,
            module_qualified_name: module_heirarchy.join("::"),
            module_heirarchy,
        })
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
            .map(|range| &self.content[range.clone()])
    }

    /// Gets the source file iterator for this source file.
    #[must_use]
    pub fn iter(&self) -> Iterator {
        Iterator {
            source_code: &self.content,
            chars: self.content.chars().peekable(),
            line: 1,
            column: 1,
            byte: 0,
        }
    }

    /// Gets the string slice of the source file at the given span.
    ///
    /// The slicing is done using the byte index of the span.
    #[must_use]
    pub fn get(&self, span: Span) -> Option<&str> {
        match span.end {
            SpanEnding::Location(end) => self.content.get(span.start.byte..end.byte),
            SpanEnding::EndOfFile => self.content.get(span.start.byte..),
        }
    }
}

/// Represents an iterator over the characters of a source file.
///
/// It outputs the [`Location`] along with the character when iterating. Moreover, it also provides
/// [peek()](Self::peek()) function right out of the box without the need to use the [`Peekable`]
/// iterator adapter.
#[derive(Clone, Debug)]
pub struct Iterator<'a> {
    source_code: &'a str,
    chars: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
    byte: usize,
}

impl<'a> std::iter::Iterator for Iterator<'a> {
    type Item = (Location, char);

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars.next()?;
        let location = Location {
            line: self.line,
            column: self.column,
            byte: self.byte,
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

impl<'a> Iterator<'a> {
    /// Creates a new source file iterator from the given source code string.
    #[must_use]
    pub fn new(source_code: &'a str) -> Self {
        Self {
            source_code,
            chars: source_code.chars().peekable(),
            line: 1,
            column: 1,
            byte: 0,
        }
    }

    /// Gets the source file that this iterator is iterating over.
    #[must_use]
    pub fn source_code(&self) -> &'a str { self.source_code }

    /// Peeks the next character in the source file without consuming it.
    pub fn peek(&mut self) -> Option<(Location, char)> {
        let c = self.chars.peek()?;
        let location = Location {
            line: self.line,
            column: self.column,
            byte: self.byte,
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

impl Index<Span> for SourceFile {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        let start = index.start.byte;
        let end = match index.end {
            SpanEnding::Location(location) => location.byte,
            SpanEnding::EndOfFile => self.content.len(),
        };

        &self.content[start..end]
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

#[cfg(test)]
mod tests;
