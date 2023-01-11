use std::{cmp::Ordering, ops::Range, path::PathBuf};

/// Represents a source file that is subject to compilation. It contains the path to the file,
/// the module-hierarchy level of the file, and the content of the file.
#[derive(Debug)]
pub struct SourceFile {
    absolute_path: PathBuf,
    content: String,
    new_line_ranges: Vec<Range<usize>>,
}

/// An enumeration of errors that can occur when creating a [`SourceFile`] instance.
pub enum SourceFileError {
    /// The module level string is invalid.
    InvalidModuleLevel,

    /// An I/O error occurred.
    IoError(std::io::Error),

    /// The absolute path is empty.
    EmptyPathBuf,
}

impl SourceFile {
    /// Creates a new [`SourceFile`] instance.
    ///
    /// # Parameters
    /// - `module_level`: The qualified module name of the source file. (format: `a::b::c`)
    /// - `absolute_path`: The absolute path to the source file.
    pub fn new(absolute_path: PathBuf) -> Result<SourceFile, SourceFileError> {
        // empty path is not allowed
        if absolute_path.as_os_str().is_empty() {
            return Err(SourceFileError::EmptyPathBuf);
        }

        // read the string
        let content = match std::fs::read_to_string(&absolute_path) {
            Ok(content) => content,
            Err(err) => return Err(SourceFileError::IoError(err)),
        };

        let mut new_line_ranges = Vec::new();
        let mut last_new_line_byte_index = 0;
        let mut char_indices = content.char_indices();
        let byte_length = content.len();

        loop {
            match char_indices.next() {
                // found a new line
                Some((byte_pos, '\n')) => {
                    // new line
                    new_line_ranges.push(last_new_line_byte_index..byte_pos);

                    last_new_line_byte_index = byte_pos + 1;
                }
                None => {
                    new_line_ranges.push(last_new_line_byte_index..byte_length);
                    break;
                }
                _ => {}
            }
        }

        Ok(SourceFile {
            absolute_path,
            content,
            new_line_ranges,
        })
    }

    /// Returns the absolute path to the source file.
    pub fn absolute_path(&self) -> &PathBuf {
        &self.absolute_path
    }

    /// Returns the content of the source file.
    pub fn content(&self) -> &str {
        self.content.as_ref()
    }

    /// Returns the content of the line at the given `line_number`. The line
    /// number starts from 1.
    pub fn line(&self, line_number: usize) -> Option<&str> {
        if line_number == 0 {
            return None;
        }

        let line_number = line_number - 1;
        if let Some(range) = self.new_line_ranges.get(line_number) {
            Some(&self.content[range.clone()])
        } else {
            None
        }
    }

    /// Returns the number of lines in the source file.
    pub fn line_number(&self) -> usize {
        self.new_line_ranges.len()
    }
}

/// Represents a particular position in the source file, consisting of column and line.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct TextPosition {
    pub line: usize,
    pub column: usize,
}

impl TextPosition {
    /// Creates a new [`TextPosition`].
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl PartialOrd for TextPosition {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.line == other.line {
            Some(self.column.cmp(&other.column))
        } else {
            Some(self.line.cmp(&other.line))
        }
    }
}
