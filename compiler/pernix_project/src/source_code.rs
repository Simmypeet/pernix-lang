use std::{cmp::Ordering, ops::Range};

/// Represent a source code.
#[derive(Debug, Clone)]
pub struct SourceCode {
    source_code: String,
    source_name: String,
    new_line_ranges: Vec<Range<usize>>,
}

/// Represent a particular position in the source code, consisting of column,
/// line and byte index.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SourcePosition {
    pub line: usize,
    pub column: usize,
    pub byte_index: usize,
}

impl From<SourcePosition> for SourcePositionWithoutByteIndex {
    fn from(position: SourcePosition) -> Self {
        SourcePositionWithoutByteIndex {
            line: position.line,
            column: position.column,
        }
    }
}

/// Represent a particular position in the source code, consisting of column and
/// line.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SourcePositionWithoutByteIndex {
    pub line: usize,
    pub column: usize,
}

impl PartialOrd for SourcePositionWithoutByteIndex {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.line == other.line {
            Some(self.column.cmp(&other.column))
        } else {
            Some(self.line.cmp(&other.line))
        }
    }
}

impl SourcePosition {
    /// Return the line number of this [`SourcePosition`].
    pub fn line(&self) -> usize {
        self.line
    }

    /// Return the column number of this [`SourcePosition`].
    pub fn column(&self) -> usize {
        self.column
    }

    /// Return the byte index of this [`SourcePosition`].
    pub fn byte_index(&self) -> usize {
        self.byte_index
    }

    /// Return a tuple of the line and column of this [`SourcePosition`].
    pub fn to_line_column(&self) -> (usize, usize) {
        (self.line, self.column)
    }
}

impl PartialOrd for SourcePosition {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let position_wise = if self.line == other.line {
            self.column.cmp(&other.column)
        } else {
            self.line.cmp(&other.line)
        };

        let byte_index_wise = self.byte_index.cmp(&other.byte_index);

        if position_wise == byte_index_wise {
            Some(position_wise)
        } else {
            None
        }
    }
}

impl SourceCode {
    /// Create a new [`SourceCode`].
    pub fn new(source_code: String, source_name: String) -> Self {
        let mut new_line_ranges = Vec::new();
        let mut last_new_line_byte_index = 0;
        let mut char_indices = source_code.char_indices();
        let byte_length = source_code.len();

        loop {
            match char_indices.next() {
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

        Self {
            source_code,
            source_name,
            new_line_ranges,
        }
    }

    /// Return a reference to the source code of this [`SourceCode`].
    pub fn source_code(&self) -> &str {
        self.source_code.as_ref()
    }

    /// Return a reference to the source name of this [`SourceCode`].
    pub fn source_name(&self) -> &str {
        self.source_name.as_ref()
    }

    /// Return the content of the line at the given `line_number`. The line
    /// number starts from 1.
    pub fn line(&self, line_number: usize) -> Option<&str> {
        if line_number == 0 {
            return None;
        }

        let line_number = line_number - 1;
        if let Some(range) = self.new_line_ranges.get(line_number) {
            Some(&self.source_code[range.clone()])
        } else {
            None
        }
    }

    /// Return the number of lines in this [`SourceCode`].
    pub fn line_number(&self) -> usize {
        self.new_line_ranges.len()
    }

    /// Check if the given `source_position` is in this [`SourceCode`].
    pub fn is_source_position_in_file(
        &self,
        source_position: SourcePosition,
    ) -> bool {
        if source_position.line == 0
            && source_position.column == 0
            && source_position.byte_index == 0
            && source_position.line > self.line_number()
            && source_position.byte_index > self.source_code.len()
        {
            return false;
        }

        let byte_index_range =
            self.new_line_ranges[source_position.line - 1].clone();
        if source_position.byte_index < byte_index_range.start
            || source_position.byte_index > byte_index_range.end
        {
            return false;
        }

        let line = self.line(source_position.line).unwrap();
        if source_position.column > line.len() {
            false
        } else {
            true
        }
    }
}

#[cfg(test)]
pub mod test;
