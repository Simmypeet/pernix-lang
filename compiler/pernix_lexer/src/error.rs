use console::Style;
use pernix_project::source_code::{SourceCode, SourcePosition};

/// An enumeration containing all lexical errors
pub enum Error<'a> {
    InvalidCharacter {
        position: SourcePosition,
        character: char,
        source_refernece: &'a SourceCode,
    },
    UnterminatedMultilineComment {
        multiline_comment_position: SourcePosition,
        source_refernece: &'a SourceCode,
    },
}

impl<'a> Error<'a> {
    /// Prints the error message to the standard output
    pub fn print_error(&self) {
        // error message of the error
        let (error_message, source_reference, position) = match self {
            Error::InvalidCharacter {
                position,
                character,
                source_refernece,
            } => (
                format!("`{}` is not a valid character", character),
                source_refernece,
                position,
            ),
            Error::UnterminatedMultilineComment {
                multiline_comment_position,
                source_refernece,
            } => (
                "unterminated multiline comment".to_string(),
                source_refernece,
                multiline_comment_position,
            ),
        };

        let red_bold = Style::new().red().bold();
        let bold = Style::new().bold();

        // print error message
        println!(
            "{}{} {}",
            red_bold.apply_to("error"),
            bold.apply_to(':'),
            bold.apply_to(error_message)
        );

        let bright_cyan = Style::new().cyan().bright().bold();

        println!(
            "{} {}:{}:{}",
            bright_cyan.apply_to("-->"),
            source_reference.source_name(),
            position.line.to_string(),
            position.column.to_string()
        );
    }
}
