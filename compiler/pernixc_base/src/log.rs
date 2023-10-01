//! Provides the functions related to logging/printing messages to the console.

use std::fmt::Display;

use derive_new::new;
use formatting::{Color, Style};

use crate::source_file::Span;

pub mod formatting;

/// Represents the severity of a log message to be printed to the console.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[allow(missing_docs)]
pub enum Severity {
    Error,
    Info,
    Warning,
}

/// Is a struct implementing [`Display`] that represents a log message to be displayed to the user.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct Message<T> {
    /// The severity of the log message.
    pub severity: Severity,

    /// The message to be displayed.
    pub display: T,
}

impl<T: Display> Display for Message<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let log_header = Style::Bold.with(match self.severity {
            Severity::Error => Color::Red.with("[error]:"),
            Severity::Info => Color::Green.with("[info]:"),
            Severity::Warning => Color::Yellow.with("[warning]:"),
        });

        let message_part = Style::Bold.with(&self.display);

        write!(f, "{log_header} {message_part}")
    }
}

fn get_digit(mut number: usize) -> usize {
    let mut digit = 0;

    while number > 0 {
        number /= 10;
        digit += 1;
    }

    digit
}

/// Structure implementing [`Display`] that prints the particular span of the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct SourceCodeDisplay<'a, T> {
    /// The span of the source code to be printed.
    pub span: &'a Span,

    /// The help message to be displayed.
    pub help_display: Option<T>,
}

impl<'a, T: Display> Display for SourceCodeDisplay<'a, T> {
    #[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start_location = self.span.start_location();
        let end_location = self.span.end_location();

        let start_line = start_location.line;
        let end_line = end_location.map_or_else(
            || self.span.source_file().line_number(),
            |end_location| end_location.line,
        );
        let is_multiline = start_line != end_line;

        // when printing the source code, show the line before the span and the line after the span
        let largest_line_number_digits = get_digit(end_line + 1);

        // prints the source location
        for _ in 0..largest_line_number_digits {
            write!(f, " ")?;
        }

        writeln!(
            f,
            "{} {}",
            Style::Bold.with(Color::Cyan.with("-->")),
            format_args!(
                "{}:{}:{}",
                self.span.source_file().full_path().display(),
                start_location.line,
                start_location.column
            )
        )?;

        // prints the empty pipe
        {
            for _ in 0..=largest_line_number_digits {
                write!(f, " ")?;
            }
            writeln!(f, "{}", Style::Bold.with(Color::Cyan.with("┃")))?;
        }

        // prints previous line
        if let Some(line) = self
            .span
            .source_file()
            .get_line(start_line.saturating_sub(1))
        {
            // prints the line number
            write!(
                f,
                "{}{}{} ",
                Style::Bold.with(Color::Cyan.with(start_line - 1)),
                format_args!(
                    "{:width$}",
                    "",
                    width = largest_line_number_digits - get_digit(start_line - 1) + 1
                ),
                Style::Bold.with(Color::Cyan.with("┃")),
            )?;

            for char in line.chars() {
                // if the char is tab, print 4 spaces
                if char == '\t' {
                    write!(f, "    ")?;
                } else if char != '\n' {
                    write!(f, "{char}")?;
                }
            }

            writeln!(f)?;
        }

        for line_number in start_line..=end_line {
            // prints the line number
            write!(
                f,
                "{}{}{} ",
                Style::Bold.with(Color::Cyan.with(line_number)),
                format_args!(
                    "{:width$}",
                    "",
                    width = largest_line_number_digits - get_digit(line_number) + 1
                ),
                Style::Bold.with(Color::Cyan.with("┃")),
            )?;

            for (index, char) in self
                .span
                .source_file()
                .get_line(line_number)
                .unwrap()
                .chars()
                .enumerate()
            {
                // if the char is tab, print 4 spaces
                if char == '\t' {
                    write!(f, "    ")?;
                } else if char != '\n' {
                    // check if the character is in the span
                    let is_in_span = {
                        let index = index + 1;
                        if is_multiline {
                            (line_number == start_line && index >= start_location.column)
                                || (line_number == end_line
                                    && (index + 1)
                                        < end_location
                                            .map_or(usize::MAX, |end_location| end_location.column))
                                || (line_number > start_line && line_number < end_line)
                        } else {
                            line_number == start_line
                                && index >= start_location.column
                                && index
                                    < end_location
                                        .map_or(usize::MAX, |end_location| end_location.column)
                        }
                    };

                    if is_in_span {
                        write!(
                            f,
                            "{}",
                            Style::Underline.with(Style::Bold.with(Color::Red.with(char)))
                        )?;
                    } else {
                        write!(f, "{char}")?;
                    }
                }
            }
            writeln!(f)?;
        }

        if let Some(message) = &self.help_display {
            if !is_multiline {
                // prints the empty pipe
                {
                    for _ in 0..=largest_line_number_digits {
                        write!(f, " ")?;
                    }
                    write!(f, "{} ", Style::Bold.with(Color::Cyan.with("┃")))?;
                }

                // prints the whitespace until the start's column
                {
                    for (index, char) in self
                        .span
                        .source_file()
                        .get_line(start_line)
                        .unwrap()
                        .chars()
                        .enumerate()
                    {
                        if index + 1 >= start_location.column {
                            break;
                        }

                        // if the char is tab, print 4 spaces
                        write!(f, "{}", if char == '\t' { "    " } else { " " })?;
                    }
                }

                // prints the message
                writeln!(f, "{}: {message}", Style::Bold.with("help"))?;
            }
        }

        // prints the post line
        if let Some(line) = self.span.source_file().get_line(end_line.saturating_add(1)) {
            // prints the line number
            write!(
                f,
                "{}{}{} ",
                Style::Bold.with(Color::Cyan.with(end_line + 1)),
                format_args!(
                    "{:width$}",
                    "",
                    width = largest_line_number_digits - get_digit(end_line + 1) + 1
                ),
                Style::Bold.with(Color::Cyan.with("┃")),
            )?;

            for char in line.chars() {
                // if the char is tab, print 4 spaces
                if char == '\t' {
                    write!(f, "    ")?;
                } else if char != '\n' {
                    write!(f, "{char}")?;
                }
            }

            writeln!(f)?;
        }

        // prints the empty pipe
        {
            for _ in 0..=largest_line_number_digits {
                write!(f, " ")?;
            }
            writeln!(f, "{}", Style::Bold.with(Color::Cyan.with("┃")))?;
        }

        if let Some(help_display) = &self.help_display {
            if is_multiline {
                {
                    for _ in 0..=largest_line_number_digits {
                        write!(f, " ")?;
                    }
                    write!(f, "{} ", Style::Bold.with(Color::Cyan.with("=")))?;
                }

                // prints the message
                writeln!(f, "{}: {help_display}", Style::Bold.with("help"))?;
            }
        }

        Ok(())
    }
}
