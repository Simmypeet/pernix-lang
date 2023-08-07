//! Provides the functions related to logging/printing messages to the console.

use colored::Colorize;
use pernixc_source::Span;

/// Represents the severity of a log message to be printed to the console.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]

pub enum LogSeverity {
    Error,
    Info,
    Warning,
}

/// Prints a log message to the console.
pub fn print(severity: LogSeverity, message: &str) {
    let log_header_message = match severity {
        LogSeverity::Error => "[error]:".bright_red().bold(),
        LogSeverity::Info => "[info]:".bright_green().bold(),
        LogSeverity::Warning => "[warning]:".yellow().bold(),
    };
    let message = message.bold();

    println!("{log_header_message} {message}");
}

fn get_digit(mut number: usize) -> usize {
    let mut digit = 0;

    while number > 0 {
        number /= 10;
        digit += 1;
    }

    digit
}

/// Prints and highlights the source code of a span, including the help message if provided.
#[allow(clippy::too_many_lines)]
pub fn print_source_code(span: &Span, help_message: Option<&str>) {
    let start_location = span.start_location();
    let end_location = span.end_location();

    let start_line = start_location.line;
    let end_line = end_location.map_or_else(
        || span.source_file().line_number(),
        |end_location| end_location.line,
    );
    let is_multiline = start_line != end_line;

    // when printing the source code, show the line before the span and the line after the span
    let largest_line_number_digits = get_digit(end_line + 1);

    // prints the source location
    for _ in 0..largest_line_number_digits {
        print!(" ");
    }

    println!(
        "{} {}",
        "-->".bright_cyan().bold(),
        format_args!(
            "{}:{}:{}",
            span.source_file().full_path().display(),
            start_location.line,
            start_location.column
        )
    );

    // prints the empty pipe
    {
        for _ in 0..=largest_line_number_digits {
            print!(" ");
        }
        println!("{}", "┃".bright_cyan().bold());
    }

    // prints previous line
    if let Some(line) = span.source_file().get_line(start_line.saturating_sub(1)) {
        // prints the line number
        print!(
            "{}{}{} ",
            (start_line - 1).to_string().bright_cyan().bold(),
            format_args!(
                "{:width$}",
                "",
                width = largest_line_number_digits - get_digit(start_line - 1) + 1
            ),
            "┃".bright_cyan().bold(),
        );

        for char in line.chars() {
            // if the char is tab, print 4 spaces
            if char == '\t' {
                print!("    ");
            } else if char != '\n' {
                print!("{char}");
            }
        }

        println!();
    }

    for line_number in start_line..=end_line {
        // prints the line number
        print!(
            "{}{}{} ",
            line_number.to_string().bright_cyan().bold(),
            format_args!(
                "{:width$}",
                "",
                width = largest_line_number_digits - get_digit(line_number) + 1
            ),
            "┃".bright_cyan().bold(),
        );

        for (index, char) in span
            .source_file()
            .get_line(line_number)
            .unwrap()
            .chars()
            .enumerate()
        {
            // if the char is tab, print 4 spaces
            if char == '\t' {
                print!("    ");
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
                    print!("{}", char.to_string().bright_red().bold().underline());
                } else {
                    print!("{char}");
                }
            }
        }
        println!();
    }

    if let Some(message) = help_message {
        if !is_multiline {
            // prints the empty pipe
            {
                for _ in 0..=largest_line_number_digits {
                    print!(" ");
                }
                print!("{} ", "┃".bright_cyan().bold());
            }

            // prints the whitespace until the start's column
            {
                for (index, char) in span
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
                    print!("{}", if char == '\t' { "    " } else { " " });
                }
            }

            // prints the message
            println!("{}: {message}", "help".bold());
        }
    }

    // prints the post line
    if let Some(line) = span.source_file().get_line(end_line.saturating_add(1)) {
        // prints the line number
        print!(
            "{}{}{} ",
            (end_line + 1).to_string().bright_cyan().bold(),
            format_args!(
                "{:width$}",
                "",
                width = largest_line_number_digits - get_digit(end_line + 1) + 1
            ),
            "┃".bright_cyan().bold(),
        );

        for char in line.chars() {
            // if the char is tab, print 4 spaces
            if char == '\t' {
                print!("    ");
            } else if char != '\n' {
                print!("{char}");
            }
        }

        println!();
    }

    // prints the empty pipe
    {
        for _ in 0..=largest_line_number_digits {
            print!(" ");
        }
        println!("{}", "┃".bright_cyan().bold());
    }

    if let Some(message) = help_message {
        if is_multiline {
            {
                for _ in 0..=largest_line_number_digits {
                    print!(" ");
                }
                print!("{} ", "=".bright_cyan().bold());
            }

            // prints the message
            println!("{}: {message}", "help".bold());
        }
    }
}
