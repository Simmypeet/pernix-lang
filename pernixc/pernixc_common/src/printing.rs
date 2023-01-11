use std::{
    cmp::{max, min},
    ops::Range,
};

use console::Style;

use crate::source_file::{SourceFile, TextPosition};

/// Is an enumeration containing all severity levels
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

/// Prints a message with a severity level
///
/// # Format
/// `[error|warning|info][category]: message`
pub fn print_message(message: &str, severity: Severity, error_category: Option<&str>) {
    let bold_white = Style::new().bold().white();
    let (severity_message, style) = {
        match severity {
            Severity::Error => ("error", Style::new().bold().red()),
            Severity::Warning => ("warning", Style::new().bold().yellow()),
            Severity::Info => ("info", Style::new().bold().blue()),
        }
    };

    match error_category {
        Some(category) => {
            println!(
                "{}{}{}{}{} {}",
                style.apply_to(severity_message),
                style.apply_to('['),
                style.apply_to(category),
                style.apply_to(']'),
                style.apply_to(':'),
                bold_white.apply_to(message)
            );
        }
        None => {
            println!(
                "{}{} {}",
                style.apply_to(severity_message),
                style.apply_to(':'),
                bold_white.apply_to(message)
            );
        }
    }
}

/// Is an enumeration containing all highlight styles
#[derive(Clone, Debug)]
pub enum HighlightStyle {
    Range(Range<TextPosition>),
    Position(TextPosition),
}

/// Prints the fragment of source code that contains the erorr
///
/// # Arguments
/// * `source_file` - The source code that contains the error
/// * `source_position_range` - The range of the source code that contains the error
/// * `message` - The message that will be printed
pub fn highlight_source_file(
    source_file: &SourceFile,
    source_position_range: HighlightStyle,
    message: Option<&str>,
) {
    match source_position_range.clone() {
        HighlightStyle::Range(range) => {
            if range.start > range.end {
                return;
            }
        }
        HighlightStyle::Position(position) => {
            if position.line > source_file.line_number() {
                return;
            }
        }
    }

    // input checking

    let line_range = {
        let start_line_range = {
            let original_line_number = match source_position_range.clone() {
                HighlightStyle::Range(range) => range.start.line,
                HighlightStyle::Position(position) => position.line,
            };
            let lowered_line_number = max(original_line_number - 1, 1);
            if !source_file.line(lowered_line_number).unwrap().is_empty() {
                lowered_line_number
            } else {
                original_line_number
            }
        };

        let end_line_range = {
            let original_line_number = match source_position_range.clone() {
                HighlightStyle::Range(range) => range.end.line,
                HighlightStyle::Position(position) => position.line,
            };
            let raised_line_number = min(source_file.line_number(), original_line_number + 1);
            if !source_file.line(raised_line_number).unwrap().is_empty() {
                raised_line_number
            } else {
                original_line_number
            }
        };

        start_line_range..end_line_range + 1
    };

    let largest_digit_number = (line_range.end - 1).to_string().len();

    let bright_cyan = Style::new().cyan().bright().bold();
    let bold_red_on_white = Style::new().bold().red().underlined();
    let bright_red = Style::new().yellow().bright();

    {
        print!(" ");
        for _ in 0..largest_digit_number + 1 {
            print!("{}", bright_cyan.apply_to('-'));
        }
        print!("{} ", bright_cyan.apply_to('>'));

        match source_position_range {
            HighlightStyle::Range(ref range) => {
                println!(
                    "{}{}{}{}{}",
                    bright_cyan.apply_to(
                        source_file
                            .absolute_path()
                            .file_name()
                            .unwrap()
                            .to_str()
                            .unwrap()
                    ),
                    bright_cyan.apply_to(':'),
                    bright_cyan.apply_to(range.start.line),
                    bright_cyan.apply_to(':'),
                    bright_cyan.apply_to(range.start.column)
                );
            }
            HighlightStyle::Position(pos) => {
                println!(
                    "{}{}{}{}{}",
                    bright_cyan.apply_to(
                        source_file
                            .absolute_path()
                            .file_name()
                            .unwrap()
                            .to_str()
                            .unwrap()
                    ),
                    bright_cyan.apply_to(':'),
                    bright_cyan.apply_to(pos.line),
                    bright_cyan.apply_to(':'),
                    bright_cyan.apply_to(pos.column)
                );
            }
        };
    }

    print!("{: <1$}", "", largest_digit_number + 1);
    println!("{}", bright_cyan.apply_to('|'));

    for line_number in line_range.clone() {
        let mut column_number = 1;
        let line = source_file.line(line_number).unwrap();

        let mut digit_number = line_number.to_string();
        let blank_space_count = largest_digit_number - digit_number.len();
        digit_number.reserve(blank_space_count);

        for _ in 0..blank_space_count {
            digit_number.insert(0, ' ');
        }

        print!(
            "{} {} ",
            bright_cyan.apply_to(digit_number),
            bright_cyan.apply_to('|')
        );

        match source_position_range.clone() {
            HighlightStyle::Range(range) => {
                let same_line = range.start.line == range.end.line;
                let mut column_set = false;
                let mut last_column = None;
                for c in line.chars() {
                    let should_highlight: bool = {
                        let line_satisfies =
                            line_number >= range.start.line && line_number <= range.end.line;
                        let column_satisfies = if same_line {
                            column_number >= range.start.column && column_number < range.end.column
                        } else {
                            if line_number == range.end.line {
                                column_number < range.end.column
                            } else {
                                true
                            }
                        };

                        if line_satisfies && column_satisfies {
                            let is_whitespace = c.is_whitespace();

                            if line_number == range.end.line && !is_whitespace {
                                if !column_set {
                                    last_column = Some(column_number);
                                    column_set = true;
                                }
                            }

                            !is_whitespace
                        } else {
                            false
                        }
                    };

                    if should_highlight {
                        print!("{}", bold_red_on_white.apply_to(c));
                    } else {
                        print!("{}", c);
                    }

                    column_number += 1;
                }
                if matches!(last_column, Some(_)) && matches!(message, Some(_)) {
                    print!("\n");
                    print!("{: <1$}", "", largest_digit_number + 1);
                    print!("{}", bright_cyan.apply_to('|'));

                    let column_number = last_column.unwrap();
                    for _ in 0..column_number {
                        print!(" ");
                    }

                    print!("{}", bright_red.apply_to(message.unwrap()));
                }

                print!("\n");
            }
            HighlightStyle::Position(pos) => {
                let mut chars = line.chars();
                let mut last_column = None;
                loop {
                    let c = match chars.next() {
                        Some(c) => c,
                        None => {
                            if line_number != pos.line {
                                break;
                            }

                            if column_number > pos.column {
                                break;
                            }

                            ' '
                        }
                    };

                    let should_highligh = line_number == pos.line && column_number == pos.column;

                    if should_highligh {
                        print!("{}", bold_red_on_white.apply_to(c));
                        last_column = Some(column_number);
                    } else {
                        print!("{}", c);
                    }

                    column_number += 1;
                }

                if matches!(last_column, Some(_)) && matches!(message, Some(_)) {
                    print!("\n");
                    print!("{: <1$}", "", largest_digit_number + 1);
                    print!("{}", bright_cyan.apply_to('|'));

                    let column_number = last_column.unwrap();
                    for _ in 0..column_number {
                        print!(" ");
                    }

                    print!("{}", bright_red.apply_to('^'));

                    print!("\n");
                    print!("{: <1$}", "", largest_digit_number + 1);
                    print!("{}", bright_cyan.apply_to('|'));

                    let column_number = last_column.unwrap();
                    for _ in 0..column_number {
                        print!(" ");
                    }

                    print!("{}", bright_red.apply_to(message.unwrap()));
                }
                print!("\n");
            }
        };
    }

    print!("{: <1$}", "", largest_digit_number + 1);
    println!("{}", bright_cyan.apply_to('|'));
}
