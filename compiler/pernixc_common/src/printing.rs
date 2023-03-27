//! Provides the functions related to logging/printing messages to the console.

use colored::Colorize;

/// Represents the severity of a log message to be printed to the console.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum LogSeverity {
    /// Represents an error log message.
    Error,

    /// Represents an info log message.
    Info,

    /// Represents a warning log message.
    Warning,
}

/// Prints a log message to the console.
pub fn log(severity: LogSeverity, message: &str) {
    let log_header_message = match severity {
        LogSeverity::Error => "[error]:".bright_red().bold(),
        LogSeverity::Info => "[info]:".bright_green().bold(),
        LogSeverity::Warning => "[warning]:".yellow().bold(),
    };

    println!("{log_header_message} {message}");
}
