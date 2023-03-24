use colored::Colorize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum LogSeverity {
    Error,
    Info,
    Warning,
}

pub fn log(severity: LogSeverity, message: &str) {
    let log_header_message = match severity {
        LogSeverity::Error => "[error]:".bright_red().bold(),
        LogSeverity::Info => "[info]:".bright_green().bold(),
        LogSeverity::Warning => "[warning]:".yellow().bold(),
    };

    println!("{} {}", log_header_message, message);
}
