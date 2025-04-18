//! The executable for the Pernix programming language.

use std::{
    fmt::Write as _, fs::File, io::Write as _, mem, path::PathBuf,
    process::ExitCode,
};

use backtrace::Backtrace;
use clap::Parser;
use pernixc_driver::Arguments;
use pernixc_log::Message;
use serde::{Deserialize, Serialize};

fn main() -> ExitCode {
    // if the program is compiled in release mode, set the panic hook to
    // nicely print the error message and exit.
    #[cfg(not(debug_assertions))]
    setup_panic();

    pernixc_driver::run(&Arguments::parse())
}

/// The struct capturing the information about an ICE (Internal Compiler Error)
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct IceReport {
    /// The **payload** of the panic info.
    pub dislay: String,

    /// The location in the compiler source code where the ICE occurred.
    pub location: Option<String>,

    /// The version of the compiler that caused the ICE.
    pub version: String,

    /// The call stack backtrace of the ICE.
    pub back_trace: Option<String>,
}

impl IceReport {
    /// Write the ICE report to a temporary file.
    ///
    /// # Returns
    ///
    /// A `std::io::Result` containing the file where the ICE report was
    /// written.
    #[allow(clippy::missing_errors_doc)]
    pub fn write_to_temp(&self) -> std::io::Result<(File, PathBuf)> {
        let temp_dir = std::env::temp_dir();
        let file_name = format!(
            "pernixc_ice_{}.toml",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs()
        );
        let file_path = temp_dir.join(&file_name);

        let mut file = std::fs::File::create(&file_path)?;
        let toml_string = toml::to_string_pretty(self)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;

        write!(file, "{toml_string}")?;

        Ok((file, file_path))
    }
}

#[allow(dead_code)]
fn setup_panic() {
    std::panic::set_hook(Box::new(|info| {
        eprintln!("{}", Message {
            severity: pernixc_log::Severity::Error,
            display: "internal compiler error (ICE) occurred; the error is \
                      caused by a bug in the compiler not the error in your \
                      code"
        });

        eprintln!("{}", Message {
            severity: pernixc_log::Severity::Info,
            display: "we're sorry for the inconvenience, please report this \
                      issue to the developers"
        });

        let payload_string = info
            .payload()
            .downcast_ref::<&'static str>()
            .map_or_else(
                || {
                    info.payload()
                        .downcast_ref::<String>()
                        .map_or("unknown", |payload| payload)
                },
                |payload| payload,
            )
            .to_owned();

        let location = info.location().map(|location| {
            let workspace_prefix = env!("PERNIXC_CARGO_WORKSPACE_DIR");
            let stripped = location.file().strip_prefix(workspace_prefix);

            format!(
                "{}:{}:{}",
                stripped.unwrap_or(location.file()),
                location.line(),
                location.column()
            )
        });

        let back_trace = render_backtrace();

        let version = env!("CARGO_PKG_VERSION").to_owned();

        let ice_report = IceReport {
            dislay: payload_string,
            location,
            back_trace: Some(back_trace),
            version,
        };

        let file = ice_report.write_to_temp();

        if let Ok((_, path)) = file {
            eprintln!("{}", Message {
                severity: pernixc_log::Severity::Info,
                display: format!(
                    "the ICE report has been written to: {}",
                    path.display()
                )
            });
        } else {
            eprintln!("{}", Message {
                severity: pernixc_log::Severity::Info,
                display: "dumping the ICE report..."
            });

            eprintln!(
                "```\n{}```",
                toml::to_string_pretty(&ice_report).unwrap()
            );
        }

        eprintln!("{}", Message {
            severity: pernixc_log::Severity::Info,
            display: "please report the issue to the developers with the \
                      written ICE report"
        });

        eprintln!("{}", Message {
            severity: pernixc_log::Severity::Info,
            display: "the report was not automatically sent to the developers \
                      because of privacy concerns"
        });

        eprintln!("{}", Message {
            severity: pernixc_log::Severity::Info,
            display: "we appreciate your effort to report the issue and help \
                      us improve the compiler <3"
        });

        std::process::exit(1);
    }));
}

// taken from https://github.com/rust-cli/human-panic/blob/master/src/report.rs
fn render_backtrace() -> String {
    //We take padding for address and extra two letters
    //to pad after index.
    #[allow(unused_qualifications)] // needed for pre-1.80 MSRV
    const HEX_WIDTH: usize = mem::size_of::<usize>() * 2 + 2;
    //Padding for next lines after frame's address
    const NEXT_SYMBOL_PADDING: usize = HEX_WIDTH + 6;

    let mut backtrace = String::new();

    //Here we iterate over backtrace frames
    //(each corresponds to function's stack)
    //We need to print its address
    //and symbol(e.g. function name),
    //if it is available
    let bt = Backtrace::new();
    let symbols = bt
        .frames()
        .iter()
        .flat_map(|frame| {
            let symbols = frame.symbols();
            if symbols.is_empty() {
                vec![(frame, None, "<unresolved>".to_owned())]
            } else {
                symbols
                    .iter()
                    .map(|s| {
                        (
                            frame,
                            Some(s),
                            s.name().map_or_else(
                                || "<unknown>".to_owned(),
                                |n| n.to_string(),
                            ),
                        )
                    })
                    .collect::<Vec<_>>()
            }
        })
        .collect::<Vec<_>>();
    let begin_unwind = "rust_begin_unwind";
    let begin_unwind_start =
        symbols.iter().position(|(_, _, n)| n == begin_unwind).unwrap_or(0);
    for (entry_idx, (frame, symbol, name)) in
        symbols.iter().skip(begin_unwind_start).enumerate()
    {
        let ip = frame.ip();
        let _ = writeln!(backtrace, "{entry_idx:4}: {ip:HEX_WIDTH$?} - {name}");
        if let Some(symbol) = symbol {
            //See if there is debug information with file name and line
            if let (Some(file), Some(line)) =
                (symbol.filename(), symbol.lineno())
            {
                let _ = writeln!(
                    backtrace,
                    "{:3$}at {}:{}",
                    "",
                    file.display(),
                    line,
                    NEXT_SYMBOL_PADDING
                );
            }
        }
    }

    backtrace
}
