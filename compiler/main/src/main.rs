//! The executable for the Pernix programming language.

use std::{
    fmt::Write, fs::File, io::Write as _, mem, path::PathBuf, process::ExitCode,
};

use backtrace::Backtrace;
use clap::Parser;
use pernixc_target::Arguments;
#[cfg(not(target_env = "msvc"))]
use tikv_jemallocator::Jemalloc;
use tracing_chrome::{ChromeLayerBuilder, FlushGuard};
use tracing_subscriber::{
    EnvFilter, Layer, layer::SubscriberExt, util::SubscriberInitExt,
};

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

#[cfg(not(target_env = "msvc"))]
fn configure_jemalloc() {
    // this configuration is equivalent to
    // "background_thread:true,dirty_decay_ms:1000,muzzy_decay_ms:1000"

    unsafe {
        let _ =
            tikv_jemalloc_ctl::raw::write::<bool>(b"background_thread\0", true);
        let _ = tikv_jemalloc_ctl::raw::write::<isize>(
            b"arenas.dirty_decay_ms\0",
            1000,
        );
        let _ = tikv_jemalloc_ctl::raw::write::<isize>(
            b"arenas.muzzy_decay_ms\0",
            1000,
        );
    }
}

async fn main_async() -> ExitCode {
    // if the program is compiled in release mode, set the panic hook to
    // nicely print the error message and exit.
    #[cfg(not(debug_assertions))]
    setup_panic();
    let argument = Arguments::parse();

    let _guard = init_tracing(argument.command.input().chrome_tracing);

    let mut stderr = std::io::stderr();
    let mut stdout = std::io::stdout();

    pernixc_driver::run(argument, &mut stderr, &mut stdout).await
}

fn main() -> ExitCode {
    #[cfg(not(target_env = "msvc"))]
    configure_jemalloc();

    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .thread_stack_size(
            // in the debug build, the stack size of the future is not
            // optimized well, so we need a larger stack size
            {
                #[cfg(debug_assertions)]
                {
                    8 * 1024 * 1024 // 8MB stack for debug build
                }
                #[cfg(not(debug_assertions))]
                {
                    2 * 1024 * 1024 // 2MB stack default
                }
            },
        )
        .build()
        .unwrap()
        .block_on(main_async())
}

fn init_tracing(chrome_tracing: bool) -> Option<FlushGuard> {
    let registry = tracing_subscriber::registry().with(
        tracing_subscriber::fmt::layer()
            .with_thread_ids(true)
            .with_thread_names(true)
            .with_span_events(tracing_subscriber::fmt::format::FmtSpan::CLOSE)
            .with_filter(
                EnvFilter::try_from_env("PERNIXC_LOG")
                    .unwrap_or_else(|_| "ERROR".into()),
            ),
    );

    if chrome_tracing {
        // Create chrome layer with field value support
        let (chrome_layer, guard) = ChromeLayerBuilder::new()
            .include_args(true) // This includes field values in the trace
            .name_fn(Box::new(|event_or_span| match event_or_span {
                tracing_chrome::EventOrSpan::Event(event) => {
                    format!(
                        "{}-{}",
                        event.metadata().target(),
                        event.metadata().name()
                    )
                }
                tracing_chrome::EventOrSpan::Span(span) => {
                    format!(
                        "{}-{}",
                        span.metadata().target(),
                        span.metadata().name()
                    )
                }
            }))
            .build();

        registry
            .with(chrome_layer)
            .with(
                EnvFilter::try_from_env("PERNIXC_CHROME")
                    .unwrap_or_else(|_| "ERROR".into()),
            )
            .init();

        Some(guard)
    } else {
        registry.init();
        None
    }
}

/// The struct capturing the information about an ICE (Internal Compiler Error)
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize,
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
            "pernixc_ice_{}.ron",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs()
        );
        let file_path = temp_dir.join(&file_name);

        let mut file = std::fs::File::create(&file_path)?;
        let toml_string = toml::to_string_pretty(self).map_err(|x| {
            std::io::Error::other(format!(
                "failed to serialize the ICE report: {x}"
            ))
        })?;

        file.write_all(toml_string.as_bytes())?;

        Ok((file, file_path))
    }
}

#[allow(dead_code, clippy::too_many_lines)]
fn setup_panic() {
    std::panic::set_hook(Box::new(|info| {
        use std::io::Write;

        use miette::{GraphicalReportHandler, GraphicalTheme};
        use pernixc_driver::diagnostic::{simple_error, simple_note};

        let handler =
            GraphicalReportHandler::new_themed(GraphicalTheme::unicode());
        let stderr = std::io::stderr();
        let mut stderr = stderr.lock();

        let mut print_diagnostic =
            |diag: &pernixc_driver::diagnostic::PernixDiagnostic| {
                let mut output = String::new();
                let _ = handler.render_report(&mut output, diag);
                let _ = writeln!(stderr, "{output}");
            };

        print_diagnostic(&simple_error(
            "internal compiler error (ICE) occurred; the error is caused by a \
             bug in the compiler not the error in your code",
        ));

        print_diagnostic(&simple_note(
            "we're sorry for the inconvenience, please report this issue to \
             the developers",
        ));

        let payload_string = info
            .payload()
            .downcast_ref::<&'static str>()
            .map_or_else(
                || {
                    info.payload()
                        .downcast_ref::<String>()
                        .map_or("unknown", |payload| payload.as_str())
                },
                |payload| *payload,
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
            print_diagnostic(&simple_note(format!(
                "the ICE report has been written to: {}",
                path.display()
            )));
        } else {
            print_diagnostic(&simple_note(
                "dumping the ICE report to stderr:".to_string(),
            ));

            eprintln!(
                "```\n{}```",
                toml::to_string_pretty(&ice_report,).unwrap()
            );
        }

        print_diagnostic(&simple_note(
            "please report the issue to the developers with the written ICE \
             report",
        ));

        print_diagnostic(&simple_note(
            "the report was not automatically sent to the developers because \
             of privacy concerns",
        ));

        print_diagnostic(&simple_note(
            "we appreciate your effort to report the issue and help us \
             improve the compiler <3",
        ));

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

    let mut backtrace = String::default();

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
