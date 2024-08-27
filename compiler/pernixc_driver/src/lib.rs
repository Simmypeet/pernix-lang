//! Contains the main `run()` function for the compiler.

use std::{
    fs::File,
    path::PathBuf,
    process::ExitCode,
    sync::{Arc, RwLock},
};

use pernixc_base::{
    diagnostic::Report,
    handler::{Handler, Storage},
    log::{Message, Severity},
    source_file::{self, SourceFile},
};
use pernixc_semantic::symbol::table::{self, BuildTableError, DisplayObject};
use pernixc_syntax::syntax_tree::target::Target;

/// The arguments to the program.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, clap::Parser)]
#[clap(
    name = "pernixc",
    about = "The Pernix programming language compiler",
    author = "Simmypeet"
)]
pub struct Arguments {
    /// The input file to run the program on.
    pub file: PathBuf,

    /// The target name of the program. If not specified, the target name will
    /// be inferred from the file name.
    #[clap(short, long)]
    pub target_name: Option<String>,
}

/// A struct that implements [`Handler`] but prints all the message to the
/// standard error stream.
#[derive(Debug)]
struct Printer {
    printed: RwLock<bool>,
}
impl Printer {
    /// Creates a new [`Printer`].
    const fn new() -> Self { Self { printed: RwLock::new(false) } }

    fn has_printed(&self) -> bool { *self.printed.read().unwrap() }
}

impl<E: Report> Handler<E> for Printer
where
    E::Parameter: Default,
{
    fn receive(&self, error: E) {
        let Ok(diagnostic) = error.report(E::Parameter::default()) else {
            return;
        };

        eprintln!("{diagnostic}\n");

        *self.printed.write().unwrap() = true;
    }
}

/// Runs the program with the given arguments.
#[must_use]
pub fn run(argument: Arguments) -> ExitCode {
    let file = match File::open(&argument.file) {
        Ok(file) => file,
        Err(error) => {
            let msg = Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
            );

            eprintln!("{msg}");
            return ExitCode::FAILURE;
        }
    };

    let source_file = match SourceFile::load(file, argument.file.clone()) {
        Ok(file) => Arc::new(file),
        Err(source_file::Error::Io(error)) => {
            let msg = Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
            );

            eprintln!("{msg}");
            return ExitCode::FAILURE;
        }
        Err(source_file::Error::Utf8(error)) => {
            let msg = Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
            );

            eprintln!("{msg}");
            return ExitCode::FAILURE;
        }
    };

    let printer = Printer::new();

    // syntactic analysis
    let target = Target::parse(
        &source_file,
        argument.target_name.map_or_else(
            || {
                source_file
                    .full_path()
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_owned()
            },
            |name| name,
        ),
        &printer,
    );

    // early exit
    if printer.has_printed() {
        return ExitCode::FAILURE;
    }

    std::mem::drop(printer);

    let storage: Storage<Box<dyn pernixc_semantic::error::Error>> =
        Storage::new();

    let result = table::build(std::iter::once(target), &storage);

    let table = match result {
        Ok(table) => table,
        Err(BuildTableError::Suboptimal(table)) => {
            dbg!(&table);

            for error in storage.into_vec() {
                let msg = DisplayObject {
                    table: &table,
                    display: error.as_display_with_table(),
                };

                eprintln!("{msg}\n");
            }

            return ExitCode::FAILURE;
        }
        Err(BuildTableError::DuplicateTargetName(target)) => {
            let msg = Message::new(
                Severity::Error,
                format!("duplicate target name: {target}",),
            );

            eprintln!("{msg}");
            return ExitCode::FAILURE;
        }
    };

    dbg!(&table);

    // semantic analysis
    ExitCode::SUCCESS
}
