use std::{fmt::Display, fs::File, path::PathBuf, process::ExitCode, sync::RwLock};

use pernixc_base::{
    diagnostic::{Handler, Storage},
    log::{Message, Severity},
    source_file::{self, SourceFile},
};
use pernixc_semantic::table::Table;
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

    /// The target name of the program. If not specified, the target name will be inferred from the
    /// file name.
    #[clap(short, long)]
    pub target_name: Option<String>,
}

/// A struct that implements [`Handler`] but prints all the message to the standard error stream.
#[derive(Debug)]
struct Printer {
    printed: RwLock<bool>,
}

impl Printer {
    /// Creates a new [`Printer`].
    fn new() -> Self {
        Self {
            printed: RwLock::new(false),
        }
    }

    fn has_printed(&self) -> bool { *self.printed.read().unwrap() }
}

impl<E: Display> Handler<E> for Printer {
    fn receive(&self, error: E) {
        eprintln!("{}", error);
        *self.printed.write().unwrap() = true;
    }
}

/// Runs the program with the given arguments.
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
        Ok(file) => file,
        Err(source_file::Error::IoError(error)) => {
            let msg = Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
            );

            eprintln!("{msg}");
            return ExitCode::FAILURE;
        }
        Err(source_file::Error::Utf8Error(error)) => {
            let msg = Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
            );

            eprintln!("{msg}");
            return ExitCode::FAILURE;
        }
    };

    let printer = Printer::new();

    // parser
    let target = Target::parse(
        &source_file,
        match argument.target_name {
            Some(name) => name,
            None => source_file
                .full_path()
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .to_owned(),
        },
        &printer,
    );

    // early exit
    if printer.has_printed() {
        return ExitCode::FAILURE;
    }

    std::mem::drop(printer);

    // semantic analysis

    let storage: Storage<pernixc_semantic::error::Error> = Storage::new();
    let table = Table::build(rayon::iter::once(target), &storage);
    dbg!(table);

    if storage.as_vec().len() > 0 {
        dbg!(storage.as_vec());
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
