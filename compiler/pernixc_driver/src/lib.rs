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
use pernixc_semantic::{
    symbol::table::{self, BuildTableError},
    table::CompilationMetaData,
};
use pernixc_syntax::syntax_tree::target::Target;
use ron::ser::PrettyConfig;
use serde::de::DeserializeSeed;

/// The compilation format of the target.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, clap::ValueEnum,
)]
pub enum TargetKind {
    /// Compiles as an executable with a main function.
    #[clap(name = "bin")]
    Executable,

    /// Compiles as a library which can be later linked to other targets.
    #[clap(name = "lib")]
    Library,
}

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

    /// Specifies the compilation format of the target.
    #[clap(short, long, default_value = "bin")]
    pub kind: TargetKind,

    /// The paths to the `plib` library to link to the target.
    #[clap(short = 'l', long = "link")]
    pub library_paths: Vec<PathBuf>,

    /// The output path of the program. If not specified, the program will be
    /// written to the current working directory with the same name as the
    /// target.
    #[clap(short, long)]
    pub output: Option<PathBuf>,
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

impl<E: Report<()>> Handler<E> for Printer {
    fn receive(&self, error: E) {
        let Ok(diagnostic) = error.report(()) else {
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

    // retrieve the output path
    let output_path = if let Some(output) = argument.output {
        output
    } else {
        let mut output = match std::env::current_dir() {
            Ok(dir) => dir,
            Err(error) => {
                let msg = Message::new(
                    Severity::Error,
                    format!("failed to retrieve cwd: {error}"),
                );

                eprintln!("{msg}");
                return ExitCode::FAILURE;
            }
        };

        output.push(target.name());

        if argument.kind == TargetKind::Library {
            output.set_extension("plib");
        }

        output
    };

    if argument.kind == TargetKind::Library {
        let storage: Storage<
            Box<dyn pernixc_semantic::diagnostic::Diagnostic>,
        > = Storage::new();

        let reflector = pernixc_semantic::table::Table::reflector();
        let mut table = pernixc_semantic::table::Table::default();

        let mut inplace_table_deserializer =
            table.as_incremental_library_deserializer(&reflector);
        let mut link_library_ids = Vec::new();

        for link_library in argument.library_paths {
            let library_file = match std::fs::read(&link_library) {
                Ok(file) => file,
                Err(error) => {
                    let msg = Message::new(
                        Severity::Error,
                        format!("failed to open file: {error}"),
                    );

                    eprintln!("{msg}");
                    return ExitCode::FAILURE;
                }
            };

            let mut deserializer =
                match ron::de::Deserializer::from_bytes(&library_file) {
                    Ok(value) => value,
                    Err(error) => {
                        let msg = Message::new(
                            Severity::Error,
                            format!(
                                "failed to create deserializer for library: \
                                 {error}"
                            ),
                        );

                        eprintln!("{msg}");

                        return ExitCode::FAILURE;
                    }
                };

            let library_meta_data = match inplace_table_deserializer
                .deserialize(&mut deserializer)
            {
                Ok(meta_data) => meta_data,
                Err(error) => {
                    let msg = Message::new(
                        Severity::Error,
                        format!("failed to deserialize library: {error}"),
                    );

                    eprintln!("{msg}");
                    return ExitCode::FAILURE;
                }
            };

            link_library_ids.push(library_meta_data.target_id);
        }

        let target_id = table
            .add_target(
                target.name().clone(),
                link_library_ids,
                target,
                &storage,
            )
            .unwrap();

        let vec = storage.into_vec();

        if !vec.is_empty() {
            for error in vec {
                let Ok(diag) = error.report(&table) else {
                    continue;
                };

                eprintln!("{diag}\n");
            }

            ExitCode::FAILURE
        } else {
            // save as `ron` format
            let file = match File::create(&output_path) {
                Ok(file) => file,
                Err(error) => {
                    let msg = Message::new(
                        Severity::Error,
                        format!("failed to create file: {error}"),
                    );

                    eprintln!("{msg}");
                    return ExitCode::FAILURE;
                }
            };

            match ron::ser::to_writer_pretty(
                file,
                &table.as_library(&CompilationMetaData { target_id }),
                PrettyConfig::default(),
            ) {
                Ok(()) => ExitCode::SUCCESS,
                Err(error) => {
                    let msg = Message::new(
                        Severity::Error,
                        format!("failed to serialize table: {error}"),
                    );

                    eprintln!("{msg}");
                    ExitCode::FAILURE
                }
            }
        }
    } else {
        let storage: Storage<Box<dyn pernixc_semantic::error::Error>> =
            Storage::new();

        let result = table::build(std::iter::once(target), &storage);

        let table = match result {
            Ok(table) => table,
            Err(BuildTableError::Suboptimal(table)) => {
                dbg!(&table);

                for error in storage.into_vec() {
                    let Ok(diag) = error.report(&table) else {
                        continue;
                    };

                    eprintln!("{diag}\n");
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
}
