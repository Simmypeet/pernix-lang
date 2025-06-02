//! Contains the main `run()` function for the compiler.

use std::{fs::File, io::BufWriter, path::PathBuf, process::ExitCode};

use argument::Arguments;
use clap::Args;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::termcolor::{self, ColorChoice, StandardStream},
};
use pernixc_source_file::{ByteIndex, GlobalSourceID, SourceFile, SourceMap};
use pernixc_target::TargetID;
use postcard::ser_flavors::io::WriteFlavor;
use term::get_coonfig;

pub mod argument;
pub mod term;

struct ReportTerm {
    config: codespan_reporting::term::Config,
    stderr: termcolor::StandardStream,
}

impl ReportTerm {
    fn report(
        &mut self,
        source_map: &mut SourceMap,
        diagnostic: &Diagnostic<GlobalSourceID>,
    ) {
        let mut writer = self.stderr.lock();
        let _ = codespan_reporting::term::emit(
            &mut writer,
            &self.config,
            source_map,
            diagnostic,
        );
    }
}

fn pernix_diagnostic_to_codespan_diagnostic(
    diagostic: pernixc_diagnostic::Diagnostic<ByteIndex>,
) -> codespan_reporting::diagnostic::Diagnostic<GlobalSourceID> {
    let result = match diagostic.severity {
        pernixc_diagnostic::Severity::Error => {
            codespan_reporting::diagnostic::Diagnostic::error()
        }
        pernixc_diagnostic::Severity::Warning => {
            codespan_reporting::diagnostic::Diagnostic::warning()
        }
        pernixc_diagnostic::Severity::Info => {
            codespan_reporting::diagnostic::Diagnostic::note()
        }
    }
    .with_message(diagostic.message)
    .with_labels(
        std::iter::once({
            let mut primary = Label::primary(
                diagostic.span.source_id,
                diagostic.span.range(),
            );

            if let Some(label_message) = diagostic.label {
                primary = primary.with_message(label_message);
            }

            primary
        })
        .chain(diagostic.related.into_iter().map(|x| {
            Label::secondary(x.span.source_id, x.span.range())
                .with_message(x.message)
        }))
        .collect(),
    );

    if let Some(msg) = diagostic.help_message {
        result.with_notes(vec![msg])
    } else {
        result
    }
}

fn create_root_source_file(
    argument: &Arguments,
    source_map: &mut SourceMap,
    report_term: &mut ReportTerm,
) -> Option<GlobalSourceID> {
    let file = match File::open(&argument.command.input().file) {
        Ok(file) => file,
        Err(error) => {
            let msg = Diagnostic::error().with_message(format!(
                "{}: {error}",
                argument.command.input().file.display()
            ));
            report_term.report(source_map, &msg);

            return None;
        }
    };

    let source_file =
        match SourceFile::load(file, argument.command.input().file.clone()) {
            Ok(file) => file,
            Err(pernixc_source_file::Error::Io(error)) => {
                let msg = Diagnostic::error().with_message(format!(
                    "{}: {error}",
                    argument.command.input().file.display()
                ));
                report_term.report(source_map, &msg);

                return None;
            }
            Err(pernixc_source_file::Error::Utf8(error)) => {
                let msg = Diagnostic::error().with_message(format!(
                    "{}: {error}",
                    argument.command.input().file.display()
                ));
                report_term.report(source_map, &msg);

                return None;
            }
        };

    let id = source_map.register(TargetID::Local, source_file);

    Some(TargetID::Local.make_global(id))
}

/// The input to the compiler.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Args)]
pub struct Input {
    /// The input file to compile.
    ///
    /// This file is the root source file of the compilation; the module will
    /// stem from this file.
    pub file: PathBuf,

    /// The name of the target; if not specified, the target name will be
    /// inferred from the file name.
    #[clap(short = 't', long = "target")]
    pub target_name: Option<String>,

    /// The paths to the `plib` library to link to the target.
    #[clap(short = 'l', long = "link")]
    pub library_paths: Vec<PathBuf>,

    /// The path to the incremental compilation data.
    #[clap(long = "inc")]
    pub incremental_path: Option<PathBuf>,

    /// Whether to show the progress of the compilation.
    #[clap(long)]
    pub show_progress: bool,
}

/// Runs the program with the given arguments.
#[must_use]
#[allow(clippy::too_many_lines)]
pub fn run(argument: &Arguments) -> ExitCode {
    let mut report_term = ReportTerm {
        config: get_coonfig(),
        stderr: StandardStream::stderr(ColorChoice::Always),
    };
    let mut source_map = SourceMap::new();

    let Some(root_source_id) =
        create_root_source_file(argument, &mut source_map, &mut report_term)
    else {
        return ExitCode::FAILURE;
    };

    let mut query_runtime = pernixc_query::runtime::Runtime::default();
    let mut serde = pernixc_query::serde::Serde::default();

    pernixc_query_base::register_runtime(&mut query_runtime, &mut serde);

    let target_name =
        argument.command.input().target_name.clone().unwrap_or_else(|| {
            source_map[root_source_id]
                .full_path()
                .file_stem()
                .unwrap()
                .to_string_lossy()
                .to_string()
        });

    let database = match pernixc_query_base::start_query_database(
        &mut source_map,
        root_source_id,
        argument.command.input().library_paths.iter().map(PathBuf::as_path),
        &target_name,
        argument
            .command
            .input()
            .incremental_path
            .as_ref()
            .map(|x| (x.as_path(), &serde)),
    ) {
        Ok(database) => database,
        Err(error) => {
            let msg = match error {
                pernixc_query_base::Error::OpenIncrementalFileIO(error) => {
                    Diagnostic::error().with_message(format!(
                        "Failed to open incremental file: {error}"
                    ))
                }
                pernixc_query_base::Error::IncrementalFileDeserialize(
                    error_kind,
                ) => Diagnostic::error().with_message(format!(
                    "Failed to load (deserialize) incremental file: \
                     {error_kind}"
                )),
                pernixc_query_base::Error::ReadIncrementalFileIO(error) => {
                    Diagnostic::error().with_message(format!(
                        "Failed to read incremental file: {error}"
                    ))
                }
            };

            report_term.report(&mut source_map, &msg);
            return ExitCode::FAILURE;
        }
    };

    if let Some(incremental_path) =
        argument.command.input().incremental_path.as_ref()
    {
        let incremental_file = match File::create(incremental_path) {
            Ok(file) => file,
            Err(error) => {
                let msg = Diagnostic::error().with_message(format!(
                    "Failed to create incremental file: {error}"
                ));
                report_term.report(&mut source_map, &msg);
                return ExitCode::FAILURE;
            }
        };
        let serializable_database = database.database.serializable(&serde);

        let writer = BufWriter::new(incremental_file);
        let flavor = WriteFlavor::new(writer);

        // Serialize using postcard and write to file
        if let Err(error) =
            postcard::serialize_with_flavor(&serializable_database, flavor)
        {
            let msg = Diagnostic::error().with_message(format!(
                "Failed to serialize incremental file: {error}"
            ));
            report_term.report(&mut source_map, &msg);
            return ExitCode::FAILURE;
        }
    }

    ExitCode::SUCCESS
}
