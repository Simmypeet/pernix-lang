//! Contains the main `run()` function for the compiler.

use std::{fs::File, io::BufWriter, path::PathBuf, process::ExitCode};

use argument::Arguments;
use clap::Args;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::termcolor::WriteColor,
};
use dashmap::DashMap;
use pernixc_diagnostic::Report;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_source_file::{
    ByteIndex, GlobalSourceID, Location, SourceFile, SourceMap,
};
use pernixc_target::TargetID;
use postcard::ser_flavors::io::WriteFlavor;
use term::get_coonfig;

pub mod argument;
pub mod term;

struct ReportTerm<'a> {
    config: codespan_reporting::term::Config,
    err_writer: &'a mut dyn WriteColor,
}

impl ReportTerm<'_> {
    fn report(
        &mut self,
        source_map: &mut SourceMap,
        diagnostic: &Diagnostic<GlobalSourceID>,
    ) {
        let _ = codespan_reporting::term::emit(
            self.err_writer,
            &self.config,
            source_map,
            diagnostic,
        );
    }
}

fn pernix_diagnostic_to_codespan_diagnostic(
    diagostic: pernixc_diagnostic::Diagnostic<ByteIndex>,
) -> codespan_reporting::diagnostic::Diagnostic<GlobalSourceID> {
    let mut result = match diagostic.severity {
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
    .with_message(diagostic.message);

    if let Some((span, label)) = diagostic.span {
        result = result.with_labels(
            std::iter::once({
                let mut primary = Label::primary(span.source_id, span.range());

                if let Some(label_message) = label {
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
    }

    if let Some(msg) = diagostic.help_message {
        result.with_notes(vec![msg])
    } else {
        result
    }
}

fn rel_pernix_diagnostic_to_codespan_diagnostic(
    diagostic: pernixc_diagnostic::Diagnostic<RelativeLocation>,
    source_map: &SourceMap,
    token_trees_by_source_id: &DashMap<
        GlobalSourceID,
        pernixc_lexical::tree::Tree,
    >,
) -> codespan_reporting::diagnostic::Diagnostic<GlobalSourceID> {
    let mut result = match diagostic.severity {
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
    .with_message(diagostic.message);

    if let Some((span, label)) = diagostic.span {
        result = result.with_labels(
            std::iter::once({
                let token_tree = token_trees_by_source_id
                    .get(&span.source_id)
                    .expect("Source ID not found in token trees");

                let source_file = source_map.get(span.source_id).unwrap();
                let begin =
                    span.start.to_absolute_index(&source_file, &token_tree);
                let end = span.end.to_absolute_index(&source_file, &token_tree);

                let mut primary = Label::primary(span.source_id, begin..end);

                if let Some(label_message) = label {
                    primary = primary.with_message(label_message);
                }

                primary
            })
            .chain(diagostic.related.into_iter().map(|x| {
                let token_tree = token_trees_by_source_id
                    .get(&x.span.source_id)
                    .expect("Source ID not found in token trees");

                let source_file = source_map.get(x.span.source_id).unwrap();

                let begin =
                    x.span.start.to_absolute_index(&source_file, &token_tree);
                let end =
                    x.span.end.to_absolute_index(&source_file, &token_tree);

                Label::secondary(x.span.source_id, begin..end)
                    .with_message(x.message)
            }))
            .collect(),
        );
    }

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
pub fn run(
    argument: &Arguments,
    err_writer: &mut dyn WriteColor,
    _out_writer: &mut dyn WriteColor,
) -> ExitCode {
    let mut report_term = ReportTerm { config: get_coonfig(), err_writer };
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
            source_map
                .get(root_source_id)
                .unwrap()
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

    for diagnostic in database.module_parsing_errors {
        let codespan_reporting = match diagnostic {
            pernixc_query_base::module::Error::Lexical(error) => {
                pernix_diagnostic_to_codespan_diagnostic(
                    error.report(&source_map),
                )
            }

            pernixc_query_base::module::Error::Syntax(error) => {
                pernix_diagnostic_to_codespan_diagnostic(
                    error.report(
                        &database
                            .token_trees_by_source_id
                            .get(&error.source_id)
                            .unwrap(),
                    ),
                )
            }

            pernixc_query_base::module::Error::RootSubmoduleConflict(
                root_submodule_conflict,
            ) => rel_pernix_diagnostic_to_codespan_diagnostic(
                root_submodule_conflict.report(()),
                &source_map,
                &database.token_trees_by_source_id,
            ),

            pernixc_query_base::module::Error::SourceFileLoadFail(
                source_file_load_fail,
            ) => rel_pernix_diagnostic_to_codespan_diagnostic(
                source_file_load_fail.report(()),
                &source_map,
                &database.token_trees_by_source_id,
            ),
            pernixc_query_base::module::Error::ModuleRedefinition(
                module_redefinition,
            ) => rel_pernix_diagnostic_to_codespan_diagnostic(
                module_redefinition.report(()),
                &source_map,
                &database.token_trees_by_source_id,
            ),
        };

        report_term.report(&mut source_map, &codespan_reporting);
    }

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
