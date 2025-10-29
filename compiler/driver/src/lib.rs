//! Contains the main `run()` function for the compiler.

use std::{path::PathBuf, process::ExitCode, sync::Arc};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle},
    term::{termcolor::WriteColor, StylesWriter},
};
use pernixc_query::{runtime::persistence::Persistence, Engine, TrackedEngine};
use pernixc_source_file::GlobalSourceID;
use pernixc_symbol_impl::source_map::{create_source_map, SourceMap};
use pernixc_target::{Arguments, Build, Command, Run, TargetID, TargetKind};
use tracing::instrument;

use crate::{
    diagnostic::pernix_diagnostic_to_codespan_diagnostic, llvm::MachineCodeKind,
};

pub mod term;

mod diagnostic;
mod llvm;

struct ReportTerm<'a, 's> {
    config: codespan_reporting::term::Config,
    writer: StylesWriter<'a, &'s mut dyn WriteColor>,
    source_map: &'s SourceMap,
}

impl ReportTerm<'_, '_> {
    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn report(&mut self, diagnostic: &Diagnostic<GlobalSourceID>) {
        let _ = codespan_reporting::term::emit_to_write_style(
            &mut self.writer,
            &self.config,
            self.source_map,
            diagnostic,
        );
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SortableDiagnostic(Diagnostic<GlobalSourceID>);

#[allow(clippy::trivially_copy_pass_by_ref)]
const fn cmp_label_style(a: &LabelStyle, b: &LabelStyle) -> std::cmp::Ordering {
    match (a, b) {
        (LabelStyle::Primary, LabelStyle::Primary) => std::cmp::Ordering::Equal,
        (LabelStyle::Primary, LabelStyle::Secondary) => {
            std::cmp::Ordering::Less
        }
        (LabelStyle::Secondary, LabelStyle::Primary) => {
            std::cmp::Ordering::Greater
        }
        (LabelStyle::Secondary, LabelStyle::Secondary) => {
            std::cmp::Ordering::Equal
        }
    }
}

fn cmp_label(
    a: &Label<GlobalSourceID>,
    b: &Label<GlobalSourceID>,
) -> std::cmp::Ordering {
    cmp_label_style(&a.style, &b.style)
        .then_with(|| a.file_id.cmp(&b.file_id))
        .then_with(|| a.range.start.cmp(&b.range.start))
        .then_with(|| a.range.end.cmp(&b.range.end))
        .then_with(|| a.message.cmp(&b.message))
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ComparableLabel<'a>(pub &'a Label<GlobalSourceID>);

impl PartialOrd for ComparableLabel<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ComparableLabel<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        cmp_label(self.0, other.0)
    }
}

impl PartialOrd for SortableDiagnostic {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SortableDiagnostic {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .severity
            .cmp(&other.0.severity)
            .then_with(|| {
                self.0
                    .labels
                    .iter()
                    .map(ComparableLabel)
                    .cmp(other.0.labels.iter().map(ComparableLabel))
            })
            .then_with(|| self.0.message.cmp(&other.0.message))
    }
}

fn get_output_path(
    argument_output: Option<PathBuf>,
    target_kind: TargetKind,
    report_term: &mut ReportTerm<'_, '_>,
    target_name: &str,
) -> Option<PathBuf> {
    if let Some(output) = argument_output {
        Some(output)
    } else {
        let mut output = match std::env::current_dir() {
            Ok(dir) => dir,
            Err(error) => {
                report_term.report(&Diagnostic::error().with_message(format!(
                    "failed to get current directory: {error}"
                )));
                return None;
            }
        };

        output.push(target_name);

        if target_kind == TargetKind::Library {
            output.set_extension("plib");
        }

        Some(output)
    }
}

/// Runs the program with the given arguments.
#[must_use]
#[allow(clippy::too_many_lines, clippy::needless_pass_by_value)]
#[instrument(skip(err_writer, _out_writer))]
pub async fn run(
    argument: Arguments,
    err_writer: &mut dyn WriteColor,
    _out_writer: &mut dyn WriteColor,
) -> ExitCode {
    let mut serde_registry = pernixc_register::SerdeRegistry::default();

    let report_styles = term::get_styles();
    let report_config = term::get_coonfig();
    let simple_file = codespan_reporting::files::SimpleFile::new("", "");

    // all the serialization/deserialization runtime information must be
    // registered before creating a persistence layer
    pernixc_register::Registration::register_serde_registry(
        &mut serde_registry,
    );

    let mut engine = Engine::default();

    // setup the persistence layer if the incremental setting is set
    if let Some(incremental_path) = &argument.command.input().incremental_path {
        let persistence = match Persistence::new(
            incremental_path.clone(),
            Arc::new(serde_registry),
        ) {
            Ok(persistence) => persistence,
            Err(error) => {
                let diag = Diagnostic::error().with_message(format!(
                    "Failed to setup persistence: {error}"
                ));

                codespan_reporting::term::emit_to_write_style(
                    &mut StylesWriter::new(err_writer, &report_styles),
                    &report_config,
                    &simple_file,
                    &diag,
                )
                .unwrap();

                return ExitCode::FAILURE;
            }
        };

        // load the database from the persistence layer
        engine.database = match persistence.load_database() {
            Ok(database) => database,
            Err(error) => {
                let diag = Diagnostic::error().with_message(format!(
                    "Failed to load incremental database: {error}"
                ));

                codespan_reporting::term::emit_to_write_style(
                    &mut StylesWriter::new(err_writer, &report_styles),
                    &report_config,
                    &simple_file,
                    &diag,
                )
                .unwrap();

                return ExitCode::FAILURE;
            }
        };

        engine.runtime.persistence = Some(persistence);
    }

    // if persistence is set, setup the value that will be skipped from saving
    // into the persistence layer.
    if let Some(persistence) = engine.runtime.persistence.as_mut() {
        pernixc_register::Registration::register_skip_persistence(persistence);
    }

    // final step, setup the query executors for the engine
    pernixc_register::Registration::register_executor(
        &mut engine.runtime.executor,
    );

    // set the initial input, the invocation arguments
    let local_target_id =
        TargetID::from_target_name(&argument.command.input().target_name());
    let target_name = argument.command.input().target_name();

    let mut engine = Arc::new(engine);
    pernixc_corelib::initialize_corelib(&mut engine).await;

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.always_reverify();

            x.set_input(
                pernixc_target::LinkKey(local_target_id),
                Arc::new(std::iter::once(TargetID::CORE).collect()),
            )
            .await;

            x.set_input(
                pernixc_target::AllTargetIDsKey,
                Arc::new(
                    [local_target_id, TargetID::CORE].into_iter().collect(),
                ),
            )
            .await;

            x.set_input(
                pernixc_target::MapKey,
                Arc::new(
                    [
                        (
                            argument.command.input().target_name(),
                            local_target_id,
                        ),
                        ("core".into(), TargetID::CORE),
                    ]
                    .into_iter()
                    .collect(),
                ),
            )
            .await;

            if let Some(explicit_seed) = argument.command.input().target_seed {
                x.set_input(
                    pernixc_target::SeedKey(local_target_id),
                    explicit_seed,
                )
                .await;
            }

            x.set_input(
                pernixc_target::Key(local_target_id),
                Arc::new(argument.clone()),
            )
            .await;
        })
        .await;

    tracing::info!(
        "Starting compilation with database version: {}",
        engine.version()
    );

    // now the query can start ...

    let tracked_engine = engine.tracked();

    let source_map = tracked_engine.create_source_map(local_target_id).await;

    let styles = term::get_styles();

    let mut report_term = ReportTerm {
        config: report_config.clone(),
        source_map: &source_map,
        writer: StylesWriter::new(err_writer, &styles),
    };

    let diagnostic_count = {
        let mut diagnostics = Vec::new();

        let symbol_errors = tracked_engine
            .query(&pernixc_symbol_impl::diagnostic::RenderedKey(
                local_target_id,
            ))
            .await
            .unwrap();

        let term_errors = tracked_engine
            .query(&pernixc_semantic_element_impl::diagnostic::AllRenderedKey(
                local_target_id,
            ))
            .await
            .unwrap();

        for diag in symbol_errors.as_ref() {
            diagnostics.push(SortableDiagnostic(
                pernix_diagnostic_to_codespan_diagnostic(diag),
            ));
        }

        for diag in term_errors.iter().flat_map(|x| x.iter()) {
            diagnostics.push(SortableDiagnostic(
                pernix_diagnostic_to_codespan_diagnostic(diag),
            ));
        }

        diagnostics.sort();

        for diagnostic in &diagnostics {
            report_term.report(&diagnostic.0);
        }

        diagnostics.len()
    };

    let result = if diagnostic_count != 0 {
        let diag = codespan_reporting::diagnostic::Diagnostic::error()
            .with_message(format!(
                "Compilation aborted due to {diagnostic_count} error(s)"
            ));

        report_term.report(&diag);

        ExitCode::FAILURE
    } else {
        build(
            &argument,
            &target_name,
            local_target_id,
            &tracked_engine,
            &mut report_term,
        )
        .await
    };

    drop(tracked_engine);
    let mut engine =
        Arc::try_unwrap(engine).expect("Engine should be unique at this point");

    if let Err(error) = engine.save_database() {
        let diag = codespan_reporting::diagnostic::Diagnostic::error()
            .with_message(format!("Failed to save database: {error}"));

        codespan_reporting::term::emit_to_write_style(
            &mut report_term.writer,
            &report_config,
            &simple_file,
            &diag,
        )
        .unwrap();
        return ExitCode::FAILURE;
    }

    result
}

#[allow(clippy::too_many_lines)]
async fn build(
    argument: &Arguments,
    target_name: &str,
    current_target_id: TargetID,
    tracked_engine: &TrackedEngine,
    report_term: &mut ReportTerm<'_, '_>,
) -> ExitCode {
    // retrieve the output path
    let output_path = match &argument.command {
        Command::Build(Build { output, .. })
        | Command::Run(Run { output, .. }) => Some(get_output_path(
            output.output.clone(),
            argument
                .command
                .as_build()
                .map_or(TargetKind::Executable, |x| x.kind),
            report_term,
            target_name,
        )),

        Command::Check(_) => None,
    };

    let output_path = match output_path {
        Some(Some(path)) => Some(path),
        None => None,

        Some(None) => {
            return ExitCode::FAILURE;
        }
    };

    match &argument.command {
        Command::Build(Build {
            opt_level,
            kind:
                TargetKind::Executable | TargetKind::LLvmIR | TargetKind::Object,
            ..
        })
        | Command::Run(Run { opt_level, .. }) => {
            let kind = match &argument.command {
                Command::Build(Build { kind, .. }) => match kind {
                    TargetKind::Executable => MachineCodeKind::Binary(false),
                    TargetKind::Object => MachineCodeKind::Object,
                    TargetKind::LLvmIR => MachineCodeKind::LlvmIR,

                    TargetKind::Library | TargetKind::Ron => unreachable!(),
                },

                Command::Run(_) => MachineCodeKind::Binary(true),
                Command::Check(_) => unreachable!(),
            };

            // emit the executable
            if !llvm::emit_as_machine_code(
                tracked_engine,
                current_target_id,
                output_path.as_ref().unwrap(),
                *opt_level,
                kind,
                report_term,
            )
            .await
            {
                return ExitCode::FAILURE;
            }

            let run_executable = matches!(kind, MachineCodeKind::Binary(true));

            if !run_executable {
                return ExitCode::SUCCESS;
            }

            let mut command =
                std::process::Command::new(output_path.as_ref().unwrap());

            command.stdin(std::process::Stdio::inherit());
            command.stdout(std::process::Stdio::inherit());
            command.stderr(std::process::Stdio::inherit());

            let mut child = match command.spawn() {
                Ok(child) => child,
                Err(error) => {
                    report_term.report(&Diagnostic::error().with_message(
                        format!("failed to spawn executable: {error}"),
                    ));

                    return ExitCode::FAILURE;
                }
            };

            let status = match child.wait() {
                Ok(status) => status,
                Err(error) => {
                    report_term.report(&Diagnostic::error().with_message(
                        format!("failed to wait for executable: {error}"),
                    ));

                    return ExitCode::FAILURE;
                }
            };

            if let Some(code) = status.code() {
                if code != 0 {
                    report_term.report(&Diagnostic::error().with_message(
                        format!("executable terminated with exit code {code}"),
                    ));

                    return ExitCode::FAILURE;
                }
            } else {
                report_term.report(&Diagnostic::error().with_message(
                    "executable terminated by signal".to_string(),
                ));

                return ExitCode::FAILURE;
            }

            ExitCode::SUCCESS
        }

        Command::Build(Build {
            kind: TargetKind::Library | TargetKind::Ron,
            ..
        })
        | Command::Check(_) => ExitCode::SUCCESS,
    }
}
