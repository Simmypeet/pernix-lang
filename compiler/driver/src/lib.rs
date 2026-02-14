//! Contains the main `run()` function for the compiler.

use std::{path::PathBuf, process::ExitCode, sync::Arc};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle},
    term::{StylesWriter, termcolor::WriteColor},
};
use pernixc_qbice::{
    Engine, InMemoryFactory, IncrementalStorageEngine, TrackedEngine,
};
use pernixc_source_file::GlobalSourceID;
use pernixc_symbol_impl::source_map::{SourceMap, create_source_map};
use pernixc_target::{Arguments, Build, Command, Run, TargetID, TargetKind};
use qbice::{serialize::Plugin, stable_hash::SeededStableHasherBuilder};
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

async fn create_engine(
    argument: &Arguments,
    writer: &mut dyn WriteColor,
    config: &codespan_reporting::term::Config,
) -> Option<Engine> {
    if let Some(inc_path) = &argument.command.input().incremental_path {
        match Engine::new_with(
            Plugin::new(),
            IncrementalStorageEngine(inc_path),
            SeededStableHasherBuilder::new(0),
        )
        .await
        {
            Ok(engine) => Some(engine),
            Err(err) => {
                codespan_reporting::term::emit_to_write_style(
                    &mut StylesWriter::new(writer, &term::get_styles()),
                    config,
                    &pernixc_source_file::SourceMap::default(),
                    &Diagnostic::error().with_message(format!(
                        "failed to create incremental engine at '{}': {err}",
                        inc_path.display()
                    )),
                )
                .unwrap();

                None
            }
        }
    } else {
        Some(
            Engine::new_with(
                Plugin::new(),
                InMemoryFactory,
                SeededStableHasherBuilder::new(0),
            )
            .await
            .expect("in-memory is infailable"),
        )
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
    let report_config = term::get_coonfig();

    let Some(mut engine) =
        create_engine(&argument, err_writer, &report_config).await
    else {
        return ExitCode::FAILURE;
    };

    // Due to how rust compiler work, if a crate is linked without having any
    // symbols in it used, the crate will be completely ignored and the
    // static distributed registration will be optimized out, causing the engine
    // to not have the executors
    pernixc_source_file_impl::black_box();
    pernixc_lexical_impl::black_box();
    pernixc_syntax_impl::black_box();

    engine.register_program(pernixc_qbice::PERNIX_PROGRAM);

    // set the initial input, the invocation arguments
    let local_target_id =
        TargetID::from_target_name(&argument.command.input().target_name());
    let target_name = argument.command.input().target_name();

    let engine = Arc::new(engine);

    {
        let mut input_session = engine.input_session().await;

        pernixc_corelib::initialize_corelib(&mut input_session).await;

        input_session
            .set_input(
                pernixc_target::LinkKey { target_id: local_target_id },
                input_session.intern(std::iter::once(TargetID::CORE).collect()),
            )
            .await;

        input_session
            .set_input(
                pernixc_target::AllTargetIDsKey,
                input_session.intern(
                    [local_target_id, TargetID::CORE].into_iter().collect(),
                ),
            )
            .await;

        input_session
            .set_input(
                pernixc_target::MapKey,
                input_session.intern(
                    [
                        (
                            input_session.intern_unsized(
                                argument.command.input().target_name(),
                            ),
                            local_target_id,
                        ),
                        (
                            input_session.intern_unsized("core".to_owned()),
                            TargetID::CORE,
                        ),
                    ]
                    .into_iter()
                    .collect(),
                ),
            )
            .await;

        if let Some(explicit_seed) = argument.command.input().target_seed {
            input_session
                .set_input(
                    pernixc_target::SeedKey { target_id: local_target_id },
                    explicit_seed,
                )
                .await;
        }

        input_session
            .set_input(
                pernixc_target::Key { target_id: local_target_id },
                input_session.intern(argument.clone()),
            )
            .await;

        pernixc_source_file_impl::refresh_source_file_executors(
            &mut input_session,
        )
        .await;

        input_session.commit().await;
    }

    // now the query can start ...

    let tracked_engine = engine.clone().tracked().await;

    let source_map = tracked_engine.create_source_map(local_target_id).await;

    let styles = term::get_styles();

    let mut report_term = ReportTerm {
        config: report_config.clone(),
        source_map: &source_map,
        writer: StylesWriter::new(err_writer, &styles),
    };

    let diagnostic_count = {
        let mut diagnostics = Vec::new();

        let check = tracked_engine
            .query(&pernixc_check::Key { target_id: local_target_id })
            .await;

        for diag in check.all_diagnostics() {
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

    // let _ = engine
    //     .visualize_html(
    //         &pernixc_check::Key { target_id: local_target_id },
    //         "after.html",
    //     )
    //     .await;

    if diagnostic_count != 0 {
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
    }
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
