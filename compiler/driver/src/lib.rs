//! Contains the main `run()` function for the compiler.

use std::{io::Write, path::PathBuf, process::ExitCode, sync::Arc};

use miette::{GraphicalReportHandler, GraphicalTheme};
use pernixc_qbice::{
    Engine, InMemoryFactory, IncrementalStorageEngine, TrackedEngine,
};
use pernixc_symbol_impl::source_map::{SourceMap, create_source_map};
use pernixc_target::{Arguments, Build, Command, Run, TargetID, TargetKind};
use qbice::{serialize::Plugin, stable_hash::SeededStableHasherBuilder};
use tracing::instrument;

use crate::{
    diagnostic::{
        PernixDiagnostic, pernix_diagnostic_to_miette_diagnostic, simple_error,
    },
    llvm::MachineCodeKind,
};

pub mod term;

pub mod diagnostic;
mod llvm;

/// A struct that handles emitting diagnostics to the terminal.
pub struct ReportTerm<'a> {
    handler: GraphicalReportHandler,
    /// The writer to emit diagnostics to.
    pub writer: &'a mut dyn Write,
    /// The source map containing source files for diagnostics.
    pub source_map: &'a SourceMap,
}

impl std::fmt::Debug for ReportTerm<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReportTerm")
            .field("handler", &self.handler)
            .field("source_map", &self.source_map)
            .finish_non_exhaustive()
    }
}

impl ReportTerm<'_> {
    fn report(&mut self, diagnostic: &PernixDiagnostic) {
        let mut output = String::new();
        let _ = self.handler.render_report(&mut output, diagnostic);
        let _ = writeln!(self.writer, "{output}");
    }
}

fn get_output_path(
    argument_output: Option<PathBuf>,
    target_kind: TargetKind,
    report_term: &mut ReportTerm<'_>,
    target_name: &str,
) -> Option<PathBuf> {
    if let Some(output) = argument_output {
        Some(output)
    } else {
        let mut output = match std::env::current_dir() {
            Ok(dir) => dir,
            Err(error) => {
                report_term.report(&simple_error(format!(
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
    writer: &mut dyn Write,
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
                let handler = GraphicalReportHandler::new_themed(
                    GraphicalTheme::unicode(),
                );
                let diagnostic = simple_error(format!(
                    "failed to create incremental engine at '{}': {err}",
                    inc_path.display()
                ));
                let mut output = String::new();
                let _ = handler.render_report(&mut output, &diagnostic);
                let _ = writeln!(writer, "{output}");

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
    err_writer: &mut dyn Write,
    _out_writer: &mut dyn Write,
) -> ExitCode {
    let Some(mut engine) = create_engine(&argument, err_writer).await else {
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

    let handler = GraphicalReportHandler::new_themed(GraphicalTheme::unicode());

    let mut report_term =
        ReportTerm { handler, source_map: &source_map, writer: err_writer };

    let diagnostic_count = {
        let check = tracked_engine
            .query(&pernixc_check::Key { target_id: local_target_id })
            .await;

        let mut diagnostics: Vec<_> = check.all_diagnostics().collect();
        // Sort by severity and then by span location for consistent output
        diagnostics.sort_by(|a, b| a.severity.cmp(&b.severity));

        for diag in &diagnostics {
            let miette_diag =
                pernix_diagnostic_to_miette_diagnostic(diag, &source_map);
            report_term.report(&miette_diag);
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
        let diag = simple_error(format!(
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
    report_term: &mut ReportTerm<'_>,
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
                    report_term.report(&simple_error(format!(
                        "failed to spawn executable: {error}"
                    )));

                    return ExitCode::FAILURE;
                }
            };

            let status = match child.wait() {
                Ok(status) => status,
                Err(error) => {
                    report_term.report(&simple_error(format!(
                        "failed to wait for executable: {error}"
                    )));

                    return ExitCode::FAILURE;
                }
            };

            if let Some(code) = status.code() {
                if code != 0 {
                    report_term.report(&simple_error(format!(
                        "executable terminated with exit code {code}"
                    )));

                    return ExitCode::FAILURE;
                }
            } else {
                report_term.report(&simple_error(
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
