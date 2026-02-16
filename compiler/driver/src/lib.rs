//! Contains the main `run()` function for the compiler.

use std::{io::Write, path::PathBuf, process::ExitCode, sync::Arc};

use pernixc_qbice::{
    Engine, InMemoryFactory, IncrementalStorageEngine, TrackedEngine,
};
use pernixc_symbol_impl::source_map::create_source_map;
use pernixc_target::{Arguments, Build, Command, Run, TargetID, TargetKind};
use qbice::{serialize::Plugin, stable_hash::SeededStableHasherBuilder};
use tracing::instrument;

use crate::{llvm::MachineCodeKind, term::ReportTerm};

pub mod term;

mod llvm;

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
                report_term.report_simple_error(format!(
                    "failed to get current directory: {error}"
                ));
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
    report_term: &mut ReportTerm<'_>,
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
                report_term.report_simple_error(format!(
                    "failed to create incremental engine at '{}': {err}",
                    inc_path.display()
                ));

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
    let mut report_term =
        ReportTerm::new(err_writer, argument.command.input().fancy);

    let Some(mut engine) = create_engine(&argument, &mut report_term).await
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

    report_term.set_source_map(&source_map);

    let diagnostic_count = {
        let check = tracked_engine
            .query(&pernixc_check::Key { target_id: local_target_id })
            .await;

        let mut diagnostics: Vec<_> = check.all_diagnostics().collect();
        diagnostics.sort();

        for diag in &diagnostics {
            report_term.report_rendered(diag);
        }

        diagnostics.len()
    };

    if diagnostic_count != 0 {
        report_term.report_simple_error(format!(
            "Compilation aborted due to {diagnostic_count} error(s)"
        ));

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
                    report_term.report_simple_error(format!(
                        "failed to spawn executable: {error}"
                    ));

                    return ExitCode::FAILURE;
                }
            };

            let status = match child.wait() {
                Ok(status) => status,
                Err(error) => {
                    report_term.report_simple_error(format!(
                        "failed to wait for executable: {error}"
                    ));

                    return ExitCode::FAILURE;
                }
            };

            if let Some(code) = status.code() {
                if code != 0 {
                    report_term.report_simple_error(format!(
                        "executable terminated with exit code {code}"
                    ));

                    return ExitCode::FAILURE;
                }
            } else {
                report_term.report_simple_error(
                    "executable terminated by signal".to_string(),
                );

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
