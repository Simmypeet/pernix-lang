use std::{io::ErrorKind, path::Path};

use codespan_reporting::diagnostic::Diagnostic;
use inkwell::{
    passes::PassBuilderOptions,
    targets::{
        InitializationConfig, RelocMode, Target as LLVMTarget, TargetMachine,
        TargetMachineOptions,
    },
};
use pernixc_diagnostic::Report;
use pernixc_query::TrackedEngine;
use pernixc_symbol::{get_target_root_module_id, member::get_members};
use pernixc_target::{Global, OptimizationLevel, TargetID};

use crate::{diagnostic::pernix_diagnostic_to_codespan_diagnostic, ReportTerm};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MachineCodeKind {
    LlvmIR,
    Object,
    Binary(bool),
}

#[allow(clippy::too_many_lines)]
pub(super) async fn emit_as_machine_code(
    engine: &TrackedEngine,
    target_id: TargetID,
    output_path: &Path,
    opt_level: OptimizationLevel,
    kind: MachineCodeKind,
    report_term: &mut ReportTerm<'_, '_>,
) -> bool {
    // initialize the traget
    inkwell::targets::Target::initialize_native(
        &InitializationConfig::default(),
    )
    .unwrap();

    let this_tripple = TargetMachine::get_default_triple();
    let target = LLVMTarget::from_triple(&this_tripple).unwrap();

    let target_machine = target
        .create_target_machine_from_options(
            &this_tripple,
            TargetMachineOptions::default().set_reloc_mode(RelocMode::PIC),
        )
        .unwrap();

    let target_data = target_machine.get_target_data();
    let inkwell_context = inkwell::context::Context::create();
    let root_module_id = Global::new(
        target_id,
        engine.get_target_root_module_id(target_id).await,
    );
    let storage =
        pernixc_handler::Storage::<pernixc_llvm::diagnostic::Diagnostic>::new();

    let result = if let Some(main_function_id) = engine
        .get_members(root_module_id)
        .await
        .member_ids_by_name
        .get("main")
        .copied()
    {
        pernixc_llvm::codegen(pernixc_llvm::Input {
            engine,
            main_function_id: Global::new(target_id, main_function_id),
            handler: &storage,
            inkwell_context: &inkwell_context,
            target_data,
        })
        .await
    } else {
        report_term.report(
            &Diagnostic::error()
                .with_message("no `main` function found in the target"),
        );
        return false;
    };

    let has_error = !storage.as_vec().is_empty();

    // for some reason, the boolean value needs to be stored in a variable
    // otherwise the borrow checker will complain about the lifetime of
    // `inkwell_context`, which is probably a bug in the rust compiler.
    let result = if let (Some(module), false) = (result, has_error) {
        if let Err(error) = module.verify() {
            let ir = module.print_to_string().to_string();
            panic!(
                "generated LLVM IR is invalid: {}\n\nLLVM IR:\n{}",
                error.to_string(),
                ir
            );
        }

        module
            .run_passes(
                match opt_level {
                    OptimizationLevel::O0 => "default<O0>",
                    OptimizationLevel::O1 => "default<O1>",
                    OptimizationLevel::O2 => "default<O2>",
                    OptimizationLevel::O3 => "default<O3>",
                },
                &target_machine,
                PassBuilderOptions::create(),
            )
            .unwrap();

        let temp_obj_path =
            matches!(kind, MachineCodeKind::Binary(_)).then(|| {
                let mut temp_obj_path = output_path.to_owned();
                temp_obj_path.set_file_name(format!(
                    "{}-{}-temp.o",
                    output_path.file_stem().unwrap().to_str().unwrap(),
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .as_secs()
                ));
                temp_obj_path
            });

        let result = match kind {
            MachineCodeKind::Object | MachineCodeKind::Binary(_) => {
                target_machine.write_to_file(
                    &module,
                    inkwell::targets::FileType::Object,
                    temp_obj_path.as_ref().map_or(output_path, |x| x.as_path()),
                )
            }
            MachineCodeKind::LlvmIR => module.print_to_file(output_path),
        };

        if let Err(error) = result {
            report_term.report(
                &Diagnostic::error().with_message(format!(
                    "failed to write object file: {error}"
                )),
            );

            return false;
        }

        if let MachineCodeKind::Binary(_) = kind {
            if !invoke_linker(
                temp_obj_path.as_ref().unwrap(),
                output_path,
                report_term,
            ) {
                report_term.report(&Diagnostic::note().with_message(format!(
                    "you can continue to link the object file manually using \
                     the appropriate linker command at `{}`",
                    temp_obj_path.as_ref().unwrap().display()
                )));
                return false;
            }

            // delete the temporary object file
            if let Err(error) =
                std::fs::remove_file(temp_obj_path.as_ref().unwrap())
            {
                report_term.report(&Diagnostic::warning().with_message(
                    format!("failed to delete temporary object file: {error}"),
                ));

                return false;
            }
        }

        true
    } else {
        for error in storage.into_vec() {
            let diag = error.report(engine).await.unwrap();
            let diag = pernix_diagnostic_to_codespan_diagnostic(&diag);

            report_term.report(&diag);
        }

        false
    };

    result
}

fn linker_command(obj: &Path, out: &Path) -> std::process::Command {
    let mut cmd = std::process::Command::new("clang");
    cmd.arg(obj).arg("-o").arg(out);

    if cfg!(target_os = "windows") {
        // allow to use stdio functions like `printf` and `scanf`
        cmd.arg("-llegacy_stdio_definitions");
    }

    cmd
}

fn invoke_linker(
    temp_obj_path: &Path,
    output_path: &Path,
    report_term: &mut ReportTerm<'_, '_>,
) -> bool {
    let mut cmd = linker_command(temp_obj_path, output_path);

    let mut child = match cmd.spawn() {
        Ok(child) => child,
        Err(err) => {
            let message = if err.kind() == ErrorKind::NotFound {
                if cfg!(target_os = "windows") {
                    Some(
                        "`clang` is not installed, please install it from \
                         LLVM's official website or from Microsoft Visual \
                         Studio",
                    )
                } else if cfg!(target_os = "linux") {
                    Some(
                        "`clang` is not installed, please install it from \
                         your package manager (e.g. `apt install clang` or \
                         `pacman -S clang`",
                    )
                } else if cfg!(target_os = "macos") {
                    Some(
                        "`clang` is not installed, please install it from \
                         Xcode command line tools by running `xcode-select \
                         --install`",
                    )
                } else {
                    None
                }
            } else {
                None
            };

            let mut diag = Diagnostic::error()
                .with_message(format!("failed to spawn linker: {err}"));
            diag = if let Some(message) = message {
                diag.with_notes(vec![message.to_string()])
            } else {
                diag
            };

            report_term.report(&diag);

            return false;
        }
    };

    let status = match child.wait() {
        Ok(status) => status,
        Err(err) => {
            report_term.report(&Diagnostic::error().with_message(format!(
                "failed to wait for linker process: {err}"
            )));

            return false;
        }
    };

    if !status.success() {
        report_term.report(&Diagnostic::error().with_message(format!(
            "linker process exited with status code: {}",
            status.code().map_or("unknown".to_string(), |c| c.to_string())
        )));

        return false;
    }

    true
}
