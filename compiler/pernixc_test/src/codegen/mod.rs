//! Contains the test related to code generation step
//!
//! The test will invoke the whole compiler pipeline and check whether the
//! code execute to the expected result. Each test will target a specific
//! language feature such as variable declaration, function declaration, etc.

use std::{path::Path, process::ExitCode};

use assert_cmd::Command;
use pernixc_driver::{Arguments, OptimizationLevel, TargetKind};

mod aggregate_drop;
mod array_index;
mod arrow_access;
mod cast;
mod discarded_drop;
mod drop;
mod drop_packed_tuple;
mod r#enum;
mod exit_failure;
mod exit_success;
mod fibonacci_program;
mod hello_world;
mod max;
mod nested_drop;
mod number_match;
mod pointer_arithmetic;
mod prefix;
mod recursion;
mod struct_access;
mod tuple;
mod tuple_pack;
mod tuple_unpack;
mod zst_borrowing;
mod zst_optimization;

fn invoke_linker_command(obj: &Path, out: &Path) {
    if cfg!(target_os = "macos") {
        Command::new("cc")
            .arg("-o")
            .arg(out)
            .arg(obj)
            .arg("-Wl")
            .arg("-dead_strip")
            .assert()
            .success();
    } else if cfg!(target_os = "linux") {
        Command::new("cc")
            .arg("-o")
            .arg(out)
            .arg(obj)
            .arg("-no-pie")
            .arg("-W")
            .arg("--data-sections")
            .assert()
            .success();
    } else if cfg!(target_os = "windows") {
        // NOTE: not really sure about the flags; need further testing.
        Command::new("link")
            .arg("/out:")
            .arg(out)
            .arg(obj)
            .arg("kernel32.lib")
            .arg("ucrt.lib")
            .arg("msvcrt.lib")
            .assert()
            .success();
    } else {
        panic!("Unsupported target OS");
    }
}

fn compile_file(content: &str) -> std::process::Output {
    compile_file_with(content, |_| ())
}

fn compile_file_with(
    content: &str,
    f: impl FnOnce(&mut Command),
) -> std::process::Output {
    let temp_dir = tempfile::tempdir().unwrap();

    let source_file = temp_dir.path().join("source.pnx");
    let object_file = temp_dir.path().join("object.o");
    let executable_file = temp_dir.path().join("executable");

    std::fs::write(&source_file, content.as_bytes()).unwrap();

    // invoke the compiler and create the object file
    assert_eq!(
        pernixc_driver::run(Arguments {
            file: source_file,
            opt_level: OptimizationLevel::O3,
            target_name: Some("test".to_string()),
            kind: TargetKind::Executable,
            library_paths: Vec::new(),
            output: Some(object_file.clone()),
            show_progress: false,
            dump_ron: false,
        }),
        ExitCode::SUCCESS
    );

    // invoke the linker
    invoke_linker_command(&object_file, &executable_file);

    // execute the executable
    let mut command = Command::new(&executable_file);

    f(&mut command);

    command.output().unwrap()
}
