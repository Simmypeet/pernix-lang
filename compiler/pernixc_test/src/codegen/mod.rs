//! Contains the test related to code generation step
//!
//! The test will invoke the whole compiler pipeline and check whether the
//! code execute to the expected result. Each test will target a specific
//! language feature such as variable declaration, function declaration, etc.

use std::process::ExitCode;

use assert_cmd::Command;
use pernixc_driver::{
    Arguments, Build, Input, OptimizationLevel, Output, TargetKind,
};

mod aggregate_drop;
mod alignof;
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
mod no_drop;
mod number_match;
mod pointer_arithmetic;
mod prefix;
mod recursion;
mod recursive_drop;
mod sizeof;
mod struct_access;
mod tuple;
mod tuple_pack;
mod tuple_unpack;
mod zst_borrowing;
mod zst_optimization;

fn compile_file(content: &str) -> std::process::Output {
    compile_file_with(content, |_| ())
}

fn compile_file_with(
    content: &str,
    f: impl FnOnce(&mut Command),
) -> std::process::Output {
    let temp_dir = tempfile::tempdir().unwrap();

    let source_file = temp_dir.path().join("source.pnx");
    let executable_file = temp_dir.path().join("executable");

    std::fs::write(&source_file, content.as_bytes()).unwrap();

    // invoke the compiler and create the object file
    assert_eq!(
        pernixc_driver::run(&Arguments {
            command: pernixc_driver::Command::Build(Build {
                input: Input {
                    file: source_file,
                    target_name: Some("test".to_string()),
                    library_paths: Vec::new(),
                    show_progress: false,
                },
                output: Output { output: Some(executable_file.clone()) },
                opt_level: OptimizationLevel::O3,
                kind: TargetKind::Executable,
            }),
        }),
        ExitCode::SUCCESS
    );

    // execute the executable
    let mut command = Command::new(&executable_file);

    f(&mut command);

    command.output().unwrap()
}

#[must_use]
fn get_output_string(output: Vec<u8>) -> String {
    // on windows, replace \r\n with \n
    let output = String::from_utf8(output).unwrap();

    if cfg!(target_os = "windows") {
        output.replace("\r\n", "\n")
    } else {
        output
    }
}
