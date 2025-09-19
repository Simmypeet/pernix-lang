#![allow(missing_docs)]

use std::{
    io::{BufRead, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use pernixc_target::{
    Arguments, Build, Command as CompilerCommand, Input, OptimizationLevel,
    TargetKind,
};
use test_generator::test_resources;

#[test_resources("compiler/e2e/test/run/**/main.pnx")]
fn main(resource: &str) {
    let file_path = PathBuf::from(resource);
    std::env::set_current_dir(env!("PERNIXC_CARGO_WORKSPACE_DIR")).unwrap();

    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("Failed to create Tokio runtime")
        .block_on(test(&file_path));
}

#[derive(Debug)]
struct Case {
    pub stdin: Vec<String>,
    pub stdout: Vec<String>,
}

/// Parse test case and expected output from a source file
fn parse_test_spec(file_path: &Path) -> Vec<Case> {
    let file = std::fs::File::open(file_path).unwrap();
    let buf_reader = std::io::BufReader::new(file);

    let mut test_cases = Vec::new();
    let mut lines = buf_reader.lines();
    let mut next_case = false;

    loop {
        if !next_case {
            if let Some(Ok(line)) = lines.next() {
                if !line.trim().starts_with("## Case") {
                    break;
                }
            } else {
                break;
            }
        }

        let mut stdin = Vec::new();
        while let Some(Ok(line)) = lines.next() {
            if !line.starts_with("## ") || line.starts_with("## Expect") {
                break;
            }

            // strip the leading "## "
            let line = line.trim_start_matches("## ").to_string();
            stdin.push(line);
        }

        // expect "## Expect "
        let mut stdout = Vec::new();
        while let Some(Ok(line)) = lines.next() {
            if !line.starts_with("## ") || line.starts_with("## Case") {
                next_case = line.starts_with("## Case");
                break;
            }

            // strip the leading "## "
            let line = line.trim_start_matches("## ").to_string();
            stdout.push(line);
        }

        test_cases.push(Case { stdin, stdout });
    }

    test_cases
}

async fn test(file_path: &Path) {
    let temp_dir = tempfile::tempdir().unwrap();
    let output_path = temp_dir.path().join("test_output");

    let arguments = Arguments {
        command: CompilerCommand::Build(Build {
            input: Input {
                file: file_path.to_path_buf(),
                target_name: None,
                library_paths: Vec::new(),
                incremental_path: Some(temp_dir.path().to_path_buf()),
                chrome_tracing: false,
                target_seed: Some(0),
            },
            output: pernixc_target::Output {
                output: Some(output_path.clone()),
            },
            opt_level: OptimizationLevel::O0,
            kind: TargetKind::Executable,
        }),
    };

    // Compile the program
    {
        let mut err_writer =
            codespan_reporting::term::termcolor::NoColor::new(Vec::new());
        let mut out_writer =
            codespan_reporting::term::termcolor::NoColor::new(Vec::new());

        let exit_code =
            pernixc_driver::run(arguments, &mut err_writer, &mut out_writer)
                .await;

        if exit_code != std::process::ExitCode::SUCCESS {
            let stderr = String::from_utf8(err_writer.into_inner()).unwrap();
            let stdout = String::from_utf8(out_writer.into_inner()).unwrap();

            panic!("Failed to compile\nstderr:\n{stderr}\nstdout:\n{stdout}",);
        }
    }

    // Run test cases
    let test_cases = parse_test_spec(file_path);
    for case in test_cases {
        println!("Running test case: {case:?}");

        let mut child = Command::new(&output_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();

        // Write input to stdin
        let mut stdin = child.stdin.take().unwrap();
        let mut stdin_content = String::new();
        for stdin_line in &case.stdin {
            stdin_content.push_str(stdin_line);
            stdin_content.push('\n');
        }

        stdin.write_all(stdin_content.as_bytes()).unwrap();

        drop(stdin);

        // Get output
        let output = child.wait_with_output().unwrap();

        let mut stdout = String::from_utf8(output.stdout).unwrap();
        stdout = stdout.replace("\r\n", "\n");

        let mut expected = String::new();
        for stdout_line in &case.stdout {
            expected.push_str(stdout_line);
            expected.push('\n');
        }

        assert_eq!(
            expected,
            stdout,
            "Test case failed in file: {}\ncase input: \n{}",
            file_path.display(),
            stdin_content
        );
    }
}
