#![allow(missing_docs)]

use std::{
    fmt::Write,
    io::{BufRead, Write as _},
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use insta::assert_snapshot;
use pernixc_target::{
    Arguments, Build, Command as CompilerCommand, Input, OptimizationLevel,
    TargetKind,
};
use test_generator::test_resources;
use tracing_subscriber::{
    layer::SubscriberExt, util::SubscriberInitExt, EnvFilter, Layer,
};

#[test_resources("compiler/e2e/test/run/**/main.pnx")]
fn main(resource: &str) {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_thread_ids(true)
                .with_thread_names(true)
                .with_span_events(
                    tracing_subscriber::fmt::format::FmtSpan::CLOSE,
                )
                .with_filter(
                    EnvFilter::try_from_env("PERNIXC_LOG")
                        .unwrap_or_else(|_| "ERROR".into()),
                ),
        )
        .init();

    let file_path = PathBuf::from(resource);
    std::env::set_current_dir(env!("PERNIXC_CARGO_WORKSPACE_DIR")).unwrap();

    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .thread_stack_size(
            // in the debug build, the stack size of the future is not
            // optimized well, so we need a larger stack size
            {
                #[cfg(debug_assertions)]
                {
                    8 * 1024 * 1024 // 8MB stack for debug build
                }
                #[cfg(not(debug_assertions))]
                {
                    2 * 1024 * 1024 // 2MB stack default
                }
            },
        )
        .build()
        .expect("Failed to create Tokio runtime")
        .block_on(test(&file_path));
}

#[derive(Debug)]
struct Case {
    pub stdin: Vec<String>,
}

/// Parse test case and expected output from a source file
fn parse_test_spec(file_path: &Path) -> Vec<Case> {
    const DOUBLE_COMMENT_PREFIX: &str = "## ";
    const CASE_PREFIX: &str = "## Case";

    let file = std::fs::File::open(file_path).unwrap();
    let buf_reader = std::io::BufReader::new(file);

    let mut test_cases = Vec::new();
    let mut lines = buf_reader.lines().peekable();

    loop {
        // expect "## Case: "
        let Some(Ok(case_prefix_line)) = lines.next() else {
            break;
        };

        if !case_prefix_line.trim().starts_with(CASE_PREFIX) {
            break;
        }

        let mut stdins = Vec::new();

        while let Some(stdin) = lines.next_if(|item| {
            item.as_ref().ok().is_some_and(|x| {
                x.trim().starts_with(DOUBLE_COMMENT_PREFIX)
                    && !x.trim().starts_with(CASE_PREFIX)
            })
        }) {
            stdins.push(
                stdin
                    .unwrap()
                    .trim()
                    .strip_prefix(DOUBLE_COMMENT_PREFIX)
                    .unwrap()
                    .to_string(),
            );
        }

        test_cases.push(Case { stdin: stdins });
    }

    // if no test case is defined, add an empty test case
    if test_cases.is_empty() {
        test_cases.push(Case { stdin: Vec::new() });
    }

    test_cases
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct RunResult {
    pub exit_code: Option<i32>,
    pub stdin: String,
    pub stdout: String,
    pub stderr: String,
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
    let mut run_results = Vec::new();

    for case in test_cases {
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

        // replace \r\n with \n
        let stdout = String::from_utf8(output.stdout)
            .unwrap()
            .trim()
            .replace("\r\n", "\n");
        let stderr = String::from_utf8(output.stderr)
            .unwrap()
            .trim()
            .replace("\r\n", "\n");

        run_results.push(RunResult {
            exit_code: output.status.code(),
            stdin: stdin_content.trim().to_string(),
            stdout,
            stderr,
        });
    }

    let mut final_string = String::new();
    for (i, result) in run_results.iter().enumerate() {
        let toml_string = toml::to_string_pretty(&result).unwrap();
        writeln!(&mut final_string, "## Case {i}").unwrap();
        writeln!(&mut final_string, "{toml_string}").unwrap();
    }

    let mut settings = insta::Settings::clone_current();

    // Convert crlf to lf.
    settings.add_filter(r"\r\n", "\n");
    settings.add_filter(r"\r", "\n");

    let full_path = std::fs::canonicalize(file_path).unwrap();
    settings.set_snapshot_path(full_path.parent().unwrap());
    settings.set_prepend_module_to_snapshot(false);
    settings.remove_snapshot_suffix();
    let _guard = settings.bind_to_scope();

    assert_snapshot!("snapshot", final_string);
}
