#![allow(missing_docs)]

use std::{
    io::BufRead,
    path::{Path, PathBuf},
};

use clap::Parser;
use insta::assert_snapshot;
use pernixc_target::{Arguments, Check, Command, Input};

#[test_generator::test_resources("compiler/e2e/test/snapshot/**/main.pnx")]
fn main(resource: &str) {
    let file_path = PathBuf::from(resource);
    std::env::set_current_dir(env!("PERNIXC_CARGO_WORKSPACE_DIR")).unwrap();

    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("Failed to create Tokio runtime")
        .block_on(test(&file_path));
}

async fn run(arguments: Arguments) -> String {
    let mut err_writer =
        codespan_reporting::term::termcolor::NoColor::new(Vec::new());
    let mut out_writer =
        codespan_reporting::term::termcolor::NoColor::new(Vec::new());

    let _ =
        pernixc_driver::run(arguments, &mut err_writer, &mut out_writer).await;

    let stderr_string = String::from_utf8(err_writer.into_inner()).unwrap();
    let stout_string = String::from_utf8(out_writer.into_inner()).unwrap();

    format!("stderr:\n{stderr_string}\n\nstdout:\n{stout_string}")
}

async fn test(file_path: &Path) {
    let file = std::fs::File::open(file_path).unwrap();
    let buf_reader = std::io::BufReader::new(file);

    let temp_dir = tempfile::tempdir().unwrap();

    // read for the custom cli interface starting with `##` comment
    let arguments = buf_reader.lines().next().map(|x| x.unwrap()).map_or_else(
        || Arguments {
            command: Command::Check(Check {
                input: Input {
                    file: file_path.to_path_buf(),
                    target_name: None,
                    library_paths: Vec::new(),
                    incremental_path: None,
                    chrome_tracing: false,
                    target_seed: Some(0),
                },
            }),
        },
        |mut first_line| {
            if first_line.starts_with("##") {
                // replace the `{}` with the file path (if any) first_line =
                first_line = first_line.replace(
                    "$FILE_PATH",
                    file_path.to_string_lossy().as_ref(),
                );

                first_line = first_line.replace(
                    "$INCREMENTAL_PATH",
                    temp_dir.path().to_string_lossy().as_ref(),
                );

                first_line.strip_prefix("##").unwrap();

                Arguments::parse_from(first_line.split_whitespace())
            } else {
                Arguments {
                    command: Command::Check(Check {
                        input: Input {
                            file: file_path.to_path_buf(),
                            target_name: None,
                            library_paths: Vec::new(),
                            incremental_path: None,
                            chrome_tracing: false,
                            target_seed: Some(0),
                        },
                    }),
                }
            }
        },
    );

    let clean_run = run(arguments.clone()).await;
    let with_incremental = run(arguments).await;

    assert!(
        clean_run == with_incremental,
        "The output of the clean run and the incremental run is \
         different:\nClean run:\n{clean_run}\n\nIncremental \
         run:\n{with_incremental}"
    );

    let mut settings = insta::Settings::clone_current();

    // Convert windows paths to Unix Paths.
    settings.add_filter(r"\\\\?([\w\d.])", "/$1");
    // Convert crlf to lf.
    settings.add_filter(r"\r\n", "\n");
    // Replace the IO error message with a generic one.
    settings.add_filter(
        r": [^(\r\n]*\(os error (\d+)\)",
        ": general IO error (os error $1)",
    );

    let full_path = std::fs::canonicalize(file_path).unwrap();
    settings.set_snapshot_path(full_path.parent().unwrap());
    settings.set_prepend_module_to_snapshot(false);
    settings.remove_snapshot_suffix();
    let _guard = settings.bind_to_scope();

    assert_snapshot!("snapshot", clean_run);
}
