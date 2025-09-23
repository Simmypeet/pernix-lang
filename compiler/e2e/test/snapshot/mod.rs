#![allow(missing_docs)]

use std::{
    io::BufRead,
    path::{Path, PathBuf},
};

use clap::Parser;
use insta::assert_snapshot;
use pernixc_target::{Arguments, Check, Command, Input};
use pretty_assertions::assert_str_eq;
use tracing_subscriber::{
    layer::SubscriberExt, util::SubscriberInitExt, EnvFilter, Layer,
};

#[test_generator::test_resources("compiler/e2e/test/snapshot/**/main.pnx")]
fn main(resource: &str) {
    stacker::maybe_grow(3 * 1024 * 1024, 8 * 1024 * 1024, || {
        let _ = tracing_subscriber::registry()
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
            .try_init();

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
    });
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
                    incremental_path: Some(temp_dir.path().to_path_buf()),
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
                            incremental_path: Some(
                                temp_dir.path().to_path_buf(),
                            ),
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

    assert_str_eq!(
        clean_run,
        with_incremental,
        "the output between clean run and incremental run are different"
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
