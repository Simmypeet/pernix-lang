#![allow(missing_docs)]

use std::{
    io::{BufRead, Write},
    path::{Path, PathBuf},
    process::ExitCode,
};

use clap::Parser;
use insta::assert_snapshot;
use pernixc_driver::{
    argument::{Arguments, Check, Command},
    Input,
};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

#[test]
#[allow(clippy::manual_assert)]
fn main() -> ExitCode {
    let scandir =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test").join("snapshot");

    std::env::set_current_dir(&scandir).unwrap();

    let mut passed = Vec::new();
    let mut failed = Vec::new();
    let mut stdout = StandardStream::stdout(ColorChoice::Always);

    visit_dirs(&scandir, &mut |file_path: &Path| {
        let relative_to_scandir = file_path.strip_prefix(&scandir).unwrap();

        // Print which test is currently running
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan))).unwrap();
        write!(&mut stdout, "🧪 Running test: ").unwrap();
        stdout.reset().unwrap();
        writeln!(&mut stdout, "{}", relative_to_scandir.display()).unwrap();
        stdout.flush().unwrap();

        let result = std::panic::catch_unwind(|| {
            // for each file that has the `main.pnx` name, run the test
            test(relative_to_scandir);
        });

        if result.is_err() {
            failed.push(relative_to_scandir.to_path_buf());
        } else {
            passed.push(relative_to_scandir.to_path_buf());
        }
    })
    .unwrap();

    let mut stdout = StandardStream::stdout(ColorChoice::Always);

    // Print passed tests
    stdout
        .set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))
        .unwrap();
    write!(&mut stdout, "✓ Passed tests: ").unwrap();
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green))).unwrap();
    writeln!(&mut stdout, "{}", passed.len()).unwrap();
    stdout.reset().unwrap();

    for file in &passed {
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green))).unwrap();
        write!(&mut stdout, "  ✓ ").unwrap();
        stdout.reset().unwrap();
        writeln!(&mut stdout, "{}", file.display()).unwrap();
    }

    writeln!(&mut stdout).unwrap();
    if failed.is_empty() {
        stdout
            .set_color(
                ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true),
            )
            .unwrap();
        writeln!(&mut stdout, "🎉 All tests passed!").unwrap();
        stdout.reset().unwrap();
        ExitCode::SUCCESS
    } else {
        stdout
            .set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))
            .unwrap();
        write!(&mut stdout, "✗ Failed tests: ").unwrap();
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red))).unwrap();
        writeln!(&mut stdout, "{}", failed.len()).unwrap();
        stdout.reset().unwrap();

        for file in &failed {
            stdout
                .set_color(ColorSpec::new().set_fg(Some(Color::Red)))
                .unwrap();
            write!(&mut stdout, "  ✗ ").unwrap();
            stdout.reset().unwrap();
            writeln!(&mut stdout, "{}", file.display()).unwrap();
        }

        writeln!(&mut stdout).unwrap();
        stdout
            .set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))
            .unwrap();
        writeln!(
            &mut stdout,
            "❌ Some tests failed, see the output above for details."
        )
        .unwrap();
        stdout.reset().unwrap();

        ExitCode::FAILURE
    }
}

// one possible implementation of walking a directory only visiting files
fn visit_dirs(dir: &Path, cb: &mut dyn FnMut(&Path)) -> std::io::Result<()> {
    if dir.is_dir() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, cb)?;
            } else if let Some(path) = entry.file_name().to_str() {
                if path == "main.pnx" {
                    cb(&entry.path());
                }
            }
        }
    }
    Ok(())
}

fn test(file_path: &Path) {
    let mut err_writer =
        codespan_reporting::term::termcolor::NoColor::new(Vec::new());
    let mut out_writer =
        codespan_reporting::term::termcolor::NoColor::new(Vec::new());

    let file = std::fs::File::open(file_path).unwrap();
    let buf_reader = std::io::BufReader::new(file);

    // read for the custom cli interface starting with `##` comment
    let arguments = buf_reader.lines().next().map(|x| x.unwrap()).map_or_else(
        || Arguments {
            command: Command::Check(Check {
                input: Input {
                    file: file_path.to_path_buf(),
                    target_name: None,
                    library_paths: Vec::new(),
                    incremental_path: None,
                    show_progress: false,
                },
            }),
        },
        |mut first_line| {
            if first_line.starts_with("##") {
                // replace the `{}` with the file path (if any) first_line =
                first_line = first_line
                    .replace("{}", file_path.to_string_lossy().as_ref());

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
                            show_progress: false,
                        },
                    }),
                }
            }
        },
    );

    let _ = pernixc_driver::run(arguments, &mut err_writer, &mut out_writer);

    let stderr_string = String::from_utf8(err_writer.into_inner()).unwrap();
    let stout_string = String::from_utf8(out_writer.into_inner()).unwrap();

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

    settings.set_snapshot_path(file_path.parent().unwrap());
    settings.set_prepend_module_to_snapshot(false);
    settings.remove_snapshot_suffix();
    let _guard = settings.bind_to_scope();

    let assert_string =
        format!("stderr:\n{stderr_string}\n\nstdout:\n{stout_string}");

    assert_snapshot!("snapshot", assert_string);
}
