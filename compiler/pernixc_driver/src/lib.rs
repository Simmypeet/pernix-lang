//! Contains the main `run()` function for the compiler.

use std::{
    fs::File,
    path::PathBuf,
    process::ExitCode,
    sync::Arc,
    time::{Duration, Instant},
};

use bincode::{DefaultOptions, Options};
use indicatif::{ProgressBar, ProgressStyle};
use parking_lot::RwLock;
use pernixc_builder::Compilation;
use pernixc_diagnostic::Report;
use pernixc_handler::{Handler, Storage};
use pernixc_log::{
    formatting::{Color, Style},
    Message, Severity,
};
use pernixc_source_file::SourceFile;
use pernixc_syntax::syntax_tree::target::Target;
use pernixc_table::{CompilationMetaData, GlobalID, Table};
use ron::ser::PrettyConfig;
use serde::de::DeserializeSeed;

/// The compilation format of the target.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, clap::ValueEnum,
)]
pub enum TargetKind {
    /// Compiles as an executable with a main function.
    #[clap(name = "bin")]
    Executable,

    /// Compiles as a library which can be later linked to other targets.
    #[clap(name = "lib")]
    Library,
}

/// The arguments to the program.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, clap::Parser)]
#[clap(
    name = "pernixc",
    about = "The Pernix programming language compiler",
    author = "Simmypeet"
)]
pub struct Arguments {
    /// The input file to run the program on.
    pub file: PathBuf,

    /// The target name of the program. If not specified, the target name will
    /// be inferred from the file name.
    #[clap(short, long)]
    pub target_name: Option<String>,

    /// Specifies the compilation format of the target.
    #[clap(short, long, default_value = "bin")]
    pub kind: TargetKind,

    /// The paths to the `plib` library to link to the target.
    #[clap(short = 'l', long = "link")]
    pub library_paths: Vec<PathBuf>,

    /// The output path of the program. If not specified, the program will be
    /// written to the current working directory with the same name as the
    /// target.
    #[clap(short, long)]
    pub output: Option<PathBuf>,

    /// Whether to show the progress of the compilation.
    #[clap(long)]
    pub show_progress: bool,

    /// Whether to dump the table in the `ron` format.
    #[clap(long)]
    pub dump_ron: bool,
}

/// A struct that implements [`Handler`] but prints all the message to the
/// standard error stream.
#[derive(Debug)]
struct Printer {
    printed: RwLock<bool>,
}
impl Printer {
    /// Creates a new [`Printer`].
    const fn new() -> Self { Self { printed: RwLock::new(false) } }

    fn has_printed(&self) -> bool { *self.printed.read() }
}

impl<E: Report<()>> Handler<E> for Printer {
    fn receive(&self, error: E) {
        let diagnostic = error.report(());

        eprintln!("{diagnostic}\n");

        *self.printed.write() = true;
    }
}

fn update_message(
    table: &Table,
    buildings: &[(GlobalID, &str)],
    progress_bar: &ProgressBar,
) {
    let message = buildings
        .iter()
        .map(|(id, name)| {
            let qualified_name = table.get_qualified_name(*id);

            format!(
                "{} {name} of {}",
                Style::Bold.with(Color::Cyan.with("Building")),
                Style::Bold.with(qualified_name),
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    progress_bar.set_message(message);
}

/// Runs the program with the given arguments.
#[must_use]
#[allow(clippy::too_many_lines)]
pub fn run(argument: Arguments) -> ExitCode {
    let file = match File::open(&argument.file) {
        Ok(file) => file,
        Err(error) => {
            let msg = Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
            );

            eprintln!("{msg}");
            return ExitCode::FAILURE;
        }
    };

    let source_file = match SourceFile::load(file, argument.file.clone()) {
        Ok(file) => Arc::new(file),
        Err(pernixc_source_file::Error::Io(error)) => {
            let msg = Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
            );

            eprintln!("{msg}");
            return ExitCode::FAILURE;
        }
        Err(pernixc_source_file::Error::Utf8(error)) => {
            let msg = Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
            );

            eprintln!("{msg}");
            return ExitCode::FAILURE;
        }
    };

    let printer = Printer::new();

    // syntactic analysis
    let target = Target::parse(
        &source_file,
        argument.target_name.map_or_else(
            || {
                source_file
                    .full_path()
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_owned()
            },
            |name| name,
        ),
        &printer,
    );

    // early exit
    if printer.has_printed() {
        return ExitCode::FAILURE;
    }

    // retrieve the output path
    let output_path = if let Some(output) = argument.output {
        output
    } else {
        let mut output = match std::env::current_dir() {
            Ok(dir) => dir,
            Err(error) => {
                let msg = Message::new(
                    Severity::Error,
                    format!("failed to retrieve cwd: {error}"),
                );

                eprintln!("{msg}");
                return ExitCode::FAILURE;
            }
        };

        output.push(target.name());

        if argument.kind == TargetKind::Library {
            output.set_extension("plib");
        }

        output
    };

    let storage = Arc::new(Storage::<
        Box<dyn pernixc_table::diagnostic::Diagnostic>,
    >::new());

    let reflector = pernixc_builder::reflector::get();
    let mut table = Table::new(storage.clone());

    let mut inplace_table_deserializer =
        table.as_incremental_library_deserializer(&reflector);
    let mut link_library_ids = Vec::new();

    for link_library in argument.library_paths {
        let library_file = match std::fs::read(&link_library) {
            Ok(file) => file,
            Err(error) => {
                let msg = Message::new(
                    Severity::Error,
                    format!("failed to open file: {error}"),
                );

                eprintln!("{msg}");
                return ExitCode::FAILURE;
            }
        };

        let mut deserializer = bincode::de::Deserializer::from_slice(
            &library_file,
            bincode::DefaultOptions::new(),
        );

        let library_meta_data =
            match inplace_table_deserializer.deserialize(&mut deserializer) {
                Ok(meta_data) => meta_data,
                Err(error) => {
                    let msg = Message::new(
                        Severity::Error,
                        format!("failed to deserialize library: {error}"),
                    );

                    eprintln!("{msg}");
                    return ExitCode::FAILURE;
                }
            };

        link_library_ids.push(library_meta_data.target_id);
    }

    let target_id = table
        .add_compilation_target(
            target.name().clone(),
            link_library_ids,
            target,
            &*storage,
        )
        .unwrap();

    let symbol_count =
        table.get_target(target_id).unwrap().all_symbols().count();

    let progress = ProgressBar::new(symbol_count as u64);
    progress.set_style(
        ProgressStyle::with_template(&format!(
            "{{spinner:.green}} {} [{{bar:40.cyan/cyan}}] \
             {{pos:>7}}/{{len:7}}\n{{msg}}",
            Style::Bold.with(Color::Green.with("Analyzing"))
        ))
        .unwrap()
        .progress_chars("=>-"),
    );
    progress.enable_steady_tick(Duration::from_millis(100));

    let building = argument
        .show_progress
        .then(|| Arc::new(RwLock::new(Vec::<(GlobalID, &str)>::new())));

    let timer = Instant::now();
    Compilation::builder()
        .table(&mut table)
        .target_id(target_id)
        .on_done(Arc::new({
            let progress = progress.clone();

            move |_: &Table, _| {
                progress.inc(1);
            }
        }))
        .on_start_building_component(Arc::new({
            let progress = progress.clone();
            let building = building.clone();

            move |table: &Table, global_id, name| {
                if let Some(building) = &building {
                    let mut building = building.write();
                    building.push((global_id, name));

                    update_message(table, &building, &progress);
                }
            }
        }))
        .on_finish_building_component(Arc::new({
            let progress = progress.clone();
            let building = building.clone();

            move |table: &Table, global_id, name| {
                if let Some(building) = &building {
                    let mut building = building.write();
                    let index = building
                        .iter()
                        .position(|(id, n)| *id == global_id && name == *n);

                    if let Some(index) = index {
                        building.remove(index);
                    }

                    update_message(table, &building, &progress);
                }
            }
        }))
        .build()
        .run();

    progress.set_style(
        ProgressStyle::with_template("{spinner:.green} {msg}").unwrap(),
    );
    progress.finish_with_message(format!(
        "{} finished in {:?}",
        Style::Bold.with(Color::Green.with("Analyis")),
        timer.elapsed()
    ));

    let vec = storage.as_vec();

    if argument.dump_ron {
        let file = match File::create(format!("{}.ron", output_path.display()))
        {
            Ok(file) => file,
            Err(error) => {
                let msg = Message::new(
                    Severity::Error,
                    format!("failed to create file: {error}"),
                );

                eprintln!("{msg}");
                return ExitCode::FAILURE;
            }
        };

        match ron::ser::to_writer_pretty(
            file,
            &table.as_library(&CompilationMetaData { target_id }, &reflector),
            PrettyConfig::default(),
        ) {
            Ok(()) => {}

            Err(error) => {
                let msg = Message::new(
                    Severity::Error,
                    format!("failed to serialize table: {error}"),
                );

                eprintln!("{msg}");
                return ExitCode::FAILURE;
            }
        }
    }

    if !vec.is_empty() {
        for error in vec.iter() {
            let diag = error.report(&table);

            eprintln!("{diag}\n");
        }

        eprintln!(
            "{}",
            Message::new(
                Severity::Error,
                format!("compilation terminated due to {} error(s)", vec.len())
            )
        );

        return ExitCode::FAILURE;
    }

    if argument.kind == TargetKind::Library {
        // save `bincode` format
        let instant = Instant::now();

        let progress = ProgressBar::new(0);
        progress.set_style(
            ProgressStyle::default_spinner()
                .template("{spinner:.green} {msg}")
                .unwrap()
                .progress_chars("=>-"),
        );
        progress.set_message(format!(
            "{} to {}",
            Style::Bold.with(Color::Green.with("Writing")),
            output_path.display()
        ));
        progress.enable_steady_tick(Duration::from_millis(100));

        let file = match File::create(&output_path) {
            Ok(file) => file,
            Err(error) => {
                let msg = Message::new(
                    Severity::Error,
                    format!("failed to create file: {error}"),
                );

                eprintln!("{msg}");
                return ExitCode::FAILURE;
            }
        };

        let result = DefaultOptions::new().serialize_into(
            file,
            &table.as_library(&CompilationMetaData { target_id }, &reflector),
        );

        progress.finish_with_message(format!(
            "{} to {} finished in {:?}",
            Style::Bold.with(Color::Green.with("Written")),
            output_path.display(),
            instant.elapsed()
        ));

        match result {
            Ok(()) => {}

            Err(error) => {
                let msg = Message::new(
                    Severity::Error,
                    format!("failed to serialize table: {error}"),
                );

                eprintln!("{msg}");
                return ExitCode::FAILURE;
            }
        }
    };

    ExitCode::SUCCESS
}
