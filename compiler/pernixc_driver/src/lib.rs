//! Contains the main `run()` function for the compiler.

use std::{
    fs::File,
    io::ErrorKind,
    path::{Path, PathBuf},
    process::ExitCode,
    sync::Arc,
    time::{Duration, Instant},
};

use bincode::{DefaultOptions, Options};
use clap::{builder::styling, Args, Subcommand};
use enum_as_inner::EnumAsInner;
use indicatif::{ProgressBar, ProgressStyle};
use inkwell::{
    passes::PassBuilderOptions,
    targets::{
        InitializationConfig, RelocMode, Target as LLVMTarget, TargetMachine,
        TargetMachineOptions,
    },
};
use parking_lot::RwLock;
use pernixc_builder::{reflector::ComponentTag, Compilation};
use pernixc_diagnostic::Report;
use pernixc_handler::Storage;
use pernixc_log::{
    formatting::{Color, Style},
    Message, Severity,
};
use pernixc_semantic::{
    component::input::Member,
    diagnostic::Diagnostic,
    table::{
        self, input::AddTargetError, CompilationMetaData, GlobalID, Table,
        TargetID,
    },
};
use pernixc_source_file::SourceFile;
use pernixc_storage::{serde::Reflector, ArcTrait};
use pernixc_syntax::syntax_tree::target::Target;
use ron::ser::PrettyConfig;
use serde::de::DeserializeSeed;

#[must_use]
const fn get_styles() -> clap::builder::Styles {
    clap::builder::Styles::styled()
        .usage(
            styling::Style::new().bold().underline().fg_color(Some(
                styling::Color::Ansi(styling::AnsiColor::Yellow),
            )),
        )
        .header(
            styling::Style::new()
                .bold()
                .underline()
                .fg_color(Some(styling::Color::Ansi(styling::AnsiColor::Cyan))),
        )
        .literal(
            styling::Style::new().fg_color(Some(styling::Color::Ansi(
                styling::AnsiColor::Green,
            ))),
        )
        .invalid(
            styling::Style::new()
                .bold()
                .fg_color(Some(styling::Color::Ansi(styling::AnsiColor::Red))),
        )
        .error(
            styling::Style::new()
                .bold()
                .fg_color(Some(styling::Color::Ansi(styling::AnsiColor::Red))),
        )
        .valid(
            styling::Style::new().bold().underline().fg_color(Some(
                styling::Color::Ansi(styling::AnsiColor::Green),
            )),
        )
        .placeholder(
            styling::Style::new().fg_color(Some(styling::Color::Ansi(
                styling::AnsiColor::White,
            ))),
        )
}

/// The input to the compiler.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Args)]
pub struct Input {
    /// The input file to compile.
    ///
    /// This file is the root source file of the compilation; the module will
    /// stem from this file.
    pub file: PathBuf,

    /// The name of the target; if not specified, the target name will be
    /// inferred from the file name.
    #[clap(short = 't', long = "target")]
    pub target_name: Option<String>,

    /// The paths to the `plib` library to link to the target.
    #[clap(short = 'l', long = "link")]
    pub library_paths: Vec<PathBuf>,

    /// Whether to show the progress of the compilation.
    #[clap(long)]
    pub show_progress: bool,
}

/// The output of the compiler.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Args)]
pub struct Output {
    /// The output path of the program. If not specified, the program will be
    /// written to the current working directory with the same name as the
    /// target.
    #[clap(short = 'o', long = "output")]
    pub output: Option<PathBuf>,
}

/// Represents the `run` subcommand of the compiler.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Args)]
pub struct Run {
    /// The input file to run the program on.
    #[clap(flatten)]
    pub input: Input,

    /// Specifies the output path of the program.
    #[clap(flatten)]
    pub output: Output,

    /// The optimization level of the compiler.
    #[clap(long = "opt", default_value = "0")]
    pub opt_level: OptimizationLevel,
}

/// Represents the `check` subcommand of the compiler.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Args)]
pub struct Check {
    /// The input file to run the program on.
    #[clap(flatten)]
    pub input: Input,
}

/// Represents the `build` subcommand of the compiler.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Args)]
pub struct Build {
    /// The input file to run the program on.
    #[clap(flatten)]
    pub input: Input,

    /// Specifies the output path of the program.
    #[clap(flatten)]
    pub output: Output,

    /// The optimization level of the compiler.
    #[clap(long = "opt", default_value = "0")]
    pub opt_level: OptimizationLevel,

    /// Specifies the compilation format of the target.
    #[clap(long = "emit", default_value = "bin")]
    pub kind: TargetKind,
}

/// The subcomamnds of the compiler.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Subcommand, EnumAsInner,
)]
pub enum Command {
    /// Compiles the program as an executable binary and runs it.
    #[clap(name = "run")]
    Run(Run),

    /// Performs semantic analysis on the program and emits the diagnostics.
    #[clap(name = "check")]
    Check(Check),

    /// Builds the program and emits the output (defaults to `bin`).
    #[clap(name = "build")]
    Build(Build),
}

impl Command {
    /// Returns the input file of the command.
    #[must_use]
    pub const fn input(&self) -> &Input {
        match self {
            Self::Run(run) => &run.input,
            Self::Check(check) => &check.input,
            Self::Build(build) => &build.input,
        }
    }
}

/// Optimizations level for the compiler.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, clap::ValueEnum,
)]
#[allow(missing_docs)]
pub enum OptimizationLevel {
    #[clap(name = "0")]
    O0,

    #[clap(name = "1")]
    O1,

    #[clap(name = "2")]
    O2,

    #[clap(name = "3")]
    O3,
}

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

    /// Compiles as LLVM IR.
    #[clap(name = "llvm")]
    LLvmIR,

    /// Compiles as an object file which can be later linked to create an
    /// executable.
    #[clap(name = "obj")]
    Object,

    /// Emits the whole information of the target in a human readable
    /// format.
    #[clap(name = "ron")]
    Ron,
}

/// The arguments to the program.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, clap::Parser)]
#[clap(name = "pernixc", version, about, author)]
#[command(styles = get_styles())]
pub struct Arguments {
    /// The subcommand to run.
    #[clap(subcommand, flatten = true)]
    pub command: Command,
}

#[derive(Debug, derive_more::From)]
enum SyntacticError {
    Token(pernixc_lexical::error::Error),
    Syntax(pernixc_syntax::error::Error),
    Target(pernixc_syntax::syntax_tree::target::Error),
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

fn create_root_source_file(argument: &Arguments) -> Option<Arc<SourceFile>> {
    let file = match File::open(&argument.command.input().file) {
        Ok(file) => file,
        Err(error) => {
            let msg = Message::new(
                Severity::Error,
                format!("{}: {error}", argument.command.input().file.display()),
            );

            eprintln!("{msg}");
            return None;
        }
    };

    let source_file =
        match SourceFile::load(file, argument.command.input().file.clone()) {
            Ok(file) => Arc::new(file),
            Err(pernixc_source_file::Error::Io(error)) => {
                let msg = Message::new(
                    Severity::Error,
                    format!(
                        "{}: {error}",
                        argument.command.input().file.display()
                    ),
                );

                eprintln!("{msg}");
                return None;
            }
            Err(pernixc_source_file::Error::Utf8(error)) => {
                let msg = Message::new(
                    Severity::Error,
                    format!(
                        "{}: {error}",
                        argument.command.input().file.display()
                    ),
                );

                eprintln!("{msg}");
                return None;
            }
        };

    Some(source_file)
}

fn parse_target(
    source_file: &Arc<SourceFile>,
    target_name_argument: Option<String>,
) -> (Target, Storage<SyntacticError>) {
    let storage = Storage::<SyntacticError>::new();

    // syntactic analysis
    let target = Target::parse(
        source_file,
        target_name_argument.map_or_else(
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
        &storage,
    );

    (target, storage)
}

fn get_output_path(
    argument_output: Option<PathBuf>,
    target_kind: TargetKind,
    target: &Target,
) -> Option<PathBuf> {
    if let Some(output) = argument_output {
        Some(output)
    } else {
        let mut output = match std::env::current_dir() {
            Ok(dir) => dir,
            Err(error) => {
                let msg = Message::new(
                    Severity::Error,
                    format!("failed to retrieve cwd: {error}"),
                );

                eprintln!("{msg}");
                return None;
            }
        };

        output.push(&target.name);

        if target_kind == TargetKind::Library {
            output.set_extension("plib");
        }

        Some(output)
    }
}

fn link_libraries(
    table: &mut Table,
    library_paths: Vec<PathBuf>,
    reflector: &Reflector<GlobalID, ArcTrait, ComponentTag, String>,
) -> Option<Vec<TargetID>> {
    let mut inplace_table_deserializer =
        table.as_incremental_library_deserializer(reflector);
    let mut link_library_ids = Vec::new();

    for link_library in library_paths {
        let library_file = match std::fs::read(&link_library) {
            Ok(file) => file,
            Err(error) => {
                let msg = Message::new(
                    Severity::Error,
                    format!("failed to open file: {error}"),
                );

                eprintln!("{msg}");
                return None;
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
                    return None;
                }
            };

        link_library_ids.push(library_meta_data.target_id);
    }

    Some(link_library_ids)
}

#[allow(clippy::type_complexity)]
fn semantic_analysis(
    target: Target,
    show_progress: bool,
    library_paths: Vec<PathBuf>,
) -> Option<(
    Table,
    TargetID,
    Arc<Storage<Box<dyn Diagnostic>>>,
    Reflector<GlobalID, ArcTrait, ComponentTag, String>,
)> {
    let storage = Arc::new(Storage::<
        Box<dyn pernixc_semantic::diagnostic::Diagnostic>,
    >::new());

    let reflector = pernixc_builder::reflector::get();

    let mut table = Table::new(storage.clone());

    let link_library_ids =
        link_libraries(&mut table, library_paths, &reflector)?;

    let target_id = match table.add_compilation_target_input(
        target.name.clone(),
        link_library_ids.into_iter().chain(std::iter::once(TargetID::CORE)),
        target,
        &*storage,
    ) {
        Ok(target_id) => target_id,

        Err(AddTargetError::DuplicateTargetName(target)) => {
            let msg = Message::new(
                Severity::Error,
                format!("duplicate target name: {target}"),
            );

            eprintln!("{msg}");
            return None;
        }

        Err(AddTargetError::UnknownTargetLink(id)) => {
            panic!("unknown target link: {id:?}");
        }
    };

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

    let building = show_progress
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
        Style::Bold.with(Color::Green.with("Analysis")),
        timer.elapsed()
    ));

    Some((table, target_id, storage, reflector))
}

fn dump_ron(
    table: &Table,
    target_id: TargetID,
    reflector: &Reflector<GlobalID, ArcTrait, ComponentTag, String>,
    output_path: &Path,
) -> bool {
    let file = match File::create(format!("{}.ron", output_path.display())) {
        Ok(file) => file,
        Err(error) => {
            let msg = Message::new(
                Severity::Error,
                format!("failed to create file: {error}"),
            );

            eprintln!("{msg}");
            return false;
        }
    };

    match ron::ser::to_writer_pretty(
        file,
        &table.as_library(&CompilationMetaData { target_id }, reflector),
        PrettyConfig::default(),
    ) {
        Ok(()) => true,

        Err(error) => {
            let msg = Message::new(
                Severity::Error,
                format!("failed to serialize table: {error}"),
            );

            eprintln!("{msg}");
            false
        }
    }
}

fn check_compiation_errors(
    table: &Table,
    semantic_diag_storage: &[Box<dyn Diagnostic>],
    syntax_diag_storage: &[SyntacticError],
) -> bool {
    if semantic_diag_storage.is_empty() && syntax_diag_storage.is_empty() {
        true
    } else {
        for error in syntax_diag_storage {
            let diag = match error {
                SyntacticError::Token(error) => error.report(()),
                SyntacticError::Syntax(error) => error.report(()),
                SyntacticError::Target(error) => error.report(()),
            };

            eprintln!("{diag}\n");
        }

        for error in semantic_diag_storage {
            let diag = error.report(table);

            eprintln!("{diag}\n");
        }

        eprintln!(
            "{}",
            Message::new(
                Severity::Error,
                format!(
                    "compilation terminated due to {} error(s)",
                    syntax_diag_storage.len() + semantic_diag_storage.len()
                )
            )
        );

        false
    }
}

#[allow(clippy::single_match_else)]
fn emit_as_library(
    table: &Table,
    target_id: TargetID,
    reflector: &Reflector<GlobalID, ArcTrait, ComponentTag, String>,
    output_path: &Path,
) -> bool {
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

    let file = match File::create(output_path) {
        Ok(file) => file,
        Err(error) => {
            let msg = Message::new(
                Severity::Error,
                format!("failed to create file: {error}"),
            );

            eprintln!("{msg}");
            return false;
        }
    };

    let result = DefaultOptions::new().serialize_into(
        file,
        &table.as_library(&CompilationMetaData { target_id }, reflector),
    );

    progress.finish_with_message(format!(
        "{} to {} finished in {:?}",
        Style::Bold.with(Color::Green.with("Written")),
        output_path.display(),
        instant.elapsed()
    ));

    match result {
        Ok(()) => true,

        Err(error) => {
            let msg = Message::new(
                Severity::Error,
                format!("failed to serialize table: {error}"),
            );

            eprintln!("{msg}");

            false
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum MachineCodeKind {
    LlvmIR,
    Object,
    Binary(bool),
}

fn linker_command(obj: &Path, out: &Path) -> std::process::Command {
    let mut cmd = std::process::Command::new("clang");
    cmd.arg(obj).arg("-o").arg(out);

    if cfg!(target_os = "windows") {
        // link to `legacy_stdio_definitions`
        cmd.arg("-llegacy_stdio_definitions");
    }

    cmd
}

fn invoke_linker(
    temp_obj_path: &Path,
    output_path: &Path,
    progress: &ProgressBar,
) -> bool {
    let mut cmd = linker_command(temp_obj_path, output_path);

    let mut child = match cmd.spawn() {
        Ok(child) => child,
        Err(err) => {
            progress.finish_and_clear();
            eprintln!(
                "{}",
                Message::new(
                    Severity::Error,
                    format!("failed to spawn linker: {err}")
                )
            );

            if err.kind() == ErrorKind::NotFound {
                if cfg!(target_os = "windows") {
                    eprintln!("{}", Message::new(Severity::Info, 
                        "`clang` is not installed, please install it from
                        LLVM's official website or from Microsoft Visual Studio"));
                } else if cfg!(target_os = "linux") {
                    eprintln!(
                        "{}",
                        Message::new(
                            Severity::Info,
                            "`clang` is not installed, please install it from \
                             your package manager (e.g. `apt install clang` \
                             or `pacman -S clang`"
                        )
                    );
                } else if cfg!(target_os = "macos") {
                    eprintln!(
                        "{}",
                        Message::new(
                            Severity::Info,
                            "`clang` is not installed, please install it from \
                             Xcode or from the official LLVM website"
                        )
                    );
                }
            }

            return false;
        }
    };

    let status = match child.wait() {
        Ok(status) => status,
        Err(err) => {
            progress.finish_and_clear();
            eprintln!(
                "{}",
                Message::new(
                    Severity::Error,
                    format!("failed to wait for linker: {err}")
                )
            );

            return false;
        }
    };

    if !status.success() {
        progress.finish_and_clear();
        eprintln!(
            "{}",
            Message::new(
                Severity::Error,
                format!("linker failed with status: {status}")
            )
        );

        return false;
    }

    true
}

#[allow(
    clippy::single_match_else,
    clippy::let_and_return,
    clippy::too_many_lines
)]
fn emit_as_machine_code(
    table: &Table,
    target_id: TargetID,
    output_path: &Path,
    opt_level: OptimizationLevel,
    kind: MachineCodeKind,
) -> bool {
    let instant = Instant::now();
    let progress = ProgressBar::new(0);
    progress.set_style(
        ProgressStyle::default_spinner()
            .template("{spinner:.green} {msg}")
            .unwrap()
            .progress_chars("=>-"),
    );
    progress.set_message("Generating code");
    progress.enable_steady_tick(Duration::from_millis(100));

    let storage = Storage::<Box<dyn Diagnostic>>::new();

    // initialize the target
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

    let result = if let Some(main_function_id) = table
        .get::<Member>(GlobalID::new(target_id, table::ID::ROOT_MODULE))
        .get("main")
        .copied()
    {
        pernixc_codegen::codegen(pernixc_codegen::Input {
            table,
            main_function_id: GlobalID::new(target_id, main_function_id),
            handler: &storage,
            inkwell_context: &inkwell_context,
            target_data,
        })
    } else {
        progress.finish_and_clear();
        eprintln!(
            "{}",
            Message::new(
                Severity::Error,
                "no main function found in the target"
            )
        );

        return false;
    };

    let has_error = !storage.as_vec().is_empty();

    // for some reason, the boolean value needs to be stored in a variable
    // otherwise the borrow checker will complain about the lifetime of
    // `inkwell_context`, which is probably a bug in the rust compiler.
    let result = match (result, has_error) {
        (Ok(module), false) => {
            module.verify().unwrap();
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

            let temp_obj_path = matches!(kind, MachineCodeKind::Binary(_))
                .then(|| {
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
                        temp_obj_path
                            .as_ref()
                            .map_or(output_path, |x| x.as_path()),
                    )
                }
                MachineCodeKind::LlvmIR => module.print_to_file(output_path),
            };

            if let Err(error) = result {
                progress.finish_and_clear();
                eprintln!(
                    "{}",
                    Message::new(
                        Severity::Error,
                        format!("failed to write object file: {error}")
                    )
                );

                return false;
            }

            if let MachineCodeKind::Binary(_) = kind {
                if !invoke_linker(
                    temp_obj_path.as_ref().unwrap(),
                    output_path,
                    &progress,
                ) {
                    eprintln!(
                        "{}",
                        Message::new(
                            Severity::Info,
                            format!(
                                "you can continue to link the object file \
                                 manually using the appropriate linker \
                                 command at `{}`",
                                temp_obj_path.as_ref().unwrap().display()
                            )
                        )
                    );
                    return false;
                }

                // delete the temporary object file
                if let Err(error) =
                    std::fs::remove_file(temp_obj_path.as_ref().unwrap())
                {
                    progress.finish_and_clear();
                    eprintln!(
                        "{}",
                        Message::new(
                            Severity::Error,
                            format!("failed to delete temporary file: {error}")
                        )
                    );

                    return false;
                }
            }

            progress.finish_with_message(format!(
                "{} finished in {:?}",
                Style::Bold.with(Color::Green.with("Code Generation")),
                instant.elapsed()
            ));

            true
        }

        (_, true) | (Err(_), _) => {
            progress.finish_and_clear();
            for error in storage.as_vec().iter() {
                let diag = error.report(table);

                eprintln!("{diag}\n");
            }

            false
        }
    };

    result
}

/// Runs the program with the given arguments.
#[must_use]
#[allow(clippy::too_many_lines)]
pub fn run(argument: &Arguments) -> ExitCode {
    let Some(source_file) = create_root_source_file(argument) else {
        return ExitCode::FAILURE;
    };

    let (target, syntax_diags) = parse_target(
        &source_file,
        argument.command.input().target_name.clone(),
    );

    // retrieve the output path
    let output_path = match &argument.command {
        Command::Build(Build { output, .. })
        | Command::Run(Run { output, .. }) => Some(get_output_path(
            output.output.clone(),
            argument
                .command
                .as_build()
                .map_or(TargetKind::Executable, |x| x.kind),
            &target,
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

    // perform semantic analysis
    let Some((table, target_id, semantic_diags, reflector)) = semantic_analysis(
        target,
        argument.command.input().show_progress,
        argument.command.input().library_paths.clone(),
    ) else {
        return ExitCode::FAILURE;
    };

    let has_compilation_error = !check_compiation_errors(
        &table,
        semantic_diags.as_vec().as_slice(),
        syntax_diags.as_vec().as_slice(),
    );

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

            if has_compilation_error {
                return ExitCode::FAILURE;
            }

            // emit the executable
            if !emit_as_machine_code(
                &table,
                target_id,
                output_path.as_ref().unwrap(),
                *opt_level,
                kind,
            ) {
                return ExitCode::FAILURE;
            }

            let run_executable = matches!(kind, MachineCodeKind::Binary(true));

            if !run_executable {
                return ExitCode::SUCCESS;
            }

            eprintln!(
                "{}",
                Message::new(
                    Severity::Info,
                    format!(
                        "invoking the executable at `{}`",
                        output_path.as_ref().unwrap().display()
                    )
                )
            );

            let mut command =
                std::process::Command::new(output_path.as_ref().unwrap());

            command.stdin(std::process::Stdio::inherit());
            command.stdout(std::process::Stdio::inherit());
            command.stderr(std::process::Stdio::inherit());

            let mut child = match command.spawn() {
                Ok(child) => child,
                Err(error) => {
                    eprintln!(
                        "{}",
                        Message::new(
                            Severity::Error,
                            format!("failed to spawn executable: {error}")
                        )
                    );

                    return ExitCode::FAILURE;
                }
            };

            let status = match child.wait() {
                Ok(status) => status,
                Err(error) => {
                    eprintln!(
                        "{}",
                        Message::new(
                            Severity::Error,
                            format!("failed to wait for executable: {error}")
                        )
                    );

                    return ExitCode::FAILURE;
                }
            };

            if let Some(code) = status.code() {
                if code != 0 {
                    eprintln!(
                        "{}",
                        Message::new(
                            Severity::Error,
                            format!("executable failed with status: {code}")
                        )
                    );

                    return ExitCode::FAILURE;
                }
            } else {
                eprintln!(
                    "{}",
                    Message::new(
                        Severity::Error,
                        "executable terminated by signal"
                    )
                );

                return ExitCode::FAILURE;
            }

            ExitCode::SUCCESS
        }

        Command::Check(_) => {
            if has_compilation_error {
                return ExitCode::FAILURE;
            }

            ExitCode::SUCCESS
        }

        Command::Build(Build { kind: TargetKind::Ron, .. }) => {
            // dump the table to a file
            if !dump_ron(
                &table,
                target_id,
                &reflector,
                output_path.as_ref().unwrap(),
            ) {
                return ExitCode::FAILURE;
            }

            ExitCode::SUCCESS
        }

        Command::Build(Build { kind: TargetKind::Library, .. }) => {
            if has_compilation_error {
                return ExitCode::FAILURE;
            }

            if emit_as_library(
                &table,
                target_id,
                &reflector,
                output_path.as_ref().unwrap(),
            ) {
                return ExitCode::SUCCESS;
            }

            ExitCode::FAILURE
        }
    }
}
