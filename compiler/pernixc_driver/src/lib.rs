//! Contains the main `run()` function for the compiler.

use std::{
    fs::File,
    path::{Path, PathBuf},
    process::ExitCode,
    sync::Arc,
    time::{Duration, Instant},
};

use bincode::{DefaultOptions, Options};
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
use pernixc_handler::{Handler, Storage};
use pernixc_intrinsic::IntrinsicExt;
use pernixc_log::{
    formatting::{Color, Style},
    Message, Severity,
};
use pernixc_source_file::SourceFile;
use pernixc_storage::{serde::Reflector, ArcTrait};
use pernixc_syntax::syntax_tree::target::Target;
use pernixc_table::{
    component::Member, diagnostic::Diagnostic, AddTargetError,
    CompilationMetaData, GlobalID, Table, TargetID,
};
use ron::ser::PrettyConfig;
use serde::de::DeserializeSeed;

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

    /// The optimization level of the compiler.
    #[clap(long = "opt", default_value = "0")]
    pub opt_level: OptimizationLevel,

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

fn create_root_source_file(argument: &Arguments) -> Option<Arc<SourceFile>> {
    let file = match File::open(&argument.file) {
        Ok(file) => file,
        Err(error) => {
            let msg = Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
            );

            eprintln!("{msg}");
            return None;
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
            return None;
        }
        Err(pernixc_source_file::Error::Utf8(error)) => {
            let msg = Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
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
) -> Option<Target> {
    let printer = Printer::new();

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
        &printer,
    );

    // early exit
    if printer.has_printed() {
        return None;
    }

    Some(target)
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

        output.push(target.name());

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
        Box<dyn pernixc_table::diagnostic::Diagnostic>,
    >::new());

    let reflector = pernixc_builder::reflector::get();

    let mut table = Table::new(storage.clone());
    table.initialize_core();

    let link_library_ids =
        link_libraries(&mut table, library_paths, &reflector)?;

    let target_id = match table.add_compilation_target(
        target.name().clone(),
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
        Style::Bold.with(Color::Green.with("Analyis")),
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

fn check_semantic_error(
    table: &Table,
    storage: &Arc<Storage<Box<dyn Diagnostic>>>,
) -> bool {
    let vec = storage.as_vec();

    if vec.is_empty() {
        true
    } else {
        for error in vec.iter() {
            let diag = error.report(table);

            eprintln!("{diag}\n");
        }

        eprintln!(
            "{}",
            Message::new(
                Severity::Error,
                format!("compilation terminated due to {} error(s)", vec.len())
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

#[allow(
    clippy::single_match_else,
    clippy::let_and_return,
    clippy::too_many_lines
)]
fn emit_as_exe(
    table: &Table,
    target_id: TargetID,
    output_path: &Path,
    opt_level: OptimizationLevel,
    kind: TargetKind,
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
        .get::<Member>(GlobalID::new(target_id, pernixc_table::ID::ROOT_MODULE))
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

            let result = match kind {
                TargetKind::Executable => target_machine.write_to_file(
                    &module,
                    inkwell::targets::FileType::Object,
                    output_path,
                ),
                TargetKind::LLvmIR => module.print_to_file(output_path),

                TargetKind::Library => unreachable!(),
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
pub fn run(argument: Arguments) -> ExitCode {
    let Some(source_file) = create_root_source_file(&argument) else {
        return ExitCode::FAILURE;
    };
    let Some(target) = parse_target(&source_file, argument.target_name) else {
        return ExitCode::FAILURE;
    };

    // retrieve the output path
    let Some(output_path) =
        get_output_path(argument.output, argument.kind, &target)
    else {
        return ExitCode::FAILURE;
    };

    // perform semantic analysis
    let Some((table, target_id, storage, reflector)) = semantic_analysis(
        target,
        argument.show_progress,
        argument.library_paths,
    ) else {
        return ExitCode::FAILURE;
    };

    // dump ron if specified
    if argument.dump_ron
        && !dump_ron(&table, target_id, &reflector, &output_path)
    {
        return ExitCode::FAILURE;
    }

    // check for semantic errors
    if !check_semantic_error(&table, &storage) {
        return ExitCode::FAILURE;
    }

    let result = match argument.kind {
        TargetKind::Executable | TargetKind::LLvmIR => emit_as_exe(
            &table,
            target_id,
            &output_path,
            argument.opt_level,
            argument.kind,
        ),
        TargetKind::Library => {
            emit_as_library(&table, target_id, &reflector, &output_path)
        }
    };

    if result {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
