//! This crate contains the information about the target of the compilation.

use std::{path::PathBuf, sync::Arc};

use clap::{builder::styling, Args, Subcommand};
use derive_new::new;
use enum_as_inner::EnumAsInner;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

/// Represents an identifier for a target.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
    StableHash,
)]
pub enum TargetID {
    /// Representing a target that is being compiled at the moment.
    #[default]
    Local,

    /// Representing a `core` target.
    Core,

    /// Represents an externally defined targets that are being consumed by the
    /// current [`Self::Local`].
    Extern(u64),
}

impl TargetID {
    /// Creates a new [`Global`] identifier from the given [`TargetID`] and the
    /// given local identifier.
    #[must_use]
    pub const fn make_global<ID>(self, id: ID) -> Global<ID> {
        Global { id, target_id: self }
    }
}

/// A struct used for identifying an entity across different targets.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
    StableHash,
    new,
)]
pub struct Global<ID> {
    /// The identifier to the target that the entity is defined in.
    pub target_id: TargetID,

    /// The identifier to the local entity defined within the target.
    pub id: ID,
}

/// The input to the compiler.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Args,
    Serialize,
    Deserialize,
    StableHash,
)]
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

    /// The path to the incremental compilation data.
    #[clap(long = "inc")]
    pub incremental_path: Option<PathBuf>,

    /// Whether to show the progress of the compilation.
    #[clap(long)]
    pub show_progress: bool,
}

/// The output of the compiler.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Args,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Output {
    /// The output path of the program. If not specified, the program will be
    /// written to the current working directory with the same name as the
    /// target.
    #[clap(short = 'o', long = "output")]
    pub output: Option<PathBuf>,
}

/// Represents the `run` subcommand of the compiler.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Args,
    Serialize,
    Deserialize,
    StableHash,
)]
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
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    Args,
)]
pub struct Check {
    /// The input file to run the program on.
    #[clap(flatten)]
    pub input: Input,
}

/// Represents the `build` subcommand of the compiler.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    Args,
)]
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
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Subcommand,
    EnumAsInner,
    Serialize,
    StableHash,
    Deserialize,
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
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    clap::ValueEnum,
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
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    clap::ValueEnum,
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

/// Represents a CLI arguments invoking the compilation process.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    clap::Parser,
)]
#[command(styles = get_styles())]
pub struct Arguments {
    /// The subcommand to run.
    #[clap(subcommand, flatten = true)]
    pub command: Command,
}

/// The key used for retrieving the [`Arguments`]
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Arc<Arguments>)]
#[extend(method(get_invocation_arguments), no_cyclic)]
pub struct Key(pub TargetID);

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
