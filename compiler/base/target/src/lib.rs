//! This crate contains the information about the target of the compilation.

use std::{collections::HashMap, hash::Hasher, path::PathBuf, sync::Arc};

use clap::{Args, Subcommand, builder::styling};
use derive_new::new;
use enum_as_inner::EnumAsInner;
use pernixc_hash::HashSet;
use pernixc_qbice::TrackedEngine;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};
use rand::Rng;
use siphasher::sip128::Hasher128;

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

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
    Encode,
    Decode,
    StableHash,
)]
pub struct TargetID {
    lo: u64,
    hi: u64,
}

impl TargetID {
    /// Represents a `core` target which is included in every compilation.
    pub const CORE: Self = Self { lo: 0, hi: 0 };

    /// A placeholder target ID commonly used for testings.
    pub const TEST: Self = Self { lo: 1, hi: 0 };

    /// Creates a new [`TargetID`] from the given ID.
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn new(id: u128) -> Self {
        assert!(
            id != 0,
            "TargetID(0) is reserved for `core` module, use `TargetID::CORE` \
             to obtain the core target"
        );

        Self { lo: id as u64, hi: (id >> 64) as u64 }
    }

    /// Creates a new [`TargetID`] from the given name.
    #[must_use]
    pub fn from_target_name(name: &str) -> Self {
        let initial_key: u128 = 0xbfb4_e73d_a005_9e37_b30e_e65f_35cf_88b0;
        let mut sip_hasher = siphasher::sip128::SipHasher24::new_with_key(
            &initial_key.to_le_bytes(),
        );

        sip_hasher.write(name.as_bytes());

        let hash = sip_hasher.finish128();

        let lo = hash.h1;
        let hi = hash.h2;

        Self { lo, hi }
    }

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
    Encode,
    Decode,
    StableHash,
    Identifiable,
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
    Encode,
    Decode,
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

    /// Produces the chrome tracing format for the compilation.
    ///
    /// This is primarily used for debugging purposes and can be viewed in
    /// the Chrome browser.
    #[clap(long = "chrome")]
    pub chrome_tracing: bool,

    /// The seed for the compiler internal ID generation.
    ///
    /// This option is meant to be used internally for testing and debugging
    /// purposes.
    #[clap(long = "target-seed")]
    pub target_seed: Option<u64>,
}

impl Input {
    /// Returns the target name of the input file.
    #[must_use]
    pub fn target_name(&self) -> String {
        self.target_name.clone().unwrap_or_else(|| {
            self.file
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .into_owned()
        })
    }
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
    Encode,
    Decode,
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
    Encode,
    Decode,
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
    Encode,
    Decode,
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
    Encode,
    Decode,
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
    Encode,
    Decode,
    StableHash,
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
    Encode,
    Decode,
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
    Encode,
    Decode,
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
    Encode,
    Decode,
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
    Encode,
    Decode,
    StableHash,
    qbice::Query,
)]
#[value(Arc<Arguments>)]
#[extend(name = get_invocation_arguments, by_val)]
pub struct Key {
    /// The target ID of the compilation session.
    pub target_id: TargetID,
}

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

/// A query input for mapping names to their target IDs.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    qbice::Query,
)]
#[value(Arc<HashMap<Interned<str>, TargetID>>)]
#[extend(name = get_target_map, by_val)]
pub struct MapKey;

/// A query for retrieving the linked targets of a given target ID.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    qbice::Query,
)]
#[value(Arc<HashSet<TargetID>>)]
#[extend(name = get_linked_targets, by_val)]
pub struct LinkKey {
    /// The target ID to retrieve the linked targets for.
    pub target_id: TargetID,
}

/// A query for retrieving the linked targets of a given target ID.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    qbice::Query,
)]
#[value(u64)]
#[extend(name = get_target_seed, by_val)]
pub struct SeedKey {
    /// The target ID to retrieve the seed for.
    pub target_id: TargetID,
}

/// The executor that uses rabndom number generator to produce a target seed.
#[qbice::executor(config = pernixc_qbice::Config)]
#[allow(clippy::unused_async)]
pub async fn target_seed_executor(_: &SeedKey, _: &TrackedEngine) -> u64 {
    rand::rng().random()
}

/// A query for retrieving the `TargetID` that's currently being compiled in
/// the current session.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    qbice::Query,
)]
#[value(TargetID)]
#[extend(name = get_local_target_id, by_val)]
pub struct LocalTargetIDKey;

/// A query for retrieving all the target IDs, including the downstream
/// dependencies.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    qbice::Query,
)]
#[value(Arc<HashSet<TargetID>>)]
#[extend(name = get_all_target_ids)]
pub struct AllTargetIDsKey;
