//! This crate contains the information about the target of the compilation.

use std::{collections::HashMap, hash::Hasher, path::PathBuf, sync::Arc};

use clap::{builder::styling, Args, Subcommand};
use derive_new::new;
use enum_as_inner::EnumAsInner;
use flexstr::SharedStr;
use pernixc_extend::extend;
use pernixc_hash::HashSet;
use pernixc_query::{
    runtime::persistence::{serde::DynamicRegistry, Persistence},
    TrackedEngine,
};
use pernixc_serialize::{
    de::Deserializer, ser::Serializer, Deserialize, Serialize,
};
use pernixc_stable_hash::StableHash;
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
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct TargetID(u128);

impl TargetID {
    /// Represents a `core` target which is included in every compilation.
    pub const CORE: Self = Self(0);

    /// A placeholder target ID commonly used for testings.
    pub const TEST: Self = Self(1);

    /// Creates a new [`TargetID`] from the given ID.
    #[must_use]
    pub fn new(id: u128) -> Self {
        assert!(
            id != 0,
            "TargetID(0) is reserved for `core` module, use `TargetID::CORE` \
             to obtain the core target"
        );

        Self(id)
    }

    /// Creates a new [`TargetID`] from the given name.
    #[must_use]
    pub fn from_target_name(name: &str) -> Self {
        let initial_key: u128 = 0xbfb4_e73d_a005_9e37_b30e_e65f_35cf_88b0;
        let mut sip_hasher = siphasher::sip128::SipHasher24::new_with_key(
            &initial_key.to_le_bytes(),
        );

        sip_hasher.write(name.as_bytes());

        Self(sip_hasher.finish128().into())
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
    pub fn target_name(&self) -> SharedStr {
        self.target_name
            .clone()
            .unwrap_or_else(|| {
                self.file
                    .file_stem()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .into_owned()
            })
            .into()
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

/// Registers all the required executors to run the queries.
pub fn register_executors(
    executor: &mut pernixc_query::runtime::executor::Registry,
) {
    executor.register(Arc::new(SeedExecutor));
}

/// Registers all the necessary runtime information for the query engine.
pub fn register_serde<
    S: Serializer<Registry>,
    D: Deserializer<Registry>,
    Registry: DynamicRegistry<S, D> + Send + Sync,
>(
    serde_registry: &mut Registry,
) where
    S::Error: Send + Sync,
{
    serde_registry.register::<Key>();
    serde_registry.register::<LinkKey>();
    serde_registry.register::<MapKey>();
    serde_registry.register::<SeedKey>();
    serde_registry.register::<LocalTargetIDKey>();
    serde_registry.register::<AllTargetIDsKey>();
}

/// Registers the keys that should be skipped during serialization and
/// deserialization in the query engine's persistence layer
pub fn skip_persistence(persistence: &mut Persistence) {
    persistence.skip_cache_value::<Key>();
    persistence.skip_cache_value::<LinkKey>();
    persistence.skip_cache_value::<MapKey>();
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
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Arc<HashMap<SharedStr, TargetID>>)]
pub struct MapKey;

/// Gets the map from the name of the target to its ID.
#[extend]
pub async fn get_target_map(
    self: &TrackedEngine,
) -> Arc<HashMap<SharedStr, TargetID>> {
    self.query(&MapKey).await.expect("should have no cyclic dependencies")
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
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Arc<HashSet<TargetID>>)]
#[extend(method(get_linked_targets), no_cyclic)]
pub struct LinkKey(pub TargetID);

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
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(u64)]
#[extend(method(get_target_seed), no_cyclic)]
pub struct SeedKey(pub TargetID);

/// Gets the initial random seed for ID generation.
#[pernixc_query::executor(key(SeedKey), name(SeedExecutor))]
#[allow(clippy::unnecessary_wraps, clippy::unused_async)]
pub async fn target_seed_executor(
    _: &SeedKey,
    _: &TrackedEngine,
) -> Result<u64, pernixc_query::runtime::executor::CyclicError> {
    Ok(rand::thread_rng().gen())
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
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(TargetID)]
pub struct LocalTargetIDKey;

/// Gets the `TargetID` that's currently being compiled in the current session.
#[extend]
async fn get_local_target_id(self: &TrackedEngine) -> TargetID {
    self.query(&LocalTargetIDKey)
        .await
        .expect("should have no cyclic dependencies")
}

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
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(Arc<HashSet<TargetID>>)]
pub struct AllTargetIDsKey;

/// Retrieves all the target IDs possible in the current session.
///
/// This includes all the target IDs of all the downstream dependencies.
#[extend]
pub async fn get_all_target_ids(
    self: &TrackedEngine,
) -> Arc<HashSet<TargetID>> {
    self.query(&AllTargetIDsKey)
        .await
        .expect("should have no cyclic dependencies")
}
