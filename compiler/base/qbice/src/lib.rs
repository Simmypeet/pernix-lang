//! Configuration for qbice specific to Pernix.

use std::hash::BuildHasherDefault;

use fxhash::FxHasher64;
use linkme::distributed_slice;
use qbice::{
    Identifiable,
    program::Registration,
    stable_hash::{SeededStableHasherBuilder, Sip128Hasher},
    storage::kv_database::rocksdb::RocksDB,
};

/// The configuration struct specificly for Pernix compiler.
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
    Identifiable,
)]
pub struct Config;

impl qbice::Config for Config {
    type Database = RocksDB;
    type BuildStableHasher = SeededStableHasherBuilder<Sip128Hasher>;
    type BuildHasher = BuildHasherDefault<FxHasher64>;
}

/// Type alias for the [`qbice::Engine`] with configuration set to [`Config`].
pub type Engine = qbice::Engine<Config>;

/// Type alias for the tracked [`qbice::Engine`] with configuration set to
/// [`Config`].
pub type TrackedEngine = qbice::TrackedEngine<Config>;

/// Distributed slice for registering all the executors required for Pernix
/// compiler.
///
/// All of the executors required to run Pernix compiler should be registered
/// to this distributed slice. This is to avoid having one main central place
/// to register all the executor, which would be a merge conflict nightmare.
#[distributed_slice]
pub static PERNIX_PROGRAM: [Registration<Config>];
