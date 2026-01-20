//! Configuration for qbice specific to Pernix.

use std::hash::BuildHasherDefault;

use fxhash::FxHasher64;
use qbice::{
    Identifiable,
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
