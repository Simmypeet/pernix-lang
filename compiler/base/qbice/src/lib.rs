//! Configuration for qbice specific to Pernix.

use std::{borrow::Borrow, hash::BuildHasherDefault, sync::Arc};

use fxhash::FxHasher64;
use linkme::distributed_slice;
use qbice::{
    Identifiable, StableHash,
    program::Registration,
    stable_hash::{SeededStableHasherBuilder, Sip128Hasher},
    storage::{intern::Interned, kv_database::rocksdb::RocksDB},
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

/// Type alias for the [`qbice::InputSession`] with configuration set to
/// [`Config`].
pub type InputSession<'x> = qbice::InputSession<'x, Config>;

/// Distributed slice for registering all the executors required for Pernix
/// compiler.
///
/// All of the executors required to run Pernix compiler should be registered
/// to this distributed slice. This is to avoid having one main central place
/// to register all the executor, which would be a merge conflict nightmare.
#[distributed_slice]
pub static PERNIX_PROGRAM: [Registration<Config>];

/// Trait for interning values in Pernix compiler.
///
/// It could be the interner from the `qbice::engine::Engine` or a custom
/// interner implementation.
pub trait Interner {
    /// Interns the given value and returns an [`Interned`] handle to it.
    fn intern<T: StableHash + Identifiable + Send + Sync + 'static>(
        &self,
        value: T,
    ) -> Interned<T>;

    /// Interns the given unsized value and returns an [`Interned`] handle to
    /// it.
    fn intern_unsized<
        T: StableHash + Identifiable + Send + Sync + 'static + ?Sized,
        Q: Borrow<T> + Send + Sync + 'static,
    >(
        &self,
        value: Q,
    ) -> Interned<T>
    where
        Arc<T>: From<Q>;
}

impl Interner for TrackedEngine {
    fn intern<T: StableHash + Identifiable + Send + Sync + 'static>(
        &self,
        value: T,
    ) -> Interned<T> {
        Self::intern(self, value)
    }

    fn intern_unsized<
        T: StableHash + Identifiable + Send + Sync + 'static + ?Sized,
        Q: Borrow<T> + Send + Sync + 'static,
    >(
        &self,
        value: Q,
    ) -> Interned<T>
    where
        Arc<T>: From<Q>,
    {
        Self::intern_unsized(self, value)
    }
}

/// The interner that implements [`Interner`] but duplicates all values instead
/// of actually interning them (for testing purposes).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, StableHash)]
pub struct DuplicatingInterner;

impl Interner for DuplicatingInterner {
    fn intern<T: StableHash + Identifiable + Send + Sync + 'static>(
        &self,
        value: T,
    ) -> Interned<T> {
        Interned::new_duplicating(value)
    }

    fn intern_unsized<
        T: StableHash + Identifiable + Send + Sync + 'static + ?Sized,
        Q: Borrow<T> + Send + Sync + 'static,
    >(
        &self,
        value: Q,
    ) -> Interned<T>
    where
        Arc<T>: From<Q>,
    {
        Interned::new_duplicating_unsized(value)
    }
}
