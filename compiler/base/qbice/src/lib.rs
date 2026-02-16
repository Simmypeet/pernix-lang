//! Configuration for qbice specific to Pernix.

use std::{borrow::Borrow, hash::BuildHasherDefault, path::Path, sync::Arc};

use enum_as_inner::EnumAsInner;
use fxhash::FxHasher64;
use linkme::distributed_slice;
use qbice::{
    Identifiable, StableHash,
    program::Registration,
    stable_hash::{SeededStableHasherBuilder, Sip128Hasher},
    storage::{
        self,
        dynamic_map::{cache::CacheDynamicMap, in_memory::InMemoryDynamicMap},
        intern::Interned,
        key_of_set_map::{
            cache::CacheKeyOfSetMap, in_memory::InMemoryKeyOfSetMap,
        },
        kv_database::{self, WideColumn, WideColumnValue, rocksdb::RocksDB},
        single_map::{cache::CacheSingleMap, in_memory::InMemorySingleMap},
        storage_engine::{
            self, StorageEngineFactory,
            db_backed::{Configuration, DbBacked, DbBackedFactory},
            in_memory::InMemoryStorageEngine,
        },
        write_batch::FauxWriteBatch,
        write_manager::FauxWriteManager,
    },
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
    type StorageEngine = StorageEngine;
    type BuildStableHasher = SeededStableHasherBuilder<Sip128Hasher>;
    type BuildHasher = BuildHasherDefault<FxHasher64>;
}

/// Type alias for the  with configuration set to [`Config`].
pub type Engine = qbice::Engine<Config>;

/// Type alias for the tracked [`qbice::Engine`] with configuration set to
/// [`Config`].
pub type TrackedEngine = qbice::TrackedEngine<Config>;

/// Type alias for the [`qbice::InputSession`] with configuration set to
/// [`Config`].
pub type InputSession = qbice::InputSession<Config>;

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

/// The database for the Pernix compiler that can either be `RocksDB` or
/// In-Memory database.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum StorageEngine {
    InMemory(InMemoryStorageEngine),
    DbBacked(DbBacked<RocksDB>),
}

#[derive(Debug, EnumAsInner)]
#[allow(missing_docs)]
pub enum WriteBatch {
    InMemory(FauxWriteBatch),
    DbBacked(qbice::storage::write_manager::write_behind::WriteBatch<RocksDB>),
}

impl qbice::storage::write_batch::WriteBatch for WriteBatch {}

#[derive(Debug, EnumAsInner)]
#[allow(missing_docs)]
pub enum SingleMap<K: WideColumn, V: WideColumnValue<K>> {
    InMemory(InMemorySingleMap<K, V>),
    DbBacked(CacheSingleMap<K, V, RocksDB>),
}

impl<K: WideColumn, V: WideColumnValue<K>> storage::single_map::SingleMap<K, V>
    for SingleMap<K, V>
{
    type WriteTransaction = WriteBatch;

    async fn get(&self, key: &<K as WideColumn>::Key) -> Option<V> {
        match self {
            Self::InMemory(map) => map.get(key).await,
            Self::DbBacked(map) => map.get(key).await,
        }
    }

    async fn insert(
        &self,
        key: <K as WideColumn>::Key,
        value: V,
        write_transaction: &mut Self::WriteTransaction,
    ) {
        match self {
            Self::InMemory(map) => {
                map.insert(
                    key,
                    value,
                    write_transaction.as_in_memory_mut().unwrap(),
                )
                .await;
            }
            Self::DbBacked(map) => {
                map.insert(
                    key,
                    value,
                    write_transaction.as_db_backed_mut().unwrap(),
                )
                .await;
            }
        }
    }

    async fn remove(
        &self,
        key: &<K as WideColumn>::Key,
        write_transaction: &mut Self::WriteTransaction,
    ) {
        match self {
            Self::InMemory(map) => {
                map.remove(key, write_transaction.as_in_memory_mut().unwrap())
                    .await;
            }
            Self::DbBacked(map) => {
                map.remove(key, write_transaction.as_db_backed_mut().unwrap())
                    .await;
            }
        }
    }
}

#[derive(Debug, EnumAsInner)]
#[allow(missing_docs)]
pub enum DynamicMap<K: WideColumn> {
    InMemory(InMemoryDynamicMap<K>),
    RocksDB(CacheDynamicMap<K, RocksDB>),
}

impl<K: WideColumn> storage::dynamic_map::DynamicMap<K> for DynamicMap<K> {
    type WriteTransaction = WriteBatch;

    async fn get<V: WideColumnValue<K>>(
        &self,
        key: &<K as WideColumn>::Key,
    ) -> Option<V> {
        match self {
            Self::InMemory(map) => map.get(key).await,
            Self::RocksDB(map) => map.get(key).await,
        }
    }

    async fn insert<V: WideColumnValue<K>>(
        &self,
        key: <K as WideColumn>::Key,
        value: V,
        write_transaction: &mut Self::WriteTransaction,
    ) {
        match self {
            Self::InMemory(map) => {
                map.insert(
                    key,
                    value,
                    write_transaction.as_in_memory_mut().unwrap(),
                )
                .await;
            }
            Self::RocksDB(map) => {
                map.insert(
                    key,
                    value,
                    write_transaction.as_db_backed_mut().unwrap(),
                )
                .await;
            }
        }
    }

    async fn remove<V: WideColumnValue<K>>(
        &self,
        key: &<K as WideColumn>::Key,
        write_transaction: &mut Self::WriteTransaction,
    ) {
        match self {
            Self::InMemory(map) => {
                map.remove::<V>(
                    key,
                    write_transaction.as_in_memory_mut().unwrap(),
                )
                .await;
            }
            Self::RocksDB(map) => {
                map.remove::<V>(
                    key,
                    write_transaction.as_db_backed_mut().unwrap(),
                )
                .await;
            }
        }
    }
}

#[derive(Debug, EnumAsInner)]
#[allow(missing_docs)]
pub enum KeyOfSetMap<
    K: kv_database::KeyOfSetColumn,
    C: storage::key_of_set_map::ConcurrentSet<Element = K::Element>,
> {
    InMemory(InMemoryKeyOfSetMap<K, C>),
    RocksDB(CacheKeyOfSetMap<K, C, RocksDB>),
}

#[derive(Debug)]
#[allow(missing_docs)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L: Iterator, R: Iterator<Item = L::Item>> Iterator for Either<L, R> {
    type Item = L::Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Left(l) => l.next(),
            Self::Right(r) => r.next(),
        }
    }
}

impl<
    K: kv_database::KeyOfSetColumn,
    C: storage::key_of_set_map::ConcurrentSet<Element = K::Element>,
> storage::key_of_set_map::KeyOfSetMap<K, C> for KeyOfSetMap<K, C>
{
    type WriteBatch = WriteBatch;

    async fn get(
        &self,
        key: &<K as kv_database::KeyOfSetColumn>::Key,
    ) -> impl Iterator<Item = <K as kv_database::KeyOfSetColumn>::Element> + Send
    {
        match self {
            Self::InMemory(map) => Either::Left(map.get(key).await),
            Self::RocksDB(map) => Either::Right(map.get(key).await),
        }
    }

    async fn insert(
        &self,
        key: <K as kv_database::KeyOfSetColumn>::Key,
        element: <K as kv_database::KeyOfSetColumn>::Element,
        write_batch: &mut Self::WriteBatch,
    ) {
        match self {
            Self::InMemory(map) => {
                map.insert(
                    key,
                    element,
                    write_batch.as_in_memory_mut().unwrap(),
                )
                .await;
            }
            Self::RocksDB(map) => {
                map.insert(
                    key,
                    element,
                    write_batch.as_db_backed_mut().unwrap(),
                )
                .await;
            }
        }
    }

    async fn remove(
        &self,
        key: &<K as kv_database::KeyOfSetColumn>::Key,
        element: &<K as kv_database::KeyOfSetColumn>::Element,
        write_batch: &mut Self::WriteBatch,
    ) {
        match self {
            Self::InMemory(map) => {
                map.remove(
                    key,
                    element,
                    write_batch.as_in_memory_mut().unwrap(),
                )
                .await;
            }
            Self::RocksDB(map) => {
                map.remove(
                    key,
                    element,
                    write_batch.as_db_backed_mut().unwrap(),
                )
                .await;
            }
        }
    }
}

#[derive(Debug, EnumAsInner)]
#[allow(missing_docs)]
pub enum WriteManager {
    InMemory(FauxWriteManager),
    RocksDB(qbice::storage::write_manager::write_behind::WriteBehind<RocksDB>),
}

impl qbice::storage::write_manager::WriteManager for WriteManager {
    type WriteBatch = WriteBatch;

    fn new_write_batch(&self) -> Self::WriteBatch {
        match self {
            Self::InMemory(manager) => {
                WriteBatch::InMemory(manager.new_write_batch())
            }
            Self::RocksDB(manager) => {
                WriteBatch::DbBacked(manager.new_write_batch())
            }
        }
    }

    fn submit_write_batch(&self, write_batch: Self::WriteBatch) {
        match self {
            Self::InMemory(manager) => manager
                .submit_write_batch(write_batch.into_in_memory().unwrap()),
            Self::RocksDB(manager) => {
                manager
                    .submit_write_batch(write_batch.into_db_backed().unwrap());
            }
        }
    }
}

/// The factory for creating in-memory databases for Pernix compiler.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InMemoryFactory;

impl StorageEngineFactory for InMemoryFactory {
    type StorageEngine = StorageEngine;

    type Error = std::convert::Infallible;

    fn open(
        self,
        _serialization_plugin: qbice::serialize::Plugin,
    ) -> Result<Self::StorageEngine, Self::Error> {
        Ok(StorageEngine::InMemory(InMemoryStorageEngine))
    }
}

impl storage_engine::StorageEngine for StorageEngine {
    type WriteTransaction = WriteBatch;

    type WriteManager = WriteManager;

    type SingleMap<
        K: kv_database::WideColumn,
        V: kv_database::WideColumnValue<K>,
    > = SingleMap<K, V>;

    type DynamicMap<K: kv_database::WideColumn> = DynamicMap<K>;

    type KeyOfSetMap<
        K: kv_database::KeyOfSetColumn,
        C: storage::key_of_set_map::ConcurrentSet<Element = K::Element>,
    > = KeyOfSetMap<K, C>;

    fn new_write_manager(&self) -> Self::WriteManager {
        match self {
            Self::InMemory(i) => WriteManager::InMemory(i.new_write_manager()),
            Self::DbBacked(i) => WriteManager::RocksDB(i.new_write_manager()),
        }
    }

    fn new_single_map<
        K: kv_database::WideColumn,
        V: kv_database::WideColumnValue<K>,
    >(
        &self,
    ) -> Self::SingleMap<K, V> {
        match self {
            Self::InMemory(i) => SingleMap::InMemory(i.new_single_map()),
            Self::DbBacked(i) => SingleMap::DbBacked(i.new_single_map()),
        }
    }

    fn new_dynamic_map<K: kv_database::WideColumn>(
        &self,
    ) -> Self::DynamicMap<K> {
        match self {
            Self::InMemory(i) => DynamicMap::InMemory(i.new_dynamic_map()),
            Self::DbBacked(i) => DynamicMap::RocksDB(i.new_dynamic_map()),
        }
    }

    fn new_key_of_set_map<
        K: kv_database::KeyOfSetColumn,
        C: storage::key_of_set_map::ConcurrentSet<Element = K::Element>,
    >(
        &self,
    ) -> Self::KeyOfSetMap<K, C> {
        match self {
            Self::InMemory(i) => KeyOfSetMap::InMemory(i.new_key_of_set_map()),
            Self::DbBacked(i) => KeyOfSetMap::RocksDB(i.new_key_of_set_map()),
        }
    }
}

/// The factory for creating `RocksDB` backed databases for Pernix compiler.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, StableHash)]
pub struct IncrementalStorageEngine<P>(pub P);

impl<P: AsRef<Path>> StorageEngineFactory for IncrementalStorageEngine<P> {
    type StorageEngine = StorageEngine;

    type Error = qbice::storage::kv_database::rocksdb::RocksDBError;

    fn open(
        self,
        serialization_plugin: qbice::serialize::Plugin,
    ) -> Result<Self::StorageEngine, Self::Error> {
        DbBackedFactory::builder()
            .configuration(Configuration::builder().build())
            .db_factory(RocksDB::factory(self.0))
            .build()
            .open(serialization_plugin)
            .map(StorageEngine::DbBacked)
    }
}
