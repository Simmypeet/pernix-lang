use std::{path::Path, sync::Arc};

use crate::runtime::persistence::backend::tuple_to_array_key;

#[allow(unused)]
const VALUE_CACHE: &str = "value_cache";
#[allow(unused)]
const VALUE_METADATA: &str = "value_metadata";

type RocksDB = rust_rocksdb::DBWithThreadMode<rust_rocksdb::MultiThreaded>;

pub struct Impl {
    db: Arc<RocksDB>,
}

impl Impl {
    const fn get_column_family_name(table: super::Table) -> &'static str {
        match table {
            super::Table::ValueCache => VALUE_CACHE,
            super::Table::ValueMetadata => VALUE_METADATA,
        }
    }
}

impl std::fmt::Debug for Impl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Impl").finish_non_exhaustive()
    }
}

#[derive(Clone)]
#[allow(unused)]
pub struct RocksDbBackend(Arc<Impl>);

impl std::fmt::Debug for RocksDbBackend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RocksDbBackend").finish_non_exhaustive()
    }
}

impl super::Backend for RocksDbBackend {
    type BackgroundWriter = BackgroundWriter;

    fn create(directory_path: &Path) -> Result<Self, std::io::Error>
    where
        Self: Sized,
    {
        let mut opts = rust_rocksdb::Options::default();
        opts.create_if_missing(true);
        opts.create_missing_column_families(true);

        // Optimize for speed - disable ACID compliance features
        // since this is just a cache
        // Disable WAL (write-ahead log) for speed since we don't need
        // durability Cache can be rebuilt if lost
        opts.set_manual_wal_flush(true);
        opts.set_use_fsync(false);
        opts.set_unordered_write(true);

        let cf_opts = rust_rocksdb::Options::default();

        let db = RocksDB::open_cf_with_opts(&opts, directory_path, vec![
            (VALUE_CACHE, cf_opts.clone()),
            (VALUE_METADATA, cf_opts),
        ])
        .map_err(|e| {
            std::io::Error::other(format!(
                "Failed to create RocksDB database at path {}: {e}",
                directory_path.display()
            ))
        })?;

        Ok(Self(Arc::new(Impl { db: Arc::new(db) })))
    }

    async fn read(
        &self,
        table: super::Table,
        key: (u128, u128),
        buffer: &mut Vec<u8>,
    ) -> std::io::Result<bool> {
        let key_bytes = tuple_to_array_key(key);
        let cf_name = Impl::get_column_family_name(table);

        let cf = self.0.db.cf_handle(cf_name).ok_or_else(|| {
            std::io::Error::other(format!(
                "Failed to get column family handle for {cf_name}"
            ))
        })?;

        let result = self.0.db.get_cf(&cf, key_bytes).map_err(|e| {
            std::io::Error::other(format!(
                "Failed to read from RocksDB column family {cf_name}: {e}"
            ))
        })?;

        Ok(result.is_some_and(|value| {
            buffer.extend_from_slice(&value);
            true
        }))
    }

    fn background_writer(&self) -> Self::BackgroundWriter {
        BackgroundWriter(Arc::clone(&self.0))
    }

    fn flush(&mut self) -> std::io::Result<()> {
        // Flush memtables to disk
        let mut flush_opts = rust_rocksdb::FlushOptions::default();
        flush_opts.set_wait(true);

        let value_cache_cf =
            self.0.db.cf_handle(VALUE_CACHE).ok_or_else(|| {
                std::io::Error::other(format!(
                    "Failed to get column family handle for {VALUE_CACHE}"
                ))
            })?;

        self.0.db.flush_cf_opt(&value_cache_cf, &flush_opts).map_err(|e| {
            std::io::Error::other(format!(
                "Failed to flush RocksDB column family {VALUE_CACHE}: {e}"
            ))
        })?;

        let value_metadata_cf =
            self.0.db.cf_handle(VALUE_METADATA).ok_or_else(|| {
                std::io::Error::other(format!(
                    "Failed to get column family handle for {VALUE_METADATA}"
                ))
            })?;

        self.0.db.flush_cf_opt(&value_metadata_cf, &flush_opts).map_err(
            |e| {
                std::io::Error::other(format!(
                    "Failed to flush RocksDB column family {VALUE_METADATA}: \
                     {e}"
                ))
            },
        )?;

        Ok(())
    }

    fn refresh_read(&mut self) -> std::io::Result<()> {
        // RocksDB reads are always up-to-date
        Ok(())
    }
}

#[derive(Clone)]
pub struct BackgroundWriter(Arc<Impl>);

impl std::fmt::Debug for BackgroundWriter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BackgroundWriter").finish_non_exhaustive()
    }
}

impl super::BackgroundWriter for BackgroundWriter {
    type Transaction<'cx>
        = WriteTransaction
    where
        Self: 'cx;

    fn new_write_transaction(&self) -> Self::Transaction<'_> {
        WriteTransaction {
            write_batch: rust_rocksdb::WriteBatch::default(),
            backend: Arc::clone(&self.0),
        }
    }
}

pub struct WriteTransaction {
    write_batch: rust_rocksdb::WriteBatch,
    backend: Arc<Impl>,
}

impl super::WriteTransaction for WriteTransaction {
    type Writer<'cx>
        = Writer<'cx>
    where
        Self: 'cx;

    fn write(&mut self, table: super::Table) -> Self::Writer<'_> {
        Writer {
            write_batch: &mut self.write_batch,
            backend: Arc::clone(&self.backend),
            table,
        }
    }

    fn commit(self) -> Result<(), std::io::Error> {
        let mut write_opts = rust_rocksdb::WriteOptions::default();
        // Disable WAL for maximum speed - cache can be rebuilt if lost
        write_opts.disable_wal(true);

        self.backend.db.write_opt(&self.write_batch, &write_opts).map_err(|e| {
            std::io::Error::other(format!(
                "Failed to commit RocksDB write batch: {e}"
            ))
        })
    }
}

pub struct Writer<'x> {
    write_batch: &'x mut rust_rocksdb::WriteBatch,
    backend: Arc<Impl>,
    table: super::Table,
}

impl super::Writer for Writer<'_> {
    fn insert(
        &mut self,
        key: (u128, u128),
        value: &[u8],
    ) -> std::io::Result<()> {
        let key_bytes = tuple_to_array_key(key);
        let cf_name = Impl::get_column_family_name(self.table);

        let cf = self.backend.db.cf_handle(cf_name).ok_or_else(|| {
            std::io::Error::other(format!(
                "Failed to get column family handle for {cf_name}"
            ))
        })?;

        self.write_batch.put_cf(&cf, key_bytes, value);
        Ok(())
    }
}
