use std::sync::Arc;

use crate::runtime::persistence::backend::tuple_to_array_key;

#[allow(unused)]
const VALUE_CACHE: &[u8] = b"value_cache";
#[allow(unused)]
const VALUE_METADATA: &[u8] = b"value_metadata";

#[derive(Debug)]
pub struct Impl {
    value_cache: sled::Tree,
    value_metadata: sled::Tree,
    database: sled::Db,
}

impl Impl {
    const fn get_tree(&self, table: super::Table) -> &sled::Tree {
        match table {
            super::Table::ValueCache => &self.value_cache,
            super::Table::ValueMetadata => &self.value_metadata,
        }
    }
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub struct SledBackend(Arc<Impl>);

impl super::Backend for SledBackend {
    type BackgroundWriter = BackgroundWriter;

    fn create(directory_path: &std::path::Path) -> Result<Self, std::io::Error>
    where
        Self: Sized,
    {
        let db = sled::open(directory_path).map_err(|e| {
            std::io::Error::other(format!(
                "Failed to create Sled database at path {}: {e}",
                directory_path.display()
            ))
        })?;

        let value_cache = db.open_tree(VALUE_CACHE).map_err(|e| {
            std::io::Error::other(format!(
                "Failed to open Sled tree {}: {e}",
                String::from_utf8_lossy(VALUE_CACHE)
            ))
        })?;
        let value_metadata = db.open_tree(VALUE_METADATA).map_err(|e| {
            std::io::Error::other(format!(
                "Failed to open Sled tree {}: {e}",
                String::from_utf8_lossy(VALUE_METADATA)
            ))
        })?;

        Ok(Self(Arc::new(Impl { database: db, value_cache, value_metadata })))
    }

    async fn read(
        &self,
        table: super::Table,
        key: (u128, u128),
        buffer: &mut Vec<u8>,
    ) -> std::io::Result<bool> {
        let key_slice = tuple_to_array_key(key);

        self.0
            .get_tree(table)
            .get(key_slice)
            .map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to read from Sled tree {}: {e}",
                    match table {
                        super::Table::ValueCache =>
                            String::from_utf8_lossy(VALUE_CACHE),
                        super::Table::ValueMetadata =>
                            String::from_utf8_lossy(VALUE_METADATA),
                    }
                ))
            })?
            .map_or(Ok(false), |value| {
                buffer.extend_from_slice(&value);
                Ok(true)
            })
    }

    fn background_writer(&self) -> Self::BackgroundWriter {
        BackgroundWriter(Arc::clone(&self.0))
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.database.flush().map_err(|e| {
            std::io::Error::other(format!("Failed to flush Sled database: {e}"))
        })?;

        self.0.value_cache.flush().map_err(|e| {
            std::io::Error::other(format!(
                "Failed to flush Sled tree {}: {e}",
                String::from_utf8_lossy(VALUE_CACHE)
            ))
        })?;

        self.0.value_metadata.flush().map_err(|e| {
            std::io::Error::other(format!(
                "Failed to flush Sled tree {}: {e}",
                String::from_utf8_lossy(VALUE_METADATA)
            ))
        })?;

        Ok(())
    }

    fn refresh_read(&mut self) -> std::io::Result<()> { Ok(()) }
}

#[derive(Debug, Clone)]
pub struct BackgroundWriter(Arc<Impl>);

impl super::BackgroundWriter for BackgroundWriter {
    type Transaction<'cx>
        = Transaction
    where
        Self: 'cx;

    fn new_write_transaction(&self) -> Self::Transaction<'_> {
        Transaction {
            value_cache_bactch: sled::Batch::default(),
            value_metadata_batch: sled::Batch::default(),
            database: Arc::clone(&self.0),
        }
    }
}

pub struct Transaction {
    value_cache_bactch: sled::Batch,
    value_metadata_batch: sled::Batch,
    database: Arc<Impl>,
}

impl super::WriteTransaction for Transaction {
    type Writer<'cx>
        = &'cx mut sled::Batch
    where
        Self: 'cx;

    fn write(&mut self, table: super::Table) -> Self::Writer<'_> {
        match table {
            super::Table::ValueCache => &mut self.value_cache_bactch,
            super::Table::ValueMetadata => &mut self.value_metadata_batch,
        }
    }

    fn commit(self) -> Result<(), std::io::Error> {
        self.database
            .value_cache
            .apply_batch(self.value_cache_bactch)
            .map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to commit Sled write transaction: {e}"
                ))
            })?;

        self.database
            .value_metadata
            .apply_batch(self.value_metadata_batch)
            .map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to commit Sled write transaction: {e}"
                ))
            })?;
        Ok(())
    }
}

impl super::Writer for &mut sled::Batch {
    fn insert(
        &mut self,
        key: (u128, u128),
        value: &[u8],
    ) -> std::io::Result<()> {
        let key_slice = tuple_to_array_key(key);

        sled::Batch::insert(self, &key_slice, value);

        Ok(())
    }
}
