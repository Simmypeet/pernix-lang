use std::sync::Arc;

use fjall::PartitionCreateOptions;

use crate::runtime::persistence::backend::tuple_to_array_key;

const DATABASE_FILE: &str = "persistence.db";

const VALUE_CACHE: &str = "value_cache";
const VALUE_METADATA: &str = "value_metadata";

pub struct Impl {
    value_cache_handle: fjall::PartitionHandle,
    value_metadata_handle: fjall::PartitionHandle,
    keyspace: fjall::Keyspace,
}

impl Impl {
    const fn get_handle(&self, table: super::Table) -> &fjall::PartitionHandle {
        match table {
            super::Table::ValueCache => &self.value_cache_handle,
            super::Table::ValueMetadata => &self.value_metadata_handle,
        }
    }
}

impl std::fmt::Debug for Impl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Impl").finish_non_exhaustive()
    }
}

#[derive(Debug, Clone)]
pub struct FjallBackend(Arc<Impl>);

impl super::Backend for FjallBackend {
    type BackgroundWriter = BackgroundWriter;

    fn create(directory_path: &std::path::Path) -> Result<Self, std::io::Error>
    where
        Self: Sized,
    {
        let keyspace = fjall::Config::new(directory_path.join(DATABASE_FILE))
            .open()
            .map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to create Fjall database at path {}: {e}",
                    directory_path.display()
                ))
            })?;

        let value_cache_handle = keyspace
            .open_partition(VALUE_CACHE, PartitionCreateOptions::default())
            .map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to open partition {VALUE_CACHE}: {e}"
                ))
            })?;

        let value_metadata_handle = keyspace
            .open_partition(VALUE_METADATA, PartitionCreateOptions::default())
            .map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to open partition {VALUE_METADATA}: {e}"
                ))
            })?;

        Ok(Self(Arc::new(Impl {
            value_cache_handle,
            value_metadata_handle,
            keyspace,
        })))
    }

    async fn read(
        &self,
        table: super::Table,
        key: (u128, u128),
        buffer: &mut Vec<u8>,
    ) -> std::io::Result<bool> {
        let handle = self.0.clone();

        let result = tokio::task::spawn_blocking(move || {
            let key = tuple_to_array_key(key);
            handle.get_handle(table).get(key)
        })
        .await
        .expect("failed to join");

        (result.map_err(|e| {
            std::io::Error::other(format!(
                "Failed to read from partition {}: {e}",
                match table {
                    super::Table::ValueCache => VALUE_CACHE,
                    super::Table::ValueMetadata => VALUE_METADATA,
                }
            ))
        })?)
        .map_or(Ok(false), |value| {
            buffer.extend_from_slice(&value);
            Ok(true)
        })
    }

    fn background_writer(&self) -> Self::BackgroundWriter {
        BackgroundWriter(Arc::clone(&self.0))
    }

    fn refresh_read(&mut self) -> std::io::Result<()> { Ok(()) }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.keyspace.persist(fjall::PersistMode::SyncAll).map_err(|e| {
            std::io::Error::other(format!(
                "Failed to flush Fjall database: {e}"
            ))
        })
    }
}

#[derive(Debug, Clone)]
pub struct BackgroundWriter(Arc<Impl>);

impl super::BackgroundWriter for BackgroundWriter {
    type Transaction<'cx>
        = WriteTransaction
    where
        Self: 'cx;

    fn new_write_transaction(&self) -> Self::Transaction<'_> {
        WriteTransaction {
            write_batch: self.0.keyspace.batch(),
            fjall: self.0.clone(),
        }
    }
}

pub struct WriteTransaction {
    write_batch: fjall::WriteBatch,
    fjall: Arc<Impl>,
}

impl super::WriteTransaction for WriteTransaction {
    type Writer<'cx>
        = Writer<'cx>
    where
        Self: 'cx;

    fn write(&mut self, table: super::Table) -> Self::Writer<'_> {
        Writer {
            write_batch: &mut self.write_batch,
            partition_handle: self.fjall.get_handle(table),
        }
    }

    fn commit(self) -> Result<(), std::io::Error> {
        let result = self.write_batch.commit().map_err(|e| {
            std::io::Error::other(format!("Failed to commit write batch: {e}"))
        });

        result
    }
}

pub struct Writer<'x> {
    write_batch: &'x mut fjall::WriteBatch,
    partition_handle: &'x fjall::PartitionHandle,
}

impl super::Writer for Writer<'_> {
    fn insert(
        &mut self,
        key: (u128, u128),
        value: &[u8],
    ) -> std::io::Result<()> {
        let key = tuple_to_array_key(key);

        self.write_batch.insert(self.partition_handle, key, value);

        Ok(())
    }
}
