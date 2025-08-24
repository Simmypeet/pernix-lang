use std::{path::Path, sync::Arc};

use parking_lot::RwLock;
use redb::TableHandle;

const DATABASE_FILE: &str = "persistence.db";

/// Table definition for the query value cache. The key is a tuple of
/// `STABLE_TYPE_ID` of the key and fingerprint of the value.
const VALUE_CACHE: redb::TableDefinition<(u128, u128), &[u8]> =
    redb::TableDefinition::new("value_cache");

const VALUE_METADATA: redb::TableDefinition<(u128, u128), &[u8]> =
    redb::TableDefinition::new("value_metadata");

#[derive(Debug)]
pub struct Impl {
    database: Arc<redb::Database>,
    read_transaction: RwLock<Option<redb::ReadTransaction>>,
}

#[derive(Debug, Clone)]
pub struct RedbBackend(Arc<Impl>);

impl super::Backend for RedbBackend {
    type BackgroundWriter = BackgroundWriter;

    fn create(directory_path: &Path) -> Result<Self, std::io::Error>
    where
        Self: Sized,
    {
        let database_path = directory_path.join(DATABASE_FILE);
        let database = redb::Database::create(&database_path).map_err(|e| {
            std::io::Error::other(format!(
                "Failed to create database at path {}: {e}",
                database_path.display()
            ))
        })?;

        // create the tables if they do not exist
        let write = database.begin_write().map_err(|e| {
            std::io::Error::other(format!(
                "Failed to begin write transaction: {e}",
            ))
        })?;

        write.open_table(VALUE_CACHE).map_err(|e| {
            std::io::Error::other(format!(
                "Failed to open table {}: {e}",
                VALUE_CACHE.name()
            ))
        })?;
        write.open_table(VALUE_METADATA).map_err(|e| {
            std::io::Error::other(format!(
                "Failed to open table {}: {e}",
                VALUE_METADATA.name()
            ))
        })?;

        write.commit().map_err(|e| {
            std::io::Error::other(format!(
                "Failed to commit write transaction: {e}",
            ))
        })?;

        Ok(Self(Arc::new(Impl {
            read_transaction: RwLock::new(Some(
                database.begin_read().map_err(|e| {
                    std::io::Error::other(format!(
                        "Failed to begin read transaction: {e}",
                    ))
                })?,
            )),
            database: Arc::new(database),
        })))
    }

    async fn read(
        &self,
        table: super::Table,
        key: (u128, u128),
        buffer: &mut Vec<u8>,
    ) -> std::io::Result<bool> {
        let value = tokio::task::spawn_blocking({
            let db = self.0.clone();

            move || {
                let table = db
                    .read_transaction
                    .read()
                    .as_ref()
                    .unwrap()
                    .open_table(match table {
                        super::Table::ValueCache => VALUE_CACHE,
                        super::Table::ValueMetadata => VALUE_METADATA,
                    })
                    .map_err(|e| {
                        std::io::Error::other(format!(
                            "Failed to open table {}: {e}",
                            match table {
                                super::Table::ValueCache => VALUE_CACHE.name(),
                                super::Table::ValueMetadata =>
                                    VALUE_METADATA.name(),
                            }
                        ))
                    })?;

                table.get(key).map_err(|e| {
                    std::io::Error::other(format!(
                        "Failed to get value from table: {e}"
                    ))
                })
            }
        })
        .await
        .expect("failed to join")?;

        if let Some(value) = value {
            buffer.extend_from_slice(value.value());
        } else {
            return Ok(false);
        }

        Ok(true)
    }

    fn background_writer(&self) -> Self::BackgroundWriter {
        BackgroundWriter(self.0.database.clone())
    }

    fn refresh_read(&mut self) -> std::io::Result<()> {
        // create a new read transaction
        *self.0.read_transaction.write() =
            Some(self.0.database.begin_read().map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to begin read transaction: {e}",
                ))
            })?);

        Ok(())
    }

    fn flush(&mut self) -> std::io::Result<()> { Ok(()) }
}

#[derive(Debug, Clone)]
pub struct BackgroundWriter(Arc<redb::Database>);

impl super::BackgroundWriter for BackgroundWriter {
    type Transaction<'cx>
        = redb::WriteTransaction
    where
        Self: 'cx;

    fn new_write_transaction(&self) -> Self::Transaction<'_> {
        self.0.begin_write().unwrap()
    }
}

impl super::WriteTransaction for redb::WriteTransaction {
    type Writer<'cx>
        = redb::Table<'cx, (u128, u128), &'static [u8]>
    where
        Self: 'cx;

    fn write(&mut self, table: super::Table) -> Self::Writer<'_> {
        match table {
            super::Table::ValueCache => self.open_table(VALUE_CACHE).unwrap(),
            super::Table::ValueMetadata => {
                self.open_table(VALUE_METADATA).unwrap()
            }
        }
    }

    fn commit(self) -> Result<(), std::io::Error> {
        self.commit().map_err(|e| {
            std::io::Error::other(format!("Failed to commit transaction: {e}"))
        })
    }
}

impl super::Writer for redb::Table<'_, (u128, u128), &'static [u8]> {
    fn insert(
        &mut self,
        key: (u128, u128),
        value: &[u8],
    ) -> std::io::Result<()> {
        self.insert(key, value)
            .map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to insert into table: {e}"
                ))
            })
            .map(|_| ())
    }
}
