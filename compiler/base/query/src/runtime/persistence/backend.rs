use std::path::Path;

pub mod fjall;
pub mod redb;
pub mod sled;

fn tuple_to_array_key(key: (u128, u128)) -> [u8; 32] {
    let mut array = [0u8; 32];
    array[..16].copy_from_slice(&key.0.to_le_bytes());
    array[16..].copy_from_slice(&key.1.to_le_bytes());
    array
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Table {
    ValueCache,
    ValueMetadata,
}

pub trait Backend: Clone {
    type BackgroundWriter: BackgroundWriter;

    fn create(directory_path: &Path) -> Result<Self, std::io::Error>
    where
        Self: Sized;

    fn read(
        &self,
        table: Table,
        key: (u128, u128),
        buffer: &mut Vec<u8>,
    ) -> impl std::future::Future<Output = std::io::Result<bool>>;

    fn background_writer(&self) -> Self::BackgroundWriter;

    fn flush(&mut self) -> std::io::Result<()>;

    fn refresh_read(&mut self) -> std::io::Result<()>;

    fn read_owned(
        &self,
        table: Table,
        key: (u128, u128),
    ) -> impl std::future::Future<Output = std::io::Result<Option<Vec<u8>>>>
    {
        async move {
            let mut buffer = Vec::new();

            if self.read(table, key, &mut buffer).await? {
                Ok(Some(buffer))
            } else {
                Ok(None)
            }
        }
    }
}

pub trait BackgroundWriter: Clone + Send + Sync + 'static {
    type Transaction<'cx>: WriteTransaction
    where
        Self: 'cx;

    fn new_write_transaction(&self) -> Self::Transaction<'_>;
}

pub trait WriteTransaction {
    type Writer<'cx>: Writer
    where
        Self: 'cx;

    fn write(&mut self, table: Table) -> Self::Writer<'_>;

    fn commit(self) -> Result<(), std::io::Error>;
}

pub trait Writer {
    fn insert(
        &mut self,
        key: (u128, u128),
        value: &[u8],
    ) -> std::io::Result<()>;
}

#[cfg(test)]
mod test;
