use std::path::Path;

pub mod redb;

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
        key: &[u8],
        buffer: &mut Vec<u8>,
    ) -> std::io::Result<bool>;

    fn background_writer(&self) -> Self::BackgroundWriter;

    fn refresh_read(&mut self) -> std::io::Result<()>;
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
    fn insert(&mut self, key: &[u8], value: &[u8]) -> std::io::Result<()>;
}
