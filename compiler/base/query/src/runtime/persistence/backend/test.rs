use crate::runtime::persistence::backend::{
    redb::RedbBackend, Backend, BackgroundWriter, Table, WriteTransaction,
    Writer,
};

fn basic_template<B: Backend>() {
    let temp_file = tempfile::tempdir().unwrap();

    {
        let mut db = B::create(temp_file.path()).unwrap();
        let writer = db.background_writer();

        let mut transaction = writer.new_write_transaction();

        let mut writer = transaction.write(Table::ValueCache);
        writer.insert((0, 0), b"test_value").unwrap();
        drop(writer);

        let mut writer = transaction.write(Table::ValueMetadata);
        writer.insert((0, 0), b"test_meta").unwrap();
        drop(writer);

        transaction.commit().unwrap();

        db.refresh_read().unwrap();

        let mut buffer = Vec::new();
        assert!(db.read(Table::ValueCache, (0, 0), &mut buffer).unwrap());
        assert_eq!(buffer, b"test_value");

        let mut buffer = Vec::new();
        assert!(db.read(Table::ValueMetadata, (0, 0), &mut buffer).unwrap());
        assert_eq!(buffer, b"test_meta");
    }

    // create again and read it
    {
        let db = B::create(temp_file.path()).unwrap();
        let mut buffer = Vec::new();
        assert!(db.read(Table::ValueCache, (0, 0), &mut buffer).unwrap());
        assert_eq!(buffer, b"test_value");

        let mut buffer = Vec::new();
        assert!(db.read(Table::ValueMetadata, (0, 0), &mut buffer).unwrap());
        assert_eq!(buffer, b"test_meta");
    }
}

#[test]
fn basic_redb() { basic_template::<RedbBackend>(); }
