use crate::runtime::persistence::{
    backend::{self, Backend, Table},
    background::Worker,
};

fn basic<B: Backend>() {
    let tempdir = tempfile::tempdir().unwrap();

    {
        let mut db = B::create(tempdir.path()).unwrap();
        let worker = Worker::new(1, db.background_writer());

        worker.new_save_task(super::SaveTask {
            key: (0, 0),
            table: Table::ValueCache,
            write: Box::new(|buffer| {
                buffer.extend_from_slice(b"test_value");
                true
            }),
        });

        worker.new_save_task(super::SaveTask {
            key: (1, 1),
            table: Table::ValueMetadata,
            write: Box::new(|buffer| {
                buffer.extend_from_slice(b"test_meta");
                true
            }),
        });

        // shutting down worker, commit value
        drop(worker);

        db.refresh_read().unwrap();

        assert_eq!(
            db.read_owned(Table::ValueCache, (0, 0)).unwrap(),
            Some(b"test_value".to_vec())
        );
        assert_eq!(
            db.read_owned(Table::ValueMetadata, (1, 1)).unwrap(),
            Some(b"test_meta".to_vec())
        );
    }

    // creating new database
    {
        let db = B::create(tempdir.path()).unwrap();

        assert_eq!(
            db.read_owned(Table::ValueCache, (0, 0)).unwrap(),
            Some(b"test_value".to_vec())
        );
        assert_eq!(
            db.read_owned(Table::ValueMetadata, (1, 1)).unwrap(),
            Some(b"test_meta".to_vec())
        );
    }
}

#[test]
fn basic_redb() { basic::<backend::redb::RedbBackend>(); }
