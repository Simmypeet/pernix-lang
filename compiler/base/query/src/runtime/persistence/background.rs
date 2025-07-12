use std::sync::Arc;

use pernixc_hash::HashMap;
use redb::TableDefinition;

mod frame;

/// Determines which table to save the value metadata to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum Table {
    Value,
    Metadata,
}

impl Table {
    pub const fn definition(
        self,
    ) -> TableDefinition<'static, (u128, u128), &'static [u8]> {
        match self {
            Self::Value => super::VALUE_CACHE,
            Self::Metadata => super::VALUE_METADATA,
        }
    }
}

#[allow(clippy::type_complexity)]
pub struct SaveTask {
    pub(super) key: (u128, u128),
    pub(super) table: Table,
    pub(super) write: Box<dyn FnOnce(&mut Vec<u8>) -> bool + Send>,
}

/// A struct that helps serializing data in a background thread and sending it
/// to the redb writer for writing to the persistent storage.
pub(super) struct Writer {
    serialize_workers: Vec<std::thread::JoinHandle<()>>,

    send_serialize_task: Option<crossbeam::channel::Sender<SaveTask>>,
}

impl Writer {
    pub(crate) fn new(
        serializer_count: usize,
        database: &Arc<redb::Database>,
    ) -> Self {
        let (send_serialize_task, receiver_task) =
            crossbeam::channel::unbounded();

        let workers = (0..serializer_count)
            .map(|i| {
                let receiver_task = receiver_task.clone();
                let database = database.clone();

                std::thread::Builder::new()
                    .name(format!("serialize-worker-{i}"))
                    .spawn(move || {
                        Self::work_loop(&receiver_task, &database);
                    })
                    .unwrap()
            })
            .collect::<Vec<_>>();

        Self {
            serialize_workers: workers,
            // NOTE: make sure only one sender is held at a time
            send_serialize_task: Some(send_serialize_task),
        }
    }

    pub(super) fn new_serialize_task(&self, task: SaveTask) {
        self.send_serialize_task.as_ref().unwrap().send(task).unwrap();
    }

    fn work_loop(
        receiver_task: &crossbeam::channel::Receiver<SaveTask>,
        database: &redb::Database,
    ) {
        const FLUSH_BATCH_SIZE: usize = 5 * 1024 * 1024; // 1 MiB
        let mut buffers = HashMap::<Table, frame::Buffer>::default();

        loop {
            // receive the task to serialize
            let Ok(task) = receiver_task.recv() else {
                break;
            };

            let buffer = buffers.entry(task.table).or_default();

            // write the task to the frame buffer for batching
            buffer.write(task.key, |buffer| (task.write)(buffer));

            if buffer.byte_size() >= FLUSH_BATCH_SIZE {
                let write = match database.begin_write() {
                    Ok(write) => write,
                    Err(e) => {
                        tracing::error!(
                            "Failed to begin write transaction: {e}"
                        );
                        continue;
                    }
                };

                let mut table = match write.open_table(task.table.definition())
                {
                    Ok(table) => table,
                    Err(e) => {
                        tracing::error!(
                            "Failed to open table {:?} for writing: {e}",
                            task.table
                        );
                        continue;
                    }
                };

                buffer.flush(&mut table);

                drop(table);

                if let Err(e) = write.commit() {
                    tracing::error!("Failed to commit write transaction: {e}");
                }
            }
        }

        for (&key, buffer) in &mut buffers {
            if buffer.byte_size() != 0 {
                let write = match database.begin_write() {
                    Ok(write) => write,
                    Err(e) => {
                        tracing::error!(
                            "Failed to begin write transaction: {e}"
                        );
                        continue;
                    }
                };

                let mut table = match write.open_table(key.definition()) {
                    Ok(table) => table,
                    Err(e) => {
                        tracing::error!(
                            "Failed to open table {:?} for writing: {e}",
                            key
                        );
                        continue;
                    }
                };

                buffer.flush(&mut table);

                drop(table);

                if let Err(e) = write.commit() {
                    tracing::error!("Failed to commit write transaction: {e}");
                }
            }
        }
    }
}

impl Drop for Writer {
    fn drop(&mut self) {
        // drop the sender to stop the workers
        self.send_serialize_task.take();

        // all the senders are dropped (which is the only one here),
        // so all the serializer workers will exit which will cause the
        // only write worker to exit as well
        for worker in self.serialize_workers.drain(..) {
            worker.join().unwrap();
        }
    }
}
