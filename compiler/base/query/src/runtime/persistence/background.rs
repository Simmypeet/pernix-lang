use std::{collections::VecDeque, sync::Arc};

use parking_lot::Mutex;
use pernixc_stable_type_id::StableTypeID;

/// Determines which table to save the value metadata to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum Table {
    Value,
    Metadata,
}

/// The result of serializing a value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct SerializeResult {
    pub table: Table,
    pub stable_type_id: StableTypeID,
    pub fingerprint: u128,
    pub buffer: Buffer,
}

type SerializeTask =
    Box<dyn FnOnce(Buffer) -> Result<SerializeResult, Buffer> + Send>;

type Buffer = Vec<u8>;

/// A helper struct for circular buffer management, allowing to reuse
/// buffers and avoid unnecessary allocations.
#[derive(Debug, Clone)]
struct BufferPool {
    buffer: Arc<Mutex<VecDeque<Buffer>>>,
}

impl BufferPool {
    fn new() -> Self { Self { buffer: Arc::new(Mutex::new(VecDeque::new())) } }

    fn get(&self) -> Buffer {
        let mut buffer = self.buffer.lock();

        buffer.pop_front().map_or_else(|| Vec::with_capacity(1024), |buf| buf)
    }

    fn put(&self, mut return_buffer: Buffer) {
        let mut buf = self.buffer.lock();
        return_buffer.clear();

        if buf.len() < 100 {
            buf.push_back(return_buffer);
        }
    }
}

/// A struct that helps serializing data in a background thread and sending it
/// to the redb writer for writing to the persistent storage.
pub(super) struct Writer {
    serialize_workers: Vec<std::thread::JoinHandle<()>>,

    send_serialize_task: Option<crossbeam::channel::Sender<SerializeTask>>,
}

struct Batch {
    cache: Vec<SerializeResult>,
    metadata: Vec<SerializeResult>,
    current_buffer_size: usize,
}

impl Default for Batch {
    fn default() -> Self {
        Self {
            cache: Vec::with_capacity(50),
            metadata: Vec::with_capacity(50),
            current_buffer_size: 0,
        }
    }
}

impl Batch {
    // the maximum size of bytes that can be held before writing to the
    // database
    const MAX_BUFFER_SIZE: usize = 1024 * 1024 * 10;

    pub fn new() -> Self { Self::default() }

    pub fn write_batch(
        mut self,
        database: &super::Database,
        buffer_pool: &BufferPool,
    ) {
        if self.cache.is_empty() && self.metadata.is_empty() {
            return;
        }

        // write the batch to the database
        Self::write_batch_internal(
            database,
            &mut self.cache,
            Table::Value,
            buffer_pool,
        );
        Self::write_batch_internal(
            database,
            &mut self.metadata,
            Table::Metadata,
            buffer_pool,
        );
    }

    fn write_batch_internal(
        database: &super::Database,
        batch: &mut Vec<SerializeResult>,
        table: Table,
        buffer_pool: &BufferPool,
    ) {
        let write = match database.write_transaction() {
            Ok(write) => write,
            Err(e) => {
                tracing::error!("Failed to get write transaction: {e}");

                for result in batch.drain(..) {
                    // return the buffer to the pool
                    buffer_pool.put(result.buffer);
                }

                return;
            }
        };

        match table {
            Table::Value => write.with_cache_table(|x| {
                Self::write_to_table(&mut x.write(), batch, buffer_pool);
            }),
            Table::Metadata => write.with_metadata_table(|x| {
                Self::write_to_table(&mut x.write(), batch, buffer_pool);
            }),
        }
    }

    fn write_to_table(
        table: &mut redb::Table<'_, (u128, u128), &[u8]>,
        batch: &mut Vec<SerializeResult>,
        buffer_pool: &BufferPool,
    ) {
        for result in batch.drain(..) {
            let key = (result.stable_type_id.as_u128(), result.fingerprint);

            if let Err(e) = table.insert(key, result.buffer.as_slice()) {
                tracing::error!("Failed to write {result:?} to database: {e}");
            }

            // return the buffer to the pool
            buffer_pool.put(result.buffer);
        }
    }

    pub fn push(&mut self, result: SerializeResult) -> Option<Self> {
        self.current_buffer_size += result.buffer.len();

        if result.table == Table::Value {
            self.cache.push(result);
        } else {
            self.metadata.push(result);
        }

        if self.current_buffer_size >= Self::MAX_BUFFER_SIZE {
            let batch = std::mem::take(self);
            Some(batch)
        } else {
            None
        }
    }
}

impl Writer {
    pub(crate) fn new(
        serializer_count: usize,
        database: &Arc<super::Database>,
    ) -> Self {
        let buffer_pool = BufferPool::new();

        let (send_serialize_task, receiver_task) =
            crossbeam::channel::unbounded();

        let batch = Arc::new(Mutex::new(Batch::new()));

        let workers = (0..serializer_count)
            .map(|i| {
                let receiver_task = receiver_task.clone();
                let buffer_pool = buffer_pool.clone();
                let batch = batch.clone();
                let database = database.clone();

                std::thread::Builder::new()
                    .name(format!("serialize-worker-{i}"))
                    .spawn(move || {
                        Self::work_loop(
                            &receiver_task,
                            &buffer_pool,
                            &batch,
                            &database,
                        );
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

    pub(super) fn new_serialize_task<F>(&self, task: F)
    where
        F: FnOnce(Buffer) -> Result<SerializeResult, Buffer> + Send + 'static,
    {
        self.send_serialize_task
            .as_ref()
            .unwrap()
            .send(Box::new(task))
            .unwrap();
    }

    fn work_loop(
        receiver_task: &crossbeam::channel::Receiver<SerializeTask>,
        buffer_pool: &BufferPool,
        batch: &Arc<Mutex<Batch>>,
        database: &Arc<super::Database>,
    ) {
        loop {
            // receive the task to serialize
            let Ok(task) = receiver_task.recv() else {
                break;
            };

            let buffer = buffer_pool.get();
            let result = match task(buffer) {
                Ok(result) => result,
                Err(buffer) => {
                    // if the task failed, return the buffer to the pool
                    buffer_pool.put(buffer);

                    continue;
                }
            };

            let batch_result = {
                let mut batch = batch.lock();
                batch.push(result)
            };

            if let Some(batch) = batch_result {
                // if the batch is full, write it to the database
                batch.write_batch(database, buffer_pool);
            } else {
                // if the batch is not full, just continue
            }
        }

        std::mem::take(&mut *batch.lock()).write_batch(database, buffer_pool);
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
