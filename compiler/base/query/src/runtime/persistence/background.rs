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
    write_worker: Option<std::thread::JoinHandle<()>>,

    send_serialize_task: Option<crossbeam::channel::Sender<SerializeTask>>,
}

impl Writer {
    pub(crate) fn new(
        serializer_count: usize,
        database: Arc<super::Database>,
    ) -> Self {
        let buffer_pool = BufferPool::new();

        let (send_serialize_task, receiver_task) =
            crossbeam::channel::unbounded();
        let (send_serialize_result, receiver_serialize_result) =
            crossbeam::channel::unbounded();

        let workers = (0..serializer_count)
            .map(|i| {
                let receiver_task = receiver_task.clone();
                let buffer_pool = buffer_pool.clone();
                let send_serialize_result = send_serialize_result.clone();

                std::thread::Builder::new()
                    .name(format!("serialize-worker-{i}"))
                    .spawn(move || {
                        Self::work_loop(
                            &receiver_task,
                            &send_serialize_result,
                            &buffer_pool,
                        );
                    })
                    .unwrap()
            })
            .collect::<Vec<_>>();

        Self {
            write_worker: {
                Some(
                    std::thread::Builder::new()
                        .name("write-worker".to_string())
                        .spawn(move || {
                            Self::write_worker_loop(
                                &receiver_serialize_result,
                                &buffer_pool,
                                &database,
                            );
                        })
                        .unwrap(),
                )
            },
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
        send_serialize_result: &crossbeam::channel::Sender<SerializeResult>,
        buffer_pool: &BufferPool,
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

            send_serialize_result
                .send(result)
                .expect("Failed to send serialize result");
        }
    }

    fn write_worker_loop(
        receiver_task: &crossbeam::channel::Receiver<SerializeResult>,
        buffer_pool: &BufferPool,
        database: &super::Database,
    ) {
        loop {
            let Ok(result) = receiver_task.recv() else {
                return;
            };

            let write = match database.write_transaction() {
                Ok(write) => write,
                Err(e) => {
                    tracing::error!(
                        "Failed to get write transaction for {result:?}: {e}"
                    );
                    buffer_pool.put(result.buffer);

                    continue;
                }
            };

            let key = (result.stable_type_id.as_u128(), result.fingerprint);
            let ok: Result<(), redb::Error> = match result.table {
                Table::Value => write.with_cache_table(|x| {
                    x.write().insert(key, result.buffer.as_slice())?;
                    Ok(())
                }),
                Table::Metadata => write.with_metadata_table(|x| {
                    x.write().insert(key, result.buffer.as_slice())?;
                    Ok(())
                }),
            };

            if let Err(e) = ok {
                tracing::error!("Failed to write {result:?} to database: {e}");
            }

            buffer_pool.put(result.buffer);
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

        self.write_worker.take().unwrap().join().unwrap();
        for worker in self.serialize_workers.drain(..) {
            worker.join().unwrap();
        }
    }
}
