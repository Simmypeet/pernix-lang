use std::sync::{
    atomic::{AtomicBool, AtomicUsize, Ordering},
    Arc,
};

use crossbeam::{
    deque::{Injector, Steal, Stealer},
    utils::Backoff,
};
use parking_lot::{Mutex, RwLock};
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

pub struct BatchWriteTransaction {
    pub(super) table: Arc<redb::Database>,

    pub(super) write: RwLock<Option<redb::WriteTransaction>>,
    pub(super) table_lockcs: HashMap<Table, Mutex<()>>,

    written_bytes: AtomicCounter,
}

#[derive(Debug, Default)]
struct AtomicCounter(AtomicUsize);

impl AtomicCounter {
    fn greater_than_and_reset(&self, threshold: usize) -> bool {
        self.0
            .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |current| {
                if current >= threshold {
                    Some(0)
                } else {
                    None
                }
            })
            .is_ok()
    }

    // Forward other methods you need
    fn load(&self, ordering: Ordering) -> usize { self.0.load(ordering) }

    fn store(&self, value: usize, ordering: Ordering) {
        self.0.store(value, ordering);
    }

    fn fetch_add(&self, val: usize, ordering: Ordering) -> usize {
        self.0.fetch_add(val, ordering)
    }
}

impl BatchWriteTransaction {
    const COMMIT_THRESHOLD: usize = 12 * 1024 * 1024; // 5 MiB

    pub fn new(table: Arc<redb::Database>) -> Self {
        let write =
            table.begin_write().expect("Failed to begin write transaction");

        Self {
            table,
            write: RwLock::new(Some(write)),
            table_lockcs: [
                (Table::Value, Mutex::new(())),
                (Table::Metadata, Mutex::new(())),
            ]
            .into_iter()
            .collect(),
            written_bytes: AtomicCounter::default(),
        }
    }

    pub fn open_table(
        &self,
        table: Table,
        invoke: impl FnOnce(&mut redb::Table<(u128, u128), &'static [u8]>) -> usize,
    ) {
        let written_bytes =
            self.written_bytes.load(std::sync::atomic::Ordering::Relaxed);

        if self.written_bytes.greater_than_and_reset(Self::COMMIT_THRESHOLD) {
            // commit the current write transaction if the threshold is reached
            let mut write = self.write.write();

            if let Some(write) = write.take() {
                let _span = tracing::info_span!(
                    "commit_write_transaction",
                    written_bytes
                )
                .entered();

                if let Err(e) = write.commit() {
                    tracing::error!("Failed to commit write transaction: {e}");
                }
            }

            *write = Some(
                self.table
                    .begin_write()
                    .expect("Failed to begin write transaction"),
            );

            self.written_bytes.store(0, std::sync::atomic::Ordering::Relaxed);
        }

        let lock = self.table_lockcs.get(&table).unwrap();
        let _guard = lock.lock();

        let write = self.write.read();
        let mut table = write
            .as_ref()
            .expect("Write transaction is not initialized")
            .open_table(table.definition())
            .expect("Failed to open table for writing");

        let written_bytes = invoke(&mut table);
        self.written_bytes
            .fetch_add(written_bytes, std::sync::atomic::Ordering::Relaxed);
    }
}

impl Drop for BatchWriteTransaction {
    fn drop(&mut self) {
        let mut write = self.write.write();

        if let Some(write) = write.take() {
            if let Err(e) = write.commit() {
                tracing::error!("Failed to commit write transaction: {e}");
            }
        }
    }
}

pub struct Worker {
    injector: Arc<Injector<SaveTask>>,
    shutdown: Arc<AtomicBool>,

    thread_handles: Vec<std::thread::JoinHandle<()>>,
}

impl Worker {
    pub fn new(worker_count: usize, database: &Arc<redb::Database>) -> Self {
        let injector = Arc::new(Injector::new());
        let shutdown = Arc::new(AtomicBool::new(false));

        let mut thread_handles = Vec::with_capacity(worker_count);

        let workers = (0..worker_count)
            .map(|_| crossbeam::deque::Worker::new_fifo())
            .collect::<Vec<_>>();
        let stealers = workers
            .iter()
            .map(crossbeam::deque::Worker::stealer)
            .collect::<Arc<[_]>>();

        let batch_transaction =
            Arc::new(BatchWriteTransaction::new(database.clone()));

        for (i, worker) in workers.into_iter().enumerate() {
            let injector = injector.clone();
            let stealers = stealers.clone();
            let shutdown = shutdown.clone();
            let batch_transaction = batch_transaction.clone();

            let handle = std::thread::Builder::new()
                .name(format!("persistence-worker-{i}"))
                .spawn(move || {
                    Self::worker_loop(
                        &worker,
                        &injector,
                        &stealers,
                        &shutdown,
                        &batch_transaction,
                    );
                })
                .expect("Failed to spawn stealing worker thread");

            thread_handles.push(handle);
        }

        Self { injector, shutdown, thread_handles }
    }

    pub fn new_save_task(&self, task: SaveTask) {
        // Push the task to the injector, which will be processed by one of the
        // worker threads.
        self.injector.push(task);

        for thread in &self.thread_handles {
            // Wake up the worker thread to process the task.
            thread.thread().unpark();
        }
    }

    fn worker_loop(
        local: &crossbeam::deque::Worker<SaveTask>,
        global: &Injector<SaveTask>,
        stealers: &[Stealer<SaveTask>],
        shutdown: &AtomicBool,
        batch_transaction: &BatchWriteTransaction,
    ) {
        let mut buffers = HashMap::<Table, frame::Buffer>::default();
        let backoff = Backoff::new();

        loop {
            // Pop a task from the local queue, if not empty.
            let task = local.pop().or_else(|| {
                // Otherwise, we need to look for a task elsewhere.
                std::iter::repeat_with(|| {
                    // Try stealing a batch of tasks from the global queue.
                    global
                        .steal_batch_and_pop(local)
                        // Or try stealing a task from one of the other threads.
                        .or_else(|| {
                            stealers.iter().map(Stealer::steal).collect()
                        })
                })
                // Loop while no task was stolen and any steal operation needs
                // to be retried.
                .find(|s| !s.is_retry())
                // Extract the stolen task, if there is one.
                .and_then(Steal::success)
            });

            if let Some(task) = task {
                backoff.reset(); // Reset backoff on successful task retrieval

                let buffer = buffers.entry(task.table).or_default();

                // Write the task to the frame buffer for batching.
                Self::process_task(task, batch_transaction, buffer);
            } else {
                if shutdown.load(std::sync::atomic::Ordering::Relaxed) {
                    break; // Exit the loop if shutdown is requested.
                }

                backoff.snooze();
            }
        }

        // Flush any remaining tasks in the buffers before exiting.
        for (&key, buffer) in &mut buffers {
            batch_transaction.open_table(key, |table| buffer.flush(table));
        }
    }

    const FLUSH_BATCH_SIZE: usize = 1024 * 1024;

    fn process_task(
        task: SaveTask,
        batch_transaction: &BatchWriteTransaction,
        buffer_frame: &mut frame::Buffer,
    ) {
        buffer_frame.write(task.key, |buffer| (task.write)(buffer));

        if buffer_frame.byte_size() >= Self::FLUSH_BATCH_SIZE {
            batch_transaction
                .open_table(task.table, |table| buffer_frame.flush(table));
        }
    }
}

impl Drop for Worker {
    fn drop(&mut self) {
        // Signal shutdown to all worker threads.
        self.shutdown.store(true, std::sync::atomic::Ordering::Relaxed);

        // Unpark all worker threads to ensure they exit.
        for thread in &self.thread_handles {
            thread.thread().unpark();
        }

        // Wait for all worker threads to finish.
        for thread in self.thread_handles.drain(..) {
            thread.join().expect("Failed to join stealing worker thread");
        }
    }
}
