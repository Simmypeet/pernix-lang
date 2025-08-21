use std::{
    collections::VecDeque,
    sync::{atomic::AtomicBool, Arc},
};

use crossbeam::{
    deque::{Injector, Steal, Stealer},
    utils::Backoff,
};
use parking_lot::RwLock;
use redb::{TableDefinition, WriteTransaction};

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

pub struct Committer {
    buffer_pool: RwLock<VecDeque<Vec<u8>>>,
    database: Arc<redb::Database>,
}

pub struct CommitTask {
    buffer: Vec<u8>,
    table: Table,
    key: (u128, u128),
}

impl Committer {
    pub fn get_serialize_buffer(&self) -> Vec<u8> {
        let buffer = self.buffer_pool.write().pop_front();

        buffer.map_or_else(Vec::default, |mut buffer| {
            buffer.clear();
            buffer
        })
    }

    pub fn return_buffer(&self, buffer: Vec<u8>) {
        self.buffer_pool.write().push_back(buffer);
    }
}

pub struct Worker {
    injector: Arc<Injector<SaveTask>>,
    shutdown: Arc<AtomicBool>,

    serialize_handles: Vec<std::thread::JoinHandle<()>>,
    commit_handle: Option<std::thread::JoinHandle<()>>,
}

impl Worker {
    pub fn new(worker_count: usize, database: &Arc<redb::Database>) -> Self {
        let injector = Arc::new(Injector::new());
        let shutdown = Arc::new(AtomicBool::new(false));
        let committer = Arc::new(Committer {
            buffer_pool: RwLock::new(VecDeque::default()),
            database: database.clone(),
        });
        let (send_commit, recv_commit) = std::sync::mpsc::channel();

        let mut thread_handles = Vec::with_capacity(worker_count);

        let workers = (0..worker_count)
            .map(|_| crossbeam::deque::Worker::new_fifo())
            .collect::<Vec<_>>();
        let stealers = workers
            .iter()
            .map(crossbeam::deque::Worker::stealer)
            .collect::<Arc<[_]>>();

        for (i, worker) in workers.into_iter().enumerate() {
            let injector = injector.clone();
            let stealers = stealers.clone();
            let shutdown = shutdown.clone();
            let send_commit = send_commit.clone();
            let committer = committer.clone();

            let handle = std::thread::Builder::new()
                .name(format!("serialize-worker-{i}"))
                .spawn(move || {
                    Self::serialize_worker_loop(
                        &worker,
                        &injector,
                        &stealers,
                        &shutdown,
                        &committer,
                        &send_commit,
                    );
                })
                .expect("Failed to spawn stealing worker thread");

            thread_handles.push(handle);
        }

        let commit_handle = std::thread::Builder::new()
            .name("commit_worker".to_string())
            .spawn(move || {
                Self::commit_worker_loop(&committer, &recv_commit);
            })
            .expect("Failed to spawn committer thread");

        Self {
            injector,
            shutdown,
            serialize_handles: thread_handles,
            commit_handle: Some(commit_handle),
        }
    }

    pub fn new_save_task(&self, task: SaveTask) {
        // Push the task to the injector, which will be processed by one of the
        // worker threads.
        self.injector.push(task);

        for thread in &self.serialize_handles {
            // Wake up the worker thread to process the task.
            thread.thread().unpark();
        }
    }

    fn insert_task(
        committer: &Committer,
        write_transaction: &mut WriteTransaction,
        task: CommitTask,
    ) {
        let mut table = write_transaction
            .open_table(task.table.definition())
            .expect("Failed to open table");

        table
            .insert(task.key, task.buffer.as_slice())
            .expect("Failed to insert into table");

        committer.return_buffer(task.buffer);
    }

    fn commit_worker_loop(
        committer: &Committer,
        recv: &std::sync::mpsc::Receiver<CommitTask>,
    ) {
        loop {
            // recieve the task blocking, wait for the next task if none.
            let first_task = match recv.recv() {
                Ok(task) => task,
                Err(std::sync::mpsc::RecvError) => return, /* Exit if the channel is disconnected. */
            };

            let mut write_transaction = committer
                .database
                .begin_write()
                .expect("Failed to begin write transaction");

            Self::insert_task(committer, &mut write_transaction, first_task);

            // try to pull as much task as possible to commit in this batch
            // without blocking

            while let Ok(more_task) = recv.try_recv() {
                Self::insert_task(committer, &mut write_transaction, more_task);
            }

            write_transaction
                .commit()
                .expect("Failed to commit write transaction");
        }
    }

    fn serialize_worker_loop(
        local: &crossbeam::deque::Worker<SaveTask>,
        global: &Injector<SaveTask>,
        stealers: &[Stealer<SaveTask>],
        shutdown: &AtomicBool,
        committer: &Committer,
        send_commit: &std::sync::mpsc::Sender<CommitTask>,
    ) {
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

                let buffer = committer.get_serialize_buffer();

                // Write the task to the frame buffer for batching.
                Self::process_task(task, committer, buffer, send_commit);
            } else {
                if shutdown.load(std::sync::atomic::Ordering::Relaxed) {
                    break; // Exit the loop if shutdown is requested.
                }

                if backoff.is_completed() {
                    // If no tasks were found, park the thread to avoid
                    // busy-waiting.
                    std::thread::park();
                } else {
                    backoff.snooze();
                }
            }
        }
    }

    fn process_task(
        task: SaveTask,
        committer: &Committer,
        mut buffer_frame: Vec<u8>,
        send_commit: &std::sync::mpsc::Sender<CommitTask>,
    ) {
        if (task.write)(&mut buffer_frame) {
            send_commit
                .send(CommitTask {
                    buffer: buffer_frame,
                    table: task.table,
                    key: task.key,
                })
                .expect("Failed to send commit task");
        } else {
            committer.return_buffer(buffer_frame);
        }
    }
}

impl Drop for Worker {
    fn drop(&mut self) {
        // Signal shutdown to all worker threads.
        self.shutdown.store(true, std::sync::atomic::Ordering::Relaxed);

        // Unpark all worker threads to ensure they exit.
        for thread in &self.serialize_handles {
            thread.thread().unpark();
        }

        // Wait for all worker threads to finish.
        for thread in self.serialize_handles.drain(..) {
            thread.join().expect("Failed to join stealing worker thread");
        }

        self.commit_handle
            .take()
            .expect("should've a commiter")
            .join()
            .expect("Failed to join committer thread");
    }
}
