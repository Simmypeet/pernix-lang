//! Contains the definition of the [`JoinList`] type and the [`scoped!`] macro.
use std::collections::VecDeque;

use tokio::task::JoinHandle;

/// A collection for managing multiple `JoinHandle`s.
///
/// This collection works together with the [`scoped!`] macro to allow guarding
/// the lifetime of `JoinHandle`s within a specific scope. It ensures that all
/// tasks are awaited before the scope exits, preventing any detached tasks
#[derive(Debug)]
pub struct JoinList<T> {
    tasks: VecDeque<JoinHandle<T>>,
}

impl<T> Default for JoinList<T> {
    fn default() -> Self { Self { tasks: VecDeque::new() } }
}

impl<T> JoinList<T> {
    /// Spawns a new task and adds its `JoinHandle` to the list.
    pub fn spawn<F: std::future::Future<Output = T> + Send + 'static>(
        &mut self,
        future: F,
    ) where
        T: Send + 'static,
    {
        let task = tokio::spawn(future);
        self.tasks.push_back(task);
    }

    /// Awaits all tasks in the list, ensuring they complete before proceeding.
    pub async fn ensure_join_all(&mut self) {
        for task in self.tasks.drain(..) {
            task.await.expect("Task panicked");
        }
    }

    /// Pops the next task from the list and awaits it, returning its result.
    pub async fn next(&mut self) -> Option<T> {
        if let Some(task) = self.tasks.pop_front() {
            Some(task.await.unwrap())
        } else {
            None // No more tasks to await
        }
    }

    /// Adds an already spawned task to the list.
    pub fn push(&mut self, task: JoinHandle<T>) { self.tasks.push_front(task); }
}

/// A macro for spawning tasks through the `JoinList` and ensuring they are
/// awaited before the scope exits.
#[macro_export]
macro_rules! scoped {
    (|$($join_list:ident),*| async $($move:ident)? $block:block) => {
        {
            $(
                let mut $join_list = $crate::join_list::JoinList::default();
            )*

            let result = {
                $(
                    let $join_list = &mut $join_list;
                )*

                async $($move)? $block .await
            };

            $(
                $join_list.ensure_join_all().await;
            )*

            result
        }
    };
}

pub use scoped;
