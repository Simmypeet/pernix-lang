//! Contains the definition of the [`JoinList`] type and the [`scoped!`] macro.

use crate::panic_propagate::PanicPropagate;

/// A collection for managing multiple `JoinHandle`s.
#[derive(Debug)]
pub struct JoinSet<T> {
    inner: tokio::task::JoinSet<T>,
}

impl<T> Default for JoinSet<T> {
    fn default() -> Self { Self { inner: tokio::task::JoinSet::new() } }
}

impl<T: 'static> JoinSet<T> {
    /// Creates a new [`JoinSet`].
    #[must_use]
    pub fn new() -> Self { Self::default() }

    /// Spawns a new task and adds its `JoinHandle` to the list.
    pub fn spawn<F: std::future::Future<Output = T> + Send + 'static>(
        &mut self,
        future: F,
    ) where
        T: Send + 'static,
    {
        self.inner.spawn(future);
    }

    /// Awaits all tasks in the list, ensuring they complete before proceeding.
    pub async fn ensure_join_all(&mut self) {
        while let Some(task) = self.inner.join_next().await {
            let _ = task.panic_propagate();
        }
    }

    /// Pops the next task from the list and awaits it, returning its result.
    pub async fn next(&mut self) -> Option<T> {
        let inner = self.inner.join_next().await?;
        let inner = inner.unwrap();

        Some(inner)
    }
}
