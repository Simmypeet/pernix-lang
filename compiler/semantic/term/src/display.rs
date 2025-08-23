//! Contains the definition of [`Display`], a custom alternative of the
//! [`std::fmt::Display`].

use pernixc_hash::HashMap;
use pernixc_query::TrackedEngine;

use crate::lifetime::Forall;

/// Stores the internal buffer for formatting.
pub struct Formatter<'x> {
    buffer: &'x mut (dyn std::fmt::Write + Send),
    forall_lifetime_names: HashMap<Forall, usize>,
}

impl Formatter<'_> {
    /// Obtains the forall lifetime number in the current formatting session.
    pub fn forall_lifetime_names(&mut self, forall_lifetime: &Forall) -> usize {
        let len = self.forall_lifetime_names.len();

        *self
            .forall_lifetime_names
            .entry(*forall_lifetime)
            .or_insert_with(|| len)
    }
}

impl std::fmt::Debug for Formatter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Formatter").finish_non_exhaustive()
    }
}

impl std::fmt::Write for Formatter<'_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.buffer.write_str(s)
    }
}

/// A specialized alternative to [`std::fmt::Display`] supporting async and
/// allows access to the [`TrackedEngine`].
pub trait Display: Send + Sync {
    /// Formats the value using the given [`TrackedEngine`] and [`Formatter`].
    fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut Formatter,
    ) -> impl std::future::Future<Output = std::fmt::Result>;

    /// Writes the value asynchronously into the given buffer.
    fn write_async<W: std::fmt::Write + Send>(
        &self,
        engine: &TrackedEngine,
        buffer: &mut W,
    ) -> impl std::future::Future<Output = std::fmt::Result> {
        async move {
            let mut formatter =
                Formatter { buffer, forall_lifetime_names: HashMap::default() };

            self.fmt(engine, &mut formatter).await
        }
    }
}
