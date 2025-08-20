//! Contains the definition of [`Display`], a custom alternative of the
//! [`std::fmt::Display`].

use pernixc_query::TrackedEngine;

/// Stores the internal buffer for formatting.
pub struct Formatter<'x> {
    buffer: &'x mut (dyn std::fmt::Write + Send),
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
#[trait_variant::make(Send)]
pub trait Display {
    /// Formats the value using the given [`TrackedEngine`] and [`Formatter`].
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut Formatter<'_>,
    ) -> std::fmt::Result;
}
