//! Contains the utility for chunking slices for Tokio tasks.

use std::slice::Chunks;

use pernixc_extend::extend;

/// Chunks a slice into manageable pieces for Tokio tasks.
#[extend]
pub fn chunk_for_tasks<'a, T: 'a>(self: &'a [T]) -> Chunks<'a, T> {
    // chunk the items to avoid spawning too many tasks
    // at once, targeting 4x the number of available
    let num_threads = std::thread::available_parallelism()
        .map(std::num::NonZero::get)
        .unwrap_or(1)
        * 4;

    let mut chunk_size = self.len().div_ceil(num_threads);
    if chunk_size == 0 {
        chunk_size = 1;
    }

    self.chunks(chunk_size)
}
