//! Contains the definition of [`Cache`] struct

/// The memoize table for the parser allowing fast incremental parsing and
/// fast alternate parsing of the same source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Cache {}
