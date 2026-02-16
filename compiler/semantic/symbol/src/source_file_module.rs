//! Defines the query key for retrieving the module ID that a source file
//! corresponds to.

use pernixc_source_file::GlobalSourceID;
use qbice::{Decode, Encode, Query, StableHash};

use crate::ID;

/// Retrieves the module ID that the source file corresponds to.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    Query,
    StableHash,
)]
#[extend(name = get_source_file_module, by_val)]
#[value(ID)]
pub struct Key {
    /// The global source file ID to get the module ID for.
    pub source_file_id: GlobalSourceID,
}
