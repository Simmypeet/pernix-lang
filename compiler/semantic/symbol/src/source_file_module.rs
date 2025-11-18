//! Defines the query key for retrieving the module ID that a source file
//! corresponds to.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::GlobalSourceID;
use pernixc_stable_hash::StableHash;

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
    Serialize,
    Deserialize,
    pernixc_query::Key,
    StableHash,
)]
#[extend(method(get_source_file_module), no_cyclic)]
#[value(ID)]
pub struct Key(pub GlobalSourceID);
