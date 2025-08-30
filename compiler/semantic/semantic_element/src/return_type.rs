//! Contains the query definition for the return type of the function symbol.

use std::sync::Arc;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::r#type::Type;

/// A query for retrieving the return type of the function symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(Arc<Type>)]
pub struct Key(pub Global<pernixc_symbol::ID>);
