//! Defines the linkage of functions and static variables.

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::{get_table_of_symbol, ID};

/// Represents the linkage of a function or static variable.
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
)]
#[allow(missing_docs)]
pub enum Linkage {
    C(C),
    Unknown,
}

/// Represents the C linkage options.
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
)]
pub struct C {
    /// Whether the function is variadic.
    pub variadic: bool,
}

#[pernixc_query::query(
    key(Key),
    id(Global<ID>),
    value(Linkage),
    executor(Executor),
    extend(method(get_linkage), no_cyclic)
)]
pub async fn executor(
    id: Global<ID>,
    engine: &TrackedEngine,
) -> Result<Linkage, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;

    Ok(table.function_linkages.get(&id.id).copied().unwrap_or_else(|| {
        panic!("invalid symbol ID or symbol is not a function: {:?}", id.id)
    }))
}

pernixc_register::register!(Key, Executor);
