//! Contains the definition of the [`Span`] query.

use derive_more::{Deref, DerefMut};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::{get_table_of_symbol, ID};

/// Points to the particular location span in the source code where the given
/// symbol is defined, typically used for diagnostics and error reporting.
///
/// It usually points to the identifier of the symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
    pernixc_query::Key,
    StableHash,
)]
#[extend(method(get_span), no_cyclic)]
#[value(Option<RelativeSpan>)]
pub struct Key(pub Global<ID>);

#[pernixc_query::executor(key(Key), name(Executor))]
#[allow(clippy::unnecessary_wraps)]
pub fn executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Result<Option<RelativeSpan>, CyclicError> {
    let table = engine.get_table_of_symbol(key.0);

    Ok(table
        .spans
        .get(&key.0.id)
        .copied()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", key.0.id)))
}
