//! Contains the definition of the [`Span`] query.

use derive_more::{Deref, DerefMut};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::ID;

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

/// An executor for the [`Span`] query that retrieves the span of the symbol
/// with the given ID.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        &Key(id): &Key,
    ) -> Result<
        Option<RelativeSpan>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        let table = engine
            .query(&crate::Key(id.target_id))
            .expect("should have no cyclic dependencies");

        Ok(table.entries_by_id.get(&id.id).expect("invalid symbol ID").span)
    }
}
