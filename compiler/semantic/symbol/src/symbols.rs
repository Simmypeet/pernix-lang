//! Contains the query for retrieving all symbol IDs defined in a target.

use std::sync::Arc;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::TargetID;

use crate::ID;

/// Query for retrieving all symbol IDs defined in a target.
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
#[value(Arc<[ID]>)]
#[extend(method(get_all_symbol_ids), no_cyclic)]
pub struct Key(pub TargetID);

/// An executor for the [`Key`] query that retrieves all symbol IDs defined in
/// the target.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        &Key(target_id): &Key,
    ) -> Result<Arc<[ID]>, pernixc_query::runtime::executor::CyclicError> {
        let table = engine
            .query(&crate::Key(target_id))
            .expect("should have no cyclic dependencies");

        Ok(Arc::from(table.entries_by_id.keys().copied().collect::<Vec<_>>()))
    }
}
