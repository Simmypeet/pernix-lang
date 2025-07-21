//! Contains the definition of [`Member`] type.

use std::sync::Arc;

use flexstr::SharedStr;
use pernixc_extend::extend;
use pernixc_hash::{HashMap, HashSet};
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::{get_table_of_symbol, ID};

/// Stores the members of a symbol in a form of `::Member`
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    Value,
    StableHash,
)]
#[id(Global<ID>)]
#[value(Arc<Member>)]
#[extend(method(get_members), no_cyclic)]
pub struct Member {
    /// A map from the member name to its ID.
    ///
    /// In case of the redefinition, the firs encounter is recorded in this
    /// map. The redefinition is recorded in the [`Self::redefinitions`] field.
    pub member_ids_by_name: HashMap<SharedStr, ID>,

    /// A set of redefinitions of the members of this symbol.
    pub redefinitions: HashSet<ID>,
}

#[pernixc_query::executor(key(Key), name(Executor))]
#[allow(clippy::unnecessary_wraps)]
pub fn executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Result<Arc<Member>, CyclicError> {
    let table = engine.get_table_of_symbol(key.0);

    Ok(table
        .members
        .get(&key.0.id)
        .cloned()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", key.0.id)))
}

/*
/// An executor for the [`Key`] query that retrieves the members of the symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        &Key(id): &Key,
    ) -> Result<Arc<Member>, pernixc_query::runtime::executor::CyclicError>
    {
        let table = engine
            .query(&crate::Key(id.target_id))
            .expect("should have no cyclic dependencies");

        Ok(table
            .entries_by_id
            .get(&id.id)
            .expect("invalid symbol ID")
            .members
            .clone()
            .expect(
                "this symbol doesn't have members, use `try_get_members` for \
                 failable retrieval",
            ))
    }
}

*/
