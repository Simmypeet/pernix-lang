use std::{fmt::Debug, hash::Hash, marker::PhantomData, sync::Arc};

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;
use pernixc_target::Global;

use crate::occurrences::Occurrences;

#[derive(Identifiable, StableHash, Serialize, Deserialize)]
pub struct Key<T, D> {
    pub id: Global<pernixc_symbol::ID>,
    _marker: PhantomData<(T, D)>,
}

impl<T, D> Key<T, D> {
    /// Creates a new key for the given ID.
    #[must_use]
    pub const fn new(id: Global<pernixc_symbol::ID>) -> Self {
        Self { id, _marker: PhantomData }
    }

    /// Returns the ID of this key.
    #[must_use]
    pub const fn id(&self) -> Global<pernixc_symbol::ID> { self.id }
}

impl<T, D> Debug for Key<T, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Key").field("id", &self.id).finish_non_exhaustive()
    }
}

impl<T, D> Clone for Key<T, D> {
    fn clone(&self) -> Self { Self { id: self.id, _marker: PhantomData } }
}

impl<T, D> PartialEq for Key<T, D> {
    fn eq(&self, other: &Self) -> bool { self.id == other.id }
}

impl<T, D> Eq for Key<T, D> {}

impl<T, D> Ord for Key<T, D> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.id.cmp(&other.id) }
}

impl<T, D> PartialOrd for Key<T, D> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T, D> Hash for Key<T, D> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.id.hash(state); }
}

impl<
        T: Debug + Identifiable + StableHash + Hash + Send + Sync + 'static,
        D: Debug + Identifiable + StableHash + Hash + Send + Sync + 'static,
    > pernixc_query::Key for Key<T, D>
{
    type Value = Build<T, D>;
}

/// A helper structs that groups the side-effects of building a query.
#[derive(
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Build<T, D> {
    /// The main result of the query.
    pub item: Arc<T>,

    /// The diagnostics produced while building the query.
    pub diagnostics: Arc<[D]>,

    /// The occurrences produced while building the query.
    ///
    /// This will be later used for well-formedness checking.
    pub occurrences: Arc<Occurrences>,
}

impl<T, D> Clone for Build<T, D> {
    fn clone(&self) -> Self {
        Self {
            item: self.item.clone(),
            diagnostics: self.diagnostics.clone(),
            occurrences: self.occurrences.clone(),
        }
    }
}
