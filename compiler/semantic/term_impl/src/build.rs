use std::{fmt::Debug, hash::Hash, marker::PhantomData, sync::Arc};

use pernixc_query::runtime::executor;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;
use pernixc_target::Global;

use crate::occurrences::Occurrences;

macro_rules! impl_key {
    ($t:ident, $d:ident, $key:ident, $value:ty) => {
        #[derive(Identifiable, StableHash, Serialize, Deserialize)]
        pub struct $key<$t, $d> {
            pub id: Global<pernixc_symbol::ID>,
            _marker: PhantomData<($t, $d)>,
        }

        impl<$t, D> $key<$t, $d> {
            /// Creates a new key for the given ID.
            #[must_use]
            #[allow(unused)]
            pub const fn new(id: Global<pernixc_symbol::ID>) -> Self {
                Self { id, _marker: PhantomData }
            }
        }

        impl<$t, $d> Debug for $key<$t, $d> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct("$key")
                    .field("id", &self.id)
                    .finish_non_exhaustive()
            }
        }

        impl<$t, $d> Clone for $key<$t, $d> {
            fn clone(&self) -> Self {
                Self { id: self.id, _marker: PhantomData }
            }
        }

        impl<$t, $d> PartialEq for $key<$t, $d> {
            fn eq(&self, other: &Self) -> bool { self.id == other.id }
        }

        impl<$t, $d> Eq for $key<$t, $d> {}

        impl<$t, $d> Ord for $key<$t, $d> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.id.cmp(&other.id)
            }
        }

        impl<$t, $d> PartialOrd for $key<$t, $d> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl<$t, $d> Hash for $key<$t, $d> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.id.hash(state);
            }
        }

        impl<
                $t: Debug
                    + Identifiable
                    + Clone
                    + StableHash
                    + Send
                    + Sync
                    + 'static,
                $d: Debug + Identifiable + StableHash + Send + Sync + 'static,
            > pernixc_query::Key for $key<$t, $d>
        {
            type Value = $value;
        }
    };
}

impl_key!(T, D, Key, Build<T, D>);
impl_key!(T, D, DiagnosticKey, Arc<[D]>);
impl_key!(T, D, OccurrencesKey, Arc<Occurrences>);

#[derive(Debug)]
pub struct DiagnosticExecutor;

#[derive(Debug)]
pub struct OccurrencesExecutor;

impl<
        T: Debug + Identifiable + Clone + StableHash + Send + Sync + 'static,
        D: Debug + Identifiable + StableHash + Send + Sync + 'static,
    > executor::Executor<DiagnosticKey<T, D>> for DiagnosticExecutor
{
    async fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &DiagnosticKey<T, D>,
    ) -> Result<Arc<[D]>, executor::CyclicError> {
        Ok(engine.query(&Key::<T, D>::new(key.id)).await?.diagnostics)
    }
}

impl<
        T: Debug + Identifiable + Clone + StableHash + Send + Sync + 'static,
        D: Debug + Identifiable + StableHash + Send + Sync + 'static,
    > executor::Executor<OccurrencesKey<T, D>> for OccurrencesExecutor
{
    async fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &OccurrencesKey<T, D>,
    ) -> Result<Arc<Occurrences>, executor::CyclicError> {
        Ok(engine.query(&Key::<T, D>::new(key.id)).await?.occurrences)
    }
}

macro_rules! impl_term_extract_executor {
    ($path:path, $value_ty:ty, $diag_ty:ty) => {
        #[pernixc_query::executor(key($path), name(Executor))]
        pub async fn get_term_executor(
            id: &$path,
            engine: &TrackedEngine,
        ) -> Result<$value_ty, pernixc_query::runtime::executor::CyclicError> {
            Ok(engine
                .query(&$crate::build::Key::<$value_ty, $diag_ty>::new(id.0))
                .await?
                .item)
        }
    };
}

pub(crate) use impl_term_extract_executor;

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
    pub item: T,

    /// The diagnostics produced while building the query.
    pub diagnostics: Arc<[D]>,

    /// The occurrences produced while building the query.
    ///
    /// This will be later used for well-formedness checking.
    pub occurrences: Arc<Occurrences>,
}

impl<T: Clone, D> Clone for Build<T, D> {
    fn clone(&self) -> Self {
        Self {
            item: self.item.clone(),
            diagnostics: self.diagnostics.clone(),
            occurrences: self.occurrences.clone(),
        }
    }
}
