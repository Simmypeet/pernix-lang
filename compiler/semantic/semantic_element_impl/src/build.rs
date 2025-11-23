use std::{fmt::Debug, hash::Hash, marker::PhantomData, sync::Arc};

use pernixc_query::runtime::executor;
#[allow(unused)]
pub use pernixc_register;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;

use crate::occurrences::Occurrences;

macro_rules! impl_key {
    ($k:ident, $key:ident, $value:ty) => {
        #[derive(Identifiable, StableHash, Serialize, Deserialize)]
        pub struct $key<$k> {
            pub id: $k,
            _marker: PhantomData<$k>,
        }

        impl<$k> $key<$k> {
            /// Creates a new key for the given ID.
            #[must_use]
            #[allow(unused)]
            pub const fn new(id: $k) -> Self {
                Self { id, _marker: PhantomData }
            }
        }

        impl<$k: Debug> Debug for $key<$k> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct("$key")
                    .field("id", &self.id)
                    .finish_non_exhaustive()
            }
        }

        impl<$k: Clone> Clone for $key<$k> {
            fn clone(&self) -> Self {
                Self { id: self.id.clone(), _marker: PhantomData }
            }
        }

        impl<$k: PartialEq> PartialEq for $key<$k> {
            fn eq(&self, other: &Self) -> bool { self.id == other.id }
        }

        impl<$k: Eq> Eq for $key<$k> {}

        impl<$k: Ord> Ord for $key<$k> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.id.cmp(&other.id)
            }
        }

        impl<$k: PartialOrd> PartialOrd for $key<$k> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                self.id.partial_cmp(&other.id)
            }
        }

        impl<$k: Hash> Hash for $key<$k> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.id.hash(state);
            }
        }

        impl<$k: $crate::build::Build> pernixc_query::Key for $key<$k> {
            type Value = $value;
        }
    };
}

impl_key!(T, Key, Output<T>);
impl_key!(T, DiagnosticKey, Arc<[T::Diagnostic]>);
impl_key!(T, OccurrencesKey, Arc<Occurrences>);

#[derive(Debug, Default)]
pub struct BuildExecutor;

impl<T: Build> executor::Executor<Key<T>> for BuildExecutor {
    async fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &Key<T>,
    ) -> Result<Output<T>, executor::CyclicError> {
        T::execute(engine, &key.id).await
    }
}

#[derive(Debug, Default)]
pub struct DiagnosticExecutor;

impl<T: Build> executor::Executor<DiagnosticKey<T>> for DiagnosticExecutor {
    async fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &DiagnosticKey<T>,
    ) -> Result<Arc<[T::Diagnostic]>, executor::CyclicError> {
        let build_result = engine.query(&Key::<T>::new(key.id.clone())).await?;

        Ok(build_result.diagnostics)
    }
}

#[derive(Debug, Default)]
pub struct OccurrencesExecutor;

impl<T: Build> executor::Executor<OccurrencesKey<T>> for OccurrencesExecutor {
    async fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &OccurrencesKey<T>,
    ) -> Result<Arc<Occurrences>, executor::CyclicError> {
        let build_result = engine.query(&Key::<T>::new(key.id.clone())).await?;

        Ok(build_result.occurrences)
    }
}

#[derive(Debug, Default)]
pub struct ElementExtractExecutor;

impl<T: Build> executor::Executor<T> for ElementExtractExecutor {
    async fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &T,
    ) -> Result<T::Value, executor::CyclicError> {
        let build_result = engine.query(&Key::<T>::new(key.clone())).await?;

        Ok(build_result.item)
    }
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
#[serde(
    ser_bound(T::Diagnostic: Serialize<__S, __E>, T::Value: Serialize<__S, __E>),
    de_bound(T::Diagnostic: Deserialize<__D, __E>, T::Value: Deserialize<__D, __E>),
)]
pub struct Output<T: Build> {
    /// The main result of the query.
    pub item: T::Value,

    /// The diagnostics produced while building the query.
    pub diagnostics: Arc<[T::Diagnostic]>,

    /// The occurrences produced while building the query.
    ///
    /// This will be later used for well-formedness checking.
    pub occurrences: Arc<Occurrences>,
}

impl<T: Build> Clone for Output<T> {
    fn clone(&self) -> Self {
        Self {
            item: self.item.clone(),
            diagnostics: self.diagnostics.clone(),
            occurrences: self.occurrences.clone(),
        }
    }
}

pub trait Build: pernixc_query::Key {
    type Diagnostic: Debug + StableHash + Send + Sync + 'static;

    fn execute<'x, 'y>(
        engine: &'x pernixc_query::TrackedEngine,
        key: &'y Self,
    ) -> impl std::future::Future<
        Output = Result<Output<Self>, executor::CyclicError>,
    > + use<'x, 'y, Self>
    + Send;
}

/// Registers the necessary executors for the given type.
#[macro_export]
macro_rules! register_build {
    ($ty:ty) => {
        $crate::build::pernixc_register::register!(
            $crate::build::Key<$ty>,
            $crate::build::BuildExecutor
        );

        $crate::build::pernixc_register::register!(
            $crate::build::DiagnosticKey<$ty>,
            $crate::build::DiagnosticExecutor
        );

        $crate::build::pernixc_register::register!(
            $crate::build::OccurrencesKey<$ty>,
            $crate::build::OccurrencesExecutor
        );

        $crate::build::pernixc_register::register!(
            $ty,
            $crate::build::ElementExtractExecutor
        );
    };
}

pub use register_build;
