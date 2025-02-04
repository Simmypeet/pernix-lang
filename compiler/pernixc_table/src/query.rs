//! Contains the logic for building the symbol.

use std::{
    any::{Any, TypeId},
    collections::{hash_map::Entry, HashMap},
    fmt::{self, Debug},
    sync::Arc,
    thread::ThreadId,
};

use parking_lot::Condvar;
use pernixc_handler::Handler;

use super::{GlobalID, Table};
use crate::{component::Derived, diagnostic::Diagnostic};

pub mod diagnostic;

/// A record to the [`Table::query`] call. This is primarily used to
/// detect cyclic dependencies.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Record {
    /// The global ID of the component.
    pub global_id: GlobalID,

    /// The type ID of the component.
    pub type_id: TypeId,

    /// The name of the component (used for error messages and debugging).
    pub name: &'static str,
}

/// Shorthand for `And + Send + Sync`.
pub trait AnySendSync: Any + Send + Sync {
    /// Returns the reference to the [`Any`] trait object.
    fn as_any(&self) -> &dyn Any;
}

impl<T: Any + Send + Sync> AnySendSync for T {
    fn as_any(&self) -> &dyn Any { self }
}

type BuilderFn = fn(
    builder: &dyn AnySendSync,
    global_id: GlobalID,
    table: &Table,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Option<Arc<dyn Any + Send + Sync>>;

/// A struct that manages/synchronizes the query.
#[derive(Clone, Default)]
pub struct Context {
    record_stacks_by_thread_id: HashMap<ThreadId, Vec<Record>>,
    condvars_by_record: HashMap<Record, Arc<Condvar>>,
    builder_fns_by_type_id: HashMap<TypeId, BuilderFn>,
    builders_by_type_id: HashMap<TypeId, Arc<dyn AnySendSync>>,

    // key ---requires---> value
    dependencies_by_dependent: HashMap<Record, Record>,
}

impl Context {
    /// Sets the builder for the given derived component type.
    ///
    /// # Returns
    ///
    /// Returns `true` if there's no existing builder for the given type.
    /// Otherwise, returns `false` and the builder is not set.
    pub fn set_builder<T: Derived, B: Builder<T>>(
        &mut self,
        builder: B,
    ) -> bool {
        match (
            self.builders_by_type_id.entry(TypeId::of::<T>()),
            self.builder_fns_by_type_id.entry(TypeId::of::<T>()),
        ) {
            (Entry::Vacant(builder_entry), Entry::Vacant(fn_entry)) => {
                fn_entry.insert(|builder, global_id, table, handler| {
                    let builder = builder.as_any().downcast_ref::<B>().unwrap();

                    builder
                        .build(global_id, table, handler)
                        .map(|x| x as Arc<dyn Any + Send + Sync>)
                });

                builder_entry.insert(Arc::new(builder));

                true
            }

            (Entry::Occupied(_), Entry::Occupied(_)) => false,

            _ => unreachable!(),
        }
    }

    /// Sets the builder for the given derived component type.
    ///
    /// This will overwrite the existing builder for the given type and returns
    /// the previous builder if there's any.
    pub fn set_builder_overwrite<T: Derived, B: Builder<T>>(
        &mut self,
        builder: B,
    ) -> Option<Arc<dyn AnySendSync>> {
        let existing = self
            .builders_by_type_id
            .insert(TypeId::of::<T>(), Arc::new(builder));
        self.builder_fns_by_type_id.insert(
            TypeId::of::<T>(),
            |builder, global_id, table, handler| {
                let builder = builder.as_any().downcast_ref::<B>().unwrap();

                builder
                    .build(global_id, table, handler)
                    .map(|x| x as Arc<dyn Any + Send + Sync>)
            },
        );

        existing
    }
}

impl std::fmt::Debug for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Context")
            .field(
                "record_stacks_by_thread_id",
                &self.record_stacks_by_thread_id,
            )
            .field("condvars_by_record", &self.condvars_by_record)
            .field("dependencies_by_dependent", &self.dependencies_by_dependent)
            .finish_non_exhaustive()
    }
}

/// An error occurred by the invalid input or the state of the compiler.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum InternalCompilerError {
    #[error(
        "the symbol is not found or the component shouldn't be in given symbol"
    )]
    SymbolNotFoundOrInvalidComponent {
        /// The global ID of the symbol.
        global_id: GlobalID,

        /// The name of the component.
        component_name: &'static str,
    },

    #[error(
        "the component of the symbol is not found and there is no builder for \
         it"
    )]
    NoBuilderFound {
        /// The global ID of the symbol.
        global_id: GlobalID,

        /// The name of the component.
        component_name: &'static str,
    },
}

/// A trait implemented by the **component builders**.
///
/// The implementation of this trait will be used to build the component for the
/// symbol of particular type.
pub trait Builder<T>: Any + Send + Sync {
    /// Builds the component for the given `global_id`.
    ///
    /// Invoked when the component for a particular type is not found in the
    /// storage.
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<T>>;
}

/// An extension trait for shorten the error handling code.
pub trait Handle {
    /// The type of the successful result.
    type Ok;

    /// Handles the [`CyclicDependency`] error in case of the error, this will
    /// transform the error type from [`Error`] to [`HandledError`].
    #[allow(clippy::missing_errors_doc)]
    fn handle(
        self,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Self::Ok>;
}

/**
A cyclic dependency detected while building a component for the particular
symbol. The error diagnostic for the user has been reported to the handler.
 */
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    displaydoc::Display,
)]
pub struct CyclicDependencyError;

impl From<CyclicDependencyError> for fmt::Error {
    fn from(CyclicDependencyError: CyclicDependencyError) -> Self { Self }
}

impl Table {
    /// Queries for the component of type `T` for the given `global_id`.
    ///
    /// The table will compute the component from scratch if the table cannot
    /// find the component in the storage.
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    #[allow(clippy::significant_drop_tightening, clippy::too_many_lines)]
    pub fn query<T: Derived + Any + Send + Sync>(
        &self,
        global_id: GlobalID,
    ) -> Result<Arc<T>, CyclicDependencyError> {
        // if the component is already computed, return it
        let mut context = self.query_context.lock();
        if let Some(result) = self.storage.get_cloned::<T>(global_id) {
            return Ok(result);
        }

        let (Some(builder), Some(build_fn)) = (
            context.builders_by_type_id.get(&TypeId::of::<T>()).cloned(),
            context.builder_fns_by_type_id.get(&TypeId::of::<T>()).copied(),
        ) else {
            panic!(
                "no builder found for the component of type `{}`",
                std::any::type_name::<T>()
            );
        };

        let current_record = Record {
            global_id,
            type_id: std::any::TypeId::of::<T>(),
            name: T::component_name(),
        };

        let current_thread_id = std::thread::current().id();

        let called_from = context
            .record_stacks_by_thread_id
            .get(&current_thread_id)
            .and_then(|x| x.last().copied());

        // check for cyclic dependencies
        if let Some(called_from) = called_from {
            // check if `target_record` can go to `called_from`
            let mut stack = vec![current_record];

            loop {
                if stack.last().unwrap() == &called_from {
                    self.handler.receive(Box::new(
                        diagnostic::CyclicDependency { records_stack: stack },
                    ));

                    return Err(CyclicDependencyError);
                }

                // follow the dependency chain
                if let Some(next) =
                    context.dependencies_by_dependent.get(stack.last().unwrap())
                {
                    stack.push(*next);
                } else {
                    break;
                }
            }

            assert!(context
                .dependencies_by_dependent
                .insert(called_from, current_record)
                .is_none());
        }

        // add the record to the stack
        context
            .record_stacks_by_thread_id
            .entry(current_thread_id)
            .or_default()
            .push(current_record);

        let sync = context.condvars_by_record.get(&current_record).cloned();
        // there's an another thread that is computing the component
        if let Some(sync) = sync {
            // wait for the other thread to finish the job
            sync.wait(&mut context);
        } else {
            // compute the component
            let sync = Arc::new(Condvar::new());
            assert!(context
                .condvars_by_record
                .insert(current_record, sync.clone())
                .is_none(),);

            drop(context); // release the context lock

            let component =
                build_fn(&*builder, global_id, self, &*self.handler);

            // re-acquire the context lock
            context = self.query_context.lock();

            if let Some(succeeded) = component {
                assert!(self
                    .storage
                    .add_component_raw::<T>(global_id, succeeded));
            }

            assert!(context
                .condvars_by_record
                .remove(&current_record)
                .is_some());

            // notify the other threads that the component is computed
            sync.notify_all();
        }

        // remove the dependency graph, record stack
        assert_eq!(
            context
                .record_stacks_by_thread_id
                .get_mut(&current_thread_id)
                .unwrap()
                .pop()
                .unwrap(),
            current_record
        );
        if let Some(called_from) = called_from {
            assert_eq!(
                context.dependencies_by_dependent.remove(&called_from).unwrap(),
                current_record
            );
        }

        // return the component
        Ok(self.storage.get_cloned::<T>(global_id).unwrap_or_else(|| {
            panic!(
                "the component `{}` doesn't belong to the symbol \
                 `{global_id:?}`",
                T::component_name(),
            )
        }))
    }
}
