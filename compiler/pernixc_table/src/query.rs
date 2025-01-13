//! Contains the logic for building the symbol.

use std::{
    any::{Any, TypeId},
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    ops::Deref,
    sync::Arc,
    thread::ThreadId,
};

use enum_as_inner::EnumAsInner;
use parking_lot::{Condvar, Mutex};
use pernixc_base::{diagnostic::Report, handler::Handler, log::Severity};

use super::{GlobalID, Table};
use crate::{
    component::{Derived, LocationSpan, Name},
    diagnostic::{Diagnostic, ReportError},
};

/// A cyclic dependency between symbols detected during the query.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "a cylic dependency detected while building a component for the \
     particular symbol"
)]
pub struct CyclicDependency {
    /// The stack of records that caused the cyclic dependency.
    pub records_stack: Vec<Record>,
}

impl Report<&Table> for CyclicDependency {
    type Error = ReportError;

    fn report(
        &self,
        parameter: &Table,
    ) -> Result<pernixc_base::diagnostic::Diagnostic, Self::Error> {
        if self.records_stack.is_empty() {
            return Err(ReportError);
        }

        Ok(pernixc_base::diagnostic::Diagnostic {
            span: parameter
                .storage
                .get::<LocationSpan>(self.records_stack[0].global_id)
                .ok_or(ReportError)?
                .0
                .clone(),
            message: format!(
                "cyclic dependency while building {}.",
                self.records_stack
                    .iter()
                    .map(|x| Ok(format!(
                        "{} for `{}`",
                        x.name,
                        parameter
                            .storage
                            .get::<Name>(x.global_id)
                            .ok_or(ReportError)?
                            .0
                    )))
                    .collect::<Result<Vec<_>, _>>()?
                    .join(" -> ")
            ),
            severity: Severity::Error,
            help_message: Some(format!(
                "required by `{}`",
                self.records_stack.first().unwrap().name
            )),
            related: self
                .records_stack
                .iter()
                .skip(1)
                .map(|x| {
                    Ok(pernixc_base::diagnostic::Related {
                        span: parameter
                            .storage
                            .get::<LocationSpan>(x.global_id)
                            .ok_or(ReportError)?
                            .0
                            .clone(),
                        message: format!("required by `{}`", x.name),
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

/// A record to the [`Derived::compute`] call. This is primarily used to
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

/// A struct that manages/synchronizes the query.
#[derive(Clone, Default)]
pub struct Context {
    record_stacks_by_thread_id: HashMap<ThreadId, Vec<Record>>,
    condvars_by_record: HashMap<Record, Arc<(Condvar, Mutex<()>)>>,
    builders_by_type_id: HashMap<TypeId, Arc<dyn Builder>>,

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
    pub fn set_builder<T: Derived + Any>(
        &mut self,
        builder: Arc<dyn Builder>,
    ) -> bool {
        match self.builders_by_type_id.entry(TypeId::of::<T>()) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(builder);
                true
            }
        }
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

/// An error that can occur during the query.
#[derive(Debug, EnumAsInner, thiserror::Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("cyclic dependency detected")]
    CyclicDependency(CyclicDependency),

    #[error(
        "the symbol is not found or the component shouldn't be in given symbol"
    )]
    SymbolNotFoundOrInvalidComponent,

    #[error(
        "the component of the symbol is not found and there is no builder for \
         it"
    )]
    NoBuilderFound,
}

impl Error {
    /// Unwraps the error as a [`CyclicDependency`].
    #[must_use]
    pub fn unwrap_cyclic_dependency(self) -> CyclicDependency {
        match self {
            Self::CyclicDependency(x) => x,

            Self::NoBuilderFound | Self::SymbolNotFoundOrInvalidComponent => {
                panic!("not a cyclic dependency")
            }
        }
    }
}

/// A trait implemented by the **component builders**.
///
/// The implementation of this trait will be used to build the component for the
/// symbol of particular type.
pub trait Builder {
    /// Builds the component for the given `global_id`.
    ///
    /// Invoked when the component for a particular type is not found in the
    /// storage.
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Box<dyn Any>>;
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
    #[allow(clippy::significant_drop_tightening)]
    pub fn query<T: Derived + Any>(
        &self,
        global_id: GlobalID,
    ) -> Result<impl Deref<Target = T> + '_, Error> {
        // if the component is already computed, return it
        if let Some(result) = self.storage.get::<T>(global_id) {
            return Ok(result);
        }

        let Some(builder) = self
            .query_context
            .read()
            .builders_by_type_id
            .get(&TypeId::of::<T>())
            .cloned()
        else {
            return Err(Error::NoBuilderFound);
        };

        let target_record = Record {
            global_id,
            type_id: std::any::TypeId::of::<T>(),
            name: T::component_name(),
        };

        let mut context = self.query_context.write();
        let current_thread_id = std::thread::current().id();

        let called_from = context
            .record_stacks_by_thread_id
            .get(&current_thread_id)
            .and_then(|x| x.last().copied());

        // check for cyclic dependencies
        if let Some(called_from) = called_from {
            // check if `target_record` can go to `called_from`
            let mut stack = vec![called_from];

            loop {
                if stack.last().unwrap() == &target_record {
                    return Err(Error::CyclicDependency(CyclicDependency {
                        records_stack: stack,
                    }));
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

            context
                .dependencies_by_dependent
                .insert(called_from, target_record);
        }

        // add the record to the stack
        context
            .record_stacks_by_thread_id
            .entry(current_thread_id)
            .or_default()
            .push(target_record);

        let sync = context.condvars_by_record.get(&target_record).cloned();

        // there's an another thread that is computing the component
        if let Some(sync) = sync {
            drop(context);

            let (sync, lock) = &*sync;

            let mut guard = lock.lock();

            // wait for the other thread to finish the job
            sync.wait(&mut guard);

            context = self.query_context.write();
        } else {
            // compute the component
            let sync = Arc::new((Condvar::new(), Mutex::new(())));
            assert!(context
                .condvars_by_record
                .insert(target_record, sync.clone(),)
                .is_none());

            drop(context); // release the context lock

            let component = builder.build(global_id, self, &*self.handler);

            if let Some(succeeded) = component {
                assert!(self
                    .storage
                    .add_component_boxed::<T>(global_id, succeeded));
            }

            // notify the other threads that the component is computed
            sync.0.notify_all();

            // remove the record from the stack
            context = self.query_context.write();

            assert!(context
                .condvars_by_record
                .remove(&target_record)
                .is_some());
        }

        // remove the dependency graph, record stack
        assert_eq!(
            context
                .record_stacks_by_thread_id
                .get_mut(&current_thread_id)
                .unwrap()
                .pop()
                .unwrap(),
            target_record
        );
        if let Some(called_from) = called_from {
            assert_eq!(
                context.dependencies_by_dependent.remove(&called_from).unwrap(),
                target_record
            );
        }

        // return the component
        self.storage
            .get::<T>(global_id)
            .ok_or(Error::SymbolNotFoundOrInvalidComponent)
    }
}
