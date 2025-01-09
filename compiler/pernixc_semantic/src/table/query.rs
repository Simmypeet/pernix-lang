//! Contains the logic for building the table.

use std::{
    any::{Any, TypeId},
    collections::HashMap,
    fmt::Debug,
    ops::Deref,
    sync::Arc,
    thread::ThreadId,
};

use parking_lot::{Condvar, Mutex};
use pernixc_base::{diagnostic::Report, log::Severity};

use super::{GlobalID, Table};
use crate::{
    component::{LocationSpan, Name},
    diagnostic::ReportError,
};

/// A cyclic dependency between symbols detected during the query.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

/// Represents a component that can be later added to the table by processing
/// the other components.
pub trait Compute {
    /// Creates the component of this type for the given `global_id`.
    fn compute(global_id: GlobalID, table: &Table) -> Option<Self>
    where
        Self: Sized;

    /// The name of the component (used for error messages and debugging).
    fn component_name() -> &'static str;
}

/// A record to the [`Compute::compute`] call. This is primarily used to
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
#[derive(Debug, Clone, Default)]
pub struct Context {
    record_stacks_by_thread_id: HashMap<ThreadId, Vec<Record>>,
    condvars_by_record: HashMap<Record, Arc<(Condvar, Mutex<()>)>>,

    // key ---requires---> value
    dependencies_by_dependent: HashMap<Record, Record>,
}

/// An error that can occur during the query.
#[derive(Debug, thiserror::Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("cyclic dependency detected")]
    CyclicDependency(CyclicDependency),

    #[error(
        "the symbol is not found or the component shouldn't be in given symbol"
    )]
    SymbolNotFoundOrInvalidComponent,
}

impl Error {
    /// Unwraps the error as a [`CyclicDependency`].
    pub fn unwrap_cyclic_dependency(self) -> CyclicDependency {
        match self {
            Error::CyclicDependency(x) => x,
            _ => panic!("not a cyclic dependency"),
        }
    }
}

impl Table {
    /// Queries for the component of type `T` for the given `global_id`.
    ///
    /// The table will compute the component from scratch if the table cannot
    /// find the component in the storage.
    pub fn query<T: Compute + Any>(
        &self,
        global_id: GlobalID,
    ) -> Result<impl Deref<Target = T> + '_, Error> {
        // if the component is already computed, return it
        if let Some(result) = self.storage.get::<T>(global_id) {
            return Ok(result);
        }

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
            .and_then(|x| x.last().cloned());

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
                if let Some(next) = context
                    .dependencies_by_dependent
                    .get(&stack.last().unwrap())
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

            let component = T::compute(global_id, self);

            if let Some(succeeded) = component {
                assert!(self.storage.add_component(global_id, succeeded));
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
