//! Contains the definition of the [`Diagnostic`] trait.

use std::{any::Any, fmt::Debug};

use pernixc_base::diagnostic::Report;

use crate::table::Table;

/// An error type used for [`Report::Error`] associated type.
///
/// This typically caused by giving an invalid table (not the same table where
/// the error originated from) to the parameter [`Report::report`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReportError;

/// Implemented by all diagnostic objects.
pub trait Diagnostic:
    for<'a> Report<&'a Table, Error = ReportError>
    + Debug
    + Any
    + Send
    + Sync
    + 'static
{
    #[allow(missing_docs)]
    fn as_any(&self) -> &dyn Any;

    #[allow(missing_docs)]
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<
        U: for<'a> Report<&'a Table, Error = ReportError>
            + Debug
            + Any
            + Send
            + Sync
            + 'static,
    > Diagnostic for U
{
    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}
