//! Contains the diagnostic related to the table.

use std::{any::Any, fmt::Debug};

use pernixc_diagnostic::Report;

use crate::table::Table;

/// Implemented by all diagnostic objects.
pub trait Diagnostic:
    for<'a> Report<&'a Table> + Debug + Any + Send + Sync + 'static
{
    #[allow(missing_docs)]
    fn as_any(&self) -> &dyn Any;

    #[allow(missing_docs)]
    fn as_any_mut(&mut self) -> &mut dyn Any;

    #[allow(missing_docs)]
    fn type_name(&self) -> &'static str;
}

impl<U: for<'a> Report<&'a Table> + Debug + Any + Send + Sync + 'static>
    Diagnostic for U
{
    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }

    fn type_name(&self) -> &'static str { std::any::type_name::<U>() }
}

impl<U: for<'a> Report<&'a Table> + Debug + Any + Send + Sync + 'static> From<U>
    for Box<dyn Diagnostic>
{
    fn from(value: U) -> Self { Box::new(value) }
}
