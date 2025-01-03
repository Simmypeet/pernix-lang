use std::{any::Any, fmt::Debug};

use pernixc_base::diagnostic::Report;

use crate::table::Representation;

/// Implemented by all semantic errors.
pub trait Error:
    for<'a> Report<&'a Representation, Error = Box<dyn crate::error::Error>>
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
        U: for<'a> Report<
                &'a Representation,
                Error = Box<dyn crate::error::Error>,
            > + Debug
            + Any
            + Send
            + Sync
            + 'static,
    > Error for U
{
    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}
