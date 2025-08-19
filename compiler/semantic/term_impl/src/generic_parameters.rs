use pernixc_query::runtime::executor;
use pernixc_term::generic_parameters::GenericParameters;

use crate::{
    build::{self, Build},
    generic_parameters::diagnostic::Diagnostic,
};
pub mod diagnostic;

#[derive(Debug)]
pub struct Executor;

impl executor::Executor<build::Key<GenericParameters, Diagnostic>>
    for Executor
{
    async fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &build::Key<GenericParameters, Diagnostic>,
    ) -> Result<Build<GenericParameters, Diagnostic>, executor::CyclicError>
    {
        todo!()
    }
}
