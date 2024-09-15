//! Contains code related to building the implementation symbols.

use pernixc_base::{handler::Handler, source_file::Span};
use pernixc_syntax::syntax_tree;

use super::{builder, occurrences::Occurrences};
use crate::{
    arena::ID,
    error::Error,
    symbol::{
        table::{
            representation::{
                building::finalizing::{self, Finalize, Finalizer},
                RwLockContainer,
            },
            resolution::{self, Observer},
            Building, Table,
        },
        GenericID, GlobalID,
    },
    type_system::{
        model::{self},
        term::{self},
    },
};

impl Table<Building<RwLockContainer, Finalizer>> {
    /// Creates the generic arguments for an implementation.
    ///
    /// The generic arguments are not assigned to the implementation symbol
    /// here.
    pub fn create_implementation_arguments<T: Finalize + finalizing::Element>(
        &self,
        implementation_id: GenericID,
        implemented_id: ID<T>,
        implemented_id_generic_parameter_state: usize,
        generic_arguments: Option<&syntax_tree::GenericArguments>,
        resolution_span: &Span,
        occurrences: &mut Occurrences,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> term::GenericArguments<model::Default>
    where
        ID<T>: Into<GenericID> + Into<GlobalID>,
    {
        // make sure the implemented symbol has generic parameters already
        let _ = self.build_to(
            implemented_id,
            Some(implementation_id.into()),
            implemented_id_generic_parameter_state,
            handler,
        );

        let generic_arguments = self
            .resolve_generic_arguments(
                generic_arguments,
                resolution_span,
                implementation_id.into(),
                implemented_id.into(),
                resolution::Config {
                    elided_lifetime_provider: None,
                    elided_type_provider: None,
                    elided_constant_provider: None,
                    observer: Some(
                        &mut (&mut builder::Resolution::basic())
                            .chain(occurrences),
                    ),
                    higher_ranked_lifetimes: None,
                },
                handler,
            )
            .expect("the referring site should be valid")
            .expect("should have generic arguments");

        generic_arguments
    }
}
