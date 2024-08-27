//! Contains code related to building the implementation symbols.

use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree;

use super::{
    finalizer::{self, builder::Basic},
    occurrences::Occurrences,
    Finalize, Finalizer,
};
use crate::{
    arena::ID,
    error::Error,
    symbol::{
        table::{
            representation::RwLockContainer,
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
    pub(super) fn create_implementation_arguments<
        T: Finalize + finalizer::Element,
    >(
        &self,
        implementation_id: GenericID,
        implemented_id: ID<T>,
        implemented_id_generic_parameter_state: usize,
        generic_identifier: &syntax_tree::GenericIdentifier,
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
                generic_identifier,
                implementation_id.into(),
                implemented_id.into(),
                resolution::Config {
                    ellided_lifetime_provider: None,
                    ellided_type_provider: None,
                    ellided_constant_provider: None,
                    observer: Some(&mut (&mut Basic).chain(occurrences)),
                    higher_ranked_lifetimes: None,
                },
                handler,
            )
            .expect("the referring site should be valid")
            .expect("should have generic arguments");

        generic_arguments
    }
}
