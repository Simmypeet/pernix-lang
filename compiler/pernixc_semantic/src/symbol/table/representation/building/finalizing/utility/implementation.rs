//! Contains code related to building the implementation symbols.

use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree;

use super::{builder, occurrences::Occurrences};
use crate::{
    arena::ID,
    error::Error,
    symbol::{
        table::{
            self,
            representation::{
                building::finalizing::{self, Finalize, Finalizer},
                RwLockContainer,
            },
            resolution::{self, Observer},
            Building, Table,
        },
        GenericID, GenericTemplate, ItemID, ImplementationTemplate,
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
    pub(in super::super) fn create_implementation_arguments<
        T: Finalize + finalizing::Element,
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
        ID<T>: Into<GenericID> + Into<ItemID>,
    {
        // make sure the implemented symbol has generic parameters already
        let _ = self.build_to(
            implemented_id,
            Some(implementation_id.into()),
            implemented_id_generic_parameter_state,
            handler,
        );

        let generic_arguments = self
            .resolve_generic_arguments_for(
                implemented_id.into(),
                generic_identifier,
                implementation_id.into(),
                resolution::Config {
                    elided_lifetime_provider: None,
                    elided_type_provider: None,
                    elided_constant_provider: None,
                    observer: Some(
                        &mut builder::Resolution::basic().chain(occurrences),
                    ),
                    higher_ranked_lifetimes: None,
                },
                handler,
            )
            .expect("the referring site should be valid");

        generic_arguments
    }

    #[allow(clippy::too_many_arguments)]
    pub(in super::super) fn build_implementation<
        ParentID: Copy + Into<ItemID>,
        Implemented: Finalize + finalizing::Element,
        Definition: 'static,
    >(
        &self,
        symbol_id: ID<
            GenericTemplate<
                ParentID,
                ImplementationTemplate<ID<Implemented>, Definition>,
            >,
        >,
        state_flag: usize,
        generic_parameter_state: usize,
        where_clause_state: usize,
        arguments_state: usize,
        check_state: usize,
        generic_parameters_syn: Option<&syntax_tree::item::GenericParameters>,
        where_caluse_syn: Option<&syntax_tree::item::WhereClause>,
        qualified_identifier: &syntax_tree::QualifiedIdentifier,
        generic_parameter_occurrences: &mut Occurrences,
        where_clause_occurrences: &mut Occurrences,
        argument_occurrences: &mut Occurrences,
        implemented_generic_parameter_state: usize,
        implemented_where_clause_state: usize,
        handler: &dyn Handler<Box<dyn Error>>,
    ) where
        GenericTemplate<
            ParentID,
            ImplementationTemplate<ID<Implemented>, Definition>,
        >: table::representation::Element + finalizing::Element,
        ID<
            GenericTemplate<
                ParentID,
                ImplementationTemplate<ID<Implemented>, Definition>,
            >,
        >: Into<ItemID> + Into<GenericID>,
        ID<Implemented>: Into<ItemID> + Into<GenericID>,
    {
        if state_flag == generic_parameter_state {
            self.create_generic_parameters(
                symbol_id,
                generic_parameters_syn,
                generic_parameter_occurrences,
                handler,
            );
        } else if state_flag == where_clause_state {
            self.create_where_clause(
                symbol_id,
                where_caluse_syn,
                where_clause_occurrences,
                handler,
            );
        } else if state_flag == arguments_state {
            let implemented_id =
                table::representation::Element::get_arena(&**self)
                    .get(symbol_id)
                    .unwrap()
                    .read()
                    .implemented_id;

            table::representation::Element::get_arena(&**self)
                .get(symbol_id)
                .unwrap()
                .write()
                .arguments = self.create_implementation_arguments(
                symbol_id.into(),
                implemented_id,
                implemented_generic_parameter_state,
                qualified_identifier.rest().last().map_or_else(
                    || {
                        qualified_identifier
                            .root()
                            .as_generic_identifier()
                            .unwrap()
                    },
                    |x| &x.1,
                ),
                argument_occurrences,
                handler,
            );
        } else if state_flag == check_state {
            // make sure the implemented trait has the where clause
            let implemented_id =
                table::representation::Element::get_arena(&**self)
                    .get(symbol_id)
                    .unwrap()
                    .read()
                    .implemented_id;

            let _ = self.build_to(
                implemented_id,
                Some(symbol_id.into()),
                implemented_where_clause_state,
                handler,
            );

            self.check_occurrences(
                symbol_id.into(),
                generic_parameter_occurrences,
                handler,
            );
            self.check_occurrences(
                symbol_id.into(),
                where_clause_occurrences,
                handler,
            );
            self.check_occurrences(
                symbol_id.into(),
                argument_occurrences,
                handler,
            );
            self.check_where_clause(symbol_id.into(), handler);
            self.implementation_signature_check(symbol_id, handler);
        } else {
            panic!("invalid state flag");
        }
    }
}
