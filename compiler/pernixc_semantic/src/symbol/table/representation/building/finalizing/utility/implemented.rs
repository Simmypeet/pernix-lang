use std::ops::Deref;

use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree;

use super::{builder, occurrences::Occurrences};
use crate::{
    arena::ID,
    error::{
        self, AmbiguousImplementation, FinalImplementationCannotBeOverriden,
    },
    symbol::{
        table::{
            self,
            representation::{
                building::finalizing::{self, Finalizer},
                Index, RwLockContainer,
            },
            Building, Representation, Table,
        },
        GenericID, GlobalID, Implementation, Implemented,
        ResolvableImplementation, ResolvableImplementationID,
    },
    type_system::{environment::Environment, normalizer, order, Premise},
};

impl Table<Building<RwLockContainer, Finalizer>> {
    #[allow(
        clippy::too_many_lines,
        clippy::too_many_arguments,
        clippy::significant_drop_tightening
    )]
    pub(in super::super) fn build_trait_and_marker<
        'a,
        ImplementedSymbol: Implemented<ImplementationID>
            + table::representation::Element
            + finalizing::Element,
        ImplementationID: Copy + Into<ResolvableImplementationID>,
    >(
        &'a self,
        symbol_id: ID<ImplementedSymbol>,
        state_flag: usize,
        generic_parameter_state: usize,
        where_clause_state: usize,
        check_state: usize,
        generic_parameters_syn: Option<&syntax_tree::item::GenericParameters>,
        where_caluse_syn: Option<&syntax_tree::item::WhereClause>,
        generic_parameter_occurrences: &mut Occurrences,
        where_clause_occurrences: &mut Occurrences,
        build_implementation_id: impl Fn(ImplementationID),
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<ImplementedSymbol>: Into<GlobalID> + Into<GenericID>,

        Representation<RwLockContainer>: Index<ImplementationID>,

        <Representation<RwLockContainer> as Index<ImplementationID>>::Output<
            'a,
        >: Deref,

        <<Representation<RwLockContainer> as Index<ImplementationID>>::Output<
            'a,
        > as Deref>::Target: ResolvableImplementation<ID<ImplementedSymbol>>,
    {
        if generic_parameter_state == state_flag {
            self.create_generic_parameters(
                symbol_id,
                generic_parameters_syn,
                generic_parameter_occurrences,
                handler,
            );
        } else if where_clause_state == state_flag {
            self.create_where_clause(
                symbol_id,
                where_caluse_syn,
                where_clause_occurrences,
                handler,
            );
        } else if check_state == state_flag {
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

            self.check_where_clause(symbol_id.into(), handler);

            // check for the ambiguous implementations

            let implemented_sym =
                Index::<ID<ImplementedSymbol>>::get(&**self, symbol_id)
                    .unwrap();

            let implementation_ids = {
                let mut implementation_ids = Vec::new();

                for id in implemented_sym.implementations().iter().copied() {
                    build_implementation_id(id);
                    implementation_ids.push(id);
                }

                implementation_ids
            };

            let definition =
                builder::TypeSystem::new(symbol_id.into(), handler);

            let default_environment = Environment::new_with(
                Premise::default(),
                self,
                normalizer::NO_OP,
                &definition,
            )
            .0;

            for (i, lhs) in implementation_ids.iter().copied().enumerate() {
                let lhs_sym = self.get(lhs).unwrap();

                // arity check
                if lhs_sym.arguments().lifetimes.len()
                    != implemented_sym
                        .generic_declaration()
                        .parameters
                        .lifetimes()
                        .len()
                    || lhs_sym.arguments().types.len()
                        != implemented_sym
                            .generic_declaration()
                            .parameters
                            .types()
                            .len()
                    || lhs_sym.arguments().constants.len()
                        != implemented_sym
                            .generic_declaration()
                            .parameters
                            .constants()
                            .len()
                {
                    continue;
                }

                for rhs in implementation_ids.iter().copied().skip(i + 1) {
                    let rhs_sym = self.get(rhs).unwrap();

                    if rhs_sym.arguments().lifetimes.len()
                        != implemented_sym
                            .generic_declaration()
                            .parameters
                            .lifetimes()
                            .len()
                        || rhs_sym.arguments().types.len()
                            != implemented_sym
                                .generic_declaration()
                                .parameters
                                .types()
                                .len()
                        || rhs_sym.arguments().constants.len()
                            != implemented_sym
                                .generic_declaration()
                                .parameters
                                .constants()
                                .len()
                    {
                        continue;
                    }

                    let Ok(result) = lhs_sym
                        .arguments()
                        .order(rhs_sym.arguments(), &default_environment)
                    else {
                        panic!("shouldn't be overflowing");
                    };

                    // ambiguous trait implementation error
                    if result == order::Order::Ambiguous {
                        handler.receive(Box::new(AmbiguousImplementation {
                            first_implementation_id: lhs.into(),
                            second_implementation_id: rhs.into(),
                        }));
                    }

                    // final cannot be more general
                    if lhs_sym.is_final() && result == order::Order::MoreGeneral
                    {
                        handler.receive(Box::new(
                            FinalImplementationCannotBeOverriden {
                                final_implementation_id: lhs.into(),
                                overriden_implementation_id: rhs.into(),
                            },
                        ));
                    }

                    if rhs_sym.is_final()
                        && result == order::Order::MoreSpecific
                    {
                        handler.receive(Box::new(
                            FinalImplementationCannotBeOverriden {
                                final_implementation_id: rhs.into(),
                                overriden_implementation_id: lhs.into(),
                            },
                        ));
                    }
                }
            }
        } else {
            panic!("invalid state flag");
        }
    }
}
