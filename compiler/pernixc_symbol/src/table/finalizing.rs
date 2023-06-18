use pernixc_system::{arena, diagnostic::Handler};

use super::{
    drafting::{States, SymbolState},
    Table,
};
use crate::{
    error::{CyclicDependency, Error},
    Function, ID,
};

impl States {
    fn mark_as_constructing(&mut self, id: ID) -> Result<(), CyclicDependency> {
        let mut symbol_state = self.symbol_states_by_id.get_mut(&id).expect("invalid ID");

        match symbol_state {
            // mark the symbol as constructing
            SymbolState::Drafted => {
                *symbol_state = SymbolState::Constructing {
                    constructing_order: self.current_constructing_order,
                };

                self.current_constructing_order += 1;
                Ok(())
            }
            SymbolState::Constructing { constructing_order } => {
                let constructing_order = *constructing_order;
                // found cyclic dependency

                // any symbols that are marked as constructing after the `constructing_order` are
                // considered as participating in the cyclic dependency

                let cyclic_symbols = self
                    .symbol_states_by_id
                    .iter()
                    .filter_map(|(k, v)| match v {
                        SymbolState::Drafted => None,
                        SymbolState::Constructing {
                            constructing_order: comparing_constructing_order,
                        } => {
                            if *comparing_constructing_order >= constructing_order {
                                Some(*k)
                            } else {
                                None
                            }
                        }
                    })
                    .collect::<Vec<_>>();

                // remove all symbols that are participating in the cyclic dependency
                for symbol in &cyclic_symbols {
                    assert!(self.symbol_states_by_id.remove(symbol).is_some());
                }

                if cyclic_symbols.is_empty() {
                    Ok(())
                } else {
                    Err(CyclicDependency {
                        participants: cyclic_symbols,
                    })
                }
            }
        }
    }
}

impl Table {
    pub(super) fn finalize_symbols(&mut self, states: &mut States, handler: &impl Handler<Error>) {
        while !states.symbol_states_by_id.is_empty() {
            let id = states
                .symbol_states_by_id
                .iter()
                .find_map(|(id, symbol_state)| {
                    if *symbol_state == SymbolState::Drafted {
                        Some(*id)
                    } else {
                        None
                    }
                })
                .unwrap();
        }
    }

    fn finalize_symbol(&mut self, id: ID, states: &mut States, handler: &impl Handler<Error>) {
        match id {
            ID::Struct(i) => todo!(),
            ID::Function(i) => self.finalize_function(i, states, handler),
            ID::Type(i) => todo!(),
            ID::Trait(i) => todo!(),
            ID::TraitFunction(i) => todo!(),
            ID::TraitType(i) => todo!(),
            ID::Field(i) => todo!(),
            ID::Parameter(i) => todo!(),
            ID::Implements(i) => todo!(),
            ID::ImplementsType(i) => todo!(),
            ID::ImplementsFunction(i) => todo!(),

            ID::Module(..)
            | ID::Enum(..)
            | ID::EnumVariant(..)
            | ID::TypeParameter(..)
            | ID::LifetimeParameter(..) => unreachable!(),
        }
    }

    fn finalize_function(
        &mut self,
        id: arena::ID<Function>,
        drafting: &mut States,
        handler: &impl Handler<Error>,
    ) {
    }
}
