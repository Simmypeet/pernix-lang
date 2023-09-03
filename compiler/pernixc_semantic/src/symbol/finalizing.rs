use pernixc_system::diagnostic::Handler;

use super::{DraftingSymbolRef, Global, State, Table};
use crate::error::{self, CyclicDependency};

impl Table {
    fn remove_constructing_symbol(&self, constructing_symbol_ref: DraftingSymbolRef) {
        let state = self
            .state_manager
            .write()
            .states_by_drafting_symbol_refs
            .remove(&constructing_symbol_ref)
            .expect("should be in constructing state");

        assert!(
            matches!(state, State::Constructing(..)),
            "should be in constructing state"
        );
    }

    #[allow(clippy::significant_drop_tightening)]
    fn mark_as_constructing(
        &self,
        constructing_symbol_ref: DraftingSymbolRef,
    ) -> Result<(), CyclicDependency> {
        let mut state_manager = self.state_manager.write();

        let current_constructing_order = state_manager.current_constructing_order;
        let symbol_state = state_manager
            .states_by_drafting_symbol_refs
            .get_mut(&constructing_symbol_ref)
            .expect("invalid ID");

        match symbol_state {
            // mark the symbol as constructing
            State::Drafted => {
                *symbol_state = State::Constructing(current_constructing_order);

                state_manager.current_constructing_order += 1;
                Ok(())
            }
            State::Constructing(constructing_order) => {
                let constructing_order = *constructing_order;
                // found cyclic dependency

                // any symbols that are marked as constructing after the `constructing_order` are
                // considered as participating in the cyclic dependency

                let cyclic_symbols = state_manager
                    .states_by_drafting_symbol_refs
                    .iter()
                    .filter_map(|(k, v)| match v {
                        State::Drafted => None,
                        State::Constructing(comparing_constructing_order) => {
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
                    assert!(state_manager
                        .states_by_drafting_symbol_refs
                        .remove(symbol)
                        .is_some());
                }

                Err(CyclicDependency {
                    participant_spans: cyclic_symbols
                        .into_iter()
                        .map(|s| {
                            let container = self.container.read();
                            match s {
                                DraftingSymbolRef::Struct(s) => {
                                    container.structs[s].span().unwrap()
                                }
                                DraftingSymbolRef::Enum(s) => container.enums[s].span().unwrap(),
                                DraftingSymbolRef::Trait(s) => container.traits[s].span().unwrap(),
                                DraftingSymbolRef::Type(s) => container.types[s].span().unwrap(),
                                DraftingSymbolRef::Function(s) => {
                                    container.functions[s].span().unwrap()
                                }
                                DraftingSymbolRef::Constant(s) => {
                                    container.constants[s].span().unwrap()
                                }
                            }
                        })
                        .collect(),
                })
            }
        }
    }

    #[must_use]
    pub(super) fn finalize(
        &self,
        drafting_symbol_ref: DraftingSymbolRef,
        handler: &impl Handler<error::Error>,
    ) -> bool {
        let Some(state) = self
            .state_manager
            .read()
            .states_by_drafting_symbol_refs
            .get(&drafting_symbol_ref)
        else {
            // the symbol has already been finalized
            return true;
        };

        match drafting_symbol_ref {
            DraftingSymbolRef::Struct(_) => todo!(),
            DraftingSymbolRef::Enum(_) => todo!(),
            DraftingSymbolRef::Trait(_) => todo!(),
            DraftingSymbolRef::Type(_) => todo!(),
            DraftingSymbolRef::Function(_) => todo!(),
            DraftingSymbolRef::Constant(_) => todo!(),
        }
    }
}
