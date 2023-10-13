use std::collections::HashMap;

use enum_as_inner::EnumAsInner;
use pernixc_syntax::syntax_tree;

use crate::symbol::{
    ConstantRef, EnumRef, FunctionRef, GlobalItemRef, ImplementsAssociatedRef, ModuleRef,
    StructRef, TraitAssociatedRef, TraitRef, TypeRef,
};

#[derive(Debug, Clone, EnumAsInner)]
pub(super) enum DraftedSymbolSyntax {
    Trait {
        trait_syntax: Option<syntax_tree::item::Trait>,
        // pair of the index of the module in which the implements syntax is declared and the
        // implements syntax
        implements_syntax: Vec<(ModuleRef, syntax_tree::item::Implements)>,
    },
    Function {
        function_syntax: syntax_tree::item::Function,
    },
    Type {
        type_syntax: syntax_tree::item::Type,
    },
    Struct {
        struct_syntax: syntax_tree::item::Struct,
    },
    Enum {
        enum_syntax: syntax_tree::item::Enum,
    },
    Constant {
        constant_syntax: syntax_tree::item::Constant,
    },
}

#[derive(Debug, EnumAsInner)]
pub(super) enum State {
    /// The symbol is drafted, the name is assigned, but the symbol is not yet fully defined.
    Drafted(DraftedSymbolSyntax),

    /// The symbol is being constructed. This is used to detect cyclic dependencies.
    Constructing(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum DraftedSymbolRef {
    Trait(TraitRef),
    Function(FunctionRef),
    Type(TypeRef),
    Struct(StructRef),
    Enum(EnumRef),
    Constant(ConstantRef),
    TraitAssociated(TraitAssociatedRef),
    ImplementsAssociated(ImplementsAssociatedRef),
}

impl TryFrom<GlobalItemRef> for DraftedSymbolRef {
    type Error = GlobalItemRef;

    fn try_from(value: GlobalItemRef) -> Result<Self, Self::Error> {
        match value {
            GlobalItemRef::Struct(struct_id) => Ok(Self::Struct(struct_id)),
            GlobalItemRef::Enum(enum_id) => Ok(Self::Enum(enum_id)),
            GlobalItemRef::Type(type_id) => Ok(Self::Type(type_id)),
            GlobalItemRef::Constant(constant_id) => Ok(Self::Constant(constant_id)),
            GlobalItemRef::Function(function_id) => Ok(Self::Function(function_id)),
            GlobalItemRef::Trait(trait_id) => Ok(Self::Trait(trait_id)),
            GlobalItemRef::TraitAssociated(trait_associated) => {
                Ok(Self::TraitAssociated(trait_associated))
            }
            GlobalItemRef::ImplementsAssociated(implements_associated) => {
                Ok(Self::ImplementsAssociated(implements_associated))
            }
            err => Err(err),
        }
    }
}

impl From<DraftedSymbolRef> for GlobalItemRef {
    fn from(drafted_symbol_ref: DraftedSymbolRef) -> Self {
        match drafted_symbol_ref {
            DraftedSymbolRef::Trait(index) => Self::Trait(index),
            DraftedSymbolRef::Function(index) => Self::Function(index),
            DraftedSymbolRef::Type(index) => Self::Type(index),
            DraftedSymbolRef::Struct(index) => Self::Struct(index),
            DraftedSymbolRef::Enum(index) => Self::Enum(index),
            DraftedSymbolRef::Constant(index) => Self::Constant(index),
            DraftedSymbolRef::TraitAssociated(trait_associated_ref) => {
                Self::TraitAssociated(trait_associated_ref)
            }
            DraftedSymbolRef::ImplementsAssociated(implements_associated_ref) => {
                Self::ImplementsAssociated(implements_associated_ref)
            }
        }
    }
}

#[derive(Debug, Default)]
pub(super) struct Manager {
    current_constructing_order: usize,
    states_by_drafting_symbol_ref: HashMap<DraftedSymbolRef, State>,
}

impl Manager {
    pub(super) fn next_drafted_symbol_ref(&self) -> Option<DraftedSymbolRef> {
        self.states_by_drafting_symbol_ref.keys().copied().next()
    }

    pub(super) fn get_item_syntax_mut(
        &mut self,
        drafted_symbol_ref: DraftedSymbolRef,
    ) -> Option<&mut DraftedSymbolSyntax> {
        self.states_by_drafting_symbol_ref
            .get_mut(&drafted_symbol_ref)
            .and_then(State::as_drafted_mut)
    }

    pub(super) fn add_drafted_symbol(
        &mut self,
        drafted_symbol_ref: DraftedSymbolRef,
        syntax: DraftedSymbolSyntax,
    ) {
        assert!(self
            .states_by_drafting_symbol_ref
            .insert(drafted_symbol_ref, State::Drafted(syntax))
            .is_none());
    }

    pub(super) fn mark_as_done(&mut self, drafted_symbol_ref: DraftedSymbolRef) {
        let state = self
            .states_by_drafting_symbol_ref
            .remove(&drafted_symbol_ref)
            .unwrap();
        assert!(matches!(state, State::Constructing(..)));
    }

    pub(super) fn mark_as_constructing(
        &mut self,
        constructing_symbol_ref: DraftedSymbolRef,
    ) -> Result<DraftedSymbolSyntax, Vec<DraftedSymbolRef>> {
        let current_constructing_order = self.current_constructing_order;
        let symbol_state = self
            .states_by_drafting_symbol_ref
            .get_mut(&constructing_symbol_ref)
            .expect("invalid ID");

        match symbol_state {
            // mark the symbol as constructing
            State::Drafted(..) => {
                let drafted_symbol_syntax = std::mem::replace(
                    symbol_state,
                    State::Constructing(current_constructing_order),
                )
                .into_drafted()
                .unwrap();

                self.current_constructing_order += 1;
                Ok(drafted_symbol_syntax)
            }
            State::Constructing(constructing_order) => {
                let constructing_order = *constructing_order;
                // found cyclic dependency

                // any symbols that are md as constructing after the `constructing_order` are are
                // considered as participating in the cyclic dependency

                let cyclic_symbols = self
                    .states_by_drafting_symbol_ref
                    .iter()
                    .filter_map(|(k, v)| match v {
                        State::Drafted(..) => None,
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
                    assert!(self.states_by_drafting_symbol_ref.remove(symbol).is_some());
                }

                Err(cyclic_symbols)
            }
        }
    }
}
