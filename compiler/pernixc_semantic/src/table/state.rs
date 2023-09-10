use std::collections::HashMap;

use enum_as_inner::EnumAsInner;
use pernixc_syntax::syntax_tree;

use crate::symbol::AssociatedItemRef;

#[derive(Debug, Clone)]
pub(super) enum GlobalItemSyntax {
    Trait {
        trait_syntax: syntax_tree::item::Trait,
        implements_syntax: Vec<syntax_tree::item::Implements>,
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

#[derive(Debug)]
pub(super) struct Constructing {
    constructing_order: usize,
}
#[derive(Debug, EnumAsInner)]
pub(super) enum State {
    /// The symbol is drafted, the name is assigned, but the symbol is not yet fully defined.
    Drafted(GlobalItemSyntax),

    /// The symbol is being constructed. This is used to detect cyclic dependencies.
    Constructing(Constructing),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum DraftedSymbolRef {
    Trait(usize),
    Function(usize),
    Type(usize),
    Struct(usize),
    Enum(usize),
    Constant(usize),
    TraitType(AssociatedItemRef),
    TraitFunction(AssociatedItemRef),
    TraitConstant(AssociatedItemRef),
}

#[derive(Debug, Default)]
pub(super) struct Manager {
    current_constructing_order: usize,
    states_by_drafting_symbol_ref: HashMap<DraftedSymbolRef, State>,
}

impl Manager {
    pub(super) fn get_item_syntax_mut(
        &mut self,
        drafted_symbol_ref: DraftedSymbolRef,
    ) -> Option<&mut GlobalItemSyntax> {
        self.states_by_drafting_symbol_ref
            .get_mut(&drafted_symbol_ref)
            .and_then(State::as_drafted_mut)
    }

    pub(super) fn add_drafted_symbol(
        &mut self,
        drafted_symbol_ref: DraftedSymbolRef,
        syntax: GlobalItemSyntax,
    ) {
        assert!(self
            .states_by_drafting_symbol_ref
            .insert(drafted_symbol_ref, State::Drafted(syntax))
            .is_none());
    }
}
