use std::collections::HashMap;

use paste::paste;
use pernixc_syntax::syntax_tree;

use crate::{
    arena::ID,
    symbol::{self, Constant, Enum, Function, GlobalID, Module, Struct, Type},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum State<T> {
    Drafted(T),
    Constructing,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Implementation {
    pub(super) in_module: ID<Module>,
    pub(super) syntax_tree: syntax_tree::item::Implements,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Trait {
    pub(super) syntax_tree: syntax_tree::item::Trait,
    pub(super) implementations: Vec<Implementation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(super) struct Manager {
    pub(super) states_by_enum_id: HashMap<ID<Enum>, State<syntax_tree::item::Enum>>,
    pub(super) states_by_struct_id: HashMap<ID<Struct>, State<syntax_tree::item::Struct>>,
    pub(super) states_by_constant_id: HashMap<ID<Constant>, State<syntax_tree::item::Constant>>,
    pub(super) states_by_type_id: HashMap<ID<Type>, State<syntax_tree::item::Type>>,
    pub(super) states_by_function_id: HashMap<ID<Function>, State<syntax_tree::item::Function>>,
    pub(super) states_by_trait_id: HashMap<ID<symbol::Trait>, State<Trait>>,
}

macro_rules! remove_state_arm {
    ($self:ident, $id:ident, $($name:ident $(,)?)*) => {
        paste! {
            match $id {
                $(GlobalID::$name(id) => $self.[<states_by_ $name:lower _id>].remove(&id).is_some()),*,
                _ => false,
            }
        }
    };
}

impl Manager {
    /// Returns `true` if the state is successfully removed.
    #[must_use]
    pub(super) fn remove_state(&mut self, global_id: GlobalID) -> bool {
        remove_state_arm!(self, global_id, Enum, Struct, Constant, Type, Function, Trait)
    }
}
