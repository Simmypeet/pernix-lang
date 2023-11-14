use std::{collections::HashMap, sync::Arc};

use enum_as_inner::EnumAsInner;
use parking_lot::{Mutex, MutexGuard};
use paste::paste;
use pernixc_syntax::syntax_tree;
use thiserror::Error;

use super::Table;
use crate::{
    arena::ID,
    symbol::{self, Constant, Enum, Function, GlobalID, Module, Struct, Type},
};

#[derive(Debug, Clone, EnumAsInner)]
pub(super) enum State<T> {
    Drafted(T),
    Constructing(Constructing),
}

/// Represents a lock on the constructing symbol.
pub(super) struct ConstructingLock<'a, 'b> {
    symbol_id: GlobalID,
    waiting_lock: Option<MutexGuard<'a, ()>>,
    table: &'b Table,
}

impl<'a, 'b> ConstructingLock<'a, 'b> {
    pub(super) fn new(
        symbol_id: GlobalID,
        waiting_lock: MutexGuard<'a, ()>,
        table: &'b Table,
    ) -> Self {
        Self {
            symbol_id,
            waiting_lock: Some(waiting_lock),
            table,
        }
    }
}

impl<'a, 'b> Drop for ConstructingLock<'a, 'b> {
    fn drop(&mut self) {
        self.waiting_lock.take();

        let _ = self
            .table
            .state_manager
            .write()
            .remove_state(self.symbol_id);
    }
}

#[derive(Debug, Clone)]
pub(super) struct Constructing {
    /// The mutex lock that is used for waiting the symbol to be finished constructing.
    pub(super) waiting_lock: Arc<Mutex<()>>,
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

#[derive(Debug, Clone, Default)]
pub(super) struct Manager {
    pub(super) states_by_enum_id: HashMap<ID<Enum>, State<syntax_tree::item::Enum>>,
    pub(super) states_by_struct_id: HashMap<ID<Struct>, State<syntax_tree::item::Struct>>,
    pub(super) states_by_constant_id: HashMap<ID<Constant>, State<syntax_tree::item::Constant>>,
    pub(super) states_by_type_id: HashMap<ID<Type>, State<syntax_tree::item::Type>>,
    pub(super) states_by_function_id: HashMap<ID<Function>, State<syntax_tree::item::Function>>,
    pub(super) states_by_trait_id: HashMap<ID<symbol::Trait>, State<Trait>>,

    pub(super) dependencies: HashMap<GlobalID, GlobalID>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("the given global ID is not found in the entry")]
pub(super) struct EntryNotFoundError(GlobalID);

impl Manager {
    /// Returns `true` if the state is successfully removed.
    #[must_use]
    pub(super) fn remove_state(&mut self, global_id: GlobalID) -> bool {
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

        remove_state_arm!(self, global_id, Enum, Struct, Constant, Type, Function, Trait)
    }

    pub(super) fn all_drafted_symbols(&self) -> impl Iterator<Item = GlobalID> + '_ {
        macro_rules! map_symbol {
            ($name:ident) => {
                paste! {
                    self.[<states_by_ $name:lower _id>]
                        .iter()
                        .filter_map(
                            |(key, state)| state.as_drafted().map(|_| (*key).into())
                        )
                }
            };
        }

        map_symbol!(ENUM)
            .chain(map_symbol!(STRUCT))
            .chain(map_symbol!(CONSTANT))
            .chain(map_symbol!(TYPE))
            .chain(map_symbol!(FUNCTION))
            .chain(map_symbol!(TRAIT))
    }

    pub(super) fn next_drafted_symbol(&self) -> Option<GlobalID> {
        macro_rules! map_symbol {
            ($name:ident) => {
                paste! {
                    self.[<states_by_ $name:lower _id>]
                        .iter()
                        .filter_map(
                            |(key, state)| state.as_drafted().map(|_| (*key).into())
                        )
                }
            };
        }

        macro_rules! next_drafted_symbol {
            ($first:ident $(, $rest:ident)*) => {paste! {{
                let mut iter = map_symbol!($first)
                    $(.chain(map_symbol!($rest)))*
                    ;

                iter.next()
            }}};
        }

        next_drafted_symbol!(ENUM, STRUCT, CONSTANT, TYPE, FUNCTION, TRAIT)
    }
}
