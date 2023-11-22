use std::{collections::HashMap, sync::Arc};

use enum_as_inner::EnumAsInner;
use parking_lot::{Mutex, MutexGuard};
use paste::paste;
use pernixc_syntax::syntax_tree;
use thiserror::Error;

use super::Table;
use crate::{
    arena::ID,
    symbol::{self, Constant, Enum, Function, GlobalID, Struct, Type},
};

#[derive(Debug, Clone, EnumAsInner)]
pub(super) enum State<T> {
    Drafted(T),
    Constructing(Constructing),
}

/// Represents a lock on the constructing symbol.
///
/// Drop the lock when the symbol is ready to be used.
#[clippy::has_significant_drop]
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

#[derive(Debug, Clone, Default)]
pub(super) struct Manager {
    pub(super) states_by_enum_id: HashMap<ID<Enum>, State<syntax_tree::item::EnumSignature>>,
    pub(super) states_by_variant_id:
        HashMap<ID<symbol::Variant>, State<syntax_tree::item::Variant>>,

    pub(super) states_by_struct_id: HashMap<ID<Struct>, State<syntax_tree::item::Struct>>,
    pub(super) states_by_constant_id: HashMap<ID<Constant>, State<syntax_tree::item::Constant>>,
    pub(super) states_by_type_id: HashMap<ID<Type>, State<syntax_tree::item::Type>>,
    pub(super) states_by_function_id: HashMap<ID<Function>, State<syntax_tree::item::Function>>,

    pub(super) states_by_trait_id:
        HashMap<ID<symbol::Trait>, State<syntax_tree::item::TraitSignature>>,
    pub(super) states_by_trait_function_id:
        HashMap<ID<symbol::TraitFunction>, State<syntax_tree::item::TraitFunction>>,
    pub(super) states_by_trait_type_id:
        HashMap<ID<symbol::TraitType>, State<syntax_tree::item::TraitType>>,
    pub(super) states_by_trait_constant_id:
        HashMap<ID<symbol::TraitConstant>, State<syntax_tree::item::TraitConstant>>,

    pub(super) states_by_negative_implementation_id: HashMap<
        ID<symbol::NegativeImplementation>,
        State<syntax_tree::item::ImplementationSignature>,
    >,
    pub(super) states_by_implementation_id:
        HashMap<ID<symbol::Implementation>, State<syntax_tree::item::ImplementationSignature>>,

    pub(super) states_by_implementation_function_id: HashMap<
        ID<symbol::ImplementationFunction>,
        State<syntax_tree::item::ImplementationFunction>,
    >,
    pub(super) states_by_implementation_type_id:
        HashMap<ID<symbol::ImplementationType>, State<syntax_tree::item::ImplementationType>>,
    pub(super) states_by_implementation_constant_id: HashMap<
        ID<symbol::ImplementationConstant>,
        State<syntax_tree::item::ImplementationConstant>,
    >,

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
                    #[allow(unreachable_patterns)]
                    match $id {
                        $(GlobalID::$name(id) => $self.[<states_by_ $name:snake _id>].remove(&id).is_some()),*,
                        _ => false,
                    }
                }
            };
        }

        remove_state_arm!(
            self,
            global_id,
            Enum,
            Struct,
            Constant,
            Type,
            Function,
            Trait,
            TraitConstant,
            TraitFunction,
            TraitType,
            ImplementationConstant,
            ImplementationType,
            ImplementationConstant,
            Variant
        )
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
            .chain(map_symbol!(TRAIT_FUNCTION))
            .chain(map_symbol!(TRAIT_TYPE))
            .chain(map_symbol!(TRAIT_CONSTANT))
            .chain(map_symbol!(IMPLEMENTATION_FUNCTION))
            .chain(map_symbol!(IMPLEMENTATION_TYPE))
            .chain(map_symbol!(IMPLEMENTATION_CONSTANT))
            .chain(map_symbol!(VARIANT))
    }
}
