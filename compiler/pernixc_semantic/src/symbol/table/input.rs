//! Contains the definition of [`Input`].

use std::collections::HashMap;

use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree::target::Target;
use serde::{Deserialize, Serialize};

use crate::{
    error,
    symbol::{component, component::parent::Parent, Global, ItemID},
};

/// Contains the input components information required for building the full
/// table.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Input {
    symbol_names: HashMap<Global<ItemID>, String>,

    /// The syntax tree map won't be serialized
    #[serde(skip)]
    pub(in crate::symbol) syntax_tree_map: component::syntax_tree::Map,
    pub(in crate::symbol) accessibility_map: component::accessibility::Map,
    pub(in crate::symbol) parent_map: component::parent::Map,
    pub(in crate::symbol) member_map: component::member::Map,
    pub(in crate::symbol) import_map: component::import::Map,
}

impl Input {
    /// Builds the [`Input`] information.
    ///
    /// # Panics
    ///
    /// If the `target_id` is already used.
    pub fn build(
        &mut self,
        target_syn: Target,
        target_id: usize,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        // make sure the target_id is not used
        assert!(self.symbol_names.keys().all(|x| x
            .target_id
            .as_normal()
            .map_or(false, |x| *x != target_id)));
    }

    /// Gets the parent [`ItemID`] of the given item symbol.
    ///
    /// If [None] is returned, it means the item symbol is a root module symbol.
    ///
    /// # Panics
    ///
    /// If the `item_id` is not a valid item symbol ID.
    pub fn get_parent_item_id(
        &self,
        item_id: Global<ItemID>,
    ) -> Option<Global<ItemID>> {
        macro_rules! impl_get_parent_item_id {
            ($(($name:ident, $test:ident)),*) => {
                match item_id.id {
                    $(
                        ItemID::$name(id) => impl_match_arm!($name, id, $test),
                    )*
                }
            };
        }

        macro_rules! impl_match_arm {
            ($name:ident, $id:ident, true) => {
                self.try_get_input::<Parent<_>, _>(Global::new(
                    item_id.target_id,
                    $id,
                ))
                .map(|x| x.0.into())
            };

            ($name:ident, $id:ident, false) => {
                Some(
                    self.get_input::<Parent<_>, _>(Global::new(
                        item_id.target_id,
                        $id,
                    ))
                    .0
                    .into(),
                )
            };
        }

        let parent_item_id: Option<ItemID> = impl_get_parent_item_id!(
            (Module, true),
            (Struct, false),
            (Enum, false),
            (Trait, false),
            (Type, false),
            (Constant, false),
            (Function, false),
            (Variant, false),
            (TraitType, false),
            (TraitFunction, false),
            (TraitConstant, false),
            (PositiveTraitImplementation, false),
            (NegativeTraitImplementation, false),
            (TraitImplementationFunction, false),
            (TraitImplementationType, false),
            (TraitImplementationConstant, false),
            (AdtImplementation, false),
            (AdtImplementationFunction, false),
            (Marker, false),
            (PositiveMarkerImplementation, false),
            (NegativeMarkerImplementation, false)
        );

        parent_item_id.map(|x| Global::new(item_id.target_id, x))
    }
}
