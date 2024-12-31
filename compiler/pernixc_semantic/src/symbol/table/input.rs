//! Contains the definition of [`Input`].

use std::collections::HashMap;

use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree::{self, target::Target};
use serde::{Deserialize, Serialize};

use crate::{
    arena::ID,
    error,
    symbol::{
        component::{self, parent::Parent},
        Accessibility, Global, ItemID, Module, TargetID,
    },
};

/// Contains the input components information required for building the full
/// table.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Input {
    symbol_names: HashMap<Global<ItemID>, String>,
    root_module_id: HashMap<TargetID, ID<Module>>,

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

    /// Returns the [`Module`] ID that is closest to the given [`ItemID`]
    /// (including itself).
    ///
    /// # Panics
    ///
    /// If the given [`ItemID`] is not a module.
    #[must_use]
    pub fn get_closet_module_id(
        &self,
        mut item_id: Global<ItemID>,
    ) -> Global<ID<Module>> {
        // including the item_id itself
        loop {
            if let ItemID::Module(module_id) = item_id.id {
                return Global::new(item_id.target_id, module_id);
            }

            item_id = self
                .get_parent_item_id(item_id)
                .expect("should've found at least one module");
        }
    }

    /// Gets the parent [`ItemID`] of the given item symbol.
    ///
    /// If [`None`] is returned, it means the item symbol is a root module
    /// symbol.
    ///
    /// # Panics
    ///
    /// If the `item_id` is not a valid item symbol ID.
    #[must_use]
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

    /// Creates the [`Accessibility`] based on where the symbol is defined in
    /// and the access modifier syntax tree.
    ///
    /// # Panics
    ///
    /// If the parent item ID is not found.
    #[must_use]
    pub fn create_accessibility(
        &self,
        parent_id: Global<ItemID>,
        access_modifier: &syntax_tree::AccessModifier,
    ) -> Accessibility {
        match access_modifier {
            syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
            syntax_tree::AccessModifier::Private(_) => {
                let parent_module_id = self.get_closet_module_id(parent_id);

                Accessibility::Scoped(parent_module_id.id)
            }
            syntax_tree::AccessModifier::Internal(_) => {
                Accessibility::Scoped(self.root_module_id[&parent_id.target_id])
            }
        }
    }
}
