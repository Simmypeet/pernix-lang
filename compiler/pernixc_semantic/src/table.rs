use std::collections::HashMap;

use parking_lot::RwLock;
use pernixc_syntax::syntax_tree::target::Target;
use pernixc_system::diagnostic::Handler;
use thiserror::Error;

use crate::{
    error,
    symbol::{Constant, Enum, Function, GlobalItem, GlobalItemRef, Module, Struct, Trait, Type},
};

mod core;
mod drafting;
pub mod resolution;
mod state;

/// Contains all the symbols and information about the program.
///
/// [`Table`] is the most important data structure in the semantic analysis phase. Most operations
/// in the semantic analysis phase will require access to the [`Table`] in order to perform their
/// work.
#[derive(Debug)]
pub struct Table {
    modules: Vec<Module>,
    types: Vec<Type>,
    functions: Vec<Function>,
    enums: Vec<Enum>,
    structs: Vec<Struct>,
    traits: Vec<Trait>,
    constants: Vec<Constant>,

    target_root_module_indices_by_name: HashMap<String, usize>,

    state_mananger: state::Manager,
}

impl Table {
    fn new() -> Self {
        Self {
            modules: Vec::new(),
            types: Vec::new(),
            functions: Vec::new(),
            enums: Vec::new(),
            structs: Vec::new(),
            traits: Vec::new(),
            constants: Vec::new(),
            target_root_module_indices_by_name: HashMap::new(),
            state_mananger: state::Manager::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
pub enum BuildError {
    #[error("multiple targets with the same name `{0}` were found")]
    TargetNameDuplication(String),
    #[error("target with the name `@core` was found")]
    TargetNamedCore,
}

impl Table {
    /// # Errors
    ///
    /// - [`BuildError::TargetNameDuplication`]: if mutliple targets with the same name were found.
    /// - [`BuildError::TargetNamedCore`]: if a target named `@core` was found.
    pub fn build(
        targets: impl Iterator<Item = Target>,
        handler: &impl Handler<error::Error>,
    ) -> Result<Self, BuildError> {
        let mut table = Self::new();

        table.create_core_module();
        table.draft_targets(targets, handler)?;

        Ok(table)
    }

    #[must_use]
    pub fn get_global_item(&self, global_item_ref: GlobalItemRef) -> Option<&dyn GlobalItem> {
        match global_item_ref {
            GlobalItemRef::Module(id) => self.modules.get(id).map(|x| x as _),
            GlobalItemRef::Struct(id) => self.structs.get(id).map(|x| x as _),
            GlobalItemRef::Enum(id) => self.enums.get(id).map(|x| x as _),
            GlobalItemRef::Trait(id) => self.traits.get(id).map(|x| x as _),
            GlobalItemRef::Function(id) => self.functions.get(id).map(|x| x as _),
            GlobalItemRef::Type(id) => self.types.get(id).map(|x| x as _),
            GlobalItemRef::Constant(id) => self.constants.get(id).map(|x| x as _),
            GlobalItemRef::Variant(id) => self
                .enums
                .get(id.parent_index)
                .and_then(|x| x.variants.get(id.associated_item_index))
                .map(|x| x as _),
            GlobalItemRef::TraitFunction(id) => self
                .traits
                .get(id.parent_index)
                .and_then(|x| x.functions.get(id.associated_item_index))
                .map(|x| x as _),
            GlobalItemRef::TraitType(id) => self
                .traits
                .get(id.parent_index)
                .and_then(|x| x.types.get(id.associated_item_index))
                .map(|x| x as _),
            GlobalItemRef::TraitConstant(id) => self
                .traits
                .get(id.parent_index)
                .and_then(|x| x.constants.get(id.associated_item_index))
                .map(|x| x as _),
        }
    }

    #[must_use]
    pub fn get_qualified_name(&self, mut global_item_ref: GlobalItemRef) -> Option<String> {
        let mut name = self.get_global_item(global_item_ref)?.name().to_string();

        while let Some(parent) = self.get_global_item(global_item_ref)?.parent() {
            name.insert_str(0, "::");
            name.insert_str(0, self.get_global_item(parent)?.name());

            global_item_ref = parent;
        }

        Some(name)
    }
}
