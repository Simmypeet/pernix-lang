//! Contains the definition of [`Table`].

use std::collections::{hash_map::Entry, HashMap};

use pernixc_syntax::syntax_tree::target::Target;
use pernixc_system::diagnostic::Handler;
use thiserror::Error;

use crate::{
    error,
    symbol::{
        Accessibility, Constant, Enum, Function, GenericItem, GenericItemRef, GlobalItem,
        GlobalItemRef, HigherRankedLifetime, HigherRankedableLifetime, ImplementsAssociatedRef,
        Index, Module, Struct, Trait, TraitAssociatedRef, TraitBound, Type, WhereClause,
    },
};

mod core;
mod drafting;
mod finalizing;
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

    target_root_module_indices_by_name: HashMap<String, Index>,

    state_mananger: state::Manager,

    builtin_copy_trait_index: Index,
    builtin_drop_trait_index: Index,
    builtin_into_trait_index: Index,
    builtin_assign_trait_index: Index,
    builtin_assign_restrict_trait_index: Index,
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

            builtin_copy_trait_index: 0,            // will be overwritten
            builtin_drop_trait_index: 0,            // will be overwritten
            builtin_into_trait_index: 0,            // will be overwritten
            builtin_assign_trait_index: 0,          // will be overwritten
            builtin_assign_restrict_trait_index: 0, // will be overwritten
        }
    }
}

/// An error that can occur during the building of the [`Table`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum BuildError {
    #[error("multiple targets with the same name `{0}` were found")]
    TargetNameDuplication(String),
    #[error("target with the name `@core` was found")]
    TargetNamedCore,
}

/// An error that can occur when interacting with the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("fatal semantic error encountered, the analysis aborted")]
    SemanticError,

    #[error("there were invalid references passed to the function")]
    InvalidReference,
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

        while let Some(drafted_symbol) = table.state_mananger.next_drafted_symbol_ref() {
            let _ = table.finalize_symbol_if_required(drafted_symbol, handler);
        }

        Ok(table)
    }

    /// Gets the [`GenericItem`] symbol from the given [`GenericItemRef`].
    #[must_use]
    pub fn get_generic_item(&self, generic_item_ref: GenericItemRef) -> Option<&dyn GenericItem> {
        match generic_item_ref {
            GenericItemRef::Enum(id) => self.enums.get(id).map(|x| x as _),
            GenericItemRef::Struct(id) => self.structs.get(id).map(|x| x as _),
            GenericItemRef::Type(id) => self.types.get(id).map(|x| x as _),
            GenericItemRef::Function(id) => self.functions.get(id).map(|x| x as _),
            GenericItemRef::Trait(id) => self.traits.get(id).map(|x| x as _),
            GenericItemRef::TraitType(id) => self
                .traits
                .get(id.parent_ref)
                .and_then(|x| x.types.get(id.associated_item_ref))
                .map(|x| x as _),
            GenericItemRef::TraitFunction(id) => self
                .traits
                .get(id.parent_ref)
                .and_then(|x| x.functions.get(id.associated_item_ref))
                .map(|x| x as _),
            GenericItemRef::Implements(id) => self
                .traits
                .get(id.parent_ref)
                .and_then(|x| x.implements.get(id.associated_item_ref))
                .map(|x| x as _),
            GenericItemRef::ImplementsType(id) => self
                .traits
                .get(id.parent_ref.parent_ref)
                .and_then(|x| x.implements.get(id.parent_ref.associated_item_ref))
                .and_then(|x| x.types.get(id.associated_item_ref))
                .map(|x| x as _),
            GenericItemRef::ImplementsFunction(id) => self
                .traits
                .get(id.parent_ref.parent_ref)
                .and_then(|x| x.implements.get(id.parent_ref.associated_item_ref))
                .and_then(|x| x.functions.get(id.associated_item_ref))
                .map(|x| x as _),
        }
    }

    fn get_generic_item_mut(
        &mut self,
        generic_item_ref: GenericItemRef,
    ) -> Option<&mut dyn GenericItem> {
        match generic_item_ref {
            GenericItemRef::Enum(id) => self.enums.get_mut(id).map(|x| x as _),
            GenericItemRef::Struct(id) => self.structs.get_mut(id).map(|x| x as _),
            GenericItemRef::Type(id) => self.types.get_mut(id).map(|x| x as _),
            GenericItemRef::Function(id) => self.functions.get_mut(id).map(|x| x as _),
            GenericItemRef::Trait(id) => self.traits.get_mut(id).map(|x| x as _),
            GenericItemRef::TraitType(id) => self
                .traits
                .get_mut(id.parent_ref)
                .and_then(|x| x.types.get_mut(id.associated_item_ref))
                .map(|x| x as _),
            GenericItemRef::TraitFunction(id) => self
                .traits
                .get_mut(id.parent_ref)
                .and_then(|x| x.functions.get_mut(id.associated_item_ref))
                .map(|x| x as _),
            GenericItemRef::Implements(id) => self
                .traits
                .get_mut(id.parent_ref)
                .and_then(|x| x.implements.get_mut(id.associated_item_ref))
                .map(|x| x as _),
            GenericItemRef::ImplementsType(id) => self
                .traits
                .get_mut(id.parent_ref.parent_ref)
                .and_then(|x| x.implements.get_mut(id.parent_ref.associated_item_ref))
                .and_then(|x| x.types.get_mut(id.associated_item_ref))
                .map(|x| x as _),
            GenericItemRef::ImplementsFunction(id) => self
                .traits
                .get_mut(id.parent_ref.parent_ref)
                .and_then(|x| x.implements.get_mut(id.parent_ref.associated_item_ref))
                .and_then(|x| x.functions.get_mut(id.associated_item_ref))
                .map(|x| x as _),
        }
    }

    /// Gets the [`Accessibility`] of the given [`GlobalItemRef`].
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn get_accessibility(&self, global_item_ref: GlobalItemRef) -> Option<Accessibility> {
        match global_item_ref {
            GlobalItemRef::Module(id) => self.modules.get(id).map(|x| x.accessibility),
            GlobalItemRef::Struct(id) => self.structs.get(id).map(|x| x.accessibility),
            GlobalItemRef::Enum(id) => self.enums.get(id).map(|x| x.accessibility),
            GlobalItemRef::Variant(id) => {
                if self
                    .enums
                    .get(id.parent_ref)
                    .and_then(|x| x.variants.get(id.associated_item_ref))
                    .is_some()
                {
                    Some(self.enums[id.parent_ref].accessibility)
                } else {
                    None
                }
            }
            GlobalItemRef::Type(id) => self.types.get(id).map(|x| x.accessibility),
            GlobalItemRef::Constant(id) => self.constants.get(id).map(|x| x.accessibility),
            GlobalItemRef::Function(id) => self.functions.get(id).map(|x| x.accessibility),
            GlobalItemRef::Trait(id) => self.traits.get(id).map(|x| x.accessibility),
            GlobalItemRef::TraitAssociated(id) => {
                if self
                    .traits
                    .get(id.parent_ref)
                    .map(|x| match id.associated_item_ref {
                        TraitAssociatedRef::Function(id) => x.functions.get(id).is_some(),
                        TraitAssociatedRef::Type(id) => x.types.get(id).is_some(),
                        TraitAssociatedRef::Constant(id) => x.constants.get(id).is_some(),
                    })?
                {
                    Some(self.traits[id.parent_ref].accessibility)
                } else {
                    None
                }
            }
            GlobalItemRef::Implements(id) => {
                if self
                    .traits
                    .get(id.parent_ref)
                    .and_then(|x| x.implements.get(id.associated_item_ref))
                    .is_some()
                {
                    Some(self.traits[id.parent_ref].accessibility)
                } else {
                    None
                }
            }
            GlobalItemRef::ImplementsAssociated(id) => {
                if self
                    .traits
                    .get(id.parent_ref.parent_ref)
                    .and_then(|x| x.implements.get(id.parent_ref.associated_item_ref))
                    .map(|x| match id.associated_item_ref {
                        ImplementsAssociatedRef::Type(id) => x.types.get(id).is_some(),
                        ImplementsAssociatedRef::Function(id) => x.functions.get(id).is_some(),
                        ImplementsAssociatedRef::Constant(id) => x.constants.get(id).is_some(),
                    })?
                {
                    Some(self.traits[id.parent_ref.parent_ref].accessibility)
                } else {
                    None
                }
            }
        }
    }

    /// Gets the index of the [`Module`] where the given [`GlobalItemRef`] symbol is defined in.
    ///
    /// If the `global_item_ref` itself is a [`Module`], then the index of the module itself will
    /// be returned.
    #[must_use]
    pub fn get_closet_module_index(&self, mut global_item_ref: GlobalItemRef) -> Option<Index> {
        let _ = self.get_global_item(global_item_ref)?;

        loop {
            if let GlobalItemRef::Module(index) = global_item_ref {
                return Some(index);
            }

            global_item_ref = self
                .get_global_item(global_item_ref)
                .expect("should be a valid reference")
                .parent()
                .expect("should have a parent symbol");
        }
    }

    /// Checks if a particular global symbol is accessible from another global symbol.
    ///
    /// # Parameters
    ///
    /// - `referred`: the global symbol that is being referred to.
    /// - `referring`: the global symbol that is referring to the `referred` symbol.
    #[must_use]
    pub fn symbol_accessible(
        &self,
        referred: GlobalItemRef,
        referring: GlobalItemRef,
    ) -> Option<bool> {
        match self.get_accessibility(referred)? {
            Accessibility::Public => {
                // PEDANTIC: check if the referring is valid
                let _ = self.get_global_item(referring)?;

                Some(true)
            }
            Accessibility::Private => {
                let referred_module_id = self.get_closet_module_index(referred)?;
                let referring_module_id = self.get_closet_module_index(referring)?;

                // if same module, it is accessible
                if referred_module_id == referring_module_id {
                    return Some(true);
                }

                let mut current_referrer_parent_id = GlobalItemRef::Module(referring_module_id);

                while let Some(parent_id) = self
                    .get_global_item(current_referrer_parent_id)
                    .expect("should've been a valid ref")
                    .parent()
                {
                    match parent_id {
                        GlobalItemRef::Module(module_id) if module_id == referred_module_id => {
                            return Some(true);
                        }
                        _ => {
                            current_referrer_parent_id = parent_id;
                        }
                    }
                }

                Some(false)
            }
            Accessibility::Internal => Some(
                self.get_target_root_module_index(referred)?
                    == self.get_target_root_module_index(referred)?,
            ),
        }
    }

    /// Gets the index of the target's root [`Module`] where the given [`GlobalItemRef`] symbol is
    /// defined in.
    #[must_use]
    pub fn get_target_root_module_index(&self, mut id: GlobalItemRef) -> Option<Index> {
        while let Some(parent_id) = self.get_global_item(id)?.parent() {
            id = parent_id;
        }

        Some(
            id.into_module()
                .expect("It should be a module at the root."),
        )
    }

    /// Gets the [`GlobalItem`] symbol from the given [`GlobalItemRef`].
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
                .get(id.parent_ref)
                .and_then(|x| x.variants.get(id.associated_item_ref))
                .map(|x| x as _),
            GlobalItemRef::Implements(id) => self
                .traits
                .get(id.parent_ref)
                .and_then(|x| x.implements.get(id.associated_item_ref))
                .map(|x| x as _),
            GlobalItemRef::ImplementsAssociated(id) => self
                .traits
                .get(id.parent_ref.parent_ref)
                .and_then(|x| x.implements.get(id.parent_ref.associated_item_ref))
                .and_then(|x| match id.associated_item_ref {
                    ImplementsAssociatedRef::Type(id) => x.types.get(id).map(|x| x as _),
                    ImplementsAssociatedRef::Function(id) => x.functions.get(id).map(|x| x as _),
                    ImplementsAssociatedRef::Constant(id) => x.constants.get(id).map(|x| x as _),
                }),
            GlobalItemRef::TraitAssociated(id) => {
                self.traits
                    .get(id.parent_ref)
                    .and_then(|x| match id.associated_item_ref {
                        TraitAssociatedRef::Function(id) => x.functions.get(id).map(|x| x as _),
                        TraitAssociatedRef::Type(id) => x.types.get(id).map(|x| x as _),
                        TraitAssociatedRef::Constant(id) => x.constants.get(id).map(|x| x as _),
                    })
            }
        }
    }

    fn trait_bound_is_subset_or_equal(target: &TraitBound, source: &TraitBound) -> bool {
        if !(target.trait_index == source.trait_index
            && target.type_substituions == source.type_substituions
            && target.constant_substituions == source.constant_substituions)
        {
            return false;
        }

        assert_eq!(
            target.lifetime_substituions.len(),
            source.lifetime_substituions.len()
        );

        let mut source_higher_ranked_lifetime_map: HashMap<
            HigherRankedLifetime,
            HigherRankedableLifetime,
        > = HashMap::new();

        for (target_lt, source_lt) in target
            .lifetime_substituions
            .iter()
            .copied()
            .zip(source.lifetime_substituions.iter().copied())
        {
            match (target_lt, source_lt) {
                // arbitrary lifetime check
                (
                    HigherRankedableLifetime::Regular(target),
                    HigherRankedableLifetime::Regular(source),
                ) => {
                    if target != source {
                        return false;
                    }
                }
                // maps the higher ranked trait bound of source to target lt
                (target_lt, HigherRankedableLifetime::HigherRanked(source)) => {
                    match source_higher_ranked_lifetime_map.entry(source) {
                        Entry::Occupied(existing) => {
                            if *existing.get() != target_lt {
                                return false;
                            }
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(target_lt);
                        }
                    }
                }
                _ => return false,
            }
        }

        true
    }

    fn combine_where_clause(target: &mut WhereClause, source: &WhereClause) {
        // combine associated bound
        for (trait_associated_ty, bound) in &source.associated_type_bounds {
            assert!(target
                .associated_type_bounds
                .insert(trait_associated_ty.clone(), bound.clone())
                .is_none());
        }
        for (trait_associated_constant, bound) in &source.associated_constant_bounds {
            assert!(target
                .associated_constant_bounds
                .insert(trait_associated_constant.clone(), bound.clone())
                .is_none());
        }

        // combine lifetime bound
        for (operand, bound) in &source.lifetime_bounds {
            let bounds = target.lifetime_bounds.entry(operand.clone()).or_default();

            for bound in bound {
                bounds.insert(*bound);
            }
        }

        // combine trait bound
        for (trait_bound, is_const) in &source.trait_bounds {
            let mut is_subset_or_equal = false;
            for (existing_trait_bound, existing_is_const) in &target.trait_bounds {
                if Self::trait_bound_is_subset_or_equal(trait_bound, existing_trait_bound)
                    && (!*is_const || *existing_is_const)
                {
                    is_subset_or_equal = true;
                    break;
                }
            }

            // there is already a superset trait bound, so we don't need to add this trait bound
            if is_subset_or_equal {
                continue;
            }

            let mut removing_trait_bounds = Vec::new();

            for (existing_trait_bound, existing_is_const) in &mut target.trait_bounds {
                if Self::trait_bound_is_subset_or_equal(existing_trait_bound, trait_bound)
                    && (!*existing_is_const || *is_const)
                {
                    removing_trait_bounds.push(existing_trait_bound.clone());
                }
            }

            for removing_trait_bound in removing_trait_bounds {
                assert!(target.trait_bounds.remove(&removing_trait_bound).is_some());
            }

            assert!(target
                .trait_bounds
                .insert(trait_bound.clone(), *is_const)
                .is_none());
        }
    }

    /// Gets all the required [`WhereClause`] up to the scope of the given [`GlobalItemRef`].
    #[must_use]
    pub fn get_active_where_clause(&self, global_item_ref: GlobalItemRef) -> Option<WhereClause> {
        let mut result_where_clause = WhereClause::default();

        let mut global_item_ref = Some(global_item_ref);

        while let Some(current_global_item_ref) = global_item_ref {
            // combine the where clause
            if let Ok(current_generic_item_ref) = GenericItemRef::try_from(current_global_item_ref)
            {
                Self::combine_where_clause(
                    &mut result_where_clause,
                    self.get_generic_item(current_generic_item_ref)?
                        .where_clause(),
                );
            }

            global_item_ref = self.get_global_item(current_global_item_ref)?.parent();
        }

        Some(result_where_clause)
    }

    /// Gets the qualified name of the given [`GlobalItemRef`].
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

#[cfg(test)]
mod tests;
