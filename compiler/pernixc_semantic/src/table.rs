//! Contains the definition of [`Table`].

use std::collections::HashMap;

use pernixc_syntax::syntax_tree::target::Target;
use pernixc_system::diagnostic::Handler;
use thiserror::Error;

use crate::{
    constant, error,
    symbol::{
        Accessibility, Bounds, Constant, Enum, Function, GenericItem, GenericItemRef, GlobalItem,
        GlobalItemRef, LocalImplementsAssociatedRef, LocalTraitAssociatedRef, Module, ModuleRef,
        Struct, Substitution, Trait, TraitRef, Type, WhereClause,
    },
    ty,
};

mod core;
mod drafting;
mod evaluation;
mod finalizing;
pub mod resolution;
mod state;
mod trait_resolution;
mod transformation;

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

    target_root_module_indices_by_name: HashMap<String, ModuleRef>,

    state_mananger: state::Manager,

    builtin_copy_trait_index: TraitRef,
    builtin_drop_trait_index: TraitRef,
    builtin_into_trait_index: TraitRef,
    builtin_assign_trait_index: TraitRef,
    builtin_assign_restrict_trait_index: TraitRef,
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

            builtin_copy_trait_index: TraitRef(0), // will be overwritten
            builtin_drop_trait_index: TraitRef(0), // will be overwritten
            builtin_into_trait_index: TraitRef(0), // will be overwritten
            builtin_assign_trait_index: TraitRef(0), // will be overwritten
            builtin_assign_restrict_trait_index: TraitRef(0), // will be overwritten
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
            GenericItemRef::Enum(id) => self.enums.get(id.0).map(|x| x as _),
            GenericItemRef::Struct(id) => self.structs.get(id.0).map(|x| x as _),
            GenericItemRef::Type(id) => self.types.get(id.0).map(|x| x as _),
            GenericItemRef::Function(id) => self.functions.get(id.0).map(|x| x as _),
            GenericItemRef::Trait(id) => self.traits.get(id.0).map(|x| x as _),
            GenericItemRef::TraitType(id) => self
                .traits
                .get(id.trait_ref.0)
                .and_then(|x| x.types.get(id.local_ref.0))
                .map(|x| x as _),
            GenericItemRef::TraitFunction(id) => self
                .traits
                .get(id.trait_ref.0)
                .and_then(|x| x.functions.get(id.local_ref.0))
                .map(|x| x as _),
            GenericItemRef::Implements(id) => self
                .traits
                .get(id.trait_ref.0)
                .and_then(|x| x.implements.get(id.local_ref.0))
                .map(|x| x as _),
            GenericItemRef::ImplementsType(id) => self
                .traits
                .get(id.implements_ref.trait_ref.0)
                .and_then(|x| x.implements.get(id.implements_ref.local_ref.0))
                .and_then(|x| x.types.get(id.local_ref.0))
                .map(|x| x as _),
            GenericItemRef::ImplementsFunction(id) => self
                .traits
                .get(id.implements_ref.trait_ref.0)
                .and_then(|x| x.implements.get(id.implements_ref.local_ref.0))
                .and_then(|x| x.functions.get(id.local_ref.0))
                .map(|x| x as _),
        }
    }

    fn get_generic_item_mut(
        &mut self,
        generic_item_ref: GenericItemRef,
    ) -> Option<&mut dyn GenericItem> {
        match generic_item_ref {
            GenericItemRef::Enum(id) => self.enums.get_mut(id.0).map(|x| x as _),
            GenericItemRef::Struct(id) => self.structs.get_mut(id.0).map(|x| x as _),
            GenericItemRef::Type(id) => self.types.get_mut(id.0).map(|x| x as _),
            GenericItemRef::Function(id) => self.functions.get_mut(id.0).map(|x| x as _),
            GenericItemRef::Trait(id) => self.traits.get_mut(id.0).map(|x| x as _),
            GenericItemRef::TraitType(id) => self
                .traits
                .get_mut(id.trait_ref.0)
                .and_then(|x| x.types.get_mut(id.local_ref.0))
                .map(|x| x as _),
            GenericItemRef::TraitFunction(id) => self
                .traits
                .get_mut(id.trait_ref.0)
                .and_then(|x| x.functions.get_mut(id.local_ref.0))
                .map(|x| x as _),
            GenericItemRef::Implements(id) => self
                .traits
                .get_mut(id.trait_ref.0)
                .and_then(|x| x.implements.get_mut(id.local_ref.0))
                .map(|x| x as _),
            GenericItemRef::ImplementsType(id) => self
                .traits
                .get_mut(id.implements_ref.trait_ref.0)
                .and_then(|x| x.implements.get_mut(id.implements_ref.local_ref.0))
                .and_then(|x| x.types.get_mut(id.local_ref.0))
                .map(|x| x as _),
            GenericItemRef::ImplementsFunction(id) => self
                .traits
                .get_mut(id.implements_ref.trait_ref.0)
                .and_then(|x| x.implements.get_mut(id.implements_ref.local_ref.0))
                .and_then(|x| x.functions.get_mut(id.local_ref.0))
                .map(|x| x as _),
        }
    }

    /// Gets the [`Accessibility`] of the given [`GlobalItemRef`].
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn get_accessibility(&self, global_item_ref: GlobalItemRef) -> Option<Accessibility> {
        match global_item_ref {
            GlobalItemRef::Module(id) => self.modules.get(id.0).map(|x| x.accessibility),
            GlobalItemRef::Struct(id) => self.structs.get(id.0).map(|x| x.accessibility),
            GlobalItemRef::Enum(id) => self.enums.get(id.0).map(|x| x.accessibility),
            GlobalItemRef::Variant(id) => {
                if self
                    .enums
                    .get(id.enum_ref.0)
                    .and_then(|x| x.variants.get(id.local_ref.0))
                    .is_some()
                {
                    Some(self.enums[id.enum_ref.0].accessibility)
                } else {
                    None
                }
            }
            GlobalItemRef::Type(id) => self.types.get(id.0).map(|x| x.accessibility),
            GlobalItemRef::Constant(id) => self.constants.get(id.0).map(|x| x.accessibility),
            GlobalItemRef::Function(id) => self.functions.get(id.0).map(|x| x.accessibility),
            GlobalItemRef::Trait(id) => self.traits.get(id.0).map(|x| x.accessibility),
            GlobalItemRef::TraitAssociated(id) => {
                if self
                    .traits
                    .get(id.trait_ref.0)
                    .map(|x| match id.local_ref {
                        LocalTraitAssociatedRef::Function(id) => x.functions.get(id.0).is_some(),
                        LocalTraitAssociatedRef::Type(id) => x.types.get(id.0).is_some(),
                        LocalTraitAssociatedRef::Constant(id) => x.constants.get(id.0).is_some(),
                    })?
                {
                    Some(self.traits[id.trait_ref.0].accessibility)
                } else {
                    None
                }
            }
            GlobalItemRef::Implements(id) => {
                if self
                    .traits
                    .get(id.trait_ref.0)
                    .and_then(|x| x.implements.get(id.local_ref.0))
                    .is_some()
                {
                    Some(self.traits[id.trait_ref.0].accessibility)
                } else {
                    None
                }
            }
            GlobalItemRef::ImplementsAssociated(id) => {
                if self
                    .traits
                    .get(id.implements_ref.trait_ref.0)
                    .and_then(|x| x.implements.get(id.implements_ref.local_ref.0))
                    .map(|x| match id.local_ref {
                        LocalImplementsAssociatedRef::Type(id) => x.types.get(id.0).is_some(),
                        LocalImplementsAssociatedRef::Function(id) => {
                            x.functions.get(id.0).is_some()
                        }
                        LocalImplementsAssociatedRef::Constant(id) => {
                            x.constants.get(id.0).is_some()
                        }
                    })?
                {
                    Some(self.traits[id.implements_ref.trait_ref.0].accessibility)
                } else {
                    None
                }
            }
        }
    }

    /// Gets the ref of the [`Module`] where the given [`GlobalItemRef`] symbol is defined in.
    ///
    /// If the `global_item_ref` itself is a [`Module`], then the ref of the module itself will
    /// be returned.
    #[must_use]
    pub fn get_closet_module_ref(&self, mut global_item_ref: GlobalItemRef) -> Option<ModuleRef> {
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
                let referred_module_id = self.get_closet_module_ref(referred)?;
                let referring_module_id = self.get_closet_module_ref(referring)?;

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

    /// Gets the [`ScopeWalker`] iterator starting from the given [`GlobalItemRef`].
    #[must_use]
    pub fn scope_walker(&self, global_item_ref: GlobalItemRef) -> Option<ScopeWalker> {
        // check if the ref is valid
        self.get_global_item(global_item_ref)?;

        Some(ScopeWalker {
            table: self,
            current_gloabal_item_ref: Some(global_item_ref),
        })
    }

    /// Gets the ref of the target's root [`Module`] where the given [`GlobalItemRef`] symbol is
    /// defined in.
    #[must_use]
    pub fn get_target_root_module_index(&self, mut id: GlobalItemRef) -> Option<ModuleRef> {
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
            GlobalItemRef::Module(id) => self.modules.get(id.0).map(|x| x as _),
            GlobalItemRef::Struct(id) => self.structs.get(id.0).map(|x| x as _),
            GlobalItemRef::Enum(id) => self.enums.get(id.0).map(|x| x as _),
            GlobalItemRef::Trait(id) => self.traits.get(id.0).map(|x| x as _),
            GlobalItemRef::Function(id) => self.functions.get(id.0).map(|x| x as _),
            GlobalItemRef::Type(id) => self.types.get(id.0).map(|x| x as _),
            GlobalItemRef::Constant(id) => self.constants.get(id.0).map(|x| x as _),
            GlobalItemRef::Variant(id) => self
                .enums
                .get(id.enum_ref.0)
                .and_then(|x| x.variants.get(id.local_ref.0))
                .map(|x| x as _),
            GlobalItemRef::Implements(id) => self
                .traits
                .get(id.trait_ref.0)
                .and_then(|x| x.implements.get(id.local_ref.0))
                .map(|x| x as _),
            GlobalItemRef::ImplementsAssociated(id) => self
                .traits
                .get(id.implements_ref.trait_ref.0)
                .and_then(|x| x.implements.get(id.implements_ref.local_ref.0))
                .and_then(|x| match id.local_ref {
                    LocalImplementsAssociatedRef::Type(id) => x.types.get(id.0).map(|x| x as _),
                    LocalImplementsAssociatedRef::Function(id) => {
                        x.functions.get(id.0).map(|x| x as _)
                    }
                    LocalImplementsAssociatedRef::Constant(id) => {
                        x.constants.get(id.0).map(|x| x as _)
                    }
                }),
            GlobalItemRef::TraitAssociated(id) => {
                self.traits
                    .get(id.trait_ref.0)
                    .and_then(|x| match id.local_ref {
                        LocalTraitAssociatedRef::Function(id) => {
                            x.functions.get(id.0).map(|x| x as _)
                        }
                        LocalTraitAssociatedRef::Type(id) => x.types.get(id.0).map(|x| x as _),
                        LocalTraitAssociatedRef::Constant(id) => {
                            x.constants.get(id.0).map(|x| x as _)
                        }
                    })
            }
        }
    }

    /// Gets the type of the [`constant::Constant`] value.
    #[must_use]
    pub fn get_constant_ty(&self, _constant: &constant::Constant) -> Option<ty::Type> { todo!() }

    /// Combining where clause in the table here we assume that everything is in the correct state
    /// and thus, should be able to combine
    fn combine_where_clause(&self, first: &WhereClause, second: &WhereClause) -> WhereClause {
        // combine associated bounds
        let associated_bounds = self
            .combine_associated_bounds(&first.associated_bounds, &second.associated_bounds)
            .expect("The associated bounds of where clauses should be valid")
            .transformed;

        let mut bounds = Bounds::combine(&first.bounds, &second.bounds);
        bounds = self
            .transform_bounds(bounds, &Substitution::default(), &associated_bounds)
            .unwrap()
            .transformed;

        WhereClause {
            associated_bounds,
            bounds,
        }
    }

    /// Gets all the required  up to the scope of the given [`GlobalItemRef`].
    #[must_use]
    pub fn get_active_where_clause(&self, global_item_ref: GlobalItemRef) -> Option<WhereClause> {
        let mut current_where_clause: Option<WhereClause> = None;

        for generic_item_ref in self.scope_walker(global_item_ref)? {
            let Ok(generic_item_ref) = GenericItemRef::try_from(generic_item_ref) else {
                continue;
            };

            let generic_item = self.get_generic_item(generic_item_ref)?;

            if let Some(where_clause) = current_where_clause {
                current_where_clause =
                    Some(self.combine_where_clause(&where_clause, generic_item.where_clause()));
            } else {
                current_where_clause = Some(generic_item.where_clause().clone());
            }
        }

        Some(current_where_clause.unwrap_or_default())
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

/// An iterator walking down the heirarchy scope of a particular [`GlobalItemRef`].
#[derive(Debug, Clone)]
pub struct ScopeWalker<'a> {
    table: &'a Table,
    current_gloabal_item_ref: Option<GlobalItemRef>,
}

impl<'a> ScopeWalker<'a> {
    /// Creates a new scope walker.
    #[must_use]
    pub fn table(&self) -> &'a Table { self.table }
}

impl<'a> Iterator for ScopeWalker<'a> {
    type Item = GlobalItemRef;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current_gloabal_item_ref {
            Some(current_id) => {
                let next_id = self.table.get_global_item(current_id).unwrap().parent();
                self.current_gloabal_item_ref = next_id;
                Some(current_id)
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod tests;
