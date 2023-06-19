//! Contains the definition of [`Table`], the symbol table of the compiler.

use std::{collections::HashMap, hash::Hash};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_syntax::syntax_tree::target::Target;
use pernixc_system::{
    arena::{self, Arena},
    diagnostic::Handler,
};

use crate::{
    error, Accessibility, Enum, EnumVariant, Field, Function, Genericable, GenericableID, Global,
    GlobalID, Implements, ImplementsFunction, ImplementsType, LifetimeParameter, Module, Parameter,
    Scoped, ScopedID, Struct, Substitution, Symbol, Trait, TraitFunction, TraitType, Type,
    TypeParameter, ID,
};

mod input;

mod core;
mod drafting;
mod finalizing;
mod module;
// mod resolution;

/// Represents a symbol table of the compiler.
#[derive(Debug, Clone)]
pub struct Table {
    modules: Arena<Module>,
    structs: Arena<Struct>,
    enums: Arena<Enum>,
    enum_variants: Arena<EnumVariant>,
    functions: Arena<Function>,
    types: Arena<Type>,
    fields: Arena<Field>,
    parameters: Arena<Parameter>,
    traits: Arena<Trait>,
    trait_types: Arena<TraitType>,
    type_parameters: Arena<TypeParameter>,
    lifetime_parameters: Arena<LifetimeParameter>,
    trait_functions: Arena<TraitFunction>,
    implements: Arena<Implements>,
    implements_types: Arena<ImplementsType>,
    implements_functions: Arena<ImplementsFunction>,

    target_root_module_ids_by_name: HashMap<String, arena::ID<Module>>,
}

impl Table {
    /// Gets a reference to the [`Global`] symbol from the given ID.
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    pub fn get_global(&self, global_id: GlobalID) -> Result<&dyn Global, arena::Error> {
        match global_id {
            GlobalID::Module(s) => self.modules.get(s).map(|x| x as _).ok_or(arena::Error),
            GlobalID::Struct(s) => self.structs.get(s).map(|x| x as _).ok_or(arena::Error),
            GlobalID::Enum(s) => self.enums.get(s).map(|x| x as _).ok_or(arena::Error),
            GlobalID::EnumVariant(s) => self
                .enum_variants
                .get(s)
                .map(|x| x as _)
                .ok_or(arena::Error),
            GlobalID::Function(s) => self.functions.get(s).map(|x| x as _).ok_or(arena::Error),
            GlobalID::Type(s) => self.types.get(s).map(|x| x as _).ok_or(arena::Error),
            GlobalID::Trait(s) => self.traits.get(s).map(|x| x as _).ok_or(arena::Error),
            GlobalID::TraitFunction(s) => self
                .trait_functions
                .get(s)
                .map(|x| x as _)
                .ok_or(arena::Error),
            GlobalID::TraitType(s) => self.trait_types.get(s).map(|x| x as _).ok_or(arena::Error),
        }
    }

    /// Gets a reference to the [`Genericable`] symbol from the given ID.
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    pub fn get_genericable(
        &self,
        genericable_id: GenericableID,
    ) -> Result<&dyn Genericable, arena::Error> {
        match genericable_id {
            GenericableID::Struct(s) => self.structs.get(s).map(|x| x as _).ok_or(arena::Error),
            GenericableID::Function(s) => self.functions.get(s).map(|x| x as _).ok_or(arena::Error),
            GenericableID::Implements(s) => {
                self.implements.get(s).map(|x| x as _).ok_or(arena::Error)
            }
            GenericableID::Trait(s) => self.traits.get(s).map(|x| x as _).ok_or(arena::Error),
            GenericableID::TraitType(s) => {
                self.trait_types.get(s).map(|x| x as _).ok_or(arena::Error)
            }
            GenericableID::TraitFunction(s) => self
                .trait_functions
                .get(s)
                .map(|x| x as _)
                .ok_or(arena::Error),
            GenericableID::Type(s) => self.types.get(s).map(|x| x as _).ok_or(arena::Error),
            GenericableID::ImplementsType(s) => self
                .implements_types
                .get(s)
                .map(|x| x as _)
                .ok_or(arena::Error),
            GenericableID::ImplementsFunction(s) => self
                .implements_functions
                .get(s)
                .map(|x| x as _)
                .ok_or(arena::Error),
        }
    }

    /// Gets a reference to the [`Symbol`] from the given ID.
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    pub fn get_symbol(&self, id: ID) -> Result<&dyn Symbol, arena::Error> {
        match id {
            ID::Module(s) => self.modules.get(s).map(|x| x as _).ok_or(arena::Error),
            ID::Struct(s) => self.structs.get(s).map(|x| x as _).ok_or(arena::Error),
            ID::Enum(s) => self.enums.get(s).map(|x| x as _).ok_or(arena::Error),
            ID::EnumVariant(s) => self
                .enum_variants
                .get(s)
                .map(|x| x as _)
                .ok_or(arena::Error),
            ID::Function(s) => self.functions.get(s).map(|x| x as _).ok_or(arena::Error),
            ID::Type(s) => self.types.get(s).map(|x| x as _).ok_or(arena::Error),
            ID::Field(s) => self.fields.get(s).map(|x| x as _).ok_or(arena::Error),
            ID::Parameter(s) => self.parameters.get(s).map(|x| x as _).ok_or(arena::Error),
            ID::Trait(s) => self.traits.get(s).map(|x| x as _).ok_or(arena::Error),
            ID::TraitType(s) => self.trait_types.get(s).map(|x| x as _).ok_or(arena::Error),
            ID::TypeParameter(s) => self
                .type_parameters
                .get(s)
                .map(|x| x as _)
                .ok_or(arena::Error),
            ID::LifetimeParameter(s) => self
                .lifetime_parameters
                .get(s)
                .map(|x| x as _)
                .ok_or(arena::Error),
            ID::TraitFunction(s) => self
                .trait_functions
                .get(s)
                .map(|x| x as _)
                .ok_or(arena::Error),
            ID::Implements(s) => self.implements.get(s).map(|x| x as _).ok_or(arena::Error),
            ID::ImplementsType(s) => self
                .implements_types
                .get(s)
                .map(|x| x as _)
                .ok_or(arena::Error),
            ID::ImplementsFunction(s) => self
                .implements_functions
                .get(s)
                .map(|x| x as _)
                .ok_or(arena::Error),
        }
    }

    /// Gets a reference to the [`Scoped`] symbol from the given ID.
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    pub fn get_scoped(&self, scoped_id: ScopedID) -> Result<&dyn Scoped, arena::Error> {
        match scoped_id {
            ScopedID::Module(s) => self.modules.get(s).map(|x| x as _).ok_or(arena::Error),
            ScopedID::Enum(s) => self.enums.get(s).map(|x| x as _).ok_or(arena::Error),
            ScopedID::Trait(s) => self.traits.get(s).map(|x| x as _).ok_or(arena::Error),
        }
    }

    /// Gets the fully qualified name of the given [`GlobalID`].
    ///
    /// The name doesn't include generics.
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    pub fn get_qualified_name(&self, mut global_id: GlobalID) -> Result<String, arena::Error> {
        let mut current_name = self.get_global(global_id)?.name().to_owned();

        while let Some(parent_symbol_id) = self.get_global(global_id)?.parent_symbol() {
            let parent_global_symbol_id: GlobalID = parent_symbol_id
                .try_into()
                .expect("Parent symbol ID returned by `Global` must belong to `GlobalID`");

            current_name.insert_str(0, "::");
            current_name.insert_str(0, self.get_global(parent_global_symbol_id)?.name());

            global_id = parent_global_symbol_id;
        }

        Ok(current_name)
    }
}

/// Is an error that occurs when a target name duplication is detected while building a [`Table`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[error("Target name duplication detected!")]
pub struct TargetDuplicationError {
    /// The duplicated target name
    pub duplication_target_names: Vec<String>,
}

/// Is an error when found a target named `@core`, which is reserved for the core module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[error("Found a target named `@core`, which is reserved for the core module.")]
pub struct CoreTargetNameError;

/// Is an error returned by [`Table::build`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[allow(missing_docs)]
pub enum BuildError {
    #[error("{0}")]
    TargetDuplication(TargetDuplicationError),

    #[error("{0}")]
    CoreTargetName(CoreTargetNameError),
}

/// Encounters a fatal semantic error while working with [`Table`].
///
/// The [`crate::error::Error`]s are reported to the [`Handler`] passed to the function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[error("Encounters a fatal semantic error ")]
pub struct FatalSemanticError;

/// Is an error returned by various methods in the [`Table`].
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error, From, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Error {
    #[error("{0}")]
    FatalSementic(FatalSemanticError),

    #[error("{0}")]
    InvalidID(arena::Error),

    #[error(
        "The syntax tree is malformed, which prevents the retrieving the span of the syntax tree."
    )]
    InvalidSyntaxTree(pernixc_source::SpanError),
}

/// Represents the result of a symbol resolution from the [`Table`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Resolution {
    /// The found symbol.
    pub symbol: GlobalID,

    /// Contains generics substitutions made during the resolution.
    pub substitution: Substitution,
}

impl Table {
    /// Finds a symbol by the given path.
    ///
    /// The search starts from the root of the table.
    pub fn find_by_path<'a>(&self, path: impl Iterator<Item = &'a str>) -> Option<GlobalID> {
        let mut current_symbol: Option<GlobalID> = None;

        for path in path {
            if let Some(id) = current_symbol {
                let Ok(scoped_id) = ScopedID::try_from(id) else {
                    break;
                };

                let scoped_symbol = self.get_scoped(scoped_id).unwrap();
                current_symbol = Some(scoped_symbol.get_child_id_by_name(path)?);
            } else {
                current_symbol = Some(
                    self.target_root_module_ids_by_name
                        .get(path)
                        .copied()?
                        .into(),
                );
            }
        }

        current_symbol
    }

    /// Gets the accessibility of the given [`GlobalID`] symbol.
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    pub fn get_accessibility(&self, global_id: GlobalID) -> Result<Accessibility, arena::Error> {
        match global_id {
            GlobalID::Module(module_id) => Ok(self
                .modules
                .get(module_id)
                .ok_or(arena::Error)?
                .accessibility),
            GlobalID::Struct(struct_id) => Ok(self
                .structs
                .get(struct_id)
                .ok_or(arena::Error)?
                .accessibility),
            GlobalID::Enum(enum_id) => {
                Ok(self.enums.get(enum_id).ok_or(arena::Error)?.accessibility)
            }
            // Accessibility of enum variants is the same as the parent enum.
            GlobalID::EnumVariant(enum_variant_id) => Ok(self.enums[self
                .enum_variants
                .get(enum_variant_id)
                .ok_or(arena::Error)?
                .parent_enum_id]
                .accessibility),
            GlobalID::Function(function_id) => Ok(self
                .functions
                .get(function_id)
                .ok_or(arena::Error)?
                .accessibility),
            GlobalID::Type(type_id) => {
                Ok(self.types.get(type_id).ok_or(arena::Error)?.accessibility)
            }
            GlobalID::Trait(trait_id) => {
                Ok(self.traits.get(trait_id).ok_or(arena::Error)?.accessibility)
            }
            GlobalID::TraitFunction(trait_function_id) => Ok(self.traits[self
                .trait_functions
                .get(trait_function_id)
                .ok_or(arena::Error)?
                .parent_trait_id]
                .accessibility),
            GlobalID::TraitType(trait_type_id) => Ok(self.traits[self
                .trait_types
                .get(trait_type_id)
                .ok_or(arena::Error)?
                .parent_trait_id]
                .accessibility),
        }
    }

    /// Checks if the given [`GlobalID`] symbol is accessible from the given [`ScopedID`] symbol.
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    pub fn symbol_accessible(
        &self,
        referree_global_id: GlobalID,
        referrer_id: ID,
    ) -> Result<bool, arena::Error> {
        match self.get_accessibility(referree_global_id)? {
            // Private symbol is only accessible from the same module or its children.
            Accessibility::Private => {
                let referree_global_module_id =
                    self.get_current_target_root_module_id(referree_global_id.into())?;
                let referrer_module_id = self.get_current_target_root_module_id(referrer_id)?;

                // if same module, it is accessible.
                if referrer_module_id == referree_global_module_id {
                    return Ok(true);
                }

                let mut current_referrer_parent_id = referrer_module_id.into();

                while let Some(parent_id) = self
                    .get_symbol(current_referrer_parent_id)
                    .unwrap()
                    .parent_symbol()
                {
                    match parent_id {
                        ID::Module(module_id) if module_id == referree_global_module_id => {
                            return Ok(true);
                        }
                        _ => {
                            current_referrer_parent_id = parent_id;
                        }
                    }
                }

                Ok(false)
            }

            // Both symbols are in the same module.
            Accessibility::Internal => Ok(self
                .get_current_target_root_module_id(referree_global_id.into())?
                == self.get_current_target_root_module_id(referrer_id)?),

            // Public symbols are always accessible.
            Accessibility::Public => {
                // PEDANTIC: check if the referrer also a valid id.
                self.get_symbol(referrer_id)?;

                Ok(true)
            }
        }
    }

    /// Gets the target root module ID where the given symbol is defined in (including itself).
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    pub fn get_current_target_root_module_id(
        &self,
        mut id: ID,
    ) -> Result<arena::ID<Module>, arena::Error> {
        while let Some(parent_id) = self.get_symbol(id)?.parent_symbol() {
            id = parent_id;
        }

        Ok(id
            .into_module()
            .expect("It should be a module at the root."))
    }

    /// Gets the module ID that contains the given symbol ID (including itself).
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    pub fn get_current_module_id(&self, mut id: ID) -> Result<arena::ID<Module>, arena::Error> {
        loop {
            // If the ID is a module ID, return it.
            if let ID::Module(module_id) = id {
                return Ok(module_id);
            }

            let Some(parent_id) = self.get_symbol(id)?.parent_symbol() else {
                panic!("should have found parent module ID already!");
            };

            id = parent_id;
        }
    }

    /// Gets the scoped ID that contains the given symbol ID (including itself).
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    pub fn get_current_scoped_id(&self, mut id: ID) -> Result<ScopedID, arena::Error> {
        loop {
            if let Ok(scoped_id) = id.try_into() {
                return Ok(scoped_id);
            }

            let Some(parent_id) = self.get_symbol(id)?.parent_symbol() else {
                panic!("should have found parent module ID already!");
            };

            id = parent_id;
        }
    }

    /// Gets the [`ScopeWalker`] iterator for the given ID.
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    pub fn scope_walker(&self, id: ID) -> Result<ScopeWalker, arena::Error> {
        // Checks if the ID is valid.
        if self.get_symbol(id).is_err() {
            return Err(arena::Error);
        }

        Ok(ScopeWalker {
            table: self,
            current_id: Some(id),
        })
    }

    /// Creates a new empty table.
    fn new() -> Self {
        Self {
            modules: Arena::new(),
            structs: Arena::new(),
            enums: Arena::new(),
            enum_variants: Arena::new(),
            functions: Arena::new(),
            types: Arena::new(),
            fields: Arena::new(),
            parameters: Arena::new(),
            traits: Arena::new(),
            trait_types: Arena::new(),
            type_parameters: Arena::new(),
            lifetime_parameters: Arena::new(),
            trait_functions: Arena::new(),
            implements: Arena::new(),
            implements_types: Arena::new(),
            implements_functions: Arena::new(),
            target_root_module_ids_by_name: HashMap::new(),
        }
    }

    /// Performs symbol resolution/analysis and builds a table populated with symbols from the
    /// given target syntax trees.
    ///
    /// # Errors
    /// - If found duplicated target names.
    pub fn build(
        targets: Vec<Target>,
        handler: &impl Handler<error::Error>,
    ) -> std::result::Result<Self, BuildError> {
        // checks input validity
        module::target_check(&targets)?;

        let mut table = Self::new();

        // creates core-language symbols
        table.create_core_symbols();

        // creates modules
        table.create_modules(&targets);

        // populates usings statements
        table.populate_usings_in_workspace(&targets, handler);

        // drafts the symbols
        let (_states, _implements_syntax_tree_with_module_ids) =
            table.draft_symbols(targets, handler);

        Ok(table)
    }
}

/// Similar to [`ScopeWalker`] but usesh/stores a mutable reference to the [`Table`] .
pub(crate) struct ScopeWalkerMut<'a> {
    table: &'a mut Table,
    current_id: Option<ID>,
}

impl<'a> ScopeWalkerMut<'a> {
    /// Creates a new scope walker.
    #[must_use]
    pub(crate) fn table(&'a mut self) -> &'a mut Table { self.table }
}

impl<'a> Iterator for ScopeWalkerMut<'a> {
    type Item = ID;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current_id {
            Some(current_id) => {
                let next_id = self.table.get_symbol(current_id).unwrap().parent_symbol();
                self.current_id = next_id;
                Some(current_id)
            }
            None => None,
        }
    }
}

pub struct ScopeWalkerReverse {}

/// Represents an iterator that walks through the scope of the given symbol. It goes through all
/// the parent symbols until it reaches the root.
#[derive(Debug, Clone)]
pub struct ScopeWalker<'a> {
    table: &'a Table,
    current_id: Option<ID>,
}

impl<'a> ScopeWalker<'a> {
    /// Creates a new scope walker.
    #[must_use]
    pub fn table(&self) -> &'a Table { self.table }
}

impl<'a> Iterator for ScopeWalker<'a> {
    type Item = ID;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current_id {
            Some(current_id) => {
                let next_id = self.table.get_symbol(current_id).unwrap().parent_symbol();
                self.current_id = next_id;
                Some(current_id)
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod tests;
