//! Contains the definition of [`Table`], the symbol table of the compiler.

use std::{collections::HashMap, hash::Hash};

use derive_more::From;
use pernixc_syntax::syntax_tree::target::Target;
use pernixc_system::{
    arena::{self, Arena},
    diagnostic::Handler,
};

use crate::{
    error, Enum, EnumVariant, Field, Function, Genericable, GenericableID, Global, GlobalID,
    Implements, ImplementsFunction, ImplementsType, LifetimeParameter, Module, Parameter, Scoped,
    ScopedID, Struct, Substitution, Symbol, Trait, TraitFunction, TraitType, Type, TypeParameter,
    ID,
};

mod core;
mod drafting;
mod finalizing;
mod module;
mod resolution;

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
    #[must_use]
    pub fn get_global(&self, global_id: GlobalID) -> Option<&dyn Global> {
        match global_id {
            GlobalID::Module(s) => self.modules.get(s).map(|x| x as _),
            GlobalID::Struct(s) => self.structs.get(s).map(|x| x as _),
            GlobalID::Enum(s) => self.enums.get(s).map(|x| x as _),
            GlobalID::EnumVariant(s) => self.enum_variants.get(s).map(|x| x as _),
            GlobalID::Function(s) => self.functions.get(s).map(|x| x as _),
            GlobalID::Type(s) => self.types.get(s).map(|x| x as _),
            GlobalID::Trait(s) => self.traits.get(s).map(|x| x as _),
            GlobalID::TraitFunction(s) => self.trait_functions.get(s).map(|x| x as _),
            GlobalID::TraitType(s) => self.trait_types.get(s).map(|x| x as _),
        }
    }

    /// Gets a reference to the [`Genericable`] symbol from the given ID.
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    #[must_use]
    pub fn get_genericable(&self, genericable_id: GenericableID) -> Option<&dyn Genericable> {
        match genericable_id {
            GenericableID::Struct(s) => self.structs.get(s).map(|x| x as _),
            GenericableID::Function(s) => self.functions.get(s).map(|x| x as _),
            GenericableID::Implements(s) => self.implements.get(s).map(|x| x as _),
            GenericableID::Trait(s) => self.traits.get(s).map(|x| x as _),
            GenericableID::TraitType(s) => self.trait_types.get(s).map(|x| x as _),
            GenericableID::TraitFunction(s) => self.trait_functions.get(s).map(|x| x as _),
            GenericableID::Type(s) => self.types.get(s).map(|x| x as _),
            GenericableID::ImplementsType(s) => self.implements_types.get(s).map(|x| x as _),
            GenericableID::ImplementsFunction(s) => {
                self.implements_functions.get(s).map(|x| x as _)
            }
        }
    }

    /// Gets a reference to the [`Symbol`] from the given ID.
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    #[must_use]
    pub fn get_symbol(&self, id: ID) -> Option<&dyn Symbol> {
        match id {
            ID::Module(s) => self.modules.get(s).map(|x| x as _),
            ID::Struct(s) => self.structs.get(s).map(|x| x as _),
            ID::Enum(s) => self.enums.get(s).map(|x| x as _),
            ID::EnumVariant(s) => self.enum_variants.get(s).map(|x| x as _),
            ID::Function(s) => self.functions.get(s).map(|x| x as _),
            ID::Type(s) => self.types.get(s).map(|x| x as _),
            ID::Field(s) => self.fields.get(s).map(|x| x as _),
            ID::Parameter(s) => self.parameters.get(s).map(|x| x as _),
            ID::Trait(s) => self.traits.get(s).map(|x| x as _),
            ID::TraitType(s) => self.trait_types.get(s).map(|x| x as _),
            ID::TypeParameter(s) => self.type_parameters.get(s).map(|x| x as _),
            ID::LifetimeParameter(s) => self.lifetime_parameters.get(s).map(|x| x as _),
            ID::TraitFunction(s) => self.trait_functions.get(s).map(|x| x as _),
            ID::Implements(s) => self.implements.get(s).map(|x| x as _),
            ID::ImplementsType(s) => self.implements_types.get(s).map(|x| x as _),
            ID::ImplementsFunction(s) => self.implements_functions.get(s).map(|x| x as _),
        }
    }

    /// Gets a reference to the [`Scoped`] symbol from the given ID.
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    #[must_use]
    pub fn get_scoped(&self, scoped_id: ScopedID) -> Option<&dyn Scoped> {
        match scoped_id {
            ScopedID::Module(s) => self.modules.get(s).map(|x| x as _),
            ScopedID::Enum(s) => self.enums.get(s).map(|x| x as _),
            ScopedID::Trait(s) => self.traits.get(s).map(|x| x as _),
        }
    }

    /// Gets the fully qualified name of the given [`GlobalID`].
    ///
    /// The name doesn't include generics.
    #[must_use]
    pub fn get_qualified_name(&self, mut global_id: GlobalID) -> Option<String> {
        let mut current_name = self.get_global(global_id)?.name().to_owned();

        while let Some(parent_symbol_id) = self.get_global(global_id)?.parent_symbol() {
            let parent_global_symbol_id: GlobalID = parent_symbol_id
                .try_into()
                .expect("Parent symbol ID returned by `Global` must belong to `GlobalID`");

            current_name.insert_str(0, "::");
            current_name.insert_str(0, self.get_global(parent_global_symbol_id)?.name());

            global_id = parent_global_symbol_id;
        }

        Some(current_name)
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error, From)]
#[allow(missing_docs)]
pub enum Error {
    #[error("{0}")]
    FatalSementic(FatalSemanticError),

    #[error("{0}")]
    InvalidID(arena::Error),
}

/// Is a result type returned by various methods in the [`Table`].
pub type Result<T> = std::result::Result<T, Error>;

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
        table.populate_usings_in_targets(&targets, handler);

        // drafts the symbols
        let states = table.draft_symbols(targets, handler);

        Ok(table)
    }
}
