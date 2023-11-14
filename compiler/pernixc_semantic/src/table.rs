//! Contains the definition of [`Table`]

use std::collections::{hash_map::Entry, HashMap};

use getset::Getters;
use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};
use paste::paste;
use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree::target::Target;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use thiserror::Error;

use crate::{
    arena::{Arena, ID},
    error,
    symbol::{
        Accessibility, Constant, Enum, Function, Generic, GenericID, Global, GlobalID,
        Implementation, ImplementationConstant, ImplementationFunction, ImplementationType, Module,
        NegativeImplementation, Struct, Trait, TraitConstant, TraitFunction, TraitType, Type,
        Variant,
    },
};

mod drafting;
pub mod evaluate;
mod finalizing;
pub mod resolution;
mod state;

/// A trait used to access the symbols defined in the table.
pub trait Index<Idx: ?Sized> {
    /// The output type of the indexing operation.
    type Output: ?Sized;

    /// Returns the output of the indexing operation if the index is valid.
    fn get(&self, index: Idx) -> Option<RwLockReadGuard<Self::Output>>;
}

trait IndexMut<Idx: ?Sized>: Index<Idx> {
    /// Returns the output of the indexing operation if the index is valid.
    fn get_mut(&self, index: Idx) -> Option<RwLockWriteGuard<Self::Output>>;
}

/// Contains all the symbols and information defined in the target.
#[derive(Debug, Getters)]
pub struct Table {
    modules: Arena<RwLock<Module>, Module>,
    structs: Arena<RwLock<Struct>, Struct>,
    enums: Arena<RwLock<Enum>, Enum>,
    variants: Arena<RwLock<Variant>, Variant>,
    types: Arena<RwLock<Type>, Type>,
    functions: Arena<RwLock<Function>, Function>,
    constants: Arena<RwLock<Constant>, Constant>,

    traits: Arena<RwLock<Trait>, Trait>,
    trait_types: Arena<RwLock<TraitType>, TraitType>,
    trait_functions: Arena<RwLock<TraitFunction>, TraitFunction>,
    trait_constants: Arena<RwLock<TraitConstant>, TraitConstant>,

    implementations: Arena<RwLock<Implementation>, Implementation>,
    negative_implementations: Arena<RwLock<NegativeImplementation>, NegativeImplementation>,

    implementation_types: Arena<RwLock<ImplementationType>, ImplementationType>,
    implementation_functions: Arena<RwLock<ImplementationFunction>, ImplementationFunction>,
    implementation_constants: Arena<RwLock<ImplementationConstant>, ImplementationConstant>,

    root_module_ids_by_name: HashMap<String, ID<Module>>,

    state_manager: RwLock<state::Manager>,
}

impl Table {
    pub(crate) fn default() -> Self {
        Self {
            modules: Arena::default(),
            structs: Arena::default(),
            enums: Arena::default(),
            types: Arena::default(),
            functions: Arena::default(),
            constants: Arena::default(),
            traits: Arena::default(),
            variants: Arena::default(),
            implementations: Arena::default(),
            implementation_constants: Arena::default(),
            implementation_functions: Arena::default(),
            implementation_types: Arena::default(),
            trait_constants: Arena::default(),
            trait_functions: Arena::default(),
            trait_types: Arena::default(),
            negative_implementations: Arena::default(),
            root_module_ids_by_name: HashMap::new(),
            state_manager: RwLock::new(state::Manager::default()),
        }
    }
}

macro_rules! index {
    ($field:ident, $struct_name:ident) => {
        impl Index<crate::arena::ID<$struct_name>> for Table {
            type Output = $struct_name;

            fn get(
                &self,
                id: crate::arena::ID<$struct_name>,
            ) -> Option<RwLockReadGuard<Self::Output>> {
                self.$field.get(id).map(|x| x.read())
            }
        }

        impl IndexMut<crate::arena::ID<$struct_name>> for Table {
            fn get_mut(
                &self,
                id: crate::arena::ID<$struct_name>,
            ) -> Option<RwLockWriteGuard<Self::Output>> {
                self.$field.get(id).map(|x| x.write())
            }
        }
    };
}

index!(modules, Module);
index!(structs, Struct);
index!(enums, Enum);
index!(types, Type);
index!(functions, Function);
index!(constants, Constant);
index!(traits, Trait);
index!(trait_types, TraitType);
index!(trait_functions, TraitFunction);
index!(trait_constants, TraitConstant);
index!(variants, Variant);
index!(implementations, Implementation);
index!(negative_implementations, NegativeImplementation);
index!(implementation_types, ImplementationType);
index!(implementation_constants, ImplementationConstant);
index!(implementation_functions, ImplementationFunction);

/// The error type returned by [`Table::build()`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Error)]
#[allow(missing_docs)]
pub enum BuildError {
    #[error("the target `{0}` was already defined")]
    DuplicateTargetName(String),
}

/// The error type returned by most operations on [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("The given ID does not exist in the table")]
    InvalidID,

    #[error("Encountered a fatal semantic error that aborts the process")]
    SemanticError,
}

macro_rules! get {
    ($self:ident, $id:ident, $kind:ident, $($field:ident),*) => {
        match $id {
            $(
                $kind::$field($id) => $self.get($id).map(|x| RwLockReadGuard::map(x, |x| x as _)),
            )*
        }
    };
}

impl Table {
    /// Builds a symbol table from the given targets.
    ///
    /// # Errors
    ///
    /// - [`BuildError::DuplicateTargetName`]: if there are two targets with the same name.
    pub fn build(
        targets: impl ParallelIterator<Item = Target>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self, BuildError> {
        let table = RwLock::new(Self::default());

        let drafting_context = drafting::Context {
            table: &table,
            handler,
            usings_by_module_id: RwLock::new(HashMap::new()),
            implementations_by_module_id: RwLock::new(HashMap::new()),
        };

        // Collect all the targets.
        targets.try_for_each(|target| {
            let (syntax_tree, name) = target.dissolve();

            let module_id = drafting_context.draft_module(syntax_tree, name.clone(), None);

            #[allow(clippy::significant_drop_in_scrutinee)]
            match table.write().root_module_ids_by_name.entry(name) {
                Entry::Occupied(error) => Err(BuildError::DuplicateTargetName(error.key().clone())),
                Entry::Vacant(entry) => {
                    entry.insert(module_id);
                    Ok(())
                }
            }
        })?;

        let drafting::Context {
            usings_by_module_id,
            implementations_by_module_id,
            ..
        } = drafting_context;

        let table = table.into_inner();

        // populate the usings
        for (module_id, usings) in usings_by_module_id.into_inner() {
            for using in usings {
                let Ok(module_id) =
                    table.resolve_module_path(using.module_path(), module_id.into(), handler)
                else {
                    continue;
                };

                table.get_mut(module_id).unwrap().usings.insert(module_id);
            }
        }

        // attach the implementations to the trait
        for (module_id, implementations) in implementations_by_module_id.into_inner() {
            for implementation in implementations {
                let Ok(trait_id) = table.resolve_trait_path(
                    implementation.signature().qualified_identifier(),
                    module_id.into(),
                    handler,
                ) else {
                    continue;
                };

                // add the implementation to the trait draft
                table
                    .state_manager
                    .write()
                    .states_by_trait_id
                    .get_mut(&trait_id)
                    .expect("should exist")
                    .as_drafted_mut()
                    .expect("should be expected")
                    .implementations
                    .push(state::Implementation {
                        in_module: module_id,
                        syntax_tree: implementation,
                    });
            }
        }

        // finalize all symbols
        while let Some(global_id) = {
            let state_manager = table.state_manager.read();
            state_manager.next_drafted_symbol()
        } {
            let _ = table.finalize(global_id, None, handler);
        }

        Ok(table)
    }

    /// Checks if the `referred` is accessible from the `referring_site`.
    ///
    /// # Returns
    ///
    /// Returns `None` if `referred` or `referring_site` is not a valid ID.
    pub fn symbol_accessible(&self, referring_site: GlobalID, referred: GlobalID) -> Option<bool> {
        match self.get_accessibility(referred)? {
            Accessibility::Public => {
                // PEDANTIC: check if the referring site is a valid ID.
                drop(self.get_global(referring_site)?);

                Some(true)
            }
            Accessibility::Private => {
                let mut referring_module_id = self.get_closet_module_id(referring_site)?;
                let referred_module_id = self.get_closet_module_id(referred)?;

                loop {
                    if referring_module_id == referred_module_id {
                        return Some(true);
                    }

                    let Some(next) = self.get(referring_module_id)?.parent_module_id else {
                        return Some(false);
                    };

                    referring_module_id = next;
                }
            }
            Accessibility::Internal => Some(
                self.get_root_module_id(referred)? == self.get_root_module_id(referring_site)?,
            ),
        }
    }

    /// Returns the root [`Module`] ID that contains the given [`GlobalID`] (including itself).
    pub fn get_root_module_id(&self, mut global_id: GlobalID) -> Option<ID<Module>> {
        while let Some(parent_id) = self.get_global(global_id)?.parent_global_id() {
            global_id = parent_id;
        }

        Some(
            global_id
                .into_module()
                .expect("It should be a module at the root."),
        )
    }

    /// Returns the [`Module`] ID that is the closest to the given [`GlobalID`] (including itself).
    pub fn get_closet_module_id(&self, mut global_id: GlobalID) -> Option<ID<Module>> {
        // including itself
        loop {
            if let GlobalID::Module(module_id) = global_id {
                drop(self.get(module_id)?);
                return Some(module_id);
            }

            global_id = self
                .get_global(global_id)?
                .parent_global_id()
                .expect("should've found at least one module");
        }
    }

    /// Gets the [`ScopeWalker`] that walks through the scope hierarchy of the given [`GlobalID`].
    ///
    /// See [`ScopeWalker`] for more information.
    #[must_use]
    pub fn scope_walker(&self, global_id: GlobalID) -> Option<ScopeWalker> {
        drop(self.get_global(global_id)?);

        Some(ScopeWalker {
            table: self,
            current_id: Some(global_id),
        })
    }

    /// Gets the accessibility of the given [`GlobalID`].
    ///
    /// # Returns
    ///
    /// Returns `None` if the given [`GlobalID`] is not a valid ID.
    pub fn get_accessibility(&self, global_id: GlobalID) -> Option<Accessibility> {
        macro_rules! arm_expression {
            ($table:ident, $id:ident, $kind:ident) => {
                paste! {
                    $table.[<$kind:lower s>].get($id).map(|$id| $id.read().accessibility)
                }
            };

            ($table:ident, $id:ident, $kind:ident, $expr:expr) => {
                paste! {
                    $table.[<$kind:snake s>].get($id).and_then(|$id| $expr)
                }
            };
        }
        macro_rules! get_accessibility {
            ($self:ident, $table:ident, $(($kind:ident $(, $expr:expr)?)),*) => {

                match $self {
                    $(
                        GlobalID::$kind($self) => arm_expression!($table, $self, $kind $(, $expr)?),
                    )*
                }
            };
        }

        get_accessibility!(
            global_id,
            self,
            (Module),
            (Struct),
            (Enum),
            (Trait),
            (Type),
            (Constant),
            (Function),
            (
                Variant,
                self.get_accessibility(global_id.read().parent_enum_id.into())
            ),
            (
                TraitType,
                self.get_accessibility(global_id.read().parent_trait_id.into())
            ),
            (
                TraitConstant,
                self.get_accessibility(global_id.read().parent_trait_id.into())
            ),
            (
                TraitFunction,
                self.get_accessibility(global_id.read().parent_trait_id.into())
            ),
            (
                Implementation,
                self.get_accessibility(global_id.read().signature.trait_id.into())
            ),
            (
                ImplementationType,
                self.get_accessibility(global_id.read().parent_implementation_id.into())
            ),
            (
                ImplementationFunction,
                self.get_accessibility(global_id.read().parent_implementation_id.into())
            ),
            (
                ImplementationConstant,
                self.get_accessibility(global_id.read().parent_implementation_id.into())
            )
        )
    }

    /// Returns the [`Generic`] symbol from the given [`GenericID`]
    pub fn get_generic(&self, generic_id: GenericID) -> Option<MappedRwLockReadGuard<dyn Generic>> {
        get!(
            self,
            generic_id,
            GenericID,
            Struct,
            Trait,
            Enum,
            Function,
            Type,
            TraitFunction,
            TraitType,
            Implementation,
            ImplementationType,
            ImplementationFunction
        )
    }

    /// Returns the [`Global`] symbol from the given [`GlobalID`].
    #[must_use]
    pub fn get_global(&self, global_id: GlobalID) -> Option<MappedRwLockReadGuard<dyn Global>> {
        get!(
            self,
            global_id,
            GlobalID,
            Module,
            Struct,
            TraitType,
            TraitConstant,
            TraitFunction,
            Trait,
            Enum,
            Type,
            Constant,
            Function,
            Variant,
            ImplementationFunction,
            Implementation,
            ImplementationType,
            ImplementationConstant
        )
    }
}

/// Represents an iterator that walks through the scope of the given symbol. It goes through all
/// the parent symbols until it reaches the root.
///
/// The iterator iterates through the scope in id-to-parent order.
#[derive(Debug, Clone)]
pub struct ScopeWalker<'a> {
    table: &'a Table,
    current_id: Option<GlobalID>,
}

impl<'a> ScopeWalker<'a> {
    /// Creates a new scope walker.
    #[must_use]
    pub fn table(&self) -> &'a Table { self.table }
}

impl<'a> Iterator for ScopeWalker<'a> {
    type Item = GlobalID;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current_id {
            Some(current_id) => {
                let next_id = self
                    .table
                    .get_global(current_id)
                    .unwrap()
                    .parent_global_id();
                self.current_id = next_id;
                Some(current_id)
            }
            None => None,
        }
    }
}
