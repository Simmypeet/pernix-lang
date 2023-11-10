//! Contains the definition of [`Table`]

use std::collections::{hash_map::Entry, HashMap};

use getset::Getters;
use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};
use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree::target::Target;
use rayon::iter::ParallelIterator;
use thiserror::Error;

use crate::{
    arena::{Arena, ID},
    error,
    symbol::{
        Accessibility, Constant, Enum, Function, Global, GlobalID, Implementation,
        ImplementationConstant, ImplementationFunction, ImplementationType, Module,
        NegativeImplementation, Struct, Trait, TraitConstant, TraitFunction, TraitType, Type,
        Variant,
    },
};

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

    implementation: Arena<RwLock<Implementation>, Implementation>,
    negative_implementation: Arena<RwLock<NegativeImplementation>, NegativeImplementation>,

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
            implementation: Arena::default(),
            implementation_constants: Arena::default(),
            implementation_functions: Arena::default(),
            implementation_types: Arena::default(),
            trait_constants: Arena::default(),
            trait_functions: Arena::default(),
            trait_types: Arena::default(),
            negative_implementation: Arena::default(),
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
index!(implementation, Implementation);
index!(negative_implementation, NegativeImplementation);
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

impl Table {
    /// Builds a symbol table from the given targets.
    ///
    /// # Errors
    pub fn build(
        targets: impl ParallelIterator<Item = Target>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self, BuildError> {
        let table = RwLock::new(Self::default());

        let error = RwLock::new(None);

        let drafting_context = drafting::Context {
            table: &table,
            handler,
            usings_by_module_id: RwLock::new(HashMap::new()),
            implementations_by_module_id: RwLock::new(HashMap::new()),
        };

        // Collect all the targets.
        targets.try_for_each(|target| {
            let (syntax_tree, name) = target.dissolve();

            let module_id = drafting_context.draft_module(
                syntax_tree.dissolve().1,
                Accessibility::Public,
                name.clone(),
                None,
                None,
            );

            #[allow(clippy::significant_drop_in_scrutinee)]
            match table.write().root_module_ids_by_name.entry(name) {
                Entry::Occupied(error) => Err(BuildError::DuplicateTargetName(error.key().clone())),
                Entry::Vacant(entry) => {
                    entry.insert(module_id);
                    Ok(())
                }
            }
        })?;

        if let Some(err) = error.into_inner() {
            return Err(err);
        }

        Ok(table.into_inner())
    }

    /// Returns the [`Global`] symbol from the given [`GlobalID`].
    #[must_use]
    pub fn get_global(&self, global_id: GlobalID) -> Option<MappedRwLockReadGuard<dyn Global>> {
        macro_rules! get {
            ($($field:ident),*) => {
                match global_id {
                    $(
                        GlobalID::$field(id) => self.get(id).map(|x| RwLockReadGuard::map(x, |x| x as _)),
                    )*
                }
            };
        }
        get!(
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
            ImplementsConstant
        )
    }
}

mod drafting;
mod state;
