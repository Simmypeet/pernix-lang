//! Contains the definition of [`Table`], the symbol table of the compiler.
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::target::{File, ModulePath, Target, Using};
use pernixc_system::{
    arena::{self, Arena},
    diagnostic::Handler,
};

use crate::{
    error::{self, ModuleNotFound, TargetNotFound, UsingOwnModule},
    Accessibility, AssociatedType, Enum, EnumVariant, Field, Function, Genericable, GenericableID,
    Global, GlobalID, Implements, ImplementsFunction, ImplementsType, LifetimeParameter, Module,
    ModuleID, Parameter, Scoped, ScopedID, Struct, Symbol, Trait, TraitFunction, TypeAlias,
    TypeParameter, ID,
};

/// Represents a symbol table of the compiler.
#[derive(Debug)]
pub struct Table {
    modules: Arena<Module>,
    structs: Arena<Struct>,
    enums: Arena<Enum>,
    enum_variants: Arena<EnumVariant>,
    functions: Arena<Function>,
    type_aliases: Arena<TypeAlias>,
    fields: Arena<Field>,
    parameters: Arena<Parameter>,
    traits: Arena<Trait>,
    associated_types: Arena<AssociatedType>,
    type_parameters: Arena<TypeParameter>,
    lifetime_parameters: Arena<LifetimeParameter>,
    trait_functions: Arena<TraitFunction>,
    implements: Arena<Implements>,
    implements_types: Arena<ImplementsType>,
    implements_functions: Arena<ImplementsFunction>,

    root_module_ids_by_target_name: HashMap<String, ModuleID>,
}

impl Table {
    /// Gets a reference to the [`Global`] symbol from the given ID.
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    pub fn get_global(&self, global_id: GlobalID) -> arena::Result<&dyn Global> {
        match global_id {
            GlobalID::Module(s) => self.modules.get(s).map(|x| x as _),
            GlobalID::Struct(s) => self.structs.get(s).map(|x| x as _),
            GlobalID::Enum(s) => self.enums.get(s).map(|x| x as _),
            GlobalID::EnumVariant(s) => self.enum_variants.get(s).map(|x| x as _),
            GlobalID::Function(s) => self.functions.get(s).map(|x| x as _),
            GlobalID::TypeAlias(s) => self.type_aliases.get(s).map(|x| x as _),
            GlobalID::Trait(s) => self.traits.get(s).map(|x| x as _),
            GlobalID::TraitFunction(s) => self.trait_functions.get(s).map(|x| x as _),
            GlobalID::AssociatedType(s) => self.associated_types.get(s).map(|x| x as _),
        }
    }

    /// Gets a reference to the [`Genericable`] symbol from the given ID.
    ///
    /// # Errors
    /// If the ID is invalid, returns an error.
    pub fn get_genericable(
        &self,
        genericable_id: GenericableID,
    ) -> arena::Result<&dyn Genericable> {
        match genericable_id {
            GenericableID::Struct(s) => self.structs.get(s).map(|x| x as _),
            GenericableID::Function(s) => self.functions.get(s).map(|x| x as _),
            GenericableID::Implements(s) => self.implements.get(s).map(|x| x as _),
            GenericableID::Trait(s) => self.traits.get(s).map(|x| x as _),
            GenericableID::AssociatedType(s) => self.associated_types.get(s).map(|x| x as _),
            GenericableID::TraitFunction(s) => self.trait_functions.get(s).map(|x| x as _),
            GenericableID::TypeAlias(s) => self.type_aliases.get(s).map(|x| x as _),
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
    pub fn get_symbol(&self, id: ID) -> arena::Result<&dyn Symbol> {
        match id {
            ID::Module(s) => self.modules.get(s).map(|x| x as _),
            ID::Struct(s) => self.structs.get(s).map(|x| x as _),
            ID::Enum(s) => self.enums.get(s).map(|x| x as _),
            ID::EnumVariant(s) => self.enum_variants.get(s).map(|x| x as _),
            ID::Function(s) => self.functions.get(s).map(|x| x as _),
            ID::TypeAlias(s) => self.type_aliases.get(s).map(|x| x as _),
            ID::Field(s) => self.fields.get(s).map(|x| x as _),
            ID::Parameter(s) => self.parameters.get(s).map(|x| x as _),
            ID::Trait(s) => self.traits.get(s).map(|x| x as _),
            ID::AssociatedType(s) => self.associated_types.get(s).map(|x| x as _),
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
    pub fn get_scoped(&self, scoped_id: ScopedID) -> arena::Result<&dyn Scoped> {
        match scoped_id {
            ScopedID::Module(s) => self.modules.get(s).map(|x| x as _),
            ScopedID::Enum(s) => self.enums.get(s).map(|x| x as _),
            ScopedID::Trait(s) => self.traits.get(s).map(|x| x as _),
        }
    }
}

/// Is an error that occurs when a target name duplication is detected while building a [`Table`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[error("Target name duplication detected!")]
pub struct TargetDuplicationError {
    /// The duplicated target name
    pub duplication_target_names: Vec<String>,
}

/// Is an error returned by [`Table::build`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[allow(missing_docs)]
pub enum BuildError {
    #[error("{0}")]
    TargetDuplication(TargetDuplicationError),
}

/// Is an error that occurs when encountering a fatal semantic error that cannot be recovered.
///
/// The subsequent errors are reported to the handler.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[error("Encounters a fatal semantic error that cannot be recovered.")]
pub struct Error;

/// Is a result type returned by various methods in the [`Table`].
pub type Result<T> = std::result::Result<T, Error>;

impl Table {
    fn target_name_duplication_check(targets: &[Target]) -> std::result::Result<(), BuildError> {
        let mut duplicated_names = HashSet::new();
        let mut found_names = HashSet::new();

        for target in targets {
            if !found_names.insert(target.name()) {
                duplicated_names.insert(target.name().to_string());
            }
        }

        if duplicated_names.is_empty() {
            Ok(())
        } else {
            Err(BuildError::TargetDuplication(TargetDuplicationError {
                duplication_target_names: duplicated_names.into_iter().collect(),
            }))
        }
    }

    fn stem_from(modules: &mut Arena<Module>, file: &File, parent_module_id: ModuleID) {
        for submodule in &file.submodules {
            let module_name = submodule.module.identifier.span.str().to_string();
            // Adds a submodule to the module list
            let id = modules.insert(Module {
                name: module_name.clone(),
                accessibility: Accessibility::from_syntax_tree(&submodule.module.access_modifier),
                parent_module_id: Some(parent_module_id),
                child_ids_by_name: HashMap::new(),
                usings: Vec::new(),
            });

            assert!(
                modules
                    .get_mut(parent_module_id)
                    .expect("should exists")
                    .child_ids_by_name
                    .insert(module_name, id.into())
                    .is_none(),
                "Duplication detected, but it should've already been checked."
            );

            Self::stem_from(modules, &submodule.file, id);
        }
    }

    fn create_modules_from_target(
        root_module_ids_by_target_name: &mut HashMap<String, ModuleID>,
        modules: &mut Arena<Module>,
        target: &Target,
    ) {
        // create a root module
        let root_module_id = modules.insert(Module {
            name: target.name().clone(),
            accessibility: Accessibility::Public,
            parent_module_id: None,
            child_ids_by_name: HashMap::new(),
            usings: Vec::new(),
        });

        root_module_ids_by_target_name.insert(target.name().clone(), root_module_id);

        Self::stem_from(modules, target, root_module_id);
    }

    fn create_modules(targets: &[Target]) -> (HashMap<String, ModuleID>, Arena<Module>) {
        let mut modules = Arena::new();
        let mut root_module_ids_by_target_name = HashMap::new();

        for target in targets {
            Self::create_modules_from_target(
                &mut root_module_ids_by_target_name,
                &mut modules,
                target,
            );
        }

        (root_module_ids_by_target_name, modules)
    }

    fn get_module_id_from_module_path(
        root_module_ids_by_target_name: &HashMap<String, ModuleID>,
        modules: &Arena<Module>,
        module_path: &ModulePath,
        handler: &impl Handler<error::Error>,
    ) -> Result<ModuleID> {
        let mut current_module_id = None;

        for path in module_path.elements() {
            if let Some(module_id) = current_module_id {
                // search from current module id
                let module = &modules.get(module_id).expect("should exists");

                let Some(new_module_id) = module
                    .child_ids_by_name
                    .get(path.span.str())
                    .copied() else {
                    handler.recieve(error::Error::ModuleNotFound(
                        ModuleNotFound {
                            in_module_id: module_id,
                            unknown_module_span: path.span.clone(),
                        }
                    ));
                    return Err(Error);
                };

                current_module_id = Some(new_module_id.into_module().expect("should be module"));
            } else {
                // search from the root
                let Some(module_id) = root_module_ids_by_target_name
                    .get(path.span.str())
                    .copied() else {
                    handler.recieve(error::Error::TargetNotFound(
                        TargetNotFound {
                            unknown_target_span: path.span.clone()
                        }
                    ));
                    return Err(Error);
                };

                current_module_id = Some(module_id);
            }
        }

        Ok(current_module_id.expect("atleast one element in module path"))
    }

    fn populate_usings_in_module(
        root_module_ids_by_target_name: &HashMap<String, ModuleID>,
        modules: &mut Arena<Module>,
        module_id: ModuleID,
        usings: &[Using],
        handler: &impl Handler<error::Error>,
    ) {
        let mut using_spans_by_module_id = HashMap::new();

        for using in usings {
            // resolve module_path
            let Ok(using_module_id) = Self::get_module_id_from_module_path(
                root_module_ids_by_target_name,
                modules,
                &using.module_path,
                handler) else {
                continue;
            };

            let using_span = using.span().expect("should be a valid syntax tree");

            // check if using own module
            if using_module_id == module_id {
                handler.recieve(error::Error::UsingOwnModule(UsingOwnModule {
                    module_id,
                    using_span: using_span.clone(),
                }));

                continue;
            }

            // check if already using
            if let Some(previous_using_span) =
                using_spans_by_module_id.insert(using_module_id, using_span.clone())
            {
                handler.recieve(error::Error::UsingDuplication(error::UsingDuplication {
                    previous_using_span,
                    duplicate_using_span: using_span,
                }));
            }
        }

        let module = modules.get_mut(module_id).expect("should exists");
        for module_id in using_spans_by_module_id.into_keys() {
            module.usings.push(module_id);
        }
    }

    fn populate_usings_in_submodules(
        root_module_ids_by_target_name: &HashMap<String, ModuleID>,
        modules: &mut Arena<Module>,
        current_module_id: ModuleID,
        current_file: &File,
        handler: &impl Handler<error::Error>,
    ) {
        // populate usings for this file
        Self::populate_usings_in_module(
            root_module_ids_by_target_name,
            modules,
            current_module_id,
            &current_file.usings,
            handler,
        );

        // populate usings for submodules
        for submodule in &current_file.submodules {
            let module_id = modules
                .get(current_module_id)
                .expect("should exists")
                .child_ids_by_name
                .get(submodule.module.identifier.span.str())
                .expect("should exists")
                .into_module()
                .expect("there must be only modules");

            Self::populate_usings_in_submodules(
                root_module_ids_by_target_name,
                modules,
                module_id,
                &submodule.file,
                handler,
            );
        }
    }

    fn populate_usings_in_targets(
        root_module_ids_by_target_name: &HashMap<String, ModuleID>,
        modules: &mut Arena<Module>,
        targets: &[Target],
        handler: &impl Handler<error::Error>,
    ) {
        for target in targets {
            let root_module_id = root_module_ids_by_target_name
                .get(target.name())
                .copied()
                .expect("should be found");

            Self::populate_usings_in_submodules(
                root_module_ids_by_target_name,
                modules,
                root_module_id,
                target,
                handler,
            );
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
        Self::target_name_duplication_check(&targets)?;

        let (root_module_ids_by_target_name, mut modules) = Self::create_modules(&targets);

        Self::populate_usings_in_targets(
            &root_module_ids_by_target_name,
            &mut modules,
            &targets,
            handler,
        );

        todo!()
    }
}
