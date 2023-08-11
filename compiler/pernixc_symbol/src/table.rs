//! Contains the definition of [`Table`], the symbol table of the compiler.

use std::{collections::HashMap, hash::Hash};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_syntax::syntax_tree::target::Target;
use pernixc_system::{
    arena::{self, Arena},
    diagnostic::Handler,
};

use crate::{
    error, ty, Accessibility, Enum, EnumVariant, Field, Function, GenericParameters, Genericable,
    GenericableID, Global, GlobalID, Implements, ImplementsFunction, ImplementsType,
    LifetimeArgument, LifetimeParameter, Module, Parameter, Scoped, ScopedID, Struct, Substitution,
    Symbol, Trait, TraitBound, TraitFunction, TraitType, Type, TypeParameter, WhereClause, ID,
};

mod core;
mod drafting;
mod finalizing;
mod module;

pub mod resolution;

/// Represents a symbol table of the compiler.
#[derive(Debug, Clone, Getters)]
pub struct Table {
    /// Contains all the module symbols in the table
    #[get = "pub"]
    modules: Arena<Module>,

    /// Contains all the struct symbols in the table
    #[get = "pub"]
    structs: Arena<Struct>,

    /// Contains all the enum symbols in the table
    #[get = "pub"]
    enums: Arena<Enum>,

    /// Contains all the enum variant symbols in the table
    #[get = "pub"]
    enum_variants: Arena<EnumVariant>,

    /// Contains all the function symbols in the table
    #[get = "pub"]
    functions: Arena<Function>,

    /// Contains all the type symbols in the table
    #[get = "pub"]
    types: Arena<Type>,

    /// Contains all the field symbols in the table
    #[get = "pub"]
    fields: Arena<Field>,

    /// Contains all the function parameter symbols in the table
    #[get = "pub"]
    function_parameters: Arena<Parameter<Function>>,

    /// Contains all the trait function parameter symbols in the table
    #[get = "pub"]
    trait_function_parameters: Arena<Parameter<TraitFunction>>,

    /// Contains all the implements function parameter symbols in the table
    #[get = "pub"]
    implements_function_parameters: Arena<Parameter<ImplementsFunction>>,

    /// Contains all the trait symbols in the table
    #[get = "pub"]
    traits: Arena<Trait>,

    /// Contains all the trait type symbols in the table.
    #[get = "pub"]
    trait_types: Arena<TraitType>,

    /// Contains all the type parameter symbols in the table.
    #[get = "pub"]
    type_parameters: Arena<TypeParameter>,

    /// Contains all the lifetime parameter symbols in the table.
    #[get = "pub"]
    lifetime_parameters: Arena<LifetimeParameter>,

    /// Contains all the trait function symbols in the table.
    #[get = "pub"]
    trait_functions: Arena<TraitFunction>,

    /// Contains all the implements symbols in the table.
    #[get = "pub"]
    implements: Arena<Implements>,

    /// Contains all the implements type symbols in the table.
    #[get = "pub"]
    implements_types: Arena<ImplementsType>,

    /// Contains all the implements function symbols in the table.
    #[get = "pub"]
    implements_functions: Arena<ImplementsFunction>,

    /// Maps the target root module name to its [`arena::ID<Module>`].
    #[get = "pub"]
    target_root_module_ids_by_name: HashMap<String, arena::ID<Module>>,
}

/// Represents a trait implemented by [`Table`] to access the symbols in it.
pub trait Access<T> {
    /// Gets a reference to the symbol from the given ID.
    fn get(&self, id: arena::ID<T>) -> Option<&arena::Symbol<T>>;

    /// Gets a mutable reference to the symbol from the given ID.
    fn get_mut(&mut self, id: arena::ID<T>) -> Option<&mut arena::Symbol<T>>;
}

macro_rules! impl_access {
    ($symbol_ty:path, $container_name:ident) => {
        impl Access<$symbol_ty> for Table {
            fn get(&self, id: arena::ID<$symbol_ty>) -> Option<&arena::Symbol<$symbol_ty>> {
                self.$container_name.get(id)
            }

            fn get_mut(
                &mut self,
                id: arena::ID<$symbol_ty>,
            ) -> Option<&mut arena::Symbol<$symbol_ty>> {
                self.$container_name.get_mut(id)
            }
        }
    };
}

impl_access!(Module, modules);
impl_access!(Struct, structs);
impl_access!(Enum, enums);
impl_access!(EnumVariant, enum_variants);
impl_access!(Function, functions);
impl_access!(Type, types);
impl_access!(Field, fields);
impl_access!(Parameter<Function>, function_parameters);
impl_access!(Parameter<TraitFunction>, trait_function_parameters);
impl_access!(
    Parameter<ImplementsFunction>,
    implements_function_parameters
);
impl_access!(Trait, traits);
impl_access!(TraitType, trait_types);
impl_access!(TypeParameter, type_parameters);
impl_access!(LifetimeParameter, lifetime_parameters);
impl_access!(TraitFunction, trait_functions);
impl_access!(Implements, implements);
impl_access!(ImplementsType, implements_types);
impl_access!(ImplementsFunction, implements_functions);

impl Table {
    /// Gets a reference to the [`Global`] symbol from the given ID.
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
            ID::FunctionParameter(s) => self.function_parameters.get(s).map(|x| x as _),
            ID::TraitFunctionParameter(s) => self.trait_function_parameters.get(s).map(|x| x as _),
            ID::ImplementsFunctionParameter(s) => {
                self.implements_function_parameters.get(s).map(|x| x as _)
            }
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
#[error("target name duplication detected")]
pub struct TargetDuplicationError {
    /// The duplicated target name
    pub duplication_target_names: Vec<String>,
}

/// Is an error when found a target named `@core`, which is reserved for the core module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[error("found a target named `@core`, which is reserved for the core module.")]
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

/// Is an error returned by various methods in the [`Table`].
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error, From, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Error {
    #[error("encountered a fatal semantic error")]
    FatalSemantic,

    #[error("the given symbol IDs were invalid")]
    InvalidID,
}

impl Table {
    /// Gets a string representation of the given [`ty::Type`].
    ///
    /// # Arguments
    /// - `ty`: The type to get the string representation of.
    #[must_use]
    pub fn get_type_string(&self, ty: &ty::Type) -> Option<String> {
        match ty {
            ty::Type::Enum(id) => self.get_qualified_name((*id).into()),
            ty::Type::Struct(struct_ty) => self.get_qualified_name_with_substitution(
                struct_ty.struct_id.into(),
                &struct_ty.substitution,
            ),
            ty::Type::Primitive(ty) => Some(
                match ty {
                    ty::Primitive::Float32 => "float32",
                    ty::Primitive::Float64 => "float64",
                    ty::Primitive::Int8 => "int8",
                    ty::Primitive::Int16 => "int16",
                    ty::Primitive::Int32 => "int32",
                    ty::Primitive::Int64 => "int64",
                    ty::Primitive::Uint8 => "uint8",
                    ty::Primitive::Uint16 => "uint16",
                    ty::Primitive::Uint32 => "uint32",
                    ty::Primitive::Uint64 => "uint64",
                    ty::Primitive::Bool => "bool",
                    ty::Primitive::Void => "void",
                }
                .to_string(),
            ),
            ty::Type::Reference(reference_ty) => {
                let mut string = self.get_type_string(&reference_ty.operand)?;

                if let Some(qualifier) = reference_ty.qualifier {
                    let qualififer_string = match qualifier {
                        ty::ReferenceQualifier::Mutable => "mutable ",
                        ty::ReferenceQualifier::Restrict => "restrict ",
                    };
                    string.insert_str(0, qualififer_string);
                }

                if let Some(lifetime_argument) = reference_ty.lifetime_argument {
                    match lifetime_argument {
                        LifetimeArgument::Static => string.insert_str(0, "'static "),
                        LifetimeArgument::Parameter(lifetime_parameter) => string.insert_str(
                            0,
                            &format!(
                                "'{} ",
                                self.lifetime_parameters.get(lifetime_parameter)?.name()
                            ),
                        ),
                    };
                }

                string.insert(0, '&');

                Some(string)
            }
            ty::Type::Parameter(parameter) => {
                Some(self.type_parameters.get(*parameter)?.name().to_string())
            }
            ty::Type::TraitType(trait_ty) => {
                let trait_path = self.get_qualified_name_with_substitution(
                    self.trait_types
                        .get(trait_ty.trait_type_id)?
                        .parent_trait_id
                        .into(),
                    &trait_ty.trait_substitution,
                )?;

                let type_part = self.get_qualified_part_string(
                    trait_ty.trait_type_id.into(),
                    &trait_ty.trait_type_substitution,
                )?;

                Some(format!("{trait_path}::{type_part}"))
            }
        }
    }

    #[must_use]
    fn get_qualified_part_string(
        &self,
        global_id: GlobalID,
        substitution: &Substitution,
    ) -> Option<String> {
        let mut string = self.get_global(global_id)?.name().to_string();

        let Ok(genericable_id) = GenericableID::try_from(global_id) else {
            return Some(string);
        };

        let genericable = self.get_genericable(genericable_id)?;

        let parameter_has_lifetiems = genericable
            .generic_parameters()
            .lifetime_parameter_order
            .iter()
            .copied()
            .any(|x| {
                substitution
                    .lifetime_arguments_by_parameter
                    .contains_key(&x)
            });
        let substitution_has_lifetime_arguments =
            !substitution.lifetime_arguments_by_parameter.is_empty();

        let has_type_parameter = !genericable
            .generic_parameters()
            .type_parameter_order
            .is_empty();

        if (parameter_has_lifetiems && substitution_has_lifetime_arguments) || has_type_parameter {
            string.push('<');
            let mut first = true;

            if parameter_has_lifetiems && substitution_has_lifetime_arguments {
                for lifetime_parameter in &genericable.generic_parameters().lifetime_parameter_order
                {
                    if !first {
                        string.push_str(", ");
                    }

                    first = false;

                    let lifetime_argument = substitution
                        .lifetime_arguments_by_parameter
                        .get(lifetime_parameter)?;

                    if lifetime_argument != &LifetimeArgument::Parameter(*lifetime_parameter) {
                        string.push('\'');
                        string.push_str(match lifetime_argument {
                            LifetimeArgument::Static => "static",
                            LifetimeArgument::Parameter(parameter) => {
                                self.lifetime_parameters.get(*parameter)?.name()
                            }
                        });
                    }
                }
            }

            if has_type_parameter {
                for type_parameter in &genericable.generic_parameters().type_parameter_order {
                    if !first {
                        string.push_str(", ");
                    }

                    first = false;

                    let type_argument = substitution
                        .type_arguments_by_parameter
                        .get(type_parameter)?;

                    string.push_str(&self.get_type_string(type_argument)?);
                }
            }

            string.push('>');
        }

        Some(string)
    }

    /// Returns the qualified name of the given global ID with the given substitution applied.
    #[must_use]
    pub fn get_qualified_name_with_substitution(
        &self,
        mut global_id: GlobalID,
        substitution: &Substitution,
    ) -> Option<String> {
        let mut current_name = self.get_qualified_part_string(global_id, substitution)?;

        while let Some(parent_symbol_id) = self.get_global(global_id)?.parent_symbol() {
            let parent_global_symbol_id: GlobalID = parent_symbol_id
                .try_into()
                .expect("Parent symbol ID returned by `Global` must belong to `GlobalID`");

            current_name.insert_str(0, "::");
            current_name.insert_str(
                0,
                &self.get_qualified_part_string(parent_global_symbol_id, substitution)?,
            );

            global_id = parent_global_symbol_id;
        }

        Some(current_name)
    }

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
    #[must_use]
    pub fn get_accessibility(&self, global_id: GlobalID) -> Option<Accessibility> {
        match global_id {
            GlobalID::Module(module_id) => Some(self.modules.get(module_id)?.accessibility),
            GlobalID::Struct(struct_id) => Some(self.structs.get(struct_id)?.accessibility),
            GlobalID::Enum(enum_id) => Some(self.enums.get(enum_id)?.accessibility),
            // Accessibility of enum variants is the same as the parent enum.
            GlobalID::EnumVariant(enum_variant_id) => Some(
                self.enums[self.enum_variants.get(enum_variant_id)?.parent_enum_id].accessibility,
            ),
            GlobalID::Function(function_id) => Some(self.functions.get(function_id)?.accessibility),
            GlobalID::Type(type_id) => Some(self.types.get(type_id)?.accessibility),
            GlobalID::Trait(trait_id) => Some(self.traits.get(trait_id)?.accessibility),
            GlobalID::TraitFunction(trait_function_id) => Some(
                self.traits[self.trait_functions.get(trait_function_id)?.parent_trait_id]
                    .accessibility,
            ),
            GlobalID::TraitType(trait_type_id) => Some(
                self.traits[self.trait_types.get(trait_type_id)?.parent_trait_id].accessibility,
            ),
        }
    }

    /// Checks if the given [`GlobalID`] symbol is accessible from the given [`ScopedID`] symbol.
    #[must_use]
    pub fn symbol_accessible(
        &self,
        referred_global_id: GlobalID,
        referring_site: ID,
    ) -> Option<bool> {
        match self.get_accessibility(referred_global_id)? {
            // Private symbol is only accessible from the same module or its children.
            Accessibility::Private => {
                let referred_global_module_id =
                    self.get_closet_module_id(referred_global_id.into())?;
                let referrer_module_id = self.get_closet_module_id(referring_site)?;

                // if same module, it is accessible.
                if referrer_module_id == referred_global_module_id {
                    return Some(true);
                }

                let mut current_referrer_parent_id = referrer_module_id.into();

                while let Some(parent_id) = self
                    .get_symbol(current_referrer_parent_id)
                    .unwrap()
                    .parent_symbol()
                {
                    match parent_id {
                        ID::Module(module_id) if module_id == referred_global_module_id => {
                            return Some(true);
                        }
                        _ => {
                            current_referrer_parent_id = parent_id;
                        }
                    }
                }

                Some(false)
            }

            // Both symbols are in the same module.
            Accessibility::Internal => Some(
                self.get_parent_target_root_module_id(referred_global_id.into())?
                    == self.get_parent_target_root_module_id(referring_site)?,
            ),

            // Public symbols are always accessible.
            Accessibility::Public => {
                // PEDANTIC: check if the referrer also a valid id.
                self.get_symbol(referring_site)?;

                Some(true)
            }
        }
    }

    /// Gets the target root module ID where the given symbol is defined in (including itself).
    #[must_use]
    pub fn get_parent_target_root_module_id(&self, mut id: ID) -> Option<arena::ID<Module>> {
        while let Some(parent_id) = self.get_symbol(id)?.parent_symbol() {
            id = parent_id;
        }

        Some(
            id.into_module()
                .expect("It should be a module at the root."),
        )
    }

    /// Gets the module ID that contains the given symbol ID (including itself).
    #[must_use]
    pub fn get_closet_module_id(&self, mut id: ID) -> Option<arena::ID<Module>> {
        loop {
            // If the ID is a module ID, return it.
            if let ID::Module(module_id) = id {
                return Some(module_id);
            }

            let Some(parent_id) = self.get_symbol(id)?.parent_symbol() else {
                panic!("should have found parent module ID already!");
            };

            id = parent_id;
        }
    }

    /// Gets the scoped ID that contains the given symbol ID (including itself).
    #[must_use]
    pub fn get_closet_scoped_id(&self, mut id: ID) -> Option<ScopedID> {
        loop {
            if let Ok(scoped_id) = id.try_into() {
                return Some(scoped_id);
            }

            let Some(parent_id) = self.get_symbol(id)?.parent_symbol() else {
                panic!("should have found parent module ID already!");
            };

            id = parent_id;
        }
    }

    /// Gets the [`ScopeWalker`] iterator for the given ID.
    #[must_use]
    pub fn scope_walker(&self, id: ID) -> Option<ScopeWalker> {
        self.get_symbol(id)?;
        Some(ScopeWalker {
            table: self,
            current_id: Some(id),
        })
    }

    /// Returns a vector of parent scope IDs for the given ID.
    ///
    /// The order for the vector is in the id-to-parent order (including the given ID itself).
    #[must_use]
    pub fn id_to_parent_scopes_vec(&self, id: ID) -> Option<Vec<ID>> {
        Some(self.scope_walker(id)?.collect())
    }

    /// Returns a vector of parent scope IDs for the given ID.
    ///
    /// The order for the vector is in the parent-to-id order (including the given ID itself).
    #[must_use]
    pub fn parent_to_id_scopes_vec(&self, id: ID) -> Option<Vec<ID>> {
        let mut vec = self.id_to_parent_scopes_vec(id)?;
        vec.reverse();
        Some(vec)
    }

    /// Gets the active [`WhereClause`] started from the given ID.
    #[must_use]
    pub fn get_active_where_clause(&self, id: ID) -> Option<WhereClause> {
        let scope_walker = self.scope_walker(id)?;
        let mut result_where_clause = WhereClause::default();

        for id in scope_walker {
            let Ok(genericable_id) = GenericableID::try_from(id) else {
                continue;
            };

            match genericable_id {
                GenericableID::Implements(implements_id) => {
                    if !self.implements[implements_id]
                        .substitution
                        .is_concrete_substitution()
                    {
                        result_where_clause.trait_bounds.insert(TraitBound {
                            trait_id: self.implements[implements_id].trait_id,
                            substitution: self.implements[implements_id].substitution.clone(),
                        });
                    }
                }
                GenericableID::Trait(trait_id) => {
                    result_where_clause.trait_bounds.insert(TraitBound {
                        trait_id,
                        substitution: Self::create_identical_substitution(
                            &self.traits[trait_id].generics.parameters,
                        ),
                    });
                }
                _ => {}
            }

            let Some(where_clause) = self.get_genericable(genericable_id)?.where_clause() else {
                continue;
            };

            Self::merge_where_caluse(&mut result_where_clause, where_clause);
        }

        Some(result_where_clause)
    }

    fn create_identical_substitution(generic_parameters: &GenericParameters) -> Substitution {
        let mut substitution = Substitution::default();

        for lifetime_parameter in generic_parameters
            .lifetime_parameter_ids_by_name
            .values()
            .copied()
        {
            substitution.lifetime_arguments_by_parameter.insert(
                lifetime_parameter,
                LifetimeArgument::Parameter(lifetime_parameter),
            );
        }

        for type_parameter in generic_parameters
            .type_parameter_ids_by_name
            .values()
            .copied()
        {
            substitution
                .type_arguments_by_parameter
                .insert(type_parameter, ty::Type::Parameter(type_parameter));
        }

        substitution
    }

    fn merge_where_caluse(merge_where_clause: &mut WhereClause, where_clause: &WhereClause) {
        for (lifetime_parameter, lifetime_argument_set) in
            &where_clause.lifetime_argument_sets_by_lifetime_parameter
        {
            let result_lifetime_argument_set = merge_where_clause
                .lifetime_argument_sets_by_lifetime_parameter
                .entry(*lifetime_parameter)
                .or_default();

            result_lifetime_argument_set.extend(lifetime_argument_set);
        }

        for (ty, lifetime_argument_set) in &where_clause.lifetime_argument_sets_by_type {
            let result_lifetime_argument_set = merge_where_clause
                .lifetime_argument_sets_by_type
                .entry(ty.clone())
                .or_default();

            result_lifetime_argument_set.extend(lifetime_argument_set);
        }

        merge_where_clause
            .trait_bounds
            .extend(where_clause.trait_bounds.iter().cloned());

        for (trait_type, lifetime_argument_set) in &where_clause.types_by_trait_type {
            assert!(
                merge_where_clause
                    .types_by_trait_type
                    .insert(trait_type.clone(), lifetime_argument_set.clone())
                    .is_none(),
                "should not have duplicated trait types in the where clause!"
            );
        }
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
            function_parameters: Arena::new(),
            trait_function_parameters: Arena::new(),
            implements_function_parameters: Arena::new(),
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
        let mut states = table.draft_symbols(targets, handler);

        while let Some(id) = states.next_drafted_symbol() {
            table.finalize_symbol(id, &mut states, handler);
        }

        Ok(table)
    }
}

/// Represents an iterator that walks through the scope of the given symbol. It goes through all
/// the parent symbols until it reaches the root.
///
/// The iterator iterates through the scope in id-to-parent order.
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
mod input;
#[cfg(test)]
mod tests;
