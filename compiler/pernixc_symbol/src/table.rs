//! Contains the definition of [`Table`], the symbol table of the compiler.
use pernixc_system::arena::{self, Arena};

use crate::{
    AssociatedType, Enum, EnumVariant, Field, Function, Genericable, GenericableID, Global,
    GlobalID, Implements, LifetimeParameter, Module, Parameter, Scoped, ScopedID, Struct, Symbol,
    Trait, TraitFunction, TypeAlias, TypeParameter, ID, ImplementsFunction, ImplementsType,
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
            GenericableID::ImplementsFunction(s) => self.implements_functions.get(s).map(|x| x as _),
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
