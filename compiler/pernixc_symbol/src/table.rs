use pernixc_system::arena::{self, Arena};

use crate::{
    AssociatedType, Enum, EnumVariant, Field, Function, Global, GlobalID, Implements,
    LifetimeParameter, Module, Parameter, Struct, Trait, TraitFunction, TypeAlias, TypeParameter,
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
}
