//! Contains the definition of [`Table`]

use getset::Getters;

use crate::{
    arena::Arena,
    symbol::{Constant, Enum, Function, Module, Struct, Trait, Type},
};

/// Contains all the symbols and information defined in the target.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Table {
    /// The modules defined in the target.
    #[get = "pub"]
    modules: Arena<Module>,

    /// The structs defined in the target.
    #[get = "pub"]
    structs: Arena<Struct>,

    /// The enums defined in the target.
    #[get = "pub"]
    enums: Arena<Enum>,

    /// The types defined in the target.
    #[get = "pub"]
    types: Arena<Type>,

    /// The functions defined in the target.
    #[get = "pub"]
    functions: Arena<Function>,

    /// The constants defined in the target.
    #[get = "pub"]
    constants: Arena<Constant>,

    /// The traits defined in the target.
    #[get = "pub"]
    traits: Arena<Trait>,
}

macro_rules! index {
    ($field:ident, $struct_name:ident) => {
        impl std::ops::Index<crate::arena::ID<$struct_name>> for Table {
            type Output = crate::arena::Item<$struct_name>;

            fn index(&self, id: crate::arena::ID<$struct_name>) -> &Self::Output {
                &self.$field[id]
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
