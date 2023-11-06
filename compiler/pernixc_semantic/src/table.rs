//! Contains the definition of [`Table`]

use getset::Getters;

use crate::{
    arena::{Arena, ID},
    symbol::{
        Constant, Enum, Field, Function, Implementation, ImplementationConstant,
        ImplementationFunction, ImplementationType, MemberID, Module, NegativeImplementation,
        Struct, Trait, TraitConstant, TraitFunction, TraitType, Type, Variant,
    },
};

/// Contains all the symbols and information defined in the target.
#[derive(Debug, Clone, PartialEq, Eq, Getters, Default)]
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

        impl pernixc_base::extension::SafeIndex<crate::arena::ID<$struct_name>> for Table {
            type Output = crate::arena::Item<$struct_name>;

            fn get(&self, id: crate::arena::ID<$struct_name>) -> Option<&Self::Output> {
                self.$field.get(id)
            }
        }
    };
}

macro_rules! index_member {
    ($field:ident, $child:ident, $parent:path) => {
        impl std::ops::Index<crate::symbol::MemberID<crate::arena::ID<$child>, $parent>> for Table {
            type Output = crate::arena::Item<$child>;

            fn index(
                &self,
                id: crate::symbol::MemberID<crate::arena::ID<$child>, $parent>,
            ) -> &Self::Output {
                &self[id.parent].$field[id.id]
            }
        }

        impl
            pernixc_base::extension::SafeIndex<
                crate::symbol::MemberID<crate::arena::ID<$child>, $parent>,
            > for Table
        {
            type Output = crate::arena::Item<$child>;

            fn get(
                &self,
                id: crate::symbol::MemberID<crate::arena::ID<$child>, $parent>,
            ) -> Option<&Self::Output> {
                self.get(id.parent)
                    .and_then(|parent| parent.$field.get(id.id))
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

index_member!(implementations, Implementation, ID<Trait>);
index_member!(negative_implementations, NegativeImplementation, ID<Trait>);

index_member!(
    types,
    ImplementationType,
    MemberID<ID<Implementation>, ID<Trait>>
);
index_member!(
    functions,
    ImplementationFunction,
    MemberID<ID<Implementation>, ID<Trait>>
);
index_member!(
    constants,
    ImplementationConstant,
    MemberID<ID<Implementation>, ID<Trait>>
);

index_member!(types, TraitType, ID<Trait>);
index_member!(functions, TraitFunction, ID<Trait>);
index_member!(constants, TraitConstant, ID<Trait>);

index_member!(variants, Variant, ID<Enum>);
index_member!(fields, Field, ID<Struct>);
