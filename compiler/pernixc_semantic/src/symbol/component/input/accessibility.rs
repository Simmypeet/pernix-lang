//! Contains the definition of [`Accessibility`] and its implementation for
//! components.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    arena,
    symbol::{
        AdtImplementationFunction, Constant, Enum, Function, Global, Marker,
        Module, Struct, Trait, TraitConstant, TraitFunction, TraitType, Type,
    },
};

/// Represents an accessibility of a symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub enum Accessibility {
    /// The symbol is accessible from anywhere.
    #[default]
    Public,

    /// The symbol is accessible from the given module and its children.
    Scoped(arena::ID<Module>),
}

macro_rules! impl_accessibility {
    ($($name:ident),*) => {
        /// An enumeration of all IDs that have accessibility.
        #[derive(
            Debug,
            Clone,
            Copy,
            PartialEq,
            Eq,
            PartialOrd,
            Ord,
            Hash,
            Serialize,
            Deserialize,
            derive_more::From
        )]
        #[allow(missing_docs)]
        pub enum ID {
            $(
                $name(arena::ID<$name>),
            )*
        }

        #[derive(
            Debug,
            Clone,
            Default,
            Serialize,
            Deserialize,
            derive_more::Deref,
            derive_more::DerefMut,
        )]
        pub(super) struct Map(pub HashMap<Global<ID>, Accessibility>);

        $(
            impl super::Input<Accessibility> for $name {
                type Requirement = super::Required;
                type ID = ID;

                fn get_map(
                    representation: &super::Map,
                ) -> &HashMap<Global<Self::ID>, Accessibility> {
                    &representation.accessibility.0
                }

                fn get_map_mut(
                    representation: &mut super::Map,
                ) -> &mut HashMap<Global<Self::ID>, Accessibility> {
                    &mut representation.accessibility.0
                }
            }
        )*
    };
}

impl_accessibility!(
    Module,
    Struct,
    Trait,
    Enum,
    Type,
    Constant,
    Function,
    TraitType,
    TraitFunction,
    TraitConstant,
    Marker,
    AdtImplementationFunction
);
