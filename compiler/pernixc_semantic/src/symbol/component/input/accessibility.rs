//! Contains the definition of [`Accessibility`] and its implementation for
//! components.

use std::collections::HashMap;

use paste::paste;
use serde::{Deserialize, Serialize};

use crate::{
    arena::ID,
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
    Scoped(ID<Module>),
}

macro_rules! impl_accessibility {
    ($($name:ident),*) => {
        paste! {
            #[derive(Debug, Clone, Default, Serialize, Deserialize)]
            pub(super) struct Map {
                $(
                    pub(super) [< $name:snake s >]:
                        HashMap<Global<ID<$name>>, Accessibility>,
                )*
            }

            $(
                impl super::Input<Accessibility> for $name {
                    type Requirement = super::Required;

                    fn get_map(
                        representation: &super::Map,
                    ) -> &HashMap<Global<ID<Self>>, Accessibility> {
                        &representation.accessibility.[< $name:snake s >]
                    }

                    fn get_map_mut(
                        representation: &mut super::Map,
                    ) -> &mut HashMap<Global<ID<Self>>, Accessibility> {
                        &mut representation.accessibility.[< $name:snake s >]
                    }
                }
            )*
        }
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