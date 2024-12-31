//! Contains the definition of [`Accessibility`] and its implementation for
//! components.

use std::collections::HashMap;

use paste::paste;
use serde::{Deserialize, Serialize};

use crate::{
    arena::ID,
    symbol::{
        component::{Input, Required},
        table, AdtImplementationFunction, Constant, Enum, Function, Global,
        Marker, Module, Struct, Trait, TraitConstant, TraitFunction, TraitType,
        Type,
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
            pub(in crate::symbol) struct Map {
                $(
                    [< $name:snake s >]:
                        HashMap<Global<ID<$name>>, Accessibility>,
                )*
            }

            $(
                impl Input<Accessibility> for $name {
                    type Requirement = Required;

                    fn get_map(
                        representation: &table::Input,
                    ) -> &HashMap<Global<ID<Self>>, Accessibility> {
                        &representation.accessibility_map.[< $name:snake s >]
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
