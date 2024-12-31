//! Contains the definition of [`Parent`] and its implementation for
//! components.

use std::collections::HashMap;

use paste::paste;
use serde::{Deserialize, Serialize};

use super::{Optional, Required};
use crate::{
    arena::ID,
    symbol::{
        AdtImplementation, AdtImplementationFunction, Constant, Enum, Function,
        Global, Marker, Module, NegativeMarkerImplementation,
        NegativeTraitImplementation, PositiveMarkerImplementation,
        PositiveTraitImplementation, Struct, Trait, TraitConstant,
        TraitFunction, TraitImplementationConstant,
        TraitImplementationFunction, TraitImplementationType, TraitType, Type,
        Variant,
    },
};

/// Used for storing the information of the parent of a symbol.
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
    derive_more::Deref,
    derive_more::DerefMut,
)]
pub struct Parent<ID>(pub ID);

macro_rules! impl_parent {
    ($(($name:ident, $syn:ty, $requirement:ty)),*) => {
        paste! {
            #[derive(Debug, Clone, Serialize, Deserialize, Default)]
            pub(super) struct Map {
                $(
                    pub(super) [< $name:snake s >]:
                        HashMap<Global<ID<$name>>, Parent<$syn>>,
                )*
            }

            $(
                impl super::Input<Parent<$syn>> for $name {
                    type Requirement = $requirement;

                    fn get_map(
                        representation: &super::Map,
                    ) -> &HashMap<Global<ID<Self>>, Parent<$syn>> {
                        &representation.parent.[< $name:snake s >]
                    }

                    fn get_map_mut(
                        representation: &mut super::Map,
                    ) -> &mut HashMap<Global<ID<Self>>, Parent<$syn>> {
                        &mut representation.parent.[< $name:snake s >]
                    }
                }
            )*
        }
    };
}

impl_parent!(
    (Module, ID<Module>, Optional),
    (Struct, ID<Module>, Required),
    (Trait, ID<Module>, Required),
    (Enum, ID<Module>, Required),
    (Type, ID<Module>, Required),
    (Constant, ID<Module>, Required),
    (Function, ID<Module>, Required),
    (Variant, ID<Enum>, Required),
    (TraitType, ID<Trait>, Required),
    (TraitFunction, ID<Trait>, Required),
    (TraitConstant, ID<Trait>, Required),
    (PositiveTraitImplementation, ID<Module>, Required),
    (NegativeTraitImplementation, ID<Module>, Required),
    (TraitImplementationFunction, ID<PositiveTraitImplementation>, Required),
    (TraitImplementationConstant, ID<PositiveTraitImplementation>, Required),
    (TraitImplementationType, ID<PositiveTraitImplementation>, Required),
    (AdtImplementation, ID<Module>, Required),
    (AdtImplementationFunction, ID<AdtImplementation>, Required),
    (Marker, ID<Module>, Required),
    (PositiveMarkerImplementation, ID<Module>, Required),
    (NegativeMarkerImplementation, ID<Module>, Required)
);