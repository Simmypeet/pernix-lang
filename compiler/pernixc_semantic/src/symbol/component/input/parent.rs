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
        Global, ImplementationID, Marker, Module, ModuleMemberID,
        NegativeMarkerImplementation, NegativeTraitImplementation,
        PositiveMarkerImplementation, PositiveTraitImplementation, Struct,
        Trait, TraitConstant, TraitFunction, TraitImplementationConstant,
        TraitImplementationFunction, TraitImplementationMemberID,
        TraitImplementationType, TraitMemberID, TraitType, Type, Variant,
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

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub(super) struct Map {
    pub(super) module_members:
        HashMap<Global<ModuleMemberID>, Parent<ID<Module>>>,
    pub(super) variants: HashMap<Global<ID<Variant>>, Parent<ID<Enum>>>,
    pub(super) trait_members: HashMap<Global<TraitMemberID>, Parent<ID<Trait>>>,
    pub(super) implementations:
        HashMap<Global<ImplementationID>, Parent<ID<Module>>>,
    pub(super) trait_implementation_members: HashMap<
        Global<TraitImplementationMemberID>,
        Parent<ID<PositiveTraitImplementation>>,
    >,
    pub(super) adt_implementation_members: HashMap<
        Global<ID<AdtImplementationFunction>>,
        Parent<ID<AdtImplementation>>,
    >,
}

macro_rules! impl_parent {
    ($(($name:ident, $id:ty, $map:ident, $syn:ty, $requirement:ty)),*) => {
        paste! {
            $(
                impl super::Input<Parent<$syn>> for $name {
                    type Requirement = $requirement;
                    type ID = $id;

                    fn get_map(
                        representation: &super::Map,
                    ) -> &HashMap<Global<Self::ID>, Parent<$syn>> {
                        &representation.parent.$map
                    }

                    fn get_map_mut(
                        representation: &mut super::Map,
                    ) -> &mut HashMap<Global<Self::ID>, Parent<$syn>> {
                        &mut representation.parent.$map
                    }
                }
            )*
        }
    };
}

impl_parent!(
    (Module, ModuleMemberID, module_members, ID<Module>, Optional),
    (Struct, ModuleMemberID, module_members, ID<Module>, Required),
    (Trait, ModuleMemberID, module_members, ID<Module>, Required),
    (Enum, ModuleMemberID, module_members, ID<Module>, Required),
    (Type, ModuleMemberID, module_members, ID<Module>, Required),
    (Constant, ModuleMemberID, module_members, ID<Module>, Required),
    (Function, ModuleMemberID, module_members, ID<Module>, Required),
    (Marker, ModuleMemberID, module_members, ID<Module>, Required),
    (Variant, ID<Variant>, variants, ID<Enum>, Required),
    (TraitType, TraitMemberID, trait_members, ID<Trait>, Required),
    (TraitFunction, TraitMemberID, trait_members, ID<Trait>, Required),
    (TraitConstant, TraitMemberID, trait_members, ID<Trait>, Required),
    (
        PositiveTraitImplementation,
        ImplementationID,
        implementations,
        ID<Module>,
        Required
    ),
    (
        NegativeTraitImplementation,
        ImplementationID,
        implementations,
        ID<Module>,
        Required
    ),
    (
        PositiveMarkerImplementation,
        ImplementationID,
        implementations,
        ID<Module>,
        Required
    ),
    (
        NegativeMarkerImplementation,
        ImplementationID,
        implementations,
        ID<Module>,
        Required
    ),
    (
        AdtImplementation,
        ImplementationID,
        implementations,
        ID<Module>,
        Required
    ),
    (
        TraitImplementationFunction,
        TraitImplementationMemberID,
        trait_implementation_members,
        ID<PositiveTraitImplementation>,
        Required
    ),
    (
        TraitImplementationConstant,
        TraitImplementationMemberID,
        trait_implementation_members,
        ID<PositiveTraitImplementation>,
        Required
    ),
    (
        TraitImplementationType,
        TraitImplementationMemberID,
        trait_implementation_members,
        ID<PositiveTraitImplementation>,
        Required
    ),
    (
        AdtImplementationFunction,
        ID<AdtImplementationFunction>,
        adt_implementation_members,
        ID<AdtImplementation>,
        Required
    )
);
