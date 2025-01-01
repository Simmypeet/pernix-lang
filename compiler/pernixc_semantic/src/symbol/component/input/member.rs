//! Contains the definition of [`Member`] and its implementation for components.

use std::collections::HashMap;

use paste::paste;
use serde::{Deserialize, Serialize};

use crate::{
    arena::ID,
    symbol::{
        AdtImplementation, AdtImplementationFunction, Enum, Global, Module,
        ModuleMemberID, PositiveTraitImplementation, Trait,
        TraitImplementationMemberID, TraitMemberID, Variant,
    },
};

/// Used for representing the symbol that contains the members.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Serialize,
    Deserialize,
    derive_more::Deref,
    derive_more::DerefMut,
)]
pub struct Member<ID>(pub HashMap<String, ID>);

impl<ID> Default for Member<ID> {
    fn default() -> Self { Self(HashMap::default()) }
}

macro_rules! impl_member {
    ($(($name:ident, $id:ty)),*) => {
        paste! {
            #[derive(Debug, Clone, Default, Serialize, Deserialize)]
            pub(super) struct Map {
                $(
                    pub(super) [< $name:snake s >]:
                        HashMap<Global<ID<$name>>, Member<$id>>,
                )*
            }

            $(
                impl super::Input<Member<$id>> for $name {
                    type Requirement = super::Required;
                    type ID = ID<Self>;

                    fn get_map(
                        representation: &super::Map
                    ) -> &HashMap<Global<ID<Self>>, Member<$id>> {
                        &representation.member.[< $name:snake s >]
                    }

                    fn get_map_mut(
                        representation: &mut super::Map
                    ) -> &mut HashMap<Global<ID<Self>>, Member<$id>> {
                        &mut representation.member.[< $name:snake s >]
                    }
                }
            )*
        }
    };
}

impl_member!(
    (Module, ModuleMemberID),
    (Trait, TraitMemberID),
    (Enum, ID<Variant>),
    (PositiveTraitImplementation, TraitImplementationMemberID),
    (AdtImplementation, ID<AdtImplementationFunction>)
);
