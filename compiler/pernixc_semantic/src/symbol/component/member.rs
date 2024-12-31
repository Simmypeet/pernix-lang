//! Contains the definition of [`Member`] and its implementation for components.

use std::collections::HashMap;

use paste::paste;
use serde::{Deserialize, Serialize};

use crate::{
    arena::ID,
    symbol::{
        component::{Input, Required},
        table, Enum, Global, Module, ModuleMemberID,
        PositiveTraitImplementation, Trait, TraitImplementationMemberID,
        TraitMemberID, Variant,
    },
};

/// Used for representing the symbol that contains the members.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    derive_more::Deref,
    derive_more::DerefMut,
)]
pub struct Member<ID>(pub HashMap<String, ID>);

macro_rules! impl_member {
    ($(($name:ident, $id:ty)),*) => {
        paste! {
            #[derive(Debug, Clone, Default, Serialize, Deserialize)]
            pub(in crate::symbol) struct Map {
                $(
                    [< $name:snake s >]:
                        HashMap<Global<ID<$name>>, Member<$id>>,
                )*
            }

            $(
                impl Input<Member<$id>> for $name {
                    type Requirement = Required;

                    fn get_map(
                        representation: &table::Input
                    ) -> &HashMap<Global<ID<Self>>, Member<$id>> {
                        &representation.member_map.[< $name:snake s >]
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
    (PositiveTraitImplementation, TraitImplementationMemberID)
);
