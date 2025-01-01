//! Contains the definition of [`Implemented`] and its implementation for
//! components.

use std::collections::HashMap;

use paste::paste;
use serde::{Deserialize, Serialize};

use crate::{
    arena::ID,
    symbol::{
        AdtID, AdtImplementation, Global, Marker, NegativeMarkerImplementation,
        NegativeTraitImplementation, PositiveMarkerImplementation,
        PositiveTraitImplementation, Trait,
    },
};

/// Used for storing the information of which ID is being implemented by the
/// current implementation.
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
pub struct Implements<ID>(pub ID);

macro_rules! impl_implemented {
    ($(($name:ident, $id:ty)),*) => {
        paste! {
            #[derive(Debug, Clone, Default, Serialize, Deserialize)]
            pub(super) struct Map {
                $(
                    pub(super) [< $name:snake s >]:
                        HashMap<Global<ID<$name>>, Implements<$id>>,
                )*
            }

            $(
                impl super::Input<Implements<$id>> for $name {
                    type Requirement = super::Required;

                    fn get_map(
                        representation: &super::Map,
                    ) -> &HashMap<Global<ID<Self>>, Implements<$id>> {
                        &representation.implemented.[< $name:snake s >]
                    }

                    fn get_map_mut(
                        representation: &mut super::Map,
                    ) -> &mut HashMap<Global<ID<Self>>, Implements<$id>> {
                        &mut representation.implemented.[< $name:snake s >]
                    }
                }
            )*
        }
    };
}

impl_implemented!(
    (PositiveTraitImplementation, ID<Trait>),
    (NegativeTraitImplementation, ID<Trait>),
    (PositiveMarkerImplementation, ID<Marker>),
    (NegativeMarkerImplementation, ID<Marker>),
    (AdtImplementation, AdtID)
);
