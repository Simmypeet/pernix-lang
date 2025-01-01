//! Contains the definition of [`Implemented`] and its implementation for
//! components.

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use paste::paste;
use serde::{Deserialize, Serialize};

use crate::{
    arena::ID,
    symbol::{
        AdtID, AdtImplementation, Enum, Global, Marker, MarkerImplementationID,
        Struct, Trait, TraitImplementationID,
    },
};

/// Used for storing the information of which ID is being implemented by the
/// current implementation.
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
pub struct Implemented<ID: Eq + Hash>(pub HashSet<Global<ID>>);

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(super) struct Map {
    pub(super) traits:
        HashMap<Global<ID<Trait>>, Implemented<TraitImplementationID>>,
    pub(super) markers:
        HashMap<Global<ID<Marker>>, Implemented<MarkerImplementationID>>,
    pub(super) adts: HashMap<Global<AdtID>, Implemented<ID<AdtImplementation>>>,
}

macro_rules! impl_implemented {
    (
        $(
            ($name:ident, $id:ty, $impl_id:ty, $map_name:ident),
        )*
    ) => {
        paste! {
            $(
                impl super::Input<Implemented<$impl_id>> for $name {
                    type Requirement = super::Required;
                    type ID = $id;

                    fn get_map(
                        representation: &super::Map,
                    ) -> &HashMap<Global<Self::ID>, Implemented<$impl_id>> {
                        &representation.implemented.$map_name
                    }

                    fn get_map_mut(
                        representation: &mut super::Map,
                    ) -> &mut HashMap<Global<Self::ID>, Implemented<$impl_id>> {
                        &mut representation.implemented.$map_name
                    }
                }
            )*
        }
    };
}

impl_implemented!(
    (Trait, ID<Trait>, TraitImplementationID, traits),
    (Marker, ID<Marker>, MarkerImplementationID, markers),
    (Struct, AdtID, ID<AdtImplementation>, adts),
    (Enum, AdtID, ID<AdtImplementation>, adts),
);
